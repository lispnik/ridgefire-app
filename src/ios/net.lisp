;;;; src/ios/net.lisp -- phones finding each other, over MultipeerConnectivity.
;;;;
;;;; MultipeerConnectivity joins devices within Bluetooth or peer-to-peer
;;;; Wi-Fi range with no network in between, which is what makes a Nearby game
;;;; one for people in the same room.  The host advertises; a guest browses,
;;;; picks the host's game and invites it; the host accepts while its lobby has
;;;; room.  Every message is one encoded S-expression (see wire.lisp).
;;;;
;;;; The framework calls the delegate on its own queue.  Nothing here touches
;;;; the game or the screen from there: each callback is handed to the main
;;;; thread, which queues messages for the scene to take in between turns.

(in-package #:ridgefire-ios)

(defparameter +service+ "ridgefire" "Up to 15 characters: letters, digits, hyphens.")

(defvar *my-id* nil "This device's id: its MCPeerID's display name.")
(defvar *me* nil)
(defvar *session* nil)
(defvar *advertiser* nil)
(defvar *browser* nil)
(defvar *net-delegate* nil)
(defvar *host-peer* nil "A guest's host, once chosen.")
(defvar *found* '() "A guest's games in range: plists :peer :name :label.")
(defvar *inbox* '() "Messages waiting for the scene: (message . sender-id), oldest first.")
(defvar *net-events* '() "Connections and disconnections waiting: (kind . peer-id).")

(defun random-code (length)
  (let ((alphabet "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"))
    (coerce (loop repeat length collect (char alphabet (random (length alphabet)))) 'string)))

(defun ensure-my-id ()
  "This phone's id, kept across launches so that a player who drops out of a
Nearby game -- or whose app is closed -- can come back to the same tank."
  (or *my-id*
      (setf *my-id*
            (let ((stored (objc:invoke (defaults) "stringForKey:" "nearbyId")))
              (if (null-object-p stored)
                  (let ((fresh (format nil "RF-~a" (random-code 6))))
                    (objc:invoke (defaults) "setObject:forKey:" fresh "nearbyId")
                    fresh)
                  (objc:ns-string-to-string stored))))))

(defun null-object-p (object)
  (or (null object) (and (cffi:pointerp object) (cffi:null-pointer-p object))))

(defun peer-name (peer)
  (objc:ns-string-to-string (objc:invoke peer "displayName")))

(defun device-name ()
  (objc:ns-string-to-string (objc:invoke (objc:invoke "UIDevice" "currentDevice") "name")))

(defun on-main (thunk)
  (ios-app-runtime:on-main
   (lambda ()
     (handler-case (funcall thunk)
       (error (condition)
         (format *error-output* "~&ridgefire: network callback failed: ~a~%" condition)
         (finish-output *error-output*))))))

;;; ------------------------------------------------------------------
;;; bytes

(defun string-data (string)
  (objc:invoke (objc:invoke "NSString" "stringWithString:" string) "dataUsingEncoding:" 4))

(defun data-string (data)
  (objc:ns-string-to-string
   (objc:invoke (objc:invoke (objc:invoke "NSString" "alloc") "initWithData:encoding:" data 4)
                "autorelease")))

(defun send-to (peers message &key (reliable t))
  "Send MESSAGE to PEERS, an NSArray; true if it was handed to the session."
  (when (and *session* peers (plusp (objc:invoke peers "count")))
    (objc:invoke-bool *session* "sendData:toPeers:withMode:error:"
                      (string-data (encode-message message)) peers (if reliable 0 1) nil)))

(defun broadcast (message &key (reliable t))
  "Host: to every guest."
  (when *session*
    (send-to (objc:invoke *session* "connectedPeers") message :reliable reliable)))

(defun send-to-peer (device message &key (reliable t))
  "Host: to the connected phone whose id is DEVICE, if it is connected."
  (when *session*
    (let ((peers (objc:invoke *session* "connectedPeers")))
      (dotimes (i (objc:invoke peers "count"))
        (let ((peer (objc:invoke peers "objectAtIndex:" i)))
          (when (equal (peer-name peer) device)
            (return (send-to (objc:invoke "NSArray" "arrayWithObject:" peer) message
                             :reliable reliable))))))))

(defun send-host (message &key (reliable t))
  "Guest: to the host."
  (when *host-peer*
    (send-to (objc:invoke "NSArray" "arrayWithObject:" *host-peer*) message :reliable reliable)))

;;; ------------------------------------------------------------------
;;; the delegate

(objc:define-objc-class net-delegate () ()
  (:objc-class-name "RidgefireNetDelegate")
  (:objc-protocols "MCSessionDelegate" "MCNearbyServiceAdvertiserDelegate"
                   "MCNearbyServiceBrowserDelegate"))

(objc:define-objc-method ("session:peer:didChangeState:" :void)
    ((self net-delegate) (session objc:objc-object-pointer) (peer objc:objc-object-pointer)
     (state (:signed :long-long)))
  (declare (ignore session))
  (let ((name (peer-name peer)))
    (on-main (lambda ()
               (setf *net-events* (append *net-events*
                                          (list (cons (case state (2 :connected) (0 :disconnected) (t :connecting))
                                                      name))))))))

(objc:define-objc-method ("session:didReceiveData:fromPeer:" :void)
    ((self net-delegate) (session objc:objc-object-pointer) (data objc:objc-object-pointer)
     (peer objc:objc-object-pointer))
  (declare (ignore session))
  (let ((message (decode-message (data-string data)))
        (from (peer-name peer)))
    (if message
        (on-main (lambda () (setf *inbox* (append *inbox* (list (cons message from))))))
        (format *error-output* "~&ridgefire: unreadable message from ~a~%" from))))

(objc:define-objc-method ("session:didReceiveStream:withName:fromPeer:" :void)
    ((self net-delegate) (session objc:objc-object-pointer) (stream objc:objc-object-pointer)
     (name objc:objc-object-pointer) (peer objc:objc-object-pointer))
  (declare (ignore session stream name peer)))

(objc:define-objc-method ("session:didStartReceivingResourceWithName:fromPeer:withProgress:" :void)
    ((self net-delegate) (session objc:objc-object-pointer) (name objc:objc-object-pointer)
     (peer objc:objc-object-pointer) (progress objc:objc-object-pointer))
  (declare (ignore session name peer progress)))

(objc:define-objc-method ("session:didFinishReceivingResourceWithName:fromPeer:atURL:withError:" :void)
    ((self net-delegate) (session objc:objc-object-pointer) (name objc:objc-object-pointer)
     (peer objc:objc-object-pointer) (url objc:objc-object-pointer) (error objc:objc-object-pointer))
  (declare (ignore session name peer url error)))

(objc:define-objc-method ("advertiser:didReceiveInvitationFromPeer:withContext:invitationHandler:" :void)
    ((self net-delegate) (advertiser objc:objc-object-pointer) (peer objc:objc-object-pointer)
     (context objc:objc-object-pointer) (handler objc:objc-object-pointer))
  (declare (ignore advertiser context))
  ;; Answered here, on the framework's queue: the block must be called.  Only
  ;; Lisp variables are read to decide (see ACCEPT-INVITATION-P).
  (objc:call-objc-block '(:void (objc:objc-c++-bool objc:objc-object-pointer))
                        handler (accept-invitation-p (peer-name peer)) *session*))

(objc:define-objc-method ("browser:foundPeer:withDiscoveryInfo:" :void)
    ((self net-delegate) (browser objc:objc-object-pointer) (peer objc:objc-object-pointer)
     (info objc:objc-object-pointer))
  (declare (ignore browser))
  (flet ((info (key)
           (unless (null-object-p info)
             (let ((value (objc:invoke info "objectForKey:" key)))
               (unless (null-object-p value) (objc:ns-string-to-string value))))))
    (let ((label (or (info "label") (peer-name peer)))
          (playing (equal (info "playing") "1"))
          (name (peer-name peer)))
      (objc:invoke peer "retain")
      (on-main (lambda ()
                 (setf *found* (append (remove name *found* :key (lambda (f) (getf f :name)) :test #'string=)
                                       (list (list :peer peer :name name :label label :playing playing))))
                 (found-changed))))))

(objc:define-objc-method ("browser:lostPeer:" :void)
    ((self net-delegate) (browser objc:objc-object-pointer) (peer objc:objc-object-pointer))
  (declare (ignore browser))
  (let ((name (peer-name peer)))
    (on-main (lambda ()
               (setf *found* (remove name *found* :key (lambda (f) (getf f :name)) :test #'string=))
               (found-changed)))))

;; The app came back to the front (registered in START).
(objc:define-objc-method ("appBecameActive:" :void)
    ((self net-delegate) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (handler-case (became-active)
    (error (condition)
      (format *error-output* "~&ridgefire: becoming active failed: ~a~%" condition))))

;; Failures to start, reported rather than silently ignored.
(objc:define-objc-method ("browser:didNotStartBrowsingForPeers:" :void)
    ((self net-delegate) (browser objc:objc-object-pointer) (error objc:objc-object-pointer))
  (declare (ignore browser))
  (format *error-output* "~&ridgefire: browsing did not start: ~a~%"
          (objc:ns-string-to-string (objc:invoke error "localizedDescription")))
  (finish-output *error-output*))

(objc:define-objc-method ("advertiser:didNotStartAdvertisingPeer:" :void)
    ((self net-delegate) (advertiser objc:objc-object-pointer) (error objc:objc-object-pointer))
  (declare (ignore advertiser))
  (format *error-output* "~&ridgefire: advertising did not start: ~a~%"
          (objc:ns-string-to-string (objc:invoke error "localizedDescription")))
  (finish-output *error-output*))

;;; ------------------------------------------------------------------
;;; starting and stopping

(defun ensure-net-delegate ()
  (or *net-delegate* (setf *net-delegate* (ui:keep (make-instance 'net-delegate)))))

(defun watch-app-state ()
  "Hear when the app returns to the front, to pick a Nearby game back up."
  (objc:invoke (objc:invoke "NSNotificationCenter" "defaultCenter")
               "addObserver:selector:name:object:"
               (objc:objc-object-pointer (ensure-net-delegate))
               (objc:coerce-to-selector "appBecameActive:")
               "UIApplicationDidBecomeActiveNotification"
               nil))

(defun open-session ()
  (close-session)
  (ensure-my-id)
  (ensure-net-delegate)
  (setf *me* (ui:keep (objc:invoke (objc:invoke (objc:invoke "MCPeerID" "alloc") "initWithDisplayName:" *my-id*)
                                   "autorelease"))
        *session* (ui:keep (objc:invoke (objc:invoke (objc:invoke "MCSession" "alloc")
                                                     "initWithPeer:securityIdentity:encryptionPreference:"
                                                     *me* nil 0)
                                        "autorelease")))
  (objc:invoke *session* "setDelegate:" (objc:objc-object-pointer *net-delegate*)))

(defun advertise (label &key playing)
  "Advertise a game called LABEL, replacing any earlier advertisement.  A game
PLAYING is still advertised, for players who dropped out to find it again."
  (stop-advertising)
  (let ((info (objc:invoke "NSMutableDictionary" "dictionary")))
    (objc:invoke info "setObject:forKey:" label "label")
    (objc:invoke info "setObject:forKey:" (if playing "1" "0") "playing")
    (setf *advertiser*
          (ui:keep (objc:invoke (objc:invoke (objc:invoke "MCNearbyServiceAdvertiser" "alloc")
                                             "initWithPeer:discoveryInfo:serviceType:"
                                             *me* info +service+)
                                "autorelease"))))
  (objc:invoke *advertiser* "setDelegate:" (objc:objc-object-pointer *net-delegate*))
  (objc:invoke *advertiser* "startAdvertisingPeer"))

(defun start-hosting (label)
  "A fresh session, and a game called LABEL advertised on it."
  (open-session)
  (advertise label))

(defun stop-advertising ()
  (when *advertiser*
    (objc:invoke *advertiser* "stopAdvertisingPeer")
    (objc:invoke *advertiser* "setDelegate:" nil)
    (ui:unkeep *advertiser*)
    (setf *advertiser* nil)))

(defun start-browsing ()
  (open-session)
  (setf *found* '()
        *browser* (ui:keep (objc:invoke (objc:invoke (objc:invoke "MCNearbyServiceBrowser" "alloc")
                                                     "initWithPeer:serviceType:" *me* +service+)
                                        "autorelease")))
  (objc:invoke *browser* "setDelegate:" (objc:objc-object-pointer *net-delegate*))
  (objc:invoke *browser* "startBrowsingForPeers"))

(defun stop-browsing ()
  (when *browser*
    (objc:invoke *browser* "stopBrowsingForPeers")
    (objc:invoke *browser* "setDelegate:" nil)
    (ui:unkeep *browser*)
    (setf *browser* nil)))

(defun join (found)
  "Guest: ask the host of FOUND to let us in."
  (setf *host-peer* (getf found :peer))
  (objc:invoke *browser* "invitePeer:toSession:withContext:timeout:" *host-peer* *session* nil 30d0))

(defun close-session ()
  (stop-advertising)
  (stop-browsing)
  (when *session*
    (objc:invoke *session* "disconnect")
    (objc:invoke *session* "setDelegate:" nil)
    (ui:unkeep *session*)
    (setf *session* nil))
  (when *me* (ui:unkeep *me*) (setf *me* nil))
  (setf *host-peer* nil *found* '() *inbox* '() *net-events* '()))
