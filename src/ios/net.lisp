;;;; src/ios/net.lisp -- phones finding each other, over Network.framework.
;;;;
;;;; A Nearby game is one host and its guests, all in the same room.  The host
;;;; runs a TCP listener advertised over Bonjour as _ridgefire._tcp; a guest
;;;; browses for it and connects.  Peer-to-peer is turned on throughout, so
;;;; phones find each other over Bluetooth and peer-to-peer Wi-Fi with no
;;;; router and no internet in between, as MultipeerConnectivity (deprecated
;;;; in iOS 27) did before.
;;;;
;;;; What a game is called travels in its Bonjour name, "id|label|playing",
;;;; so a guest can list games without connecting to any.  A connection's
;;;; first message is the guest's (:hello :id ...); the host answers
;;;; (:welcome) or (:refused), and only then is the guest "connected".  Every
;;;; message after that is one encoded S-expression (see wire.lisp), sent with
;;;; a four-byte length in front, because TCP is a stream.
;;;;
;;;; All handlers run on the main queue, so they share the main thread with
;;;; the scene and need no locks.  They still only queue what happened, in
;;;; *NET-EVENTS* and *INBOX*, for the scene to take in between turns.

(in-package #:ridgefire-ios)

(defparameter +service+ "_ridgefire._tcp")

(defvar *my-id* nil "This device's id, kept across launches.")
(defvar *listener* nil "Host: the listener, while advertising.")
(defvar *listener-failed* nil)
(defvar *browser* nil "Guest: the Bonjour browser, while looking.")
(defvar *links* '() "Host: a LINK per guest connection.")
(defvar *host-link* nil "Guest: the LINK to the host.")
(defvar *host-peer* nil "Guest: the game being joined, an entry of *FOUND*.")
(defvar *found* '() "Guest: games in range, plists :peer :name :label :playing.")
(defvar *inbox* '() "Messages waiting for the scene: (message . sender-id), oldest first.")
(defvar *net-events* '() "Connections and disconnections waiting: (kind . peer-id).")
(defvar *blocks* '() "The blocks handed over again and again, which are never freed.")
(defvar *network-denied* nil
  "True once iOS has said this app may not use the local network.")

(defun random-code (length)
  (let ((alphabet "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"))
    (coerce (loop repeat length collect (char alphabet (random (length alphabet)))) 'string)))

(defun null-object-p (object)
  (or (null object) (and (cffi:pointerp object) (cffi:null-pointer-p object))))

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

(defun host-peer-name ()
  "Guest: the id of the host being joined, or nil."
  (getf *host-peer* :name))

;;; ------------------------------------------------------------------
;;; calling Network.framework and libdispatch

(defun call (name result-type argument-types &rest arguments)
  (si:call-cfun (cffi:foreign-symbol-pointer name) result-type argument-types arguments))

(defun global (name)
  "The value of the C global NAME, a pointer."
  (cffi:mem-ref (cffi:foreign-symbol-pointer name) :pointer))

(defun main-queue ()
  (cffi:foreign-symbol-pointer "_dispatch_main_q"))

(objc:define-objc-block-type state-block :void ((:unsigned :int) objc:objc-object-pointer))
(objc:define-objc-block-type object-block :void (objc:objc-object-pointer))
(objc:define-objc-block-type receive-block :void
  (objc:objc-object-pointer objc:objc-object-pointer objc:objc-c++-bool objc:objc-object-pointer))
(objc:define-objc-block-type browse-block :void
  (objc:objc-object-pointer objc:objc-object-pointer objc:objc-c++-bool))

(defmacro shielded (&body body)
  "BODY, with no condition allowed out: it runs inside a block Cocoa called."
  `(handler-case (progn ,@body)
     (error (condition)
       (format *error-output* "~&ridgefire: network handler failed: ~a~%" condition)
       (finish-output *error-output*))))

(defun block-pointer (type function)
  "A block of TYPE calling FUNCTION, kept for good, as the raw pointer C wants.
For the few blocks that are handed over again and again."
  (let ((block (objc:make-objc-block type function)))
    (push block *blocks*)
    (objc:objc-object-pointer block)))

(defmacro with-handler ((var type function) &body body)
  "BODY with VAR a block of TYPE calling FUNCTION, as the raw pointer C wants,
for handing to the framework once.  The framework copies a handler it is
given, and the copy keeps FUNCTION alive for as long as the framework wants
it, so ours can go as soon as BODY is done."
  (let ((block (gensym "BLOCK")))
    `(objc:with-objc-block (,block ,type ,function)
       (let ((,var (objc:objc-object-pointer ,block)))
         ,@body))))

(defun policy-denied-p (error)
  "True if ERROR, an nw_error_t or null, is iOS refusing the local network."
  (and (not (null-object-p error))
       (= -65570 (call "nw_error_get_error_code" :int '(:pointer-void) error))))

(defun note-network-denied (denied)
  (unless (eq denied *network-denied*)
    (setf *network-denied* denied)
    (network-denied-changed)))

(defun release (object)
  (unless (null-object-p object) (objc:invoke object "release")))

(defvar *configure-tcp* nil)

(defun parameters ()
  "New parameters, the caller's to release: TCP with no TLS, over peer-to-peer links as well as Wi-Fi, with keepalive
so that a phone that walks away is noticed in seconds rather than minutes."
  (unless *configure-tcp*
    (setf *configure-tcp*
          (block-pointer 'object-block
                         (lambda (options)
                           (shielded
                             (call "nw_tcp_options_set_no_delay" :void '(:pointer-void :int) options 1)
                             (call "nw_tcp_options_set_enable_keepalive" :void '(:pointer-void :int) options 1)
                             (call "nw_tcp_options_set_keepalive_idle_time" :void '(:pointer-void :unsigned-int) options 4)
                             (call "nw_tcp_options_set_keepalive_interval" :void '(:pointer-void :unsigned-int) options 2)
                             (call "nw_tcp_options_set_keepalive_count" :void '(:pointer-void :unsigned-int) options 3))))))
  (let ((parameters (call "nw_parameters_create_secure_tcp" :pointer-void '(:pointer-void :pointer-void)
                          (global "_nw_parameters_configure_protocol_disable") *configure-tcp*)))
    (call "nw_parameters_set_include_peer_to_peer" :void '(:pointer-void :int) parameters 1)
    parameters))

;;; ------------------------------------------------------------------
;;; bytes

(defun data-octets (data)
  "The bytes of a dispatch_data_t, as a Lisp vector."
  (cffi:with-foreign-objects ((buffer :pointer) (size :uint64))
    (let* ((mapped (call "dispatch_data_create_map" :pointer-void '(:pointer-void :pointer-void :pointer-void)
                         data buffer size))
           (n (cffi:mem-ref size :uint64))
           (bytes (cffi:mem-ref buffer :pointer))
           (octets (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n) (setf (aref octets i) (cffi:mem-aref bytes :uint8 i)))
      (release mapped)
      octets)))

(defun octets-data (octets)
  "A dispatch_data_t holding a copy of OCTETS."
  (let ((n (length octets)))
    (cffi:with-foreign-object (bytes :uint8 n)
      (dotimes (i n) (setf (cffi:mem-aref bytes :uint8 i) (aref octets i)))
      (call "dispatch_data_create" :pointer-void '(:pointer-void :unsigned-long :pointer-void :pointer-void)
            bytes n (cffi:null-pointer) (cffi:null-pointer)))))

(defun frame (message)
  "MESSAGE encoded, behind its length as four bytes, most significant first."
  (let* ((body (babel:string-to-octets (encode-message message) :encoding :utf-8))
         (n (length body))
         (framed (make-array (+ 4 n) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 4
          do (setf (aref framed i) (ldb (byte 8 (* 8 (- 3 i))) n)))
    (replace framed body :start1 4)
    framed))

;;; ------------------------------------------------------------------
;;; links: one connection, its unread bytes, and who is at the other end

(defstruct link
  connection
  (id nil)                              ; the other phone's id, once it says
  (welcomed nil)                        ; true once the host has let it in
  (closed nil)
  (buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  receive                               ; its receive handler, reused
  receive-block)                        ; and the block that is, to free

(defvar *send-done* nil)

(defun link-send (link message)
  (when (and link (not (link-closed link)))
    (unless *send-done*
      (setf *send-done* (block-pointer 'object-block (lambda (error) (declare (ignore error))))))
    (let ((data (octets-data (frame message))))
      (call "nw_connection_send" :void '(:pointer-void :pointer-void :pointer-void :int :pointer-void)
            (link-connection link) data (global "_nw_content_context_default_message") 1 *send-done*)
      (release data))
    t))

(defun link-close (link &key (quietly nil))
  "Close LINK.  Unless QUIETLY, say who went, if anyone had been let in."
  (unless (link-closed link)
    (setf (link-closed link) t)
    ;; Cancelled, the connection is the framework's to finish with: it keeps
    ;; what it still needs, and its own copy of each handler.
    (call "nw_connection_cancel" :void '(:pointer-void) (link-connection link))
    (release (link-connection link))
    (when (link-receive-block link) (objc:free-objc-block (link-receive-block link)))
    (setf (link-connection link) nil (link-receive link) nil (link-receive-block link) nil)
    (when (and (not quietly) (link-id link)
               (or (link-welcomed link) (eq link *host-link*)))
      (setf *net-events* (append *net-events* (list (cons :disconnected (link-id link))))))
    (setf *links* (remove link *links*))
    (when (eq link *host-link*) (setf *host-link* nil))))

(defun arm-receive (link)
  (call "nw_connection_receive" :void '(:pointer-void :unsigned-int :unsigned-int :pointer-void)
        (link-connection link) 1 65536 (link-receive link)))

(defparameter *largest-frame* (* 4 1024 1024)
  "No message is anywhere near this long: a longer one is not one of ours.")

(defun take-frames (link)
  "Every whole message waiting in LINK's buffer, decoded, oldest first.  An
error if what is waiting cannot be ours."
  (let ((buffer (link-buffer link))
        (messages '())
        (start 0))
    (loop
      (when (< (- (length buffer) start) 4) (return))
      (let ((n (loop for i from 0 below 4
                     sum (ash (aref buffer (+ start i)) (* 8 (- 3 i))))))
        (when (> n *largest-frame*) (error "a frame of ~d bytes" n))
        (when (< (- (length buffer) start 4) n) (return))
        (let ((message (decode-message
                        (babel:octets-to-string buffer :start (+ start 4) :end (+ start 4 n)
                                                       :encoding :utf-8))))
          (when message (push message messages)))
        (incf start (+ 4 n))))
    (when (plusp start)
      (let ((rest (subseq buffer start)))
        (setf (fill-pointer buffer) 0)
        (loop for byte across rest do (vector-push-extend byte buffer))))
    (nreverse messages)))

(defun listen-to (link on-message)
  "Give LINK its receive handler, which takes in what arrived, calls
ON-MESSAGE with LINK and each whole message, and asks for more.  A link that
sends what cannot be read is closed, rather than left open and never listened
to again."
  (let ((block
          (objc:make-objc-block
           'receive-block
           (lambda (content context complete error)
             (declare (ignore context))
             (handler-case
                 (unless (link-closed link)
                   (unless (null-object-p content)
                     (loop for byte across (data-octets content)
                           do (vector-push-extend byte (link-buffer link)))
                     (dolist (message (take-frames link))
                       (unless (link-closed link)
                         (funcall on-message link message))))
                   (cond ((link-closed link))
                         ((or complete (not (null-object-p error))) (link-close link))
                         (t (arm-receive link))))
               (error (condition)
                 (format *error-output* "~&ridgefire: closing a link: ~a~%" condition)
                 (finish-output *error-output*)
                 (link-close link)))))))
    (setf (link-receive-block link) block
          (link-receive link) (objc:objc-object-pointer block))))

(defun watch-link (link on-state)
  "Put LINK's connection on the main queue, calling ON-STATE as its state
changes; a connection that fails or is cancelled closes LINK."
  (let ((connection (link-connection link)))
    (call "nw_connection_set_queue" :void '(:pointer-void :pointer-void) connection (main-queue))
    (with-handler (handler 'state-block
                           (lambda (state error)
                             (declare (ignore error))
                             (shielded
                               (if (member state '(4 5))
                                   (link-close link)
                                   (funcall on-state state)))))
      (call "nw_connection_set_state_changed_handler" :void '(:pointer-void :pointer-void)
            connection handler))))

(defun open-link (connection on-message)
  "Host: start a guest's CONNECTION on the main queue, calling ON-MESSAGE with
each message."
  (let ((link (make-link :connection connection)))
    (listen-to link on-message)
    (watch-link link (lambda (state) (declare (ignore state))))
    (call "nw_connection_start" :void '(:pointer-void) connection)
    (arm-receive link)
    link))

;;; ------------------------------------------------------------------
;;; the host

(defun host-link-message (link message)
  "Host: a message from a guest.  The first must say who it is."
  (cond ((link-welcomed link)
         (setf *inbox* (append *inbox* (list (cons message (link-id link))))))
        ((and (eq (first message) :hello) (device-id-p (getf (rest message) :id)))
         (let* ((id (getf (rest message) :id))
                (earlier (find id *links* :key #'link-id :test #'equal)))
           (cond ((or earlier (accept-invitation-p id))
                  ;; Someone back before their old connection was seen to
                  ;; drop takes the place of it.
                  (when (and earlier (not (eq earlier link))) (link-close earlier :quietly t))
                  (setf (link-id link) id (link-welcomed link) t)
                  (link-send link (list :welcome))
                  (setf *net-events* (append *net-events* (list (cons :connected id)))
                        *inbox* (append *inbox* (list (cons message id)))))
                 (t (link-send link (list :refused))
                    (after 1d0 (lambda () (link-close link :quietly t)))))))
        (t (link-close link :quietly t))))

(defun bonjour-name (label playing)
  "What the game is called on the network: who hosts it, and what it is."
  (format nil "~a|~a|~:[0~;1~]" *my-id* (remove #\| label) playing))

(defun welcomed-ids ()
  "Host: the ids of the guests that have been let in."
  (loop for link in *links* when (link-welcomed link) collect (link-id link)))

(defun start-listener ()
  (let ((parameters (parameters)))
    (setf *listener-failed* nil
          *listener* (call "nw_listener_create" :pointer-void '(:pointer-void) parameters))
    (release parameters))
  (call "nw_listener_set_queue" :void '(:pointer-void :pointer-void) *listener* (main-queue))
  (let ((listener *listener*))
    (with-handler (handler 'state-block
                           (lambda (state error)
                             (shielded
                               ;; A listener stopped since says nothing about this one.
                               (when (and *listener* (cffi:pointer-eq listener *listener*))
                                 (cond ((policy-denied-p error) (note-network-denied t))
                                       ((= state 2) (note-network-denied nil)))
                                 (when (= state 3)
                                   (format *error-output* "~&ridgefire: the listener failed~%")
                                   (setf *listener-failed* t))))))
      (call "nw_listener_set_state_changed_handler" :void '(:pointer-void :pointer-void)
            *listener* handler)))
  (with-handler (handler 'object-block
                         (lambda (connection)
                           (shielded
                             (objc:invoke connection "retain")
                             (push (open-link connection #'host-link-message) *links*))))
    (call "nw_listener_set_new_connection_handler" :void '(:pointer-void :pointer-void)
          *listener* handler)))

(defun advertise-as (name)
  "Advertise *LISTENER* over Bonjour as NAME, or change what it is called."
  (let ((descriptor (call "nw_advertise_descriptor_create_bonjour_service" :pointer-void
                          '(:cstring :cstring :pointer-void) name +service+ (cffi:null-pointer))))
    (call "nw_listener_set_advertise_descriptor" :void '(:pointer-void :pointer-void)
          *listener* descriptor)
    (release descriptor)))

(defun advertise (label &key playing)
  "Advertise a game called LABEL, starting the listener if need be.  A game
PLAYING is still advertised, for players who dropped out to find it again."
  (cond ((or (null *listener*) *listener-failed*)
         (stop-listener)
         (start-listener)
         (advertise-as (bonjour-name label playing))
         (call "nw_listener_start" :void '(:pointer-void) *listener*))
        (t (advertise-as (bonjour-name label playing)))))

(defun stop-listener ()
  (when *listener*
    (call "nw_listener_cancel" :void '(:pointer-void) *listener*)
    (release *listener*)
    (setf *listener* nil)))

(defun start-hosting (label)
  "A fresh start, and a game called LABEL advertised."
  (close-session)
  (ensure-my-id)
  (advertise label))

(defun broadcast (message &key (reliable t))
  "Host: to every guest that has been let in."
  (declare (ignore reliable))
  (dolist (link *links*)
    (when (link-welcomed link) (link-send link message))))

(defun send-to-peer (device message &key (reliable t))
  "Host: to the guest whose id is DEVICE, if it is connected."
  (declare (ignore reliable))
  (link-send (find-if (lambda (link) (and (link-welcomed link) (equal (link-id link) device))) *links*)
             message))

;;; ------------------------------------------------------------------
;;; a guest

(defun parse-bonjour-name (name)
  "(values id label playing) from a game's Bonjour name, or nil."
  (let* ((one (position #\| name))
         (two (and one (position #\| name :start (1+ one)))))
    (when two
      (values (subseq name 0 one) (subseq name (1+ one) two)
              (string= (subseq name (1+ two)) "1")))))

(defun endpoint-name (endpoint)
  (let ((name (call "nw_endpoint_get_bonjour_service_name" :pointer-void '(:pointer-void) endpoint)))
    (and (not (null-object-p name)) (cffi:foreign-string-to-lisp name :encoding :utf-8))))

(defun browse-changed (old new)
  "A game appeared (NEW only), went (OLD only), or changed (both)."
  (flet ((forget (id)
           (let ((entry (find id *found* :key (lambda (f) (getf f :name)) :test #'equal)))
             (when entry
               ;; A connection made to it keeps the endpoint for itself.
               (setf *found* (remove entry *found*))
               (release (getf entry :peer))))))
    (unless (null-object-p old)
      (let ((endpoint (call "nw_browse_result_copy_endpoint" :pointer-void '(:pointer-void) old)))
        (let ((id (parse-bonjour-name (or (endpoint-name endpoint) ""))))
          (when id (forget id)))
        (release endpoint)))
    (unless (null-object-p new)
      (let ((endpoint (call "nw_browse_result_copy_endpoint" :pointer-void '(:pointer-void) new)))
        (multiple-value-bind (id label playing) (parse-bonjour-name (or (endpoint-name endpoint) ""))
          (if (and id (not (equal id *my-id*)))
              (progn (forget id)
                     (setf *found* (append *found* (list (list :peer endpoint :name id
                                                               :label label :playing playing)))))
              (release endpoint)))))
    (found-changed)))

(defun start-browsing ()
  (close-session)
  (ensure-my-id)
  (let ((parameters (call "nw_parameters_create" :pointer-void '()))
        (descriptor (call "nw_browse_descriptor_create_bonjour_service" :pointer-void
                          '(:cstring :pointer-void) +service+ (cffi:null-pointer))))
    (call "nw_parameters_set_include_peer_to_peer" :void '(:pointer-void :int) parameters 1)
    (setf *browser* (call "nw_browser_create" :pointer-void '(:pointer-void :pointer-void)
                          descriptor parameters))
    (release descriptor)
    (release parameters))
  (call "nw_browser_set_queue" :void '(:pointer-void :pointer-void) *browser* (main-queue))
  (let ((browser *browser*))
    (flet ((current-p () (and *browser* (cffi:pointer-eq browser *browser*))))
      (with-handler (handler 'state-block
                             (lambda (state error)
                               (shielded
                                 (when (current-p)
                                   (cond ((policy-denied-p error) (note-network-denied t))
                                         ((= state 1) (note-network-denied nil)))))))
        (call "nw_browser_set_state_changed_handler" :void '(:pointer-void :pointer-void)
              *browser* handler))
      (with-handler (handler 'browse-block
                             (lambda (old new batch-complete)
                               (declare (ignore batch-complete))
                               (shielded (when (current-p) (browse-changed old new)))))
        (call "nw_browser_set_browse_results_changed_handler" :void '(:pointer-void :pointer-void)
              *browser* handler))))
  (call "nw_browser_start" :void '(:pointer-void) *browser*))

(defun stop-browsing ()
  (when *browser*
    (call "nw_browser_cancel" :void '(:pointer-void) *browser*)
    (release *browser*)
    (setf *browser* nil)))

(defun guest-link-message (link message)
  "Guest: a message from the host.  Being let in, or not, is handled here."
  (case (first message)
    (:welcome
     (setf (link-welcomed link) t
           *net-events* (append *net-events* (list (cons :connected (link-id link))))))
    (:refused (link-close link))
    (t (setf *inbox* (append *inbox* (list (cons message (link-id link))))))))

(defun join (found)
  "Guest: ask the host of FOUND to let us in.  Gives up after half a minute."
  (when *host-link* (link-close *host-link* :quietly t))
  (setf *host-peer* found)
  (let* ((parameters (parameters))
         (connection (call "nw_connection_create" :pointer-void '(:pointer-void :pointer-void)
                           (getf found :peer) parameters))
         (link (make-link :connection connection :id (getf found :name))))
    (release parameters)
    (setf *host-link* link)
    (listen-to link #'guest-link-message)
    (watch-link link (lambda (state)
                       (when (= state 3) (link-send link (list :hello :id *my-id*)))))
    (call "nw_connection_start" :void '(:pointer-void) connection)
    (arm-receive link)
    (after 30d0 (lambda () (when (and (eq link *host-link*) (not (link-welcomed link)))
                             (link-close link))))))

(defun host-connected-p ()
  (and *host-link* (link-welcomed *host-link*) (not (link-closed *host-link*))))

(defun send-host (message &key (reliable t))
  "Guest: to the host."
  (declare (ignore reliable))
  (when (host-connected-p) (link-send *host-link* message)))

;;; ------------------------------------------------------------------
;;; stopping

(defun close-session ()
  "Stop advertising, browsing and every connection, saying nothing."
  (stop-listener)
  (stop-browsing)
  (dolist (link *links*) (link-close link :quietly t))
  (when *host-link* (link-close *host-link* :quietly t))
  (dolist (entry *found*) (release (getf entry :peer)))
  (setf *links* '() *host-link* nil *host-peer* nil *found* '() *inbox* '() *net-events* '()
        *network-denied* nil))

(defun nearby-active-p ()
  (or *listener* *browser* *host-link* *links*))

;;; ------------------------------------------------------------------
;;; the app coming back to the front

(objc:define-objc-class app-watcher () ()
  (:objc-class-name "RidgefireAppWatcher"))

(objc:define-objc-method ("appBecameActive:" :void)
    ((self app-watcher) (notification objc:objc-object-pointer))
  (declare (ignore notification))
  (shielded (became-active)))

(defvar *app-watcher* nil)

(defun watch-app-state ()
  "Hear when the app returns to the front, to pick a Nearby game back up."
  (unless *app-watcher*
    (setf *app-watcher* (ui:keep (make-instance 'app-watcher)))
    (objc:invoke (objc:invoke "NSNotificationCenter" "defaultCenter")
                 "addObserver:selector:name:object:"
                 (objc:objc-object-pointer *app-watcher*)
                 (objc:coerce-to-selector "appBecameActive:")
                 "UIApplicationDidBecomeActiveNotification"
                 nil)))
