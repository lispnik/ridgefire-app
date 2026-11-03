;;;; src/ios/app.lisp -- the scene, touches, and the entry point.

(in-package #:ridgefire-ios)

(defvar *fitted* nil)
(defvar *last-time* nil)
(defvar *failed* nil)
(defvar *repeat-timer* nil "Repeats a held button.")
(defvar *drag* nil "While dragging to aim: (angle . power) as floats.")

;;; ------------------------------------------------------------------
;;; the scene

(objc:define-objc-class ridgefire-scene () ()
  (:objc-class-name "RidgefireScene")
  (:objc-superclass-name "SKScene"))

(defun fit-world (&optional force)
  "Scale the 800x400 world into the safe area, centred."
  (when (and *scene* *world*)
    (let* ((size (objc:invoke *scene* "size"))
           (insets (objc:invoke *view* "safeAreaInsets"))
           (left (aref insets 1))
           (bottom (aref insets 2))
           (w (- (aref size 0) left (aref insets 3)))
           (h (- (aref size 1) (aref insets 0) bottom)))
      (when (and (plusp w) (plusp h)
                 (or force (not (equalp (list size insets) *fitted*))))
        (setf *fitted* (list size insets))
        (let ((scale (min (/ w *width*) (/ h *height*))))
          (objc:invoke *world* "setScale:" scale)
          (objc:invoke *world* "setPosition:"
                       (point (+ left (/ (- w (* scale *width*)) 2))
                              (+ bottom (/ (- h (* scale *height*)) 2)))))))))

(defun world-scale ()
  (objc:invoke *world* "xScale"))

(objc:define-objc-method ("update:" :void)
    ((self ridgefire-scene) (time :double))
  (declare (ignore self))
  (let ((dt (if *last-time* (min 0.1d0 (max 0d0 (- time *last-time*))) 0d0)))
    (setf *last-time* time)
    (unless *failed*
      (handler-case
          (progn
            (fit-world)
            (tick-steps dt)
            (pump-network)
            (send-aim-if-due dt))
        (error (condition)
          (setf *failed* t)
          (cancel-steps)
          (format *error-output* "~&ridgefire: frame failed: ~a~%" condition)
          (finish-output *error-output*))))))

;;; ------------------------------------------------------------------
;;; touches

(defun world-point (recognizer)
  "Where RECOGNIZER's touch is, in world coordinates."
  (let* ((in-view (objc:invoke recognizer "locationInView:" *view*))
         (in-world (objc:invoke *world* "convertPoint:fromNode:"
                                (objc:invoke *scene* "convertPointFromView:" in-view) *scene*)))
    (values (aref in-world 0) (aref in-world 1))))

(defun guarded (function)
  (lambda (recognizer)
    (handler-case (funcall function recognizer)
      (error (condition)
        (format *error-output* "~&ridgefire: touch failed: ~a~%" condition)
        (finish-output *error-output*)))))

(defun tapped (recognizer)
  (multiple-value-bind (x y) (world-point recognizer)
    (let ((button (button-at x y)))
      (when button (press button)))))

(defun stop-repeating ()
  (when *repeat-timer*
    (objc:invoke *repeat-timer* "invalidate")
    (ui:unkeep *repeat-timer*)
    (setf *repeat-timer* nil)))

(defun held (recognizer)
  "A long press: press the button once, and keep pressing a repeating one."
  (case (objc:invoke recognizer "state")
    (1 (multiple-value-bind (x y) (world-point recognizer)
         (let ((button (button-at x y)))
           (when button
             (press button)
             (when (getf button :repeat)
               (stop-repeating)
               (setf *repeat-timer*
                     (ui:after-every 0.06d0 (lambda (timer)
                                              (declare (ignore timer))
                                              (if (can-aim-p)
                                                  (funcall (getf button :fn))
                                                  (stop-repeating))))))))))
    ((3 4 5) (stop-repeating))))

(defun dragged (recognizer)
  "Drag to aim: sideways turns the barrel, up and down sets the power."
  (case (objc:invoke recognizer "state")
    (1 (multiple-value-bind (x y) (world-point recognizer)
         (setf *drag* (and (can-aim-p) (null (button-at x y))
                           (let ((tank (active-tank *game*)))
                             (cons (float (tank-angle tank) 1d0) (float (tank-power tank) 1d0)))))))
    (2 (when (and *drag* (can-aim-p))
         (let ((delta (objc:invoke recognizer "translationInView:" *view*))
               (scale (world-scale)))
           (objc:invoke recognizer "setTranslation:inView:" #(0d0 0d0) *view*)
           ;; View points to world points, then a third of a degree (or of a
           ;; power step) per point: fine enough to finish a shot by dragging.
           (setf (car *drag*) (max 0d0 (min 180d0 (- (car *drag*) (/ (* 0.35d0 (aref delta 0)) scale))))
                 (cdr *drag*) (max 0d0 (min 100d0 (- (cdr *drag*) (/ (* 0.3d0 (aref delta 1)) scale)))))
           (aim-to (car *drag*) (cdr *drag*)))))
    ((3 4 5) (setf *drag* nil))))

(defun add-recognizer (class-name function &optional setup)
  (let ((recognizer (objc:invoke (objc:invoke class-name "alloc") "initWithTarget:action:"
                                 (ui:action-target (guarded function)) "fire:")))
    (when setup (funcall setup recognizer))
    (objc:invoke *view* "addGestureRecognizer:" (ui:keep recognizer))))

;;; ------------------------------------------------------------------
;;; development: open a screen at launch, for looking at it
;;;
;;;   SIMCTL_CHILD_RIDGEFIRE_SCREEN=shop xcrun simctl launch booted com.burnsidemk.ridgefire

(defun open-screen (name)
  (let ((name (string-downcase name)))
    (cond ((string= name "setup") (show-setup))
          ((string= name "nearby") (show-nearby))
          ((string= name "help") (show-help))
          ;; A Nearby game that plays itself: one phone host, the other join.
          ((string= name "host") (setf *demo* t *rounds* 2) (show-title) (after 1d0 #'host-game))
          ((string= name "join") (setf *demo* t) (show-title) (after 1d0 #'join-game))
          ;; The host of a saved Nearby game, taking it up again.
          ((string= name "resume") (setf *demo* t) (show-title) (after 1d0 #'resume-saved-game))
          ((member name '("battle" "shop" "results") :test #'string=)
           (setf *slots* (list :human :cpu :human :off))
           (start-local-game)
           (when (string= name "shop")
             (setf (tank-money (aref (game-tanks *game*) 0)) 4200
                   (game-phase *game*) :shop)
             (enter-phase))
           (when (string= name "results")
             (setf (tank-wins (aref (game-tanks *game*) 2)) 2
                   (game-phase *game*) :over)
             (enter-phase)))
          (t (show-title)))))

;;; ------------------------------------------------------------------
;;; starting

(defun start ()
  (objc:ensure-objc-initialized)
  (setf *random-state* (make-random-state t))
  (let* ((root (ui:root-view))
         (view (ui:new "SKView"))
         (background (rgb 0.05 0.05 0.08)))
    (objc:invoke root "setBackgroundColor:" background)
    (objc:invoke root "addSubview:" view)
    (ui:pin view "topAnchor" root "topAnchor")
    (ui:pin view "bottomAnchor" root "bottomAnchor")
    (ui:pin view "leadingAnchor" root "leadingAnchor")
    (ui:pin view "trailingAnchor" root "trailingAnchor")
    (objc:invoke view "setMultipleTouchEnabled:" nil)
    (let ((scene (objc:invoke (make-instance 'ridgefire-scene) "initWithSize:" (point 852 393))))
      (objc:invoke scene "setBackgroundColor:" background)
      (objc:invoke scene "setScaleMode:" 3)            ; resize fill: see FIT-WORLD
      (setf *view* (ui:keep view)
            *scene* (ui:keep scene)
            *world* (ui:keep (empty-node)))
      (add scene *world*)
      (objc:invoke (objc:invoke "AVAudioSession" "sharedInstance") "setCategory:error:"
                   "AVAudioSessionCategoryAmbient" nil)
      (ensure-my-id)
      (watch-app-state)
      (cond ((ext:getenv "RIDGEFIRE_DEMO")
             (setf *demo* t
                   *slots* (list :cpu :cpu :cpu :cpu)
                   *cpu-skill* 0.7
                   *rounds* 2)
             (start-local-game))
            ((ext:getenv "RIDGEFIRE_SCREEN")
             (open-screen (ext:getenv "RIDGEFIRE_SCREEN")))
            (t (show-title)))
      (objc:invoke view "presentScene:" scene))
    (add-recognizer "UITapGestureRecognizer" #'tapped)
    (add-recognizer "UILongPressGestureRecognizer" #'held
                    (lambda (r) (objc:invoke r "setMinimumPressDuration:" 0.3d0)))
    (add-recognizer "UIPanGestureRecognizer" #'dragged
                    (lambda (r) (objc:invoke r "setMaximumNumberOfTouches:" 1)))
    (format t "RIDGEFIRE: started as ~a~%" *my-id*)
    (finish-output)
    (values)))
