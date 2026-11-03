;;;; src/ios/nodes.lisp -- SpriteKit in small pieces, sound, haptics, settings.

(in-package #:ridgefire-ios)

(defvar *view* nil "The SKView.")
(defvar *scene* nil "The scene, a RIDGEFIRE-SCENE.")
(defvar *world* nil "The node everything is drawn in, 800x400, scaled to the safe area.")

;;; ------------------------------------------------------------------
;;; settings, kept in NSUserDefaults

(defun defaults ()
  (objc:invoke "NSUserDefaults" "standardUserDefaults"))

(defun saved (key)
  (objc:invoke (defaults) "integerForKey:" key))

(defun save (key value)
  (objc:invoke (defaults) "setInteger:forKey:" value key))

;;; ------------------------------------------------------------------
;;; nodes

(defun point (x y)
  (vector (float x 1d0) (float y 1d0)))

(defun rgb (r g b &optional (a 1))
  (ui:color (float r 1d0) (float g 1d0) (float b 1d0) (float a 1d0)))

(defun add (parent child)
  (objc:invoke parent "addChild:" child)
  child)

(defun at (node x y)
  (objc:invoke node "setPosition:" (point x y))
  node)

(defun z (node z)
  (objc:invoke node "setZPosition:" (float z 1d0))
  node)

(defun alpha (node a)
  (objc:invoke node "setAlpha:" (float a 1d0))
  node)

(defun remove-node (node)
  (when node (objc:invoke node "removeFromParent"))
  nil)

(defun empty-node ()
  (objc:invoke "SKNode" "node"))

(defun rect-sprite (colour width height)
  (objc:invoke "SKSpriteNode" "spriteNodeWithColor:size:" colour (point width height)))

(defun styled (node fill stroke line-width)
  (objc:invoke node "setFillColor:" fill)
  (objc:invoke node "setStrokeColor:" (or stroke (rgb 0 0 0 0)))
  (objc:invoke node "setLineWidth:" (float line-width 1d0))
  node)

(defun rounded (width height radius fill &optional stroke (line-width 2))
  (styled (objc:invoke "SKShapeNode" "shapeNodeWithRect:cornerRadius:"
                       (vector (- (/ width 2d0)) (- (/ height 2d0))
                               (float width 1d0) (float height 1d0))
                       (float radius 1d0))
          fill stroke line-width))

(defun ellipse (width height fill &optional stroke (line-width 2))
  (styled (objc:invoke "SKShapeNode" "shapeNodeWithEllipseOfSize:" (point width height))
          fill stroke line-width))

(defun text (string size colour &key (font "AvenirNext-Bold") (align :centre))
  (let ((node (objc:invoke "SKLabelNode" "labelNodeWithFontNamed:" font)))
    (objc:invoke node "setText:" string)
    (objc:invoke node "setFontSize:" (float size 1d0))
    (objc:invoke node "setFontColor:" colour)
    (objc:invoke node "setVerticalAlignmentMode:" 1) ; centre
    (objc:invoke node "setHorizontalAlignmentMode:" (ecase align (:centre 0) (:left 1) (:right 2)))
    node))

(defun set-text (label string)
  (objc:invoke label "setText:" string))

(defun bezier (points &key close)
  "A UIBezierPath through POINTS, a flat list x0 y0 x1 y1 ..."
  (let ((path (objc:invoke "UIBezierPath" "bezierPath")))
    (loop for (x y) on points by #'cddr
          for first = t then nil
          do (objc:invoke path (if first "moveToPoint:" "addLineToPoint:") (point x y)))
    (when close (objc:invoke path "closePath"))
    path))

(defun path-node (points fill &key stroke (line-width 0) close)
  (styled (objc:invoke "SKShapeNode" "shapeNodeWithPath:"
                       (objc:invoke (bezier points :close close) "CGPath"))
          fill stroke line-width))

(defun set-path (node points &key close)
  (objc:invoke node "setPath:" (objc:invoke (bezier points :close close) "CGPath")))

(defun shader (source)
  (objc:invoke "SKShader" "shaderWithSource:" source))

;;; ------------------------------------------------------------------
;;; actions

(defun action (name &rest arguments)
  (apply #'objc:invoke "SKAction" name arguments))

(defun run (node &rest actions)
  (objc:invoke node "runAction:"
               (if (rest actions)
                   (action "sequence:" (coerce actions 'vector))
                   (first actions))))

(defun together (&rest actions)
  (action "group:" (coerce actions 'vector)))

(defun forever (&rest actions)
  (action "repeatActionForever:"
          (if (rest actions) (action "sequence:" (coerce actions 'vector)) (first actions))))

(defun move-to (x y &optional (seconds 0.2d0))
  (action "moveTo:duration:" (point x y) (float seconds 1d0)))

(defun move-by (dx dy &optional (seconds 0.2d0))
  (action "moveByX:y:duration:" (float dx 1d0) (float dy 1d0) (float seconds 1d0)))

(defun scale-to (scale &optional (seconds 0.15d0))
  (action "scaleTo:duration:" (float scale 1d0) (float seconds 1d0)))

(defun fade-to (a &optional (seconds 0.2d0))
  (action "fadeAlphaTo:duration:" (float a 1d0) (float seconds 1d0)))

(defun wait-action (seconds)
  (action "waitForDuration:" (float seconds 1d0)))

(defun vanish-after (node seconds &optional (fade 0.3d0))
  (run node (wait-action seconds) (fade-to 0 fade) (action "removeFromParent")))

(defun shake (node &optional (amount 6))
  (run node
       (move-by amount 0 0.04d0)
       (move-by (* -2 amount) 0 0.08d0)
       (move-by amount 0 0.04d0)))

(defun after (seconds thunk)
  "Call THUNK once, SECONDS from now, on the main run loop."
  (ui:after-every seconds
                  (lambda (timer)
                    (declare (ignore timer))
                    (handler-case (funcall thunk)
                      (error (condition)
                        (format *error-output* "~&ridgefire: timer failed: ~a~%" condition)
                        (finish-output *error-output*))))
                  :repeats nil))

;;; ------------------------------------------------------------------
;;; haptics and sound

(defvar *impacts* (make-hash-table))
(defvar *notifier* nil)
(defvar *sounds* (make-hash-table :test 'equal))

(defun vibration-p () (zerop (saved "no vibration")))
(defun sound-p () (zerop (saved "no sound")))

(defun haptic (&optional (style 0) intensity)
  "A tap: STYLE 0 light, 1 medium, 2 heavy."
  (when (vibration-p)
    (let ((generator (or (gethash style *impacts*)
                         (setf (gethash style *impacts*)
                               (ui:keep (objc:invoke (objc:invoke "UIImpactFeedbackGenerator" "alloc")
                                                     "initWithStyle:" style))))))
      (if intensity
          (objc:invoke generator "impactOccurredWithIntensity:" (float intensity 1d0))
          (objc:invoke generator "impactOccurred")))))

(defun notify (kind)
  (when (vibration-p)
    (objc:invoke (or *notifier*
                     (setf *notifier* (ui:keep (objc:alloc-init-object "UINotificationFeedbackGenerator"))))
                 "notificationOccurred:" (ecase kind (:success 0) (:warning 1) (:error 2)))))

(defun sound (name)
  (when (and (sound-p) *scene*)
    (let ((play (or (gethash name *sounds*)
                    (setf (gethash name *sounds*)
                          (ui:keep (action "playSoundFileNamed:waitForCompletion:"
                                           (format nil "~a.wav" name) nil))))))
      (objc:invoke *scene* "runAction:" play))))

;;; ------------------------------------------------------------------
;;; colours

(defparameter *tank-rgb*
  #((0.93 0.27 0.27) (0.27 0.58 0.98) (0.47 0.86 0.30) (0.99 0.76 0.20)))

(defun tank-rgb (colour &optional (shade 1) (a 1))
  (destructuring-bind (r g b) (coerce (aref *tank-rgb* colour) 'list)
    (rgb (* r shade) (* g shade) (* b shade) a)))

(defun white (&optional (a 1)) (rgb 1 1 1 a))
(defun ink (&optional (a 1)) (rgb 0.06 0.06 0.09 a))
(defparameter *gold* '(0.98 0.80 0.30))
(defun gold (&optional (a 1)) (destructuring-bind (r g b) *gold* (rgb r g b a)))
