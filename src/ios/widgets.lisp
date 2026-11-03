;;;; src/ios/widgets.lisp -- buttons and screens drawn in the world.
;;;;
;;;; Every screen is SpriteKit nodes in *WORLD*.  A button is a node plus a
;;;; rectangle registered in *BUTTONS*; a tap finds the topmost rectangle under
;;;; it.  A modal panel pushes its own buttons in front of the screen's.

(in-package #:ridgefire-ios)

(defparameter *width* 800)
(defparameter *height* 400)

(defvar *buttons* '() "Plists :x :y :w :h :fn :repeat :node, topmost first.")
(defvar *modal* nil "The modal panel's node, if one is up.")
(defvar *modal-buttons* '())
(defvar *screen* nil "A keyword naming what is on screen.")

(defun clear-world ()
  "Take everything off the screen."
  (objc:invoke *world* "removeAllChildren")
  (setf *buttons* '() *modal* nil *modal-buttons* '()))

(defun register-button (x y w h fn &key repeat node)
  (let ((button (list :x x :y y :w w :h h :fn fn :repeat repeat :node node)))
    (if *modal*
        (push button *modal-buttons*)
        (push button *buttons*))
    button))

(defun button-at (x y)
  (find-if (lambda (b)
             (and (<= (abs (- x (getf b :x))) (/ (getf b :w) 2))
                  (<= (abs (- y (getf b :y))) (/ (getf b :h) 2))))
           (if *modal* *modal-buttons* *buttons*)))

(defun press (button)
  (let ((node (getf button :node)))
    (when node
      (objc:invoke node "removeActionForKey:" "press")
      (objc:invoke node "setScale:" 1d0)
      (objc:invoke node "runAction:withKey:"
                   (action "sequence:" (vector (scale-to 0.92d0 0.05d0) (scale-to 1 0.08d0)))
                   "press")))
  (funcall (getf button :fn)))

(defun button (parent label x y w h fn &key (fill (rgb 1 1 1 0.14)) (stroke (rgb 1 1 1 0.35))
                                           (colour (white)) (size 17) repeat (font "AvenirNext-DemiBold"))
  "A rounded button with LABEL, calling FN when tapped (and, with REPEAT, while held)."
  (let ((node (add parent (at (rounded w h (min 14 (/ h 2)) fill stroke 1.5d0) x y))))
    (when label
      (add node (text label size colour :font font)))
    (register-button x y w h fn :repeat repeat :node node)
    node))

(defun label (parent string x y &key (size 16) (colour (white)) (align :centre)
                                     (font "AvenirNext-DemiBold"))
  (add parent (at (text string size colour :font font :align align) x y)))

(defun panel (parent x y w h &key (fill (rgb 0.07 0.07 0.10 0.92)) (stroke (rgb 1 1 1 0.3)))
  (add parent (at (rounded w h 18 fill stroke 2) x y)))

(defun backdrop (parent &optional (a 0.55))
  (add parent (at (rect-sprite (rgb 0 0 0 a) (* 3 *width*) (* 3 *height*))
                  (/ *width* 2) (/ *height* 2))))

;;; ------------------------------------------------------------------
;;; modal panels: a question and some buttons

(defun close-modal ()
  (remove-node *modal*)
  (setf *modal* nil *modal-buttons* '()))

(defun show-modal (title body choices)
  "A panel over everything.  CHOICES: list of (label function); each closes it."
  (close-modal)
  (let ((node (z (empty-node) 500)))
    (add *world* node)
    (setf *modal* node)
    (backdrop node)
    ;; Swallow taps on the backdrop.
    (register-button (/ *width* 2) (/ *height* 2) (* 3 *width*) (* 3 *height*) (lambda ()))
    (panel node 400 200 440 200)
    (label node title 400 262 :size 24 :font "AvenirNext-Bold")
    (let ((lines (split-lines body)))
      (loop for line in lines
            for y downfrom 225 by 22
            do (label node line 400 y :size 15 :colour (white 0.8) :font "AvenirNext-Medium")))
    (let* ((n (length choices))
           (w (if (= n 1) 160 150))
           (span (* (1- n) 170)))
      (loop for (text function) in choices
            for i from 0
            for x = (+ 400 (- (/ span 2)) (* i 170))
            do (let ((function function))
                 (button node text x 135 w 44
                         (lambda () (close-modal) (funcall function))))))))

(defun split-lines (string)
  (loop with start = 0
        for end = (position #\Newline string :start start)
        collect (subseq string start end)
        while end
        do (setf start (1+ end))))
