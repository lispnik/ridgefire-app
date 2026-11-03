;;;; src/core/terrain.lisp -- the ground, as one height per column.
;;;;
;;;; A heightmap cannot hold an overhang, and that is the point: when a blast
;;;; removes a slice of a column, whatever was above the slice falls into it,
;;;; so earth always settles without a separate simulation.

(in-package #:ridgefire)

(defconstant +world-width+ 800 "Columns of ground; also the world's width in points.")
(defconstant +world-height+ 400 "The world's height in points; y grows upwards.")

(defconstant +tau+ (float (* 2 pi) 1d0) "A full turn, as a double: ECL's PI is a long float.")

(defparameter *min-ground* 24 "Generated ground never starts lower than this.")
(defparameter *max-ground* 290 "Nor higher than this.")
(defparameter *ceiling* 360 "Dirt never piles higher than this.")

(defun make-heights (&optional (initial 0))
  (make-array +world-width+ :element-type 'fixnum :initial-element initial))

(defun column (x)
  "The column holding X, clamped to the world."
  (max 0 (min (1- +world-width+) (floor x))))

(defun height-at (heights x)
  (aref heights (column x)))

(defun generate-heights (rng)
  "A fresh landscape: a few sine layers of random frequency and phase, over a
shape -- rolling hills, a central mountain, or a valley -- chosen by RNG."
  (let* ((heights (make-heights))
         (style (rng-below rng 3))
         (base (+ 90 (rng-below rng 70)))
         (layers (loop for (low spread amplitude) in '((1 2 55) (2 3 28) (5 5 12) (11 8 4))
                       collect (list (+ low (rng-below rng spread))
                                     (* amplitude (+ 0.6d0 (* 0.8d0 (rng-float rng))))
                                     (* +tau+ (rng-float rng))))))
    (dotimes (x +world-width+ heights)
      (let* ((u (/ x (float +world-width+ 1d0)))
             (shape (ecase style
                      (0 0d0)
                      (1 (* 120 (exp (- (expt (/ (- u 0.5d0) 0.16d0) 2)))))
                      (2 (* 150 (expt (- (* 2 u) 1) 2)))))
             (h (+ base shape
                   (loop for (frequency amplitude phase) in layers
                         sum (* amplitude (sin (+ phase (* +tau+ frequency u))))))))
        (setf (aref heights x)
              (max *min-ground* (min *max-ground* (round h))))))))

(defun circle-span (cx cy radius x)
  "The bottom and top of the circle's slice through column X, or nil."
  (let ((dx (- (+ x 0.5d0) cx)))
    (when (< (abs dx) radius)
      (let ((dy (sqrt (- (* radius radius) (* dx dx)))))
        (values (- cy dy) (+ cy dy))))))

(defun circle-columns (cx radius)
  (values (max 0 (floor (- cx radius)))
          (min (1- +world-width+) (ceiling (+ cx radius)))))

(defun carve-circle (heights cx cy radius)
  "Remove the earth inside the circle.  Earth above it drops into the gap."
  (multiple-value-bind (from to) (circle-columns cx radius)
    (loop for x from from to to
          do (multiple-value-bind (bottom top) (circle-span cx cy radius x)
               (when bottom
                 (let* ((h (aref heights x))
                        (removed (- (min h top) (max 0 bottom))))
                   (when (plusp removed)
                     (setf (aref heights x) (max 0 (round (- h removed))))))))))
  heights)

(defun add-dirt (heights cx cy radius)
  "Fill the circle with earth.  Whatever part of it is above the ground
lands on the ground, so a mound grows where it hits."
  (multiple-value-bind (from to) (circle-columns cx radius)
    (loop for x from from to to
          do (multiple-value-bind (bottom top) (circle-span cx cy radius x)
               (when bottom
                 (let* ((h (aref heights x))
                        (added (- top (max h bottom))))
                   (when (plusp added)
                     (setf (aref heights x) (min *ceiling* (round (+ h added))))))))))
  heights)

(defun flatten (heights x half-width)
  "Level the ground around X to its average, for a tank to sit on."
  (let* ((from (column (- x half-width)))
         (to (column (+ x half-width)))
         (level (round (loop for i from from to to sum (aref heights i))
                       (1+ (- to from)))))
    (loop for i from from to to do (setf (aref heights i) level))
    level))
