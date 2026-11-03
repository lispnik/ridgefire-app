;;;; tests/terrain-tests.lisp

(in-package #:ridgefire/tests)

(def-suite terrain :in all-tests)
(in-suite terrain)

(test rng-is-repeatable-and-in-range
  (let ((a (make-rng 42)) (b (make-rng 42)) (c (make-rng 43)))
    (is (equal (loop repeat 20 collect (rng-next a))
               (loop repeat 20 collect (rng-next b))))
    (is (not (equal (loop repeat 5 collect (rng-next (make-rng 42)))
                    (loop repeat 5 collect (rng-next c))))))
  (let ((rng (make-rng 1)))
    (is (every (lambda (n) (<= -3 n 3)) (loop repeat 500 collect (rng-range rng -3 3))))
    (is (= 7 (length (remove-duplicates (loop repeat 500 collect (rng-range rng -3 3))))))
    (is (every (lambda (f) (and (<= 0 f) (< f 1))) (loop repeat 500 collect (rng-float rng)))))
  ;; A zero seed must not stick at zero.
  (is (plusp (rng-next (make-rng 0)))))

(test generated-ground-is-in-bounds-and-repeatable
  (loop for seed from 1 to 30
        for heights = (generate-heights (make-rng seed))
        do (is (= +world-width+ (length heights)))
           (is (every (lambda (h) (<= 24 h 290)) heights))
           (is (equalp heights (generate-heights (make-rng seed))))))

(test a-blast-digs-a-bowl
  (let ((heights (make-heights 100)))
    (carve-circle heights 400 100 30)
    ;; Deepest under the centre, untouched outside the radius.
    (is (= 70 (aref heights 400)))
    (is (< (aref heights 400) (aref heights 415) 100))
    (is (= 100 (aref heights 369)))
    (is (= 100 (aref heights 431)))))

(test a-blast-in-the-air-takes-nothing
  (let ((heights (make-heights 100)))
    (carve-circle heights 400 200 30)
    (is (every (lambda (h) (= h 100)) heights))))

(test a-buried-blast-drops-the-earth-above
  ;; A blast entirely under the surface removes its slice; the column above
  ;; falls into the gap, so the surface sinks by the slice's height.
  (let ((heights (make-heights 200)))
    (carve-circle heights 400 100 20)
    (is (= 160 (aref heights 400)))))

(test ground-never-goes-below-zero
  (let ((heights (make-heights 10)))
    (carve-circle heights 400 10 60)
    (is (every (lambda (h) (>= h 0)) heights))
    (is (zerop (aref heights 400)))))

(test dirt-makes-a-mound
  (let ((heights (make-heights 100)))
    (add-dirt heights 400 100 30)
    (is (= 130 (aref heights 400)))
    (is (> (aref heights 400) (aref heights 420) 100))
    (is (= 100 (aref heights 300))))
  ;; Dirt falling from the sky lands as a whole circle's worth.
  (let ((heights (make-heights 100)))
    (add-dirt heights 400 300 20)
    (is (= 140 (aref heights 400)))))

(test flatten-levels-a-pad
  (let ((heights (make-heights)))
    (dotimes (x +world-width+) (setf (aref heights x) x))
    (let ((level (flatten heights 100 5)))
      (is (= 100 level))
      (is (every (lambda (x) (= 100 (aref heights x))) '(95 98 100 103 105)))
      (is (= 94 (aref heights 94))))))
