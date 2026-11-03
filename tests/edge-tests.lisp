;;;; tests/edge-tests.lisp -- the generator, the ground and the shop at their limits.

(in-package #:ridgefire/tests)

(def-suite edges :in all-tests)
(in-suite edges)

;;; the generator

(test a-zero-seed-still-makes-numbers
  (let ((rng (make-rng 0)))
    (is (plusp (rng-state rng)))
    (is (/= (rng-next rng) (rng-next rng)))))

(test different-seeds-differ
  (is (/= (rng-next (make-rng 1)) (rng-next (make-rng 2))))
  (is (/= (rng-next (make-rng 12345)) (rng-next (make-rng 12346)))))

(test ranges-reach-both-ends
  (let ((rng (make-rng 42))
        (seen (make-array 7 :initial-element nil)))
    (loop repeat 2000
          for n = (rng-range rng -3 3)
          do (is (<= -3 n 3))
             (setf (aref seen (+ n 3)) t))
    (is (every #'identity seen))
    (is (zerop (rng-below rng 1)))
    (is (zerop (rng-below rng 0)))
    (is (= 5 (rng-range rng 5 5)))))

(test floats-stay-below-one
  (let ((rng (make-rng 9)))
    (loop repeat 2000
          for f = (rng-float rng)
          do (is (and (typep f 'double-float) (<= 0d0 f) (< f 1d0))))))

(test huge-and-negative-seeds-are-fine
  (dolist (seed (list -1 (expt 2 40) most-negative-fixnum))
    (let ((rng (make-rng seed)))
      (is (typep (rng-next rng) '(unsigned-byte 32))))))

;;; the ground

(test columns-are-clamped-to-the-world
  (is (= 0 (column -5)))
  (is (= 0 (column -0.5)))
  (is (= 12 (column 12.7)))
  (is (= (1- +world-width+) (column 9999)))
  (let ((heights (make-heights 50)))
    (setf (aref heights 0) 7 (aref heights (1- +world-width+)) 9)
    (is (= 7 (height-at heights -100)))
    (is (= 9 (height-at heights 5000)))))

(test every-landscape-is-in-bounds-and-they-differ
  (let ((landscapes (loop for seed from 1 to 40
                          collect (generate-heights (make-rng seed)))))
    (dolist (heights landscapes)
      (is (= +world-width+ (length heights)))
      (is (every (lambda (h) (<= 24 h 290)) heights)))
    (is (> (length (remove-duplicates landscapes :test #'equalp)) 35))
    ;; Hills, not a plain: each has some rise and fall.
    (dolist (heights landscapes)
      (is (> (- (reduce #'max heights) (reduce #'min heights)) 20)))))

(test dirt-stops-at-the-ceiling
  (let ((heights (make-heights 340)))
    (add-dirt heights 400 340 80)
    (is (every (lambda (h) (<= h ridgefire::*ceiling*)) heights))
    (is (= ridgefire::*ceiling* (aref heights 400)))))

(test dirt-in-the-air-still-lands
  ;; A mudball bursting high up drops its earth on the ground below.
  (let ((heights (make-heights 100)))
    (add-dirt heights 400 300 20)
    (is (> (aref heights 400) 100))
    (is (= 100 (aref heights 300)) "Nothing outside the circle's columns.")))

(test blasts-at-the-edges-are-safe
  (let ((heights (make-heights 100)))
    (carve-circle heights 0 100 30)
    (carve-circle heights (1- +world-width+) 100 30)
    (carve-circle heights -50 100 30)
    (carve-circle heights 900 100 120)
    (is (< (aref heights 0) 100))
    (is (< (aref heights (1- +world-width+)) 100))
    (add-dirt heights -10 100 30)
    (is (every (lambda (h) (<= 0 h ridgefire::*ceiling*)) heights))))

(test an-enormous-blast-cannot-dig-below-zero
  (let ((heights (make-heights 100)))
    (carve-circle heights 400 0 300)
    (is (every (lambda (h) (>= h 0)) heights))
    (is (zerop (aref heights 400)))))

(test flattening-at-the-edge
  (let ((heights (make-heights 0)))
    (loop for x from 0 below 30 do (setf (aref heights x) x))
    (let ((level (flatten heights 0 11)))
      ;; Columns 0..11 average to 5.5, which rounds to even.
      (is (= 6 level))
      (is (every (lambda (h) (= h level)) (subseq heights 0 12)))
      (is (= 12 (aref heights 12)) "No further than asked."))))

;;; weapons and the shop

(test every-weapon-is-well-formed
  (dolist (w *weapons*)
    (is (plusp (weapon-radius w)))
    (is (plusp (weapon-bundle w)))
    (is (plusp (length (weapon-blurb w))))
    (is (member (weapon-effect w) '(:blast :split :roll :dirt :dig)))
    (if (member (weapon-effect w) '(:dirt :dig))
        (is (zerop (weapon-damage w)) "~a hurts nobody" (weapon-name w))
        (is (plusp (weapon-damage w)) "~a hurts" (weapon-name w))))
  (is (= 7 (length *weapons*)))
  (is (= (length *weapons*)
         (length (remove-duplicates (mapcar #'weapon-key *weapons*))))))

(test only-pebbles-are-free
  (is (equal '(:pebble) (mapcar #'weapon-key
                                (remove-if #'plusp *weapons* :key #'weapon-price)))))

(test unknown-weapons
  (signals error (find-weapon :bazooka))
  (is (not (weapon-key-p :bazooka)))
  (is (not (weapon-key-p :shield)) "A shield is bought, not fired.")
  (is (weapon-key-p :spade))
  (is (null (find-shop-item :pebble)))
  (is (null (find-shop-item :bazooka)))
  (is (= *shield-price* (getf (find-shop-item :shield) :price))))

(test buying-nothing-costs-nothing
  (let* ((game (game-in-shop :money 9000))
         (buyer (tank-of game 0))
         (money (tank-money buyer)))
    (is (null (buy game 0 :bazooka)))
    (is (null (buy game 0 :pebble)))
    (is (= money (tank-money buyer)))))

(test you-can-spend-down-to-nothing
  (let* ((game (game-in-shop))
         (buyer (tank-of game 1)))
    (setf (tank-money buyer) (* 2 (weapon-price (find-weapon :boomer))))
    (is-true (buy game 1 :boomer))
    (is-true (buy game 1 :boomer))
    (is (zerop (tank-money buyer)))
    (is (null (buy game 1 :boomer)))
    (is (= (+ 2 (* 2 (weapon-bundle (find-weapon :boomer)))) (ammo buyer :boomer))
        "Two bundles on top of the starter kit's two.")))

(test two-shields-last-two-rounds
  (let ((game (game-in-shop :rounds 5 :money 10000)))
    (buy game 0 :shield)
    (buy game 0 :shield)
    (is (= 2 (ammo (tank-of game 0) :shield)))
    (finish-shopping game 0)
    (finish-shopping game 1)
    (is (= *shield-strength* (tank-shield (tank-of game 0))))
    (is (= 1 (ammo (tank-of game 0) :shield)))))

(test finishing-twice-is-harmless
  (let ((game (game-in-shop)))
    (finish-shopping game 0)
    (finish-shopping game 0)
    (is (eq :shop (game-phase game)))
    (is (equal '(0) (game-shop-ready game)))
    (is (null (finish-shopping (flat-game) 0)) "Not in the shop: nothing happens.")))

(test the-computer-keeps-something-back
  (let ((rich (make-tank :money 20000))
        (poor (make-tank :money 400)))
    (auto-shop rich)
    (auto-shop poor)
    (is (>= (tank-money rich) 500))
    (is (= 1 (ammo rich :shield)))
    (is (plusp (ammo rich :thunderhead)))
    (is (= 400 (tank-money poor)))
    (is (null (tank-inventory poor)))))

(test a-computer-with-a-shield-buys-no-other
  (let ((tank (make-tank :money 20000 :inventory (list (cons :shield 1)))))
    (auto-shop tank)
    (is (= 1 (ammo tank :shield)))))
