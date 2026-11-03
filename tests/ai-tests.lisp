;;;; tests/ai-tests.lisp

(in-package #:ridgefire/tests)

(def-suite ai :in all-tests)
(in-suite ai)

(test the-computer-picks-the-nearest-enemy
  (let ((game (flat-game :xs '(100 300 700))))
    (is (eq (tank-of game 1) (pick-target game (tank-of game 0))))
    (is (eq (tank-of game 1) (pick-target game (tank-of game 2))))))

(test a-perfect-computer-hits
  (dolist (wind '(-8 0 8))
    (let* ((game (flat-game :wind wind))
           (shooter (tank-of game 0)))
      (multiple-value-bind (angle power weapon) (plan-shot game shooter :skill 1)
        (set-aim shooter :angle angle :power power :weapon weapon)
        (fire game)
        (is (< (tank-health (tank-of game 1)) 100) "Missed in wind ~d" wind)))))

(test a-computer-aims-over-a-hill
  (let* ((game (flat-game :xs '(150 650)))
         (heights (game-heights game))
         (shooter (tank-of game 0)))
    (loop for x from 350 below 450 do (setf (aref heights x) 260))
    (multiple-value-bind (angle power) (plan-shot game shooter :skill 1)
      (set-aim shooter :angle angle :power power)
      (fire game)
      (is (< (tank-health (tank-of game 1)) 100)))))

(test a-clumsy-computer-is-still-sensible
  (let* ((game (flat-game))
         (shooter (tank-of game 0)))
    (multiple-value-bind (angle power) (plan-shot game shooter :skill 0.3)
      (is (<= 0 angle 90) "Faces its target")
      (is (<= 0 power 100)))))

(test it-will-not-thunder-at-close-range
  (let* ((game (flat-game :xs '(380 420)))
         (shooter (tank-of game 0)))
    (setf (tank-inventory shooter) '((:thunderhead . 1)))
    (is (eq :pebble (nth-value 2 (plan-shot game shooter :skill 1))))))

(test with-nobody-left-the-computer-holds-still
  (let* ((game (flat-game))
         (shooter (tank-of game 0)))
    (setf (tank-alive (tank-of game 1)) nil)
    (set-aim shooter :angle 33 :power 44)
    (is (equal '(33 44 :pebble)
               (multiple-value-list (plan-shot game shooter :skill 1))))))

(test a-perfect-computer-needs-no-luck
  (let* ((game (flat-game))
         (shooter (tank-of game 0)))
    (finishes (plan-shot game shooter :skill 1
                                      :noise (lambda (n) (declare (ignore n))
                                               (error "Asked for noise."))))))

(test a-clumsy-computer-misses-by-a-little
  ;; Noise at its extremes moves the aim by no more than the spread.
  (let* ((game (flat-game))
         (shooter (tank-of game 0)))
    (multiple-value-bind (angle power) (plan-shot game shooter :skill 1)
      (dolist (noise (list (constantly 0) (lambda (n) (1- n))))
        (multiple-value-bind (a p) (plan-shot game shooter :skill 0 :noise noise)
          (is (<= (abs (- a angle)) 8))
          (is (<= (abs (- p power)) 10))
          (is (or (/= a angle) (/= p power)) "Skill 0 spoils the shot."))))))

(test the-computer-chooses-its-weapon-by-range
  (let ((shooter (make-tank :x 100 :inventory (list (cons :thunderhead 1) (cons :boomer 1))))
        (far (make-tank :x 600))
        (near (make-tank :x 180))
        (touching (make-tank :x 120)))
    (is (eq :thunderhead (ridgefire::pick-weapon shooter far)))
    (is (eq :boomer (ridgefire::pick-weapon shooter near)) "Too close for thunder.")
    (is (eq :pebble (ridgefire::pick-weapon shooter touching)))
    (is (eq :pebble (ridgefire::pick-weapon (make-tank :x 100) far)) "Nothing else held.")))

(test the-computer-always-aims-within-limits
  (loop for seed from 1 to 8
        do (let* ((game (new-game :seed seed :players (players :cpu :cpu :cpu)))
                  (shooter (active-tank game))
                  (rng (make-rng seed)))
             (multiple-value-bind (angle power weapon)
                 (plan-shot game shooter :skill 0.4 :noise (lambda (n) (rng-below rng n)))
               (is (<= 0 angle 180))
               (is (<= 0 power 100))
               (is (has-weapon-p shooter weapon))
               ;; Towards its target.
               (let ((target (pick-target game shooter)))
                 (if (> (tank-x target) (tank-x shooter))
                     (is (<= angle 98))
                     (is (>= angle 82))))))))
