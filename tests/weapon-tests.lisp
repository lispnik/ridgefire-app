;;;; tests/weapon-tests.lisp

(in-package #:ridgefire/tests)

(def-suite weapons :in all-tests)
(in-suite weapons)

(defun armed-game (key &rest options)
  (let* ((game (apply #'flat-game options))
         (shooter (tank-of game 0)))
    (setf (tank-inventory shooter) (list (cons key 1)))
    (set-aim shooter :weapon key)
    game))

(test ammo-is-used-up
  (let* ((game (armed-game :boomer))
         (shooter (tank-of game 0)))
    (is (eq :boomer (tank-weapon shooter)))
    (aim-at game 0 600)
    (fire game)
    (is (zerop (ammo shooter :boomer)))
    (is (eq :pebble (tank-weapon shooter)) "Back to pebbles when the last is gone.")
    (is (eq :infinite (ammo shooter :pebble)))
    (is (equal '(:pebble) (owned-weapons shooter)))))

(test cycling-only-offers-what-you-hold
  (let ((tank (make-tank :inventory (list (cons :roller 2) (cons :spade 1)))))
    (is (equal '(:pebble :roller :spade) (owned-weapons tank)))
    (cycle-weapon tank)
    (is (eq :roller (tank-weapon tank)))
    (cycle-weapon tank)
    (is (eq :spade (tank-weapon tank)))
    (cycle-weapon tank)
    (is (eq :pebble (tank-weapon tank)))
    (cycle-weapon tank -1)
    (is (eq :spade (tank-weapon tank)))))

(test splitter-bursts-into-five
  (let ((game (armed-game :splitter :xs '(100 600))))
    (aim-at game 0 600)
    (let* ((events (fire game))
           (shots (events-of :shot events)))
      (is (= 2 (length shots)))
      (is (= 5 (length (getf (rest (second shots)) :paths))))
      (is (<= 3 (length (events-of :explosion events)) 6)))))

(test roller-rolls-downhill
  (let* ((game (armed-game :roller :xs '(100 700)))
         (heights (game-heights game)))
    ;; A slope falling to the right from x=300 to x=500.
    (dotimes (x +world-width+)
      (setf (aref heights x) (cond ((< x 300) 200) ((< x 500) (- 200 (floor (- x 300) 2))) (t 100))))
    (setf (tank-y (tank-of game 0)) 200 (tank-y (tank-of game 1)) 100)
    (aim-at game 0 400)
    (let* ((events (fire game))
           (roll (first (events-of :roll events)))
           (path (getf (rest roll) :path))
           (blast (first (events-of :explosion events))))
      (is-true roll)
      ;; It ends further right than it started, and stops short of the tank.
      (is (> (aref path (- (length path) 2)) (aref path 0)))
      (is (> (getf (rest blast) :x) 400))
      (is (< (getf (rest blast) :x) 700)))))

(test mudball-raises-ground-and-tanks
  (let* ((game (armed-game :mudball))
         (target (tank-of game 1)))
    (aim-at game 0 600)
    (let ((events (fire game)))
      (is (null (events-of :damage events)))
      (is (> (tank-y target) 100))
      (is (= (tank-y target) (height-at (game-heights game) (tank-x target))))
      (is (= 100 (tank-health target))))))

(test spade-hurts-nobody
  (let ((game (armed-game :spade)))
    (aim-at game 0 500)
    (let ((events (fire game)))
      (is (null (events-of :damage events)))
      (is (< (height-at (game-heights game) 500) 100)))))

(test a-tank-blowing-up-can-take-a-neighbour
  (let* ((game (flat-game :xs '(100 600 630)))
         (first-victim (tank-of game 1))
         (neighbour (tank-of game 2)))
    (setf (tank-health first-victim) 1
          (tank-health neighbour) 5)
    (aim-at game 0 600)
    (let ((events (fire game)))
      (is (not (tank-alive first-victim)))
      (is (not (tank-alive neighbour)))
      (is (= 2 (length (events-of :death events))))
      ;; Both kills are the shooter's.
      (is (= 2 (tank-kills (tank-of game 0)))))))
