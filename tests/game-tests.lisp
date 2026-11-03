;;;; tests/game-tests.lisp

(in-package #:ridgefire/tests)

(def-suite game :in all-tests)
(in-suite game)

(test a-new-game-is-ready-to-play
  (let ((game (new-game :seed 3 :players (players :human :cpu :human) :rounds 5)))
    (is (= 1 (game-round game)))
    (is (= 5 (game-rounds game)))
    (is (eq :aiming (game-phase game)))
    (is (= 3 (length (game-tanks game))))
    (is (<= (- *max-wind*) (game-wind game) *max-wind*))
    (loop for tank across (game-tanks game)
          do (is (= 100 (tank-health tank)))
             (is (tank-alive tank))
             ;; Each sits on a flat pad.
             (is (= (tank-y tank) (height-at (game-heights game) (tank-x tank))))
             (is (= (tank-y tank) (height-at (game-heights game) (- (tank-x tank) 8))))
             (is (= (tank-y tank) (height-at (game-heights game) (+ (tank-x tank) 8))))
             (is (equal '((:boomer . 2) (:spade . 1)) (tank-inventory tank))))
    (is (equal '("Crimson" "Azure" "Lime") (map 'list #'tank-name (game-tanks game))))
    ;; Spread apart.
    (let ((xs (sort (map 'list #'tank-x (game-tanks game)) #'<)))
      (is (every (lambda (a b) (> (- b a) 60)) xs (rest xs))))))

(test the-same-seed-makes-the-same-game
  (flet ((make () (new-game :seed 99 :players (players :human :human))))
    (is (equalp (game-heights (make)) (game-heights (make))))
    (is (equal (map 'list #'tank-x (game-tanks (make)))
               (map 'list #'tank-x (game-tanks (make)))))))

(test too-few-or-too-many-players
  (signals error (new-game :players (players :human)))
  (signals error (new-game :players (players :human :human :human :human :human))))

(test aim-is-clamped
  (let ((tank (make-tank)))
    (set-aim tank :angle 200 :power -5)
    (is (= 180 (tank-angle tank)))
    (is (= 0 (tank-power tank)))
    (set-aim tank :angle 44.6 :power 70.2)
    (is (= 45 (tank-angle tank)))
    (is (= 70 (tank-power tank)))
    ;; A weapon it does not hold is ignored.
    (set-aim tank :weapon :thunderhead)
    (is (eq :pebble (tank-weapon tank)))))

(test straight-up-comes-straight-down
  (let* ((game (flat-game))
         (shooter (tank-of game 0)))
    (multiple-value-bind (x y) (barrel-tip shooter 90)
      (multiple-value-bind (vx vy) (launch-velocity 90 40)
        (multiple-value-bind (path impact) (trace-path game x y vx vy)
          (declare (ignore path))
          ;; With no grace period it comes down on its own tank.
          (is (eq :tank (first impact)))
          (is (= 0 (fourth impact))))))))

(test wind-pushes-shells
  (flet ((landing (wind)
           (let* ((game (flat-game :wind wind))
                  (shooter (tank-of game 0)))
             (multiple-value-bind (x y) (barrel-tip shooter 60)
               (multiple-value-bind (vx vy) (launch-velocity 60 50)
                 (second (nth-value 1 (trace-path game x y vx vy :shooter shooter))))))))
    (is (< (landing -10) (landing 0) (landing 10)))))

(test shells-off-the-edge-are-lost
  (let* ((game (flat-game :xs '(780 400)))
         (shooter (tank-of game 0)))
    (set-aim shooter :angle 30 :power 100)
    (let ((events (fire game)))
      (is (null (events-of :explosion events)))
      (is (= 100 (tank-health (tank-of game 1)))))))

(test a-hit-hurts-and-pays
  (let* ((game (flat-game))
         (target (tank-of game 1)))
    (aim-at game 0 600)
    (let* ((events (fire game))
           (damage (first (events-of :damage events))))
      (is (= 1 (length (events-of :explosion events))))
      (is (eql 1 (getf (rest damage) :tank)))
      (is (< (tank-health target) 100))
      (is (= (* *money-per-damage* (- 100 (tank-health target)))
             (tank-money (tank-of game 0))))
      ;; The turn passes, and the wind shifts by at most three.
      (let ((turn (first (events-of :turn events))))
        (is (eql 1 (getf (rest turn) :tank)))
        (is (= 1 (game-turn game)))
        (is (<= -3 (game-wind game) 3))))))

(test blast-damage-falls-off
  (is (= 35 (blast-damage 0 16 35)))
  (is (= 0 (blast-damage 24 16 35)))
  (is (< 0 (blast-damage 12 16 35) 35))
  (is (> (blast-damage 8 16 35) (blast-damage 16 16 35))))

(test shields-soak-first
  (let* ((game (flat-game))
         (target (tank-of game 1)))
    (setf (tank-shield target) 60)
    (aim-at game 0 600)
    (fire game)
    (let ((taken (- 100 (tank-health target))))
      (is (= 100 (tank-health target)) "Taken ~d" taken)
      (is (< (tank-shield target) 60)))))

(test falling-hurts
  (is (= 0 (fall-damage 15)))
  (is (= 10 (fall-damage 35)))
  (is (= 50 (fall-damage 400)))
  ;; Dig out the ground under a tank: it drops, and the drop hurts.
  (let* ((game (flat-game :ground 200))
         (target (tank-of game 1)))
    (setf (tank-inventory (tank-of game 0)) '((:spade . 1)))
    (set-aim (tank-of game 0) :weapon :spade)
    (aim-at game 0 600)
    (let* ((events (fire game))
           (move (first (events-of :move events))))
      (is-true move)
      (is (eql 1 (getf (rest move) :tank)))
      (is (< (tank-y target) 200))
      (is (= (tank-y target) (height-at (game-heights game) (tank-x target))))
      (is (= (- 100 (fall-damage (- 200 (tank-y target)))) (tank-health target))))))

(test a-kill-ends-the-round
  (let* ((game (flat-game))
         (target (tank-of game 1)))
    (setf (tank-health target) 1)
    (aim-at game 0 600)
    (let ((events (fire game)))
      (is (not (tank-alive target)))
      (is (= 1 (length (events-of :death events))))
      ;; The tank blows up in its turn: a second explosion.
      (is (= 2 (length (events-of :explosion events))))
      (is (= 1 (tank-kills (tank-of game 0))))
      (is (= 1 (tank-wins (tank-of game 0))))
      (is (eq :shop (game-phase game)))
      (is (eql 0 (game-round-winner game)))
      (is (getf (rest (first (events-of :round-over events))) :phase))
      (is (null (events-of :turn events)))
      (is (> (tank-money (tank-of game 0))
             (+ *kill-bonus* *survivor-bonus* *winner-bonus*))))))

(test turns-skip-the-dead
  (let ((game (flat-game :xs '(100 300 500 700))))
    (setf (tank-alive (tank-of game 1)) nil)
    (set-aim (tank-of game 0) :angle 90 :power 0)
    ;; A dud at power 0 lands at the shooter's feet; give it a cheap one.
    (set-aim (tank-of game 0) :angle 0 :power 10)
    (fire game)
    (is (= 2 (game-turn game)))))

(test the-last-round-ends-the-game
  (let* ((game (flat-game :rounds 1))
         (target (tank-of game 1)))
    (setf (tank-health target) 1)
    (aim-at game 0 600)
    (fire game)
    (is (eq :over (game-phase game)))
    (is (eq (tank-of game 0) (first (standings game))))))
