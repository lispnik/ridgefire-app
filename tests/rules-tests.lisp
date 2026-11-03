;;;; tests/rules-tests.lisp -- turns, rounds and money, at their edges.

(in-package #:ridgefire/tests)

(def-suite rules :in all-tests)
(in-suite rules)

(defun fire-straight-up (game &optional (power 40))
  "The active tank lobs a pebble straight up; it comes down on itself."
  (set-aim (active-tank game) :angle 90 :power power :weapon :pebble)
  (fire game))

(defun fire-a-dud (game)
  "A short, harmless shot that hurts nobody."
  (let ((tank (active-tank game)))
    (set-aim tank :angle (if (< (tank-x tank) 400) 0 180) :power 10 :weapon :pebble)
    (fire game)))

(test a-turn-begins-with-fire-and-ends-with-the-next-turn
  (let ((game (flat-game)))
    (aim-at game 0 600)
    (let ((events (fire game)))
      (is (eq :fire (first (first events))))
      (is (eq :turn (first (car (last events)))))
      (is (eq :pebble (getf (rest (first events)) :weapon)))
      (is (= 1 (game-shots game))))))

(test events-come-in-order
  ;; Flight before the blast, the blast before the ground changes, the ground
  ;; before the damage it does.
  (let ((game (flat-game)))
    (aim-at game 0 600)
    (let ((kinds (mapcar #'first (fire game))))
      (is (< (position :shot kinds) (position :explosion kinds)
             (position :terrain kinds) (position :turn kinds))))))

(test a-weapon-you-lack-fires-a-pebble
  (let* ((game (flat-game))
         (shooter (tank-of game 0)))
    (setf (tank-inventory shooter) '()
          (tank-weapon shooter) :thunderhead)
    (aim-at game 0 600)
    (let ((fired (first (fire game))))
      (is (eq :pebble (getf (rest fired) :weapon))))
    (is (eq :pebble (tank-weapon shooter)))))

(test there-is-no-firing-between-rounds
  (let ((game (flat-game)))
    (setf (game-phase game) :shop)
    (signals error (fire game))
    (setf (game-phase game) :over)
    (signals error (fire game))))

(test hitting-yourself-hurts-and-pays-nothing
  (let* ((game (flat-game))
         (shooter (tank-of game 0)))
    (let ((events (fire-straight-up game)))
      (is (= 1 (length (events-of :explosion events))))
      (is (eql 0 (getf (rest (first (events-of :damage events))) :tank))))
    (is (< (tank-health shooter) 100))
    (is (zerop (tank-money shooter)))
    (is (zerop (tank-earned shooter)))))

(test blowing-yourself-up-gives-the-round-away
  (let* ((game (flat-game))
         (shooter (tank-of game 0))
         (other (tank-of game 1)))
    (setf (tank-health shooter) 1)
    (fire-straight-up game)
    (is (not (tank-alive shooter)))
    (is (zerop (tank-kills shooter)) "A suicide is no kill.")
    (is (eq :shop (game-phase game)))
    (is (eql 1 (game-round-winner game)))
    (is (= 1 (tank-wins other)))
    (is (= (+ *survivor-bonus* *winner-bonus*) (tank-money other)))))

(test when-everyone-dies-nobody-wins
  ;; A Thunderhead at a neighbour: the blast reaches back to the shooter.
  (let* ((game (armed-game :thunderhead :xs '(200 250)))
         (shooter (tank-of game 0))
         (target (tank-of game 1)))
    (setf (tank-health shooter) 1 (tank-health target) 1)
    (aim-at game 0 250)
    (let ((events (fire game)))
      (is (= 2 (length (events-of :death events))))
      (is (null (getf (rest (first (events-of :round-over events))) :winner))))
    (is (notany #'tank-alive (game-tanks game)))
    (is (null (game-round-winner game)))
    (is (every (lambda (tank) (zerop (tank-wins tank))) (game-tanks game)))
    (is (eq :shop (game-phase game)))
    ;; The other's death still counts to the shooter.
    (is (= 1 (tank-kills shooter)))))

(test a-new-round-sets-everyone-up-again
  (let* ((game (game-in-shop))
         (winner (tank-of game 0))
         (money (tank-money winner)))
    (setf (tank-weapon winner) :boomer
          (tank-angle winner) 10
          (tank-power winner) 99)
    (finish-shopping game 0)
    (finish-shopping game 1)
    (is (= 2 (game-round game)))
    (is (eq :aiming (game-phase game)))
    (is (null (game-shop-ready game)))
    (is (null (game-round-winner game)))
    (is (= money (tank-money winner)) "Money carries over.")
    (loop for tank across (game-tanks game)
          do (is (eq :pebble (tank-weapon tank)))
             (is (= 55 (tank-power tank)))
             (is (= (if (< (tank-x tank) 400) 60 120) (tank-angle tank))
                 "Each faces the middle.")
             (is (zerop (tank-earned tank)))
             (is (= (tank-y tank) (height-at (game-heights game) (tank-x tank)))))
    (is (<= 0 (game-turn game) 1) "Someone's turn: ~a" (game-turn game))))

(test players-may-bring-names-and-colours
  (let ((game (new-game :players (list (list :name "Ada" :colour 3)
                                       (list :colour 2 :kind :cpu)))))
    (is (equal "Ada" (tank-name (tank-of game 0))))
    (is (= 3 (tank-colour (tank-of game 0))))
    (is (equal "Lime" (tank-name (tank-of game 1))) "Named for its colour.")
    (is (eq :cpu (tank-kind (tank-of game 1))))))

(test each-tank-has-its-own-kit
  (let ((game (new-game :players (players :human :human))))
    (add-ammo (tank-of game 0) :boomer 5)
    (is (= 7 (ammo (tank-of game 0) :boomer)))
    (is (= 2 (ammo (tank-of game 1) :boomer)) "Not shared with the starter kit.")
    (is (= 2 (cdr (assoc :boomer *starter-kit*))))))

(test turns-wrap-around-past-the-dead
  (let ((game (flat-game :xs '(100 300 500))))
    (setf (tank-alive (tank-of game 0)) nil
          (game-turn game) 2)
    (fire-a-dud game)
    (is (= 1 (game-turn game)))
    (fire-a-dud game)
    (is (= 2 (game-turn game)))))

(test the-wind-never-blows-too-hard
  (let ((game (flat-game :xs '(100 700) :wind *max-wind*)))
    (loop repeat 60
          do (fire-a-dud game)
             (is (<= (- *max-wind*) (game-wind game) *max-wind*)))
    (is (= 60 (game-shots game)))
    (is (every #'tank-alive (game-tanks game)))))

(test standings-break-ties-on-money
  (let ((game (flat-game :xs '(100 300 500))))
    (setf (tank-wins (tank-of game 0)) 1 (tank-money (tank-of game 0)) 10
          (tank-wins (tank-of game 1)) 1 (tank-money (tank-of game 1)) 900
          (tank-wins (tank-of game 2)) 2 (tank-money (tank-of game 2)) 0)
    (is (equal '(2 1 0) (mapcar #'tank-id (standings game))))))

(test a-path-runs-from-the-muzzle-to-the-impact
  (let* ((game (flat-game))
         (shooter (tank-of game 0)))
    (multiple-value-bind (x y) (barrel-tip shooter 45)
      (multiple-value-bind (vx vy) (launch-velocity 45 50)
        (multiple-value-bind (path impact) (trace-path game x y vx vy :shooter shooter)
          (is (evenp (length path)))
          (is (= x (aref path 0)))
          (is (= y (aref path 1)))
          (is (= (second impact) (aref path (- (length path) 2))))
          (is (eq :ground (first impact)))
          (is (= 100 (third impact)) "Lands on the ground's surface."))))))

(test launch-velocity-points-where-the-barrel-does
  (multiple-value-bind (vx vy) (launch-velocity 0 10)
    (is (> vx 0)) (is (< (abs vy) 1d-9)))
  (multiple-value-bind (vx vy) (launch-velocity 90 10)
    (is (< (abs vx) 1d-9)) (is (> vy 0)))
  (multiple-value-bind (vx vy) (launch-velocity 180 10)
    (is (< vx 0)) (is (< (abs vy) 1d-9)))
  (multiple-value-bind (vx vy) (launch-velocity 45 0)
    (is (zerop vx)) (is (zerop vy))))

(test damage-and-falls-are-monotonic
  (loop for d from 0 to 60
        do (is (>= (blast-damage d 28 55) (blast-damage (1+ d) 28 55)))
           (is (<= 0 (blast-damage d 28 55) 55)))
  (loop for d from 0 to 200
        do (is (<= (fall-damage d) (fall-damage (1+ d))))
           (is (<= 0 (fall-damage d) 50))))
