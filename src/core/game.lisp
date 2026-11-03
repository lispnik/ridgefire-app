;;;; src/core/game.lisp -- tanks, turns, shots and rounds.
;;;;
;;;; Everything here is plain Lisp with no clock and no screen.  FIRE resolves a
;;;; whole turn at once -- flight, blasts, craters, falls, deaths, the next turn
;;;; or the end of the round -- and returns it as a list of events for a view to
;;;; play back at whatever pace it likes.

(in-package #:ridgefire)

;;; ------------------------------------------------------------------
;;; physics, in world points and seconds

(defparameter *gravity* 220d0)
(defparameter *dt* (/ 1d0 60) "One step of flight; the view plays one per frame.")
(defparameter *speed-per-power* 5d0 "Launch speed is POWER times this.")
(defparameter *wind-accel* 4d0 "Sideways acceleration per unit of wind.")
(defparameter *max-wind* 12)
(defparameter *barrel-length* 13d0)
(defparameter *turret-height* 8d0 "Barrel pivot above the ground a tank sits on.")
(defparameter *tank-hit-radius* 10d0)
(defparameter *tank-centre-height* 5d0)
(defparameter *max-steps* 2400 "Forty seconds of flight: long enough for any shot that lands.")
(defparameter *muzzle-grace* 8 "Steps during which a shell cannot hit the tank that fired it.")
(defparameter *death-blast* '(30 40) "Radius and damage of a tank blowing up.")

;;; ------------------------------------------------------------------
;;; money

(defparameter *money-per-damage* 20)
(defparameter *kill-bonus* 1000)
(defparameter *survivor-bonus* 500)
(defparameter *winner-bonus* 1500)
(defparameter *starter-kit* '((:boomer . 2) (:spade . 1)))

;;; ------------------------------------------------------------------
;;; tanks and the game

(defparameter *colour-names* #("Crimson" "Azure" "Lime" "Amber")
  "Players are known by their colour: index 0-3.")

(defstruct (tank (:copier nil))
  (id 0)
  (name "")
  (colour 0)
  (kind :human)                         ; :human or :cpu
  (owner nil)                           ; the device that controls it, or nil
  (x 0)
  (y 0)
  (health 100)
  (angle 60)                            ; degrees; 0 points right, 90 up, 180 left
  (power 55)                            ; 0-100
  (weapon :pebble)
  (inventory '())                       ; alist of (key . count); pebbles are free
  (money 0)
  (shield 0)
  (alive t)
  (wins 0)
  (kills 0)
  (earned 0))                           ; money earned this round, for the summary

(defstruct (game (:copier nil))
  (heights (make-heights))
  (tanks #())
  (turn 0)                              ; index of the tank whose turn it is
  (wind 0)
  (round 0)
  (rounds 3)
  (phase :aiming)                       ; :aiming, :shop or :over
  (rng (make-rng 1))
  (sky 0)                               ; which sky the view paints this round
  (shop-ready '())                      ; ids of tanks that have finished shopping
  (round-winner nil)
  (shots 0))

(defun tank (game id)
  (aref (game-tanks game) id))

(defun active-tank (game)
  (tank game (game-turn game)))

(defun alive-tanks (game)
  (remove-if-not #'tank-alive (coerce (game-tanks game) 'list)))

(defun tank-centre (tank)
  (values (float (tank-x tank) 1d0)
          (+ (tank-y tank) *tank-centre-height*)))

;;; ------------------------------------------------------------------
;;; inventory

(defun ammo (tank key)
  "How many of KEY the tank holds; :infinite for pebbles."
  (if (eq key :pebble)
      :infinite
      (or (cdr (assoc key (tank-inventory tank))) 0)))

(defun add-ammo (tank key count)
  (let ((cell (assoc key (tank-inventory tank))))
    (if cell
        (incf (cdr cell) count)
        (setf (tank-inventory tank)
              (append (tank-inventory tank) (list (cons key count)))))))

(defun use-ammo (tank key)
  (unless (eq key :pebble)
    (let ((cell (assoc key (tank-inventory tank))))
      (when (and cell (plusp (cdr cell)))
        (decf (cdr cell))
        (when (zerop (cdr cell))
          (setf (tank-inventory tank) (remove cell (tank-inventory tank))))))))

(defun has-weapon-p (tank key)
  (let ((n (ammo tank key)))
    (or (eq n :infinite) (plusp n))))

(defun owned-weapons (tank)
  "Keys of the weapons TANK can fire now, in selector order."
  (loop for w in *weapons*
        when (has-weapon-p tank (weapon-key w))
          collect (weapon-key w)))

(defun cycle-weapon (tank &optional (direction 1))
  "Select the next (or, DIRECTION -1, the previous) weapon the tank holds."
  (let* ((owned (owned-weapons tank))
         (here (or (position (tank-weapon tank) owned) 0)))
    (setf (tank-weapon tank)
          (nth (mod (+ here direction) (length owned)) owned))))

;;; ------------------------------------------------------------------
;;; starting a game and a round

(defun new-game (&key (seed 1) players (rounds 3))
  "A game for PLAYERS, each a plist of :name :kind :owner :colour, already in
its first round."
  (assert (<= 2 (length players) 4) () "Ridgefire takes two to four players.")
  (let* ((tanks (loop for player in players
                      for id from 0
                      for colour = (getf player :colour id)
                      collect (make-tank :id id
                                         :colour colour
                                         :name (or (getf player :name)
                                                   (aref *colour-names* colour))
                                         :kind (getf player :kind :human)
                                         :owner (getf player :owner)
                                         :inventory (copy-alist *starter-kit*))))
         (game (make-game :tanks (coerce tanks 'vector)
                          :rounds rounds
                          :rng (make-rng seed))))
    (start-round game)
    game))

(defun shuffle (rng list)
  (let ((vector (coerce list 'vector)))
    (loop for i from (1- (length vector)) downto 1
          do (rotatef (aref vector i) (aref vector (rng-below rng (1+ i)))))
    (coerce vector 'list)))

(defun place-tanks (game)
  "Spread the tanks across the world in a random order, each on a flat pad."
  (let* ((rng (game-rng game))
         (tanks (shuffle rng (coerce (game-tanks game) 'list)))
         (margin 40)
         (slot (/ (- +world-width+ (* 2 margin)) (length tanks))))
    (loop for tank in tanks
          for i from 0
          for x = (round (+ margin (* slot (+ i 1/2))
                            (- (rng-below rng (max 1 (floor slot 2))) (floor slot 4))))
          do (setf (tank-x tank) x
                   (tank-y tank) (flatten (game-heights game) x 11)))))

(defun start-round (game)
  "The next round: new ground, new places, full health."
  (let ((rng (game-rng game)))
    (incf (game-round game))
    (setf (game-heights game) (generate-heights rng)
          (game-sky game) (rng-below rng 4))
    (place-tanks game)
    (loop for tank across (game-tanks game)
          do (setf (tank-health tank) 100
                   (tank-alive tank) t
                   (tank-shield tank) 0
                   (tank-earned tank) 0
                   (tank-weapon tank) :pebble
                   (tank-angle tank) (if (< (tank-x tank) (/ +world-width+ 2)) 60 120)
                   (tank-power tank) 55)
             (when (plusp (ammo tank :shield))
               (use-ammo tank :shield)
               (setf (tank-shield tank) *shield-strength*)))
    (setf (game-wind game) (rng-range rng -8 8)
          (game-turn game) (rng-below rng (length (game-tanks game)))
          (game-phase game) :aiming
          (game-shop-ready game) '()
          (game-round-winner game) nil)
    game))

;;; ------------------------------------------------------------------
;;; aiming

(defun clamp (value low high)
  (max low (min high value)))

(defun set-aim (tank &key angle power weapon)
  "Point TANK; out-of-range values are clamped, and a weapon it lacks ignored."
  (when angle (setf (tank-angle tank) (clamp (round angle) 0 180)))
  (when power (setf (tank-power tank) (clamp (round power) 0 100)))
  (when (and weapon (weapon-key-p weapon) (has-weapon-p tank weapon))
    (setf (tank-weapon tank) weapon))
  tank)

(defun radians (degrees)
  (* degrees (/ +tau+ 360)))

(defun barrel-tip (tank &optional (angle (tank-angle tank)))
  (let ((a (radians angle)))
    (values (+ (tank-x tank) (* *barrel-length* (cos a)))
            (+ (tank-y tank) *turret-height* (* *barrel-length* (sin a))))))

(defun launch-velocity (angle power)
  (let ((a (radians angle))
        (speed (* power *speed-per-power*)))
    (values (* speed (cos a)) (* speed (sin a)))))

;;; ------------------------------------------------------------------
;;; flight

(defun tank-at (game x y &optional ignore)
  "A living tank whose hit circle holds X, Y, other than IGNORE."
  (loop for tank across (game-tanks game)
        when (and (tank-alive tank) (not (eq tank ignore)))
          do (multiple-value-bind (cx cy) (tank-centre tank)
               (when (< (+ (expt (- x cx) 2) (expt (- y cy) 2))
                        (expt *tank-hit-radius* 2))
                 (return tank)))))

(defun trace-path (game x y vx vy &key shooter until-apex)
  "Fly a shell from X, Y.  Returns the path, a flat vector x0 y0 x1 y1 ..., and
how it ended: (:ground x y), (:tank x y id), (:lost x y), or with UNTIL-APEX,
(:apex x y vx vy) when it starts to fall."
  (let ((path (make-array 256 :adjustable t :fill-pointer 0))
        (ax (* (game-wind game) *wind-accel*))
        (heights (game-heights game))
        (x (float x 1d0)) (y (float y 1d0))
        (vx (float vx 1d0)) (vy (float vy 1d0)))
    (flet ((done (impact)
             (vector-push-extend x path)
             (vector-push-extend y path)
             (return-from trace-path (values path impact))))
      (dotimes (step *max-steps* (done (list :lost x y)))
        (vector-push-extend x path)
        (vector-push-extend y path)
        (incf vx (* ax *dt*))
        (decf vy (* *gravity* *dt*))
        (incf x (* vx *dt*))
        (incf y (* vy *dt*))
        (let ((hit (tank-at game x y (and (< step *muzzle-grace*) shooter))))
          (cond (hit (done (list :tank x y (tank-id hit))))
                ((and (<= 0 x) (< x +world-width+) (<= y (height-at heights x)))
                 (done (list :ground x (float (height-at heights x) 1d0))))
                ((or (< x -80) (> x (+ +world-width+ 80)) (< y -40))
                 (done (list :lost x y)))
                ((and until-apex (minusp vy))
                 (done (list :apex x y vx vy)))))))))

(defun roll-path (game x direction)
  "A roller's way along the ground from column X: downhill or level, never up,
stopping against a tank.  Returns the path and the stopping point."
  (let* ((heights (game-heights game))
         (path (make-array 64 :adjustable t :fill-pointer 0))
         (here (column x)))
    (flet ((note (column)
             (vector-push-extend (+ column 0.5d0) path)
             (vector-push-extend (float (aref heights column) 1d0) path)))
      (note here)
      (loop repeat 500
            for next = (+ here direction)
            while (and (< -1 next +world-width+)
                       (<= (aref heights next) (aref heights here))
                       (not (find-if (lambda (tank)
                                       (and (tank-alive tank)
                                            (< (abs (- (tank-x tank) next)) 9)))
                                     (game-tanks game))))
            do (setf here next)
               (note here))
      (values path (+ here 0.5d0) (float (aref heights here) 1d0)))))

;;; ------------------------------------------------------------------
;;; blasts, damage and deaths

(defvar *events* '() "Events of the turn being resolved, newest first.")

(defun emit (&rest event)
  (push event *events*))

(defun credit (game owner victim amount)
  "Pay OWNER for AMOUNT damage done to someone else."
  (when (and owner (/= owner (tank-id victim)) (plusp amount))
    (let ((shooter (tank game owner)))
      (incf (tank-money shooter) (* amount *money-per-damage*))
      (incf (tank-earned shooter) (* amount *money-per-damage*)))))

(defun hurt (game tank amount owner)
  (when (and (tank-alive tank) (plusp amount))
    (let ((absorbed (min (tank-shield tank) amount)))
      (decf (tank-shield tank) absorbed)
      (setf (tank-health tank) (max 0 (- (tank-health tank) (- amount absorbed))))
      (credit game owner tank amount)
      (emit :damage :tank (tank-id tank) :amount amount
                    :health (tank-health tank) :shield (tank-shield tank)))))

(defun blast-damage (distance radius damage)
  "Full DAMAGE near the centre, falling to nothing at the edge of the blast."
  (let ((reach (+ radius 8)))
    (cond ((< distance 6) damage)
          ((>= distance reach) 0)
          (t (round (* damage (- 1 (/ distance reach))))))))

(defun fall-damage (distance)
  (if (<= distance 15) 0 (min 50 (floor (- distance 15) 2))))

(defun settle-tanks (game owner)
  "Drop every tank to the ground under it, or lift it onto new earth."
  (loop for tank across (game-tanks game)
        for ground = (height-at (game-heights game) (tank-x tank))
        when (and (tank-alive tank) (/= ground (tank-y tank)))
          do (let* ((from (tank-y tank))
                    (damage (if (< ground from) (fall-damage (- from ground)) 0)))
               (setf (tank-y tank) ground)
               (emit :move :tank (tank-id tank) :from from :to ground :damage damage)
               (hurt game tank damage owner))))

(defun collect-deaths (game owner)
  "Tanks out of health are dead; return them."
  (loop for tank across (game-tanks game)
        when (and (tank-alive tank) (<= (tank-health tank) 0))
          collect (progn
                    (setf (tank-alive tank) nil)
                    (when (and owner (/= owner (tank-id tank)))
                      (let ((killer (tank game owner)))
                        (incf (tank-kills killer))
                        (incf (tank-money killer) *kill-bonus*)
                        (incf (tank-earned killer) *kill-bonus*)))
                    (emit :death :tank (tank-id tank) :by owner)
                    tank)))

(defun resolve-blasts (game blasts)
  "Explode each of BLASTS, (x y radius damage effect owner), and every tank
blown up by them in turn."
  (loop while blasts
        do (destructuring-bind (x y radius damage effect owner) (pop blasts)
             (let ((heights (game-heights game)))
               (emit :explosion :x x :y y :radius radius :effect effect)
               (ecase effect
                 (:dirt (add-dirt heights x y radius))
                 (:dig (carve-circle heights x y radius))
                 (:blast
                  (loop for tank across (game-tanks game)
                        when (tank-alive tank)
                          do (multiple-value-bind (cx cy) (tank-centre tank)
                               (hurt game tank
                                     (blast-damage (sqrt (+ (expt (- x cx) 2) (expt (- y cy) 2)))
                                                   radius damage)
                                     owner)))
                  (carve-circle heights x y radius)))
               (emit :terrain :heights (copy-seq heights))
               (settle-tanks game owner)
               (dolist (dead (collect-deaths game owner))
                 (multiple-value-bind (cx cy) (tank-centre dead)
                   (setf blasts (append blasts
                                        (list (list cx cy (first *death-blast*)
                                                    (second *death-blast*) :blast owner))))))))))

(defun blast-for (weapon x y owner)
  (list x y (weapon-radius weapon) (weapon-damage weapon)
        (case (weapon-effect weapon) ((:dirt :dig) (weapon-effect weapon)) (t :blast))
        owner))

;;; ------------------------------------------------------------------
;;; a turn

(defun fly (game weapon x y vx vy shooter)
  "Emit the flight of WEAPON and return the blasts it ends in."
  (let ((owner (tank-id shooter)))
    (ecase (weapon-effect weapon)
      ((:blast :dirt :dig)
       (multiple-value-bind (path impact) (trace-path game x y vx vy :shooter shooter)
         (emit :shot :paths (list path))
         (unless (eq (first impact) :lost)
           (list (blast-for weapon (second impact) (third impact) owner)))))
      (:split
       (multiple-value-bind (path impact)
           (trace-path game x y vx vy :shooter shooter :until-apex t)
         (emit :shot :paths (list path))
         (case (first impact)
           (:lost '())
           (:apex
            (destructuring-bind (ax ay avx avy) (rest impact)
              (declare (ignore avy))
              (let ((ends '()))
                (emit :shot :paths
                      (loop for spread in '(-60 -30 0 30 60)
                            collect (multiple-value-bind (p end)
                                        (trace-path game ax ay (+ avx spread) 0 :shooter shooter)
                                      (push end ends)
                                      p)))
                (loop for end in (nreverse ends)
                      unless (eq (first end) :lost)
                        collect (blast-for weapon (second end) (third end) owner)))))
           (t (list (blast-for weapon (second impact) (third impact) owner))))))
      (:roll
       (multiple-value-bind (path impact) (trace-path game x y vx vy :shooter shooter)
         (emit :shot :paths (list path))
         (case (first impact)
           (:lost '())
           (:tank (list (blast-for weapon (second impact) (third impact) owner)))
           (t
            (let* ((heights (game-heights game))
                   (c (column (second impact)))
                   (left (aref heights (column (- c 3))))
                   (right (aref heights (column (+ c 3))))
                   (direction (cond ((< left right) -1) ((> left right) 1) ((minusp vx) -1) (t 1))))
              (multiple-value-bind (rolled rx ry) (roll-path game c direction)
                (emit :roll :path rolled)
                (list (blast-for weapon rx ry owner)))))))))))

(defun fire (game)
  "The active tank fires.  Resolves the whole turn and returns its events,
oldest first: :shot :roll :explosion :terrain :damage :move :death, then
:turn or :round-over."
  (assert (eq (game-phase game) :aiming))
  (let* ((*events* '())
         (shooter (active-tank game))
         (weapon (find-weapon (if (has-weapon-p shooter (tank-weapon shooter))
                                  (tank-weapon shooter)
                                  :pebble))))
    (use-ammo shooter (weapon-key weapon))
    (incf (game-shots game))
    (multiple-value-bind (x y) (barrel-tip shooter)
      (multiple-value-bind (vx vy) (launch-velocity (tank-angle shooter) (tank-power shooter))
        (emit :fire :tank (tank-id shooter) :weapon (weapon-key weapon))
        (resolve-blasts game (fly game weapon x y vx vy shooter))))
    (unless (has-weapon-p shooter (tank-weapon shooter))
      (setf (tank-weapon shooter) :pebble))
    (end-turn game)
    (nreverse *events*)))

(defun next-alive (game from)
  (let ((n (length (game-tanks game))))
    (loop for step from 1 to n
          for i = (mod (+ from step) n)
          when (tank-alive (tank game i))
            return i)))

(defun end-turn (game)
  (let ((alive (alive-tanks game)))
    (if (<= (length alive) 1)
        (end-round game (first alive))
        (let ((rng (game-rng game)))
          (setf (game-turn game) (next-alive game (game-turn game))
                (game-wind game) (clamp (+ (game-wind game) (rng-range rng -3 3))
                                        (- *max-wind*) *max-wind*))
          (emit :turn :tank (game-turn game) :wind (game-wind game))))))

(defun end-round (game winner)
  (loop for tank across (game-tanks game)
        when (tank-alive tank)
          do (incf (tank-money tank) *survivor-bonus*)
             (incf (tank-earned tank) *survivor-bonus*))
  (when winner
    (incf (tank-wins winner))
    (incf (tank-money winner) *winner-bonus*)
    (incf (tank-earned winner) *winner-bonus*))
  (setf (game-round-winner game) (and winner (tank-id winner))
        (game-phase game) (if (>= (game-round game) (game-rounds game)) :over :shop))
  (when (eq (game-phase game) :shop)
    ;; Computer players shop at once.
    (loop for tank across (game-tanks game)
          when (eq (tank-kind tank) :cpu)
            do (auto-shop tank)
               (push (tank-id tank) (game-shop-ready game))))
  (emit :round-over :winner (game-round-winner game) :phase (game-phase game)))

;;; ------------------------------------------------------------------
;;; the shop

(defun buy (game id key)
  "TANK ID buys one bundle of KEY.  True if it could afford it."
  (let ((tank (tank game id))
        (item (find-shop-item key)))
    (when (and item (eq (game-phase game) :shop)
               (>= (tank-money tank) (getf item :price)))
      (decf (tank-money tank) (getf item :price))
      (add-ammo tank key (getf item :bundle))
      t)))

(defun auto-shop (tank)
  "A computer player's shopping: a shield if it has none, then the best
weapons it can afford, keeping a little back."
  (flet ((try (key)
           (let ((item (find-shop-item key)))
             (when (>= (- (tank-money tank) 500) (getf item :price))
               (decf (tank-money tank) (getf item :price))
               (add-ammo tank key (getf item :bundle))
               t))))
    (when (zerop (ammo tank :shield)) (try :shield))
    (loop repeat 6
          while (or (try :thunderhead) (try :splitter) (try :boomer) (try :roller)))))

(defun finish-shopping (game id)
  "TANK ID is done shopping.  When everyone is, the next round starts; true then."
  (when (eq (game-phase game) :shop)
    (pushnew id (game-shop-ready game))
    (when (every (lambda (tank) (member (tank-id tank) (game-shop-ready game)))
                 (game-tanks game))
      (start-round game)
      t)))

(defun standings (game)
  "Tanks, most round wins first, then most money."
  (sort (coerce (game-tanks game) 'list)
        (lambda (a b)
          (or (> (tank-wins a) (tank-wins b))
              (and (= (tank-wins a) (tank-wins b))
                   (> (tank-money a) (tank-money b)))))))
