;;;; tests/play-tests.lisp -- whole games, played at random, checked after every shot.
;;;;
;;;; Each game is driven by its own seeded generator, so a failure names a seed
;;;; that repeats it exactly.

(in-package #:ridgefire/tests)

(def-suite play :in all-tests)
(in-suite play)

(defparameter *every-weapon*
  '((:boomer . 3) (:splitter . 2) (:roller . 3) (:thunderhead . 2)
    (:mudball . 3) (:spade . 3) (:shield . 1)))

(defun problems (game events)
  "What is wrong with GAME after a turn that produced EVENTS, as strings."
  (let ((wrong '()))
    (flet ((wrong (control &rest args)
             (push (apply #'format nil control args) wrong)))
      (unless (every (lambda (h) (<= 0 h ridgefire::*ceiling*)) (game-heights game))
        (wrong "ground out of bounds"))
      (unless (<= (- *max-wind*) (game-wind game) *max-wind*)
        (wrong "wind ~a" (game-wind game)))
      (unless (member (game-phase game) '(:aiming :shop :over))
        (wrong "phase ~a" (game-phase game)))
      (loop for tank across (game-tanks game)
            for id = (tank-id tank)
            do (cond ((tank-alive tank)
                      (unless (<= 1 (tank-health tank) 100)
                        (wrong "tank ~d alive at health ~a" id (tank-health tank)))
                      (unless (= (tank-y tank) (height-at (game-heights game) (tank-x tank)))
                        (wrong "tank ~d floating at ~a over ground ~a" id (tank-y tank)
                               (height-at (game-heights game) (tank-x tank)))))
                     ((/= 0 (tank-health tank))
                      (wrong "tank ~d dead with health ~a" id (tank-health tank))))
               (unless (<= 0 (tank-shield tank) *shield-strength*)
                 (wrong "tank ~d shield ~a" id (tank-shield tank)))
               (unless (>= (tank-money tank) 0)
                 (wrong "tank ~d money ~a" id (tank-money tank)))
               (unless (has-weapon-p tank (tank-weapon tank))
                 (wrong "tank ~d holds ~a but has none" id (tank-weapon tank))))
      (if (eq (game-phase game) :aiming)
          (progn
            (unless (tank-alive (active-tank game))
              (wrong "a dead tank's turn"))
            (unless (>= (length (alive-tanks game)) 2)
              (wrong "a round with one tank left goes on")))
          (let ((winner (game-round-winner game)))
            (unless (or (null winner) (tank-alive (tank-of game winner)))
              (wrong "the round went to a dead tank"))))
      (when events
        (unless (eq :fire (first (first events)))
          (wrong "turn began with ~a" (first (first events))))
        (unless (member (first (car (last events))) '(:turn :round-over))
          (wrong "turn ended with ~a" (first (car (last events))))))
      (let ((plist (game->plist game)))
        (unless (and (plain-data-p plist) (equal plist (game->plist (plist->game plist))))
          (wrong "the game does not survive the wire"))))
    (nreverse wrong)))

(defun play-at-random (seed &key (players 3) (rounds 2) (max-shots 400))
  "Play a game with random aim and weapons.  Returns the game and the
problems found, each tagged with the shot it followed."
  (let* ((rng (make-rng seed))
         (game (new-game :seed seed :rounds rounds
                         :players (apply #'players (make-list players :initial-element :human))))
         (found '()))
    (loop for tank across (game-tanks game)
          do (setf (tank-inventory tank) (copy-alist *every-weapon*)))
    (loop for shot from 1 to max-shots
          until (eq (game-phase game) :over)
          do (ecase (game-phase game)
               (:aiming
                (let* ((tank (active-tank game))
                       (owned (owned-weapons tank)))
                  (set-aim tank :angle (rng-range rng 0 180)
                                :power (rng-range rng 10 100)
                                :weapon (nth (rng-below rng (length owned)) owned))
                  (let ((events (fire game)))
                    (dolist (problem (problems game events))
                      (push (format nil "seed ~d, shot ~d: ~a" seed shot problem) found)))))
               (:shop
                (loop for tank across (game-tanks game)
                      do (loop repeat 3
                               do (buy game (tank-id tank)
                                       (getf (nth (rng-below rng (length (shop-items))) (shop-items))
                                             :key))))
                (loop for tank across (game-tanks game)
                      do (finish-shopping game (tank-id tank)))
                (dolist (problem (problems game nil))
                  (push (format nil "seed ~d, new round: ~a" seed problem) found)))))
    (values game (nreverse found))))

(test random-games-keep-the-rules
  (loop for seed from 1 to 24
        do (multiple-value-bind (game found)
               (play-at-random seed :players (+ 2 (mod seed 3)))
             (declare (ignore game))
             (is (null found) "~{~a~^~%~}" (subseq found 0 (min 5 (length found)))))))

(test random-games-end
  ;; Random shots at two tanks finish a two-round game well within the limit.
  (let ((ended (loop for seed from 100 to 115
                     count (eq :over (game-phase (play-at-random seed :players 2 :max-shots 600))))))
    (is (>= ended 14) "Only ~d of 16 ended" ended)))

(test a-game-replays-exactly
  ;; What a Nearby guest relies on: the same game and the same shots give the
  ;; same result, down to the generator.
  (is (equal (game->plist (play-at-random 7 :players 4))
             (game->plist (play-at-random 7 :players 4)))))

(test computers-finish-a-game-between-themselves
  (let* ((game (new-game :seed 11 :rounds 2 :players (players :cpu :cpu)))
         (rng (make-rng 11))
         (noise (lambda (n) (rng-below rng n))))
    (loop repeat 200
          until (eq (game-phase game) :over)
          do (if (eq (game-phase game) :shop)
                 (progn (finish-shopping game 0) (finish-shopping game 1))
                 (let ((tank (active-tank game)))
                   (multiple-value-bind (angle power weapon)
                       (plan-shot game tank :skill 0.8 :noise noise)
                     (set-aim tank :angle angle :power power :weapon weapon))
                   (let ((events (fire game)))
                     (is (null (problems game events)))))))
    (is (eq :over (game-phase game)))
    (is (= 2 (game-round game)))
    (is (<= (reduce #'+ (game-tanks game) :key #'tank-wins) 2))))
