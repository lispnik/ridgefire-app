;;;; tests/package.lisp

(defpackage #:ridgefire/tests
  (:use #:common-lisp #:fiveam #:ridgefire)
  (:export #:all-tests #:run-tests))

(in-package #:ridgefire/tests)

(def-suite all-tests :description "Everything.")

(defun run-tests ()
  "Run every test; true if they all passed."
  (let ((results (run 'all-tests)))
    (explain! results)
    (results-status results)))

(defun players (&rest kinds)
  (loop for kind in (or kinds '(:human :human))
        for i from 0
        collect (list :kind kind :owner "me")))

(defun flat-game (&key (xs '(200 600)) (ground 100) (wind 0) (kinds nil) (rounds 3))
  "A game on level ground with tanks at XS, no wind, tank 0 to fire."
  (let ((game (new-game :seed 7 :rounds rounds
                        :players (apply #'players (or kinds (make-list (length xs) :initial-element :human))))))
    (fill (game-heights game) ground)
    (loop for tank across (game-tanks game)
          for x in xs
          do (setf (tank-x tank) x (tank-y tank) ground
                   (tank-shield tank) 0 (tank-health tank) 100))
    (setf (game-wind game) wind
          (game-turn game) 0)
    game))

(defun events-of (kind events)
  (remove kind events :key #'first :test-not #'eq))

(defun aim-at (game shooter-id target-x)
  "Search for the power that lands closest to TARGET-X at 45 or 135 degrees."
  (let* ((shooter (tank-of game shooter-id))
         (angle (if (> target-x (tank-x shooter)) 45 135)))
    (loop with best = nil and best-power = 0
          for power from 1 to 100
          do (multiple-value-bind (x y) (barrel-tip shooter angle)
               (multiple-value-bind (vx vy) (launch-velocity angle power)
                 (let ((impact (nth-value 1 (trace-path game x y vx vy :shooter shooter))))
                   (when (member (first impact) '(:ground :tank))
                     (let ((miss (abs (- (second impact) target-x))))
                       (when (or (null best) (< miss best))
                         (setf best miss best-power power)))))))
          finally (set-aim shooter :angle angle :power best-power))
    shooter))

(defun tank-of (game id)
  (aref (game-tanks game) id))
