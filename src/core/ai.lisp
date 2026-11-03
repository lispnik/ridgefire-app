;;;; src/core/ai.lisp -- a computer player that aims by trying shots.
;;;;
;;;; It flies candidate shells through the real ground and wind with
;;;; TRACE-PATH, keeps the one landing nearest its target, refines it, then
;;;; spoils it a little according to SKILL so that it can miss.

(in-package #:ridgefire)

(defun pick-target (game shooter)
  "The nearest living enemy."
  (let ((enemies (remove shooter (alive-tanks game))))
    (first (sort enemies #'< :key (lambda (tank) (abs (- (tank-x tank) (tank-x shooter))))))))

(defun pick-weapon (shooter target)
  "The hardest-hitting weapon held, but nothing that would catch the shooter."
  (let ((distance (abs (- (tank-x target) (tank-x shooter)))))
    (or (loop for key in '(:thunderhead :splitter :boomer :roller)
              for weapon = (find-weapon key)
              when (and (has-weapon-p shooter key)
                        (> distance (+ (weapon-radius weapon) 40)))
                return key)
        :pebble)))

(defun shot-miss (game shooter angle power target)
  "How far from TARGET a shell fired at ANGLE, POWER lands, or nil if it is
lost or would land on the shooter."
  (multiple-value-bind (x y) (barrel-tip shooter angle)
    (multiple-value-bind (vx vy) (launch-velocity angle power)
      (let ((impact (nth-value 1 (trace-path game x y vx vy :shooter shooter))))
        (unless (eq (first impact) :lost)
          (let ((ix (second impact)) (iy (third impact)))
            (multiple-value-bind (sx sy) (tank-centre shooter)
              (unless (< (sqrt (+ (expt (- ix sx) 2) (expt (- iy sy) 2))) 45)
                (multiple-value-bind (tx ty) (tank-centre target)
                  (sqrt (+ (expt (- ix tx) 2) (expt (- iy ty) 2))))))))))))

(defun plan-shot (game shooter &key (skill 0.8) (noise #'random))
  "Choose (values ANGLE POWER WEAPON) for SHOOTER.  SKILL 1 aims as well as
it can; lower spoils the aim.  NOISE is (lambda (n)) giving a number below N."
  (let* ((target (pick-target game shooter))
         (best nil) (best-angle 45) (best-power 50))
    (unless target
      (return-from plan-shot (values (tank-angle shooter) (tank-power shooter) :pebble)))
    (let ((rightward (> (tank-x target) (tank-x shooter))))
      (flet ((try (angle power)
               (let ((miss (shot-miss game shooter angle power target)))
                 (when (and miss (or (null best) (< miss best)))
                   (setf best miss best-angle angle best-power power)))))
        ;; Coarse: a grid over the half of the sky facing the target.
        (loop for a from 15 to 85 by 7
              for angle = (if rightward a (- 180 a))
              do (loop for power from 15 to 100 by 5 do (try angle power)))
        ;; Fine: around the best so far.
        (loop repeat 2
              do (let ((angle best-angle) (power best-power))
                   (loop for da from -3 to 3
                         do (loop for dp from -3 to 3
                                  do (try (clamp (+ angle da) 0 180) (clamp (+ power dp) 0 100))))))))
    (let ((wobble (- 1 (clamp skill 0 1))))
      (flet ((spoil (value spread)
               (if (zerop wobble)
                   value
                   (let ((range (max 1 (round (* wobble spread)))))
                     (+ value (- (funcall noise (1+ (* 2 range))) range))))))
        (values (clamp (spoil best-angle 8) 0 180)
                (clamp (spoil best-power 10) 0 100)
                (pick-weapon shooter target))))))
