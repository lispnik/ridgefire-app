;;;; src/ios/battle.lisp -- the battlefield, its HUD, and playing a turn back.
;;;;
;;;; FIRE in the core returns a turn as events.  PLAY-EVENTS turns each event
;;;; into a step -- a function of the frame's elapsed time that answers true
;;;; when it is finished -- and the scene's update: runs them one after another.

(in-package #:ridgefire-ios)

(defvar *game* nil)
(defvar *battle* nil "The battlefield's root node, a child of *WORLD*.")
(defvar *terrain* nil)
(defvar *grass* nil)
(defvar *trails* nil "Layer of trail dots.")
(defvar *fx* nil "Layer of explosions and popups.")
(defvar *tank-views* #() "Per tank: a plist of :node :barrel :bar :shield.")
(defvar *marker* nil "The arrow over the tank whose turn it is.")
(defvar *hud* '() "Plist of HUD labels and nodes.")
(defvar *controls* nil "The aiming controls, shown only to whoever is aiming.")
(defvar *built-round* nil "(round . shots-at-build) of the battlefield on screen.")

(defvar *steps* '() "Steps still to play.")
(defvar *step* nil "The step playing now.")
(defvar *steps-done* nil "Called when the last step finishes.")
(defvar *busy* nil "True while a turn is being played back.")

;;; ------------------------------------------------------------------
;;; skies and ground

(defparameter *palettes*
  ;; sky top, sky bottom, earth top, earth bottom, grass, far hills
  '(((0.10 0.08 0.26) (0.96 0.52 0.36) (0.46 0.30 0.21) (0.18 0.11 0.09) (0.50 0.66 0.28) (0.32 0.18 0.30))
    ((0.20 0.45 0.85) (0.72 0.87 0.97) (0.56 0.42 0.25) (0.28 0.19 0.11) (0.36 0.72 0.26) (0.45 0.60 0.75))
    ((0.02 0.03 0.10) (0.13 0.16 0.33) (0.27 0.24 0.33) (0.09 0.08 0.13) (0.42 0.52 0.66) (0.10 0.12 0.24))
    ((0.12 0.16 0.20) (0.50 0.54 0.48) (0.42 0.38 0.31) (0.17 0.15 0.13) (0.60 0.62 0.42) (0.28 0.32 0.32))))

(defun palette (part)
  (nth (position part '(:sky-top :sky-bottom :earth-top :earth-bottom :grass :hills))
       (nth (mod (game-sky *game*) (length *palettes*)) *palettes*)))

(defun glsl-colour (rgb)
  (format nil "vec3(~,3f, ~,3f, ~,3f)" (first rgb) (second rgb) (third rgb)))

(defun gradient-shader (top bottom &optional (stripes 0))
  "A vertical gradient over the node's texture coordinates; STRIPES > 0 adds
faint strata, for earth."
  (shader (format nil "void main() {
  float t = clamp(v_tex_coord.y, 0.0, 1.0);
  vec3 c = mix(~a, ~a, ~a);
  ~a
  gl_FragColor = vec4(c, 1.0);
}"
                  (glsl-colour bottom) (glsl-colour top)
                  (if (plusp stripes) "pow(t, 0.6)" "t")
                  (if (plusp stripes)
                      (format nil "c *= 0.93 + 0.07 * sin(t * ~,1f);" stripes)
                      ""))))

(defun ground-points (heights)
  (append (list -300 -200 -300 (aref heights 0))
          (loop for x from 0 below +world-width+ by 2
                append (list (+ x 1) (aref heights x)))
          (list (+ +world-width+ 300) (aref heights (1- +world-width+)) (+ +world-width+ 300) -200)))

(defun grass-points (heights)
  (append (list -300 (aref heights 0))
          (loop for x from 0 below +world-width+ by 2
                append (list (+ x 1) (aref heights x)))
          (list (+ +world-width+ 300) (aref heights (1- +world-width+)))))

(defun draw-ground (heights)
  (set-path *terrain* (ground-points heights) :close t)
  (set-path *grass* (grass-points heights)))

(defun far-hills ()
  "A silhouette behind the battlefield, different each round."
  (let* ((seed (+ (* 7 (game-round *game*)) (game-sky *game*)))
         (points (append (list -300 -200)
                         (loop for x from -300 to (+ *width* 300) by 8
                               append (list x (+ 150 (* 40 (sin (+ seed (* x 0.007))))
                                             (* 22 (sin (+ (* 3 seed) (* x 0.019)))))))
                         (list (+ *width* 300) -200))))
    (path-node points (apply #'rgb (append (palette :hills) '(0.55))) :close t)))

(defun stars ()
  (let ((node (empty-node))
        (rng (make-rng (game-round *game*))))
    (dotimes (i 70 node)
      (let ((s (+ 1 (rng-below rng 2))))
        (add node (alpha (at (rect-sprite (white) s s) (rng-below rng *width*) (+ 170 (rng-below rng 230)))
                         (+ 0.3 (* 0.6 (rng-float rng)))))))))

;;; ------------------------------------------------------------------
;;; tanks

(defun make-tank-view (tank)
  (let* ((colour (tank-colour tank))
         (node (z (empty-node) 20))
         (barrel (rect-sprite (tank-rgb colour 0.55) *barrel-length* 3.5d0))
         (bar-back (rect-sprite (ink 0.7) 26 4))
         (bar (rect-sprite (rgb 0.4 0.9 0.4) 26 4))
         (shield (ellipse 38 34 (rgb 0.4 0.8 1 0.10) (rgb 0.5 0.9 1 0.7) 1.5d0)))
    (add node (at (rounded 24 7 3.5d0 (rgb 0.13 0.13 0.15) (rgb 0 0 0 0.5) 1) 0 3.5d0))
    (add node (at (rounded 20 6 3 (tank-rgb colour 0.75)) 0 8))
    (objc:invoke barrel "setAnchorPoint:" (point 0 0.5d0))
    (add node (at barrel 0 *turret-height*))
    (add node (at (ellipse 12 11 (tank-rgb colour) (tank-rgb colour 0.6) 1) 0 9))
    (add node (at bar-back 0 25))
    (objc:invoke bar "setAnchorPoint:" (point 0 0.5d0))
    (add node (at bar -13 25))
    (add node (at shield 0 9))
    (list :node node :barrel barrel :bar bar :bar-back bar-back :shield shield)))

(defun tank-view (id) (aref *tank-views* id))

(defun show-health (id health shield)
  (let* ((view (tank-view id))
         (bar (getf view :bar)))
    (objc:invoke bar "setXScale:" (float (max 0.001 (/ health 100)) 1d0))
    (objc:invoke bar "setColor:" (cond ((> health 60) (rgb 0.4 0.9 0.4))
                                       ((> health 30) (rgb 0.98 0.8 0.3))
                                       (t (rgb 0.95 0.35 0.3))))
    (objc:invoke (getf view :shield) "setHidden:" (not (plusp shield)))))

(defun show-dead (id)
  (let ((view (tank-view id)))
    (alpha (getf view :node) 0.35d0)
    (dolist (key '(:barrel :bar :bar-back :shield))
      (objc:invoke (getf view key) "setHidden:" t))))

(defun point-barrel (tank)
  (objc:invoke (getf (tank-view (tank-id tank)) :barrel) "setZRotation:"
               (float (radians (tank-angle tank)) 1d0)))

(defun sync-tank (tank)
  "Put TANK's view where the model says it is."
  (let ((view (tank-view (tank-id tank))))
    (at (getf view :node) (tank-x tank) (tank-y tank))
    (point-barrel tank)
    (if (tank-alive tank)
        (show-health (tank-id tank) (tank-health tank) (tank-shield tank))
        (show-dead (tank-id tank)))))

(defun place-marker ()
  (let ((tank (active-tank *game*)))
    (when *marker*
      (objc:invoke *marker* "setHidden:" (not (and (tank-alive tank) (eq (game-phase *game*) :aiming))))
      (at *marker* (tank-x tank) (+ (tank-y tank) 40))
      (objc:invoke *marker* "setFontColor:" (tank-rgb (tank-colour tank))))))

;;; ------------------------------------------------------------------
;;; building the battlefield

(defun build-battle ()
  "Draw the battlefield for *GAME* from scratch."
  (clear-world)
  (setf *screen* :battle
        *built-round* (game-round *game*)
        *battle* (add *world* (empty-node)))
  (let ((sky (rect-sprite (white) (+ *width* 400) (+ *height* 200))))
    (objc:invoke sky "setShader:" (gradient-shader (palette :sky-top) (palette :sky-bottom)))
    (add *battle* (z (at sky (/ *width* 2) (/ *height* 2)) -30)))
  (when (= 2 (game-sky *game*))
    (add *battle* (z (stars) -29)))
  (add *battle* (z (far-hills) -28))
  (setf *terrain* (add *battle* (z (path-node '(0 0 1 1 2 0) (white) :close t) -10)))
  (objc:invoke *terrain* "setFillShader:"
               (gradient-shader (palette :earth-top) (palette :earth-bottom) 90))
  (setf *grass* (add *battle* (z (path-node '(0 0 1 1) (rgb 0 0 0 0)
                                            :stroke (apply #'rgb (palette :grass)) :line-width 3)
                                 -9)))
  (draw-ground (game-heights *game*))
  (setf *trails* (add *battle* (z (empty-node) 5))
        *fx* (add *battle* (z (empty-node) 40)))
  (setf *tank-views* (map 'vector #'make-tank-view (game-tanks *game*)))
  (loop for view across *tank-views* do (add *battle* (getf view :node)))
  (loop for tank across (game-tanks *game*) do (sync-tank tank))
  (setf *marker* (z (add *battle* (text "▼︎" 16 (white))) 30))
  (run *marker* (forever (move-by 0 5 0.35d0) (move-by 0 -5 0.35d0)))
  (build-hud)
  (place-marker)
  (refresh-hud))

;;; ------------------------------------------------------------------
;;; the HUD

(defun hud (key) (getf *hud* key))

(defun readout (parent x y caption)
  (add parent (at (rounded 64 40 10 (ink 0.55) (white 0.18) 1) x y))
  (label parent caption x (+ y 10) :size 9 :colour (white 0.55) :font "AvenirNext-Bold")
  (label parent "" x (- y 6) :size 17 :font "AvenirNext-Bold"))

(defun build-hud ()
  (let ((top (z (add *battle* (empty-node)) 60))
        (controls (z (add *battle* (empty-node)) 60))
        (y 28))
    (setf *controls* controls)
    ;; The weapon box first, so the weapon's name is drawn over it.
    (add controls (at (rounded 170 40 10 (ink 0.55) (white 0.18) 1) 505 y))
    (add top (at (rect-sprite (ink 0.45) (+ *width* 400) 34) (/ *width* 2) 383))
    (setf *hud*
          (list :chip (add top (at (ellipse 14 14 (white)) 20 383))
                :name (label top "" 34 383 :size 16 :align :left :font "AvenirNext-Bold")
                :health (label top "" 150 383 :size 14 :align :left)
                :wind (label top "" 400 383 :size 15)
                :round (label top "" 700 383 :size 13 :align :right :colour (white 0.7))
                :status (label *battle* "" 400 340 :size 15 :colour (white 0.9))
                :angle (readout controls 88 y "ANGLE")
                :power (readout controls 268 y "POWER")
                :weapon (label controls "" 505 (+ y 6) :size 16 :font "AvenirNext-Bold")
                :ammo (label controls "" 505 (- y 11) :size 11 :colour (white 0.6))))
    (button top "II" 770 383 40 28 #'open-pause-menu :size 14 :fill (white 0.12))
    ;; Angle: the arrow says which way the barrel turns.
    (flet ((ctl (glyph x fn &key (w 46) (fill (ink 0.5)))
             (button controls glyph x y w 40 fn :repeat t :size 20 :fill fill :stroke (white 0.25))))
      (ctl "◀︎" 34 (lambda () (adjust-aim 1 0)))
      (ctl "▶︎" 142 (lambda () (adjust-aim -1 0)))
      (ctl "−" 214 (lambda () (adjust-aim 0 -1)))
      (ctl "+" 322 (lambda () (adjust-aim 0 1)))
      (ctl "‹" 400 (lambda () (change-weapon -1)) :w 36)
      (ctl "›" 610 (lambda () (change-weapon 1)) :w 36))
    (button controls "FIRE" 728 y 116 46 #'fire-pressed
            :size 22 :font "AvenirNext-Heavy"
            :fill (rgb 0.85 0.22 0.18 0.92) :stroke (rgb 1 0.7 0.6 0.8))))

(defun wind-text (wind)
  (cond ((zerop wind) "Wind: calm")
        ((minusp wind) (format nil "◀︎ ~d  Wind" (- wind)))
        (t (format nil "Wind  ~d ▶︎" wind))))

(defun refresh-hud ()
  (when (and (eq *screen* :battle) *game*)
    (let* ((tank (active-tank *game*))
           (weapon (find-weapon (tank-weapon tank)))
           (count (ammo tank (tank-weapon tank))))
      (objc:invoke (hud :chip) "setFillColor:" (tank-rgb (tank-colour tank)))
      (set-text (hud :name) (tank-name tank))
      (set-text (hud :health)
                (format nil "♥ ~d~:[~;   ◈ ~d~]" (tank-health tank)
                        (plusp (tank-shield tank)) (tank-shield tank)))
      (set-text (hud :wind) (wind-text (game-wind *game*)))
      (set-text (hud :round) (format nil "Round ~d of ~d" (game-round *game*) (game-rounds *game*)))
      (set-text (hud :angle) (format nil "~d°" (tank-angle tank)))
      (set-text (hud :power) (format nil "~d" (tank-power tank)))
      (set-text (hud :weapon) (weapon-name weapon))
      (set-text (hud :ammo) (if (eq count :infinite) "unlimited" (format nil "~d left" count)))
      (objc:invoke *controls* "setHidden:" (not (can-aim-p)))
      (set-text (hud :status) (status-line))
      (place-marker))))

;;; ------------------------------------------------------------------
;;; playing events back

(defun run-steps (steps on-done)
  (setf *steps* (append *steps* steps)
        *steps-done* on-done
        *busy* t))

(defun tick-steps (dt)
  (loop
    (unless *step*
      (if *steps*
          (setf *step* (pop *steps*))
          (progn
            (when *busy*
              (setf *busy* nil)
              (let ((done *steps-done*))
                (setf *steps-done* nil)
                (when done (funcall done))))
            (return))))
    (if (funcall *step* dt)
        (setf *step* nil dt 0d0)
        (return))))

(defun cancel-steps ()
  (setf *steps* '() *step* nil *steps-done* nil *busy* nil))

(defun now (thunk)
  (lambda (dt) (declare (ignore dt)) (funcall thunk) t))

(defun pause (seconds)
  (let ((left seconds))
    (lambda (dt) (minusp (decf left dt)))))

(defun projectile ()
  (let ((node (ellipse 6 6 (white) (rgb 1 0.9 0.6) 1)))
    (objc:invoke node "setGlowWidth:" 3d0)
    (z (add *fx* node) 5)))

(defun flight (paths colour-fn &key (speed 1.7d0) (dot-every 3))
  "A step flying a shell along each of PATHS at once, leaving a trail.
COLOUR-FN makes the trail colour: made fresh, since a UIColor made when the
steps were built is autoreleased before the step runs."
  (let ((shells nil) (cursor 0d0) (drawn (make-list (length paths) :initial-element 0)))
    (lambda (dt)
      (unless shells
        (setf shells (mapcar (lambda (p) (declare (ignore p)) (projectile)) paths)))
      (incf cursor (* dt 60 speed))
      (let ((finished t))
        (loop for path in paths
              for shell in shells
              for cell on drawn
              for n = (floor (length path) 2)
              for i = (min (1- n) (floor cursor))
              do (let ((x (aref path (* 2 i))) (y (aref path (1+ (* 2 i)))))
                   (at shell x (min y 394))
                   (alpha shell (if (> y 394) 0.4d0 1d0)))
                 (loop for j from (car cell) to i
                       when (zerop (mod j dot-every))
                         do (let ((dot (rect-sprite (funcall colour-fn) 2.5d0 2.5d0)))
                              (add *trails* (at dot (aref path (* 2 j)) (aref path (1+ (* 2 j)))))
                              (vanish-after dot 2.5d0 0.8d0)))
                 (setf (car cell) (1+ i))
                 (when (< i (1- n)) (setf finished nil)))
        (when finished
          (mapc #'remove-node shells)
          t)))))

(defun boom (x y radius effect)
  (let* ((d (* 2 radius))
         (node (ecase effect
                 (:blast (ellipse d d (rgb 1 0.55 0.15 0.9) (rgb 1 0.92 0.55) 2))
                 (:dirt (ellipse d d (rgb 0.50 0.35 0.22 0.85) (rgb 0.7 0.55 0.35) 2))
                 (:dig (ellipse d d (rgb 0.9 0.8 0.6 0.35) (rgb 1 0.95 0.8 0.8) 2)))))
    (add *fx* (at node x y))
    (objc:invoke node "setScale:" 0.15d0)
    (run node (scale-to 1 0.16d0) (fade-to 0 0.35d0) (action "removeFromParent"))
    (when (eq effect :blast)
      (let ((core (ellipse radius radius (rgb 1 1 0.85 0.95))))
        (add *fx* (at core x y))
        (run core (scale-to 1.4d0 0.1d0) (fade-to 0 0.15d0) (action "removeFromParent"))))
    (cond ((not (eq effect :blast)) (sound "thud") (haptic 0 0.6d0))
          ((>= radius 40) (sound "bigboom") (haptic 2) (shake *battle* 10))
          ((>= radius 26) (sound "boom") (haptic 1) (shake *battle* 5))
          (t (sound "boom") (haptic 1 0.7d0)))))

(defun popup (x y string colour)
  (let ((node (text string 15 colour :font "AvenirNext-Heavy")))
    (add *fx* (at node x y))
    (run node (together (move-by 0 28 0.9d0) (action "sequence:" (vector (wait-action 0.5d0) (fade-to 0 0.4d0))))
         (action "removeFromParent"))))

(defun banner (string &key (colour (white)) (seconds 1.2d0) (y 300) (size 30))
  (let ((node (z (text string size colour :font "AvenirNext-Heavy") 90)))
    (add *world* (at node 400 y))
    (objc:invoke node "setScale:" 0.6d0)
    (run node (scale-to 1 0.18d0) (wait-action seconds) (fade-to 0 0.3d0) (action "removeFromParent"))
    node))

(defun event-steps (event)
  (destructuring-bind (kind &rest plist) event
    (ecase kind
      (:fire
       (let ((tank (tank-of (getf plist :tank))))
         (list (now (lambda ()
                      (sound "fire")
                      (haptic 1)
                      (let ((barrel (getf (tank-view (tank-id tank)) :barrel)))
                        (run barrel (scale-to 0.7d0 0.05d0) (scale-to 1 0.12d0))))))))
      (:shot
       (let ((colour (tank-colour (active-tank-at-fire))))
         (list (flight (getf plist :paths) (lambda () (tank-rgb colour 1 0.85d0))))))
      (:roll
       (list (flight (list (getf plist :path)) (lambda () (rgb 1 1 1 0.6)) :speed 4d0 :dot-every 4)))
      (:explosion
       (let ((r (getf plist :radius)))
         (list (now (lambda () (boom (getf plist :x) (getf plist :y) r (getf plist :effect))))
               (pause (if (>= r 40) 0.45d0 0.25d0)))))
      (:terrain
       (let ((heights (getf plist :heights)))
         (list (now (lambda () (draw-ground heights))))))
      (:damage
       (let ((id (getf plist :tank)))
         (list (now (lambda ()
                      (let ((tank (tank-of id)))
                        (popup (tank-x tank) (+ (tank-y tank) 30)
                               (format nil "−~d" (getf plist :amount)) (rgb 1 0.45 0.4)))
                      (show-health id (getf plist :health) (getf plist :shield)))))))
      (:move
       (let ((id (getf plist :tank)) (to (getf plist :to)))
         (list (now (lambda ()
                      (run (getf (tank-view id) :node)
                           (move-to (tank-x (tank-of id)) to 0.3d0))))
               (pause 0.32d0))))
      (:death
       (let ((id (getf plist :tank)))
         (list (now (lambda () (show-dead id) (notify :warning))))))
      (:turn '())
      (:round-over
       (let ((winner (getf plist :winner)))
         (list (now (lambda ()
                      (sound "win")
                      (notify :success)
                      (banner (if winner
                                  (format nil "~a wins round ~d!" (tank-name (tank-of winner)) (game-round *game*))
                                  (format nil "Nobody survives round ~d" (game-round *game*)))
                              :colour (if winner (tank-rgb (tank-colour (tank-of winner))) (white))
                              :seconds 2d0 :y 230)))
               (pause 2.6d0)))))))

(defvar *firing-tank* nil "Id of the tank whose turn is being played back.")

(defun active-tank-at-fire ()
  (tank-of *firing-tank*))

(defun tank-of (id)
  (aref (game-tanks *game*) id))

(defun play-events (events on-done)
  (setf *firing-tank* (getf (rest (first events)) :tank))
  (objc:invoke *controls* "setHidden:" t)
  (objc:invoke *marker* "setHidden:" t)
  (set-text (hud :status) "")
  (run-steps (loop for event in events append (event-steps event)) on-done))
