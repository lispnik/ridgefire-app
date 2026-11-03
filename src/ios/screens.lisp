;;;; src/ios/screens.lisp -- everything that is not the battlefield.

(in-package #:ridgefire-ios)


(defun chip (parent colour x y &optional (size 16))
  (add parent (at (ellipse size size (tank-rgb colour) (tank-rgb colour 0.6) 1.5d0) x y)))

(defun scenery (&key (sky 0) (seed (random 100000)) (dim 0.0))
  "A sky and hills behind a menu."
  (destructuring-bind (top bottom earth-top earth-bottom grass hills) (nth sky *palettes*)
    (let ((back (rect-sprite (white) (+ *width* 400) (+ *height* 200)))
          (heights (generate-heights (make-rng seed))))
      (objc:invoke back "setShader:" (gradient-shader top bottom))
      (add *world* (z (at back (/ *width* 2) (/ *height* 2)) -30))
      (add *world* (z (path-node (append '(-300 -200)
                                         (loop for x from -300 to 1100 by 8
                                               append (list x (+ 170 (* 35 (sin (+ seed (* x 0.008)))))))
                                         '(1100 -200))
                                 (apply #'rgb (append hills '(0.55))) :close t)
                      -28))
      (let ((ground (add *world* (z (path-node (ground-points heights) (white) :close t) -10))))
        (objc:invoke ground "setFillShader:" (gradient-shader earth-top earth-bottom 90)))
      (add *world* (z (path-node (grass-points heights) (rgb 0 0 0 0)
                                 :stroke (apply #'rgb grass) :line-width 3)
                      -9))
      (when (plusp dim)
        (add *world* (z (at (rect-sprite (rgb 0 0 0 dim) (+ *width* 400) (+ *height* 200)) 400 200) -5))))))

(defun title-text (string y &key (size 30) (colour (white)))
  (label *world* string 400 y :size size :colour colour :font "AvenirNext-Heavy"))

;;; ------------------------------------------------------------------
;;; title

(defun on-off (key) (if (zerop (saved key)) "on" "off"))

(defun toggle (key)
  (save key (if (zerop (saved key)) 1 0)))

(defun show-title ()
  (clear-world)
  (setf *screen* :title)
  (scenery :sky 0 :dim 0.25d0)
  (let ((title (title-text "RIDGEFIRE" 300 :size 64 :colour (gold))))
    (objc:invoke title "setScale:" 0.8d0)
    (run title (scale-to 1 0.35d0)))
  (title-text "Tanks, hills and wind, for people in the same room" 258 :size 15 :colour (white 0.85))
  (button *world* "Pass & Play" 400 196 260 48 (lambda () (sound "click") (show-setup))
          :size 20 :fill (rgb 0.85 0.22 0.18 0.9) :stroke (rgb 1 0.7 0.6 0.8))
  (button *world* "Nearby Game" 400 138 260 48 (lambda () (sound "click") (show-nearby))
          :size 20 :fill (rgb 0.20 0.45 0.85 0.9) :stroke (rgb 0.6 0.8 1 0.8))
  (button *world* "How to Play" 400 82 200 40 (lambda () (sound "click") (show-help)) :size 16)
  (let ((saved (saved-game)))
    (when saved
      (button *world* (if (eq (getf saved :mode) :host)
                          (format nil "Resume Game ~a" (getf saved :code))
                          "Resume game")
              400 30 220 38 (lambda () (sound "click") (resume-saved-game))
              :size 16 :fill (rgb 0.95 0.75 0.2 0.85) :colour (rgb 0.1 0.08 0.02))))
  (button *world* (format nil "Sound: ~a" (on-off "no sound")) 90 30 150 34
          (lambda () (toggle "no sound") (show-title)) :size 14)
  (button *world* (format nil "Vibration: ~a" (on-off "no vibration")) 710 30 150 34
          (lambda () (toggle "no vibration") (haptic 1) (show-title)) :size 14))

;;; ------------------------------------------------------------------
;;; Pass & Play setup

(defun kind-label (kind)
  (ecase kind (:human "Player") (:cpu "Computer") (:off "—")))

(defun rounds-picker (y on-change)
  (label *world* "Rounds" 300 y :size 18)
  (button *world* "−" 390 y 44 38 (lambda () (setf *rounds* (max 1 (1- *rounds*))) (funcall on-change)) :size 22)
  (label *world* (princ-to-string *rounds*) 450 y :size 24 :font "AvenirNext-Bold")
  (button *world* "+" 510 y 44 38 (lambda () (setf *rounds* (min 10 (1+ *rounds*))) (funcall on-change)) :size 22))

(defun show-setup ()
  (clear-world)
  (setf *screen* :setup)
  (scenery :sky 1 :seed 4 :dim 0.45d0)
  (title-text "Pass & Play" 365 :size 30)
  (label *world* "One phone, passed around. Tap a player to change who plays that colour."
         400 335 :size 13 :colour (white 0.75) :font "AvenirNext-Medium")
  (loop for kind in *slots*
        for i from 0
        for y = (- 290 (* i 46))
        do (let ((i i))
             (chip *world* i 300 y 20)
             (label *world* (aref *colour-names* i) 320 y :size 19 :align :left :font "AvenirNext-Bold")
             (button *world* (kind-label kind) 500 y 150 38
                     (lambda ()
                       (sound "click")
                       (setf (nth i *slots*) (ecase (nth i *slots*) (:human :cpu) (:cpu :off) (:off :human)))
                       (show-setup))
                     :size 16 :fill (if (eq kind :off) (white 0.06) (tank-rgb i 0.6 0.7d0)))))
  (rounds-picker 100 #'show-setup)
  (let ((enough (>= (count :off *slots* :test-not #'eq) 2)))
    (button *world* "Back" 150 40 140 42 (lambda () (sound "click") (show-title)))
    (button *world* (if enough "Start" "Need two players") 620 40 220 42
            (lambda () (when enough (sound "click") (start-local-game)))
            :size 18
            :fill (if enough (rgb 0.85 0.22 0.18 0.9) (white 0.08)))))

;;; ------------------------------------------------------------------
;;; Nearby

(defun show-nearby ()
  (clear-world)
  (setf *screen* :nearby)
  (scenery :sky 2 :seed 9 :dim 0.35d0)
  (title-text "Nearby Game" 340 :size 30)
  (label *world* "Everyone plays on their own phone, in the same room." 400 300 :size 15 :colour (white 0.85))
  (label *world* "Turn on Bluetooth or Wi-Fi. No internet is needed." 400 278 :size 15 :colour (white 0.85))
  (button *world* "Host a game" 400 210 260 48 (lambda () (sound "click") (host-game))
          :size 20 :fill (rgb 0.85 0.22 0.18 0.9))
  (button *world* "Join a game" 400 150 260 48 (lambda () (sound "click") (join-game))
          :size 20 :fill (rgb 0.20 0.45 0.85 0.9))
  (button *world* "Back" 400 70 140 40 (lambda () (sound "click") (show-title))))

(defun host-game ()
  (setf *mode* :host
        *game* nil
        *host-code* (random-code 3)
        *lobby* (list (list :id (ensure-my-id) :kind :human)))
  (start-hosting (format nil "Game ~a" *host-code*))
  (show-lobby))

(defun lobby-row (entry i y &key you remove)
  (chip *world* i 250 y 20)
  (label *world* (aref *colour-names* i) 270 y :size 19 :align :left :font "AvenirNext-Bold")
  (label *world* (cond (you "this phone")
                       ((eq (getf entry :kind) :cpu) "computer")
                       (t "nearby phone"))
         400 y :size 15 :align :left :colour (white 0.7))
  (when remove
    (button *world* "Remove" 560 y 100 32 remove :size 13)))

(defun show-lobby ()
  "Host: who has joined, and the start button."
  (clear-world)
  (setf *screen* :lobby)
  (scenery :sky 2 :seed 9 :dim 0.45d0)
  (title-text (format nil "Game ~a" *host-code*) 365 :size 30 :colour (gold))
  (label *world* (format nil "On the other phones: Nearby Game, Join, then pick Game ~a." *host-code*)
         400 335 :size 14 :colour (white 0.8) :font "AvenirNext-Medium")
  (loop for entry in *lobby*
        for i from 0
        for y = (- 290 (* i 42))
        do (let ((entry entry))
             (lobby-row entry i y
                        :you (equal (getf entry :id) *my-id*)
                        :remove (and (eq (getf entry :kind) :cpu)
                                     (lambda ()
                                       (setf *lobby* (remove entry *lobby*))
                                       (broadcast-lobby)
                                       (show-lobby))))))
  (when (< (length *lobby*) 4)
    (button *world* "+ Computer" 400 (- 290 (* (length *lobby*) 42)) 160 34
            (lambda ()
              (setf *lobby* (append *lobby* (list (list :id (format nil "cpu-~a" (random-code 4)) :kind :cpu))))
              (broadcast-lobby)
              (show-lobby))
            :size 14 :fill (white 0.08)))
  (rounds-picker 100 (lambda () (broadcast-lobby) (show-lobby)))
  (let ((enough (>= (length *lobby*) 2)))
    (button *world* "Cancel" 150 40 140 42 (lambda () (leave-game)))
    (button *world* (if enough "Start" "Waiting for players…") 620 40 240 42
            (lambda () (when enough (sound "click") (host-start)))
            :size 18 :fill (if enough (rgb 0.85 0.22 0.18 0.9) (white 0.08)))))

(defun join-game ()
  (setf *mode* :guest *game* nil *lobby* '() *join-note* nil)
  (start-browsing)
  (show-join))

(defun found-changed ()
  (reconnect-now)
  (when (eq *screen* :join)
    (show-join)
    (when (and *demo* *found* (null *host-peer*))
      (join (first *found*))
      (show-join (getf (first *found*) :label)))))

(defun show-join (&optional joining)
  (clear-world)
  (setf *screen* :join)
  (scenery :sky 2 :seed 9 :dim 0.45d0)
  (title-text "Join a game" 365 :size 30)
  (label *world* (cond (joining (format nil "Asking to join ~a…" joining))
                       (*join-note* *join-note*)
                       (*found* "Tap the game to join.")
                       (t "Looking for games nearby…"))
         400 330 :size 15 :colour (if (and *join-note* (not joining)) (rgb 1 0.75 0.4) (white 0.8)))
  (loop for found in *found*
        for y downfrom 270 by 52
        repeat 4
        do (let ((found found))
             (button *world* (if (getf found :playing)
                                 (format nil "~a · in progress" (getf found :label))
                                 (getf found :label))
                     400 y 420 44
                     (lambda ()
                       (sound "click")
                       (setf *join-note* nil)
                       (join found)
                       (show-join (getf found :label)))
                     :size 17 :fill (rgb 0.20 0.45 0.85 0.85))))
  (button *world* "Back" 400 40 140 42 (lambda () (leave-game))))

(defun show-guest-lobby ()
  (clear-world)
  (setf *screen* :guest-lobby)
  (scenery :sky 2 :seed 9 :dim 0.45d0)
  (title-text "Joined" 365 :size 30 :colour (gold))
  (label *world* (format nil "Waiting for the host to start · ~d round~:p" *rounds*)
         400 335 :size 15 :colour (white 0.8))
  (loop for entry in *lobby*
        for i from 0
        do (lobby-row entry i (- 290 (* i 42)) :you (equal (getf entry :id) *my-id*)))
  (button *world* "Leave" 400 40 140 42 (lambda () (leave-game))))

;;; ------------------------------------------------------------------
;;; the shop

(defun round-summary ()
  (let ((winner (game-round-winner *game*)))
    (format nil "Round ~d of ~d: ~a"
            (game-round *game*) (game-rounds *game*)
            (if winner (format nil "~a won" (tank-name (tank-of winner))) "nobody survived"))))

(defun show-shop ()
  (clear-world)
  (setf *screen* :shop)
  (scenery :sky 3 :seed (game-round *game*) :dim 0.6d0)
  (let ((shopper (local-shopper)))
    (if (null shopper)
        (progn
          (title-text (round-summary) 300 :size 22)
          (label *world* "Waiting for everyone to finish shopping…" 400 250 :size 16 :colour (white 0.8))
          (label *world* (format nil "~{~a~^, ~}"
                                 (loop for tank across (game-tanks *game*)
                                       unless (member (tank-id tank) (game-shop-ready *game*))
                                         collect (tank-name tank)))
                 400 220 :size 15 :colour (white 0.6))
          (button *world* "Leave game" 400 50 160 40 #'confirm-leave :size 15))
        (let ((id (tank-id shopper)))
          (when *demo*
            (after 1.5d0 (lambda () (when (and (eq *screen* :shop) (eq (local-shopper) shopper))
                                     (shop-done id)))))
          (chip *world* (tank-colour shopper) 30 375 18)
          (label *world* (format nil "~a's shop" (tank-name shopper)) 48 375
                 :size 22 :align :left :font "AvenirNext-Heavy")
          (label *world* (format nil "$~:d" (tank-money shopper)) 770 375
                 :size 24 :align :right :colour (gold) :font "AvenirNext-Heavy")
          (label *world* (format nil "~a · you earned $~:d" (round-summary) (tank-earned shopper))
                 400 345 :size 14 :colour (white 0.75))
          (loop for item in (shop-items)
                for i from 0
                for x = (if (evenp i) 205 595)
                for y = (- 292 (* 60 (floor i 2)))
                do (let* ((key (getf item :key))
                          (affordable (>= (tank-money shopper) (getf item :price)))
                          (held (ammo shopper key))
                          (card (button *world* nil x y 370 52
                                        (lambda () (shop-buy id key))
                                        :fill (if affordable (white 0.12) (white 0.04))
                                        :stroke (if affordable (white 0.35) (white 0.12)))))
                     (add card (at (text (getf item :name) 17 (if affordable (white) (white 0.45))
                                         :align :left :font "AvenirNext-Bold")
                                   -172 9))
                     (add card (at (text (getf item :blurb) 11 (white 0.6) :align :left :font "AvenirNext-Medium")
                                   -172 -11))
                     (add card (at (text (format nil "$~:d~@[ for ~d~]" (getf item :price)
                                                 (and (> (getf item :bundle) 1) (getf item :bundle)))
                                         14 (if affordable (gold) (white 0.35)) :align :right)
                                   172 9))
                     (add card (at (text (format nil "have ~d" held) 11 (white 0.6) :align :right
                                         :font "AvenirNext-Medium")
                                   172 -11))))
          (button *world* "Leave" 90 32 120 38 #'confirm-leave :size 14)
          (button *world* "Done" 400 32 200 42 (lambda () (shop-done id))
                  :size 20 :fill (rgb 0.85 0.22 0.18 0.9))))))

;;; ------------------------------------------------------------------
;;; the end

(defun show-results ()
  (clear-world)
  (setf *screen* :results)
  (scenery :sky 0 :seed 77 :dim 0.55d0)
  (let ((ranking (standings *game*)))
    (title-text (format nil "~a wins!" (tank-name (first ranking))) 355 :size 36
                :colour (tank-rgb (tank-colour (first ranking))))
    (loop for caption in '("Rounds" "Kills" "Money") for x in '(470 560 670)
          do (label *world* caption x 312 :size 12 :colour (white 0.6)))
    (loop for tank in ranking
          for place from 1
          for y = (- 280 (* 40 (1- place)))
          do (label *world* (format nil "~d." place) 200 y :size 18 :colour (white 0.7))
             (chip *world* (tank-colour tank) 235 y 18)
             (label *world* (tank-name tank) 255 y :size 19 :align :left :font "AvenirNext-Bold")
             (label *world* (princ-to-string (tank-wins tank)) 470 y :size 19)
             (label *world* (princ-to-string (tank-kills tank)) 560 y :size 19)
             (label *world* (format nil "$~:d" (tank-money tank)) 670 y :size 17)))
  (sound "win")
  (when (eq *mode* :local)
    (button *world* "Play again" 520 45 180 44 (lambda () (sound "click") (start-local-game))
            :size 18 :fill (rgb 0.85 0.22 0.18 0.9)))
  (button *world* "Main menu" (if (eq *mode* :local) 280 400) 45 180 44 #'leave-game :size 18)
  (when (and *demo* (eq *mode* :local))
    (after 6d0 (lambda () (when (eq *screen* :results) (start-local-game))))))

;;; ------------------------------------------------------------------
;;; help, and the pause menu

(defparameter *help*
  '("Take turns to lob shells at the other tanks. Last tank standing wins the round."
    "Drag anywhere on the battlefield: sideways turns the barrel, up and down sets the power."
    "Or use ◀︎ ▶︎ for the angle and − + for the power; hold them down to keep going."
    "Watch the wind at the top of the screen: it pushes every shell sideways."
    "Blasts dig craters, and tanks fall into them. A long fall hurts."
    "Damage earns money; kills and surviving earn more. Spend it in the shop between rounds."
    "Pass & Play: one phone, passed around.  Nearby: a phone each, in the same room."))

(defun show-help ()
  (clear-world)
  (setf *screen* :help)
  (scenery :sky 1 :seed 21 :dim 0.6d0)
  (title-text "How to Play" 360 :size 30)
  (loop for line in *help*
        for y downfrom 312 by 32
        do (label *world* line 400 y :size 14 :font "AvenirNext-Medium"))
  (loop for w in (remove :pebble *weapons* :key #'weapon-key :test-not #'eq)
        do (label *world* (format nil "~a: ~a" (weapon-name w) (weapon-blurb w)) 400 84 :size 13
                  :colour (white 0.7)))
  (button *world* "Back" 400 36 140 40 (lambda () (sound "click") (show-title))))

(defun confirm-leave ()
  (show-modal "Leave the game?"
              (if (eq *mode* :host)
                  (format nil "You are the host:~%the game ends for everyone.")
                  (format nil "This game will be lost."))
              (list (list "Keep playing" (lambda ()))
                    (list "Leave" #'leave-game))))

(defun open-pause-menu ()
  (unless *busy*
    (show-modal "Paused" (format nil "Round ~d of ~d" (game-round *game*) (game-rounds *game*))
                (list (list "Resume" (lambda () (refresh-hud)))
                      (list "Leave game" #'confirm-leave)))))
