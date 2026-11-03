;;;; src/ios/flow.lisp -- whose turn, who decides, and what the phones say.
;;;;
;;;; Three modes.  :LOCAL is Pass & Play: one phone, everyone around it.
;;;; :HOST and :GUEST are a Nearby game.  The host's game is the real one: it
;;;; plays the computer players, checks every guest's request, and sends the
;;;; game to everyone after each change.  A guest replays each shot itself, from
;;;; the angle, power and weapon, for the animation, and then takes the host's
;;;; game as the truth.

(in-package #:ridgefire-ios)

(defvar *mode* :local)
(defvar *lobby* '() "Players in a Nearby lobby: plists :id :kind.")
(defvar *rounds* 3 "Rounds chosen for the next game.")
(defvar *slots* (list :human :cpu :off :off) "Pass & Play: what plays each colour.")
(defvar *awaiting-shot* nil "Guest: a shot sent, the host's answer not yet in.")
(defvar *shop-sent* '() "Guest: tanks whose shopping is done, the host not yet told us so.")
(defvar *aim-dirty* nil)
(defvar *aim-clock* 0d0)
(defvar *turn-token* 0 "Bumped whenever the turn changes, so stale timers do nothing.")
(defvar *demo* nil "True when the computer plays itself, for watching.")
(defvar *dropped* '()
  "Host: players who dropped out of the game, as (device-id . tank-ids).  The
computer plays their tanks until they come back.")
(defvar *host-code* "" "Host: the three letters naming this game.")
(defvar *join-note* nil "Guest: why the last attempt to join came to nothing.")
(defparameter *cpu-skill* 0.78)

(defun authority-p ()
  (member *mode* '(:local :host)))

(defun nearby-p ()
  (member *mode* '(:host :guest)))

(defun local-control-p (tank)
  (and (tank-alive tank)
       (eq (tank-kind tank) :human)
       (or (eq *mode* :local) (equal (tank-owner tank) *my-id*))))

(defun can-aim-p ()
  (and *game*
       (eq *screen* :battle)
       (eq (game-phase *game*) :aiming)
       (not *busy*)
       (not *awaiting-shot*)
       (null *modal*)
       (local-control-p (active-tank *game*))))

(defun status-line ()
  (let ((tank (active-tank *game*)))
    (cond (*busy* "")
          (*awaiting-shot* "Firing…")
          ((can-aim-p)
           (if (< (game-shots *game*) 3) "Drag anywhere to aim: sideways for angle, up and down for power" ""))
          ((not (tank-alive tank)) "")
          ((eq (tank-kind tank) :cpu) (format nil "~a is aiming…" (tank-name tank)))
          (t (format nil "Waiting for ~a…" (tank-name tank))))))

;;; ------------------------------------------------------------------
;;; aiming and firing

(defun aim-changed (tank)
  (point-barrel tank)
  (refresh-hud)
  (when (nearby-p) (setf *aim-dirty* t)))

(defun adjust-aim (dangle dpower)
  (when (can-aim-p)
    (let ((tank (active-tank *game*)))
      (set-aim tank :angle (+ (tank-angle tank) dangle) :power (+ (tank-power tank) dpower))
      (aim-changed tank))))

(defun aim-to (angle power)
  "Set the aim outright, for dragging."
  (when (can-aim-p)
    (let ((tank (active-tank *game*)))
      (unless (and (= (round angle) (tank-angle tank)) (= (round power) (tank-power tank)))
        (set-aim tank :angle angle :power power)
        (aim-changed tank)))))

(defun change-weapon (direction)
  (when (can-aim-p)
    (let ((tank (active-tank *game*)))
      (cycle-weapon tank direction)
      (sound "click")
      (aim-changed tank))))

(defun aim-message ()
  (let ((tank (active-tank *game*)))
    (list :aim :tank (tank-id tank) :angle (tank-angle tank)
               :power (tank-power tank) :weapon (tank-weapon tank))))

(defun send-aim-if-due (dt)
  (when *aim-dirty*
    (incf *aim-clock* dt)
    (when (> *aim-clock* 0.1d0)
      (setf *aim-dirty* nil *aim-clock* 0d0)
      (when (and *game* (eq (game-phase *game*) :aiming))
        (ecase *mode*
          (:host (broadcast (aim-message) :reliable nil))
          (:guest (send-host (aim-message) :reliable nil))
          (:local))))))

(defun fire-pressed ()
  (when (can-aim-p)
    (ecase *mode*
      ((:local :host) (perform-fire))
      (:guest
       (let ((tank (active-tank *game*)))
         (setf *awaiting-shot* t)
         (send-host (list :fire :tank (tank-id tank) :angle (tank-angle tank)
                                :power (tank-power tank) :weapon (tank-weapon tank)))
         (refresh-hud))))))

(defun perform-fire ()
  "Fire the active tank, tell any guests, and play the turn back."
  (let* ((tank (active-tank *game*))
         (shot (list :tank (tank-id tank) :angle (tank-angle tank)
                     :power (tank-power tank) :weapon (tank-weapon tank)))
         (events (fire *game*)))
    (when (eq *mode* :host)
      (broadcast (list* :shot :game (game->plist *game*) shot)))
    (play-turn events)))

(defun play-turn (events)
  (incf *turn-token*)
  (setf *aim-dirty* nil)
  (play-events events #'enter-phase))

(defun cpu-move (token)
  "The computer aims, shows it, and fires."
  (flet ((current-p () (and (= token *turn-token*) *game* (eq (game-phase *game*) :aiming)
                            (eq *screen* :battle) (not *busy*))))
    (cond ((not (current-p)))
          (*modal* (after 0.5d0 (lambda () (cpu-move token))))
          (t
           (let ((tank (active-tank *game*)))
             (multiple-value-bind (angle power weapon) (plan-shot *game* tank :skill *cpu-skill*)
               (set-aim tank :angle angle :power power :weapon weapon)
               (point-barrel tank)
               (refresh-hud)
               (when (eq *mode* :host) (broadcast (aim-message)))
               (after 0.55d0 (lambda ()
                               (cond ((not (current-p)))
                                     (*modal* (after 0.5d0 (lambda () (cpu-move token))))
                                     (t (perform-fire)))))))))))

;;; ------------------------------------------------------------------
;;; phases

(defun install-state (plist)
  (setf *game* (plist->game plist)
        *awaiting-shot* nil))

(defun broadcast-state ()
  (when (eq *mode* :host)
    (broadcast (list :state :game (game->plist *game*)))))

(defun all-shopped-p ()
  (every (lambda (tank) (member (tank-id tank) (game-shop-ready *game*)))
         (game-tanks *game*)))

(defun keep-awake (on)
  "Stop the phone locking itself during a game.  A locked phone drops out of
a Nearby game, and a Pass & Play game is often left alone between turns."
  (objc:invoke (objc:invoke "UIApplication" "sharedApplication") "setIdleTimerDisabled:" on))

(defun enter-phase ()
  "Show whatever *GAME* is doing now."
  (incf *turn-token*)
  (keep-awake t)
  (close-modal)
  (unless (eq (game-phase *game*) :shop)
    (setf *shop-sent* '()))
  (ecase (game-phase *game*)
    (:aiming
     (if (and (eq *screen* :battle) (eql *built-round* (game-round *game*)))
         (progn (draw-ground (game-heights *game*))
                (loop for tank across (game-tanks *game*) do (sync-tank tank))
                (refresh-hud))
         (build-battle))
     (begin-turn))
    (:shop
     (if (and (authority-p) (all-shopped-p))
         ;; Nobody left to shop: only computers are playing.
         (progn (finish-shopping *game* 0)
                (broadcast-state)
                (enter-phase))
         (show-shop)))
    (:over (show-results))))

(defun begin-turn ()
  (let ((tank (active-tank *game*))
        (token *turn-token*))
    (banner (format nil "~a's turn" (tank-name tank))
            :colour (tank-rgb (tank-colour tank)) :seconds 0.6d0 :size 24 :y 300)
    (when (local-control-p tank) (sound "turn"))
    (refresh-hud)
    (when (and (authority-p) (eq (tank-kind tank) :cpu) (tank-alive tank))
      (after (if *demo* 0.6d0 1.0d0) (lambda () (cpu-move token))))
    (when (and *demo* (local-control-p tank))
      (after 0.8d0 (lambda () (autopilot token))))))

(defun autopilot (token)
  "Demo: aim and fire a person's tank as the computer would."
  (when (and (= token *turn-token*) (can-aim-p))
    (let ((tank (active-tank *game*)))
      (multiple-value-bind (angle power weapon) (plan-shot *game* tank :skill *cpu-skill*)
        (set-aim tank :angle angle :power power :weapon weapon)
        (aim-changed tank)
        (after 0.5d0 (lambda () (when (= token *turn-token*) (fire-pressed))))))))

;;; ------------------------------------------------------------------
;;; the shop

(defun local-shopper ()
  "The tank this phone shops for now, or nil."
  (find-if (lambda (tank)
             (and (eq (tank-kind tank) :human)
                  (not (member (tank-id tank) (game-shop-ready *game*)))
                  (not (member (tank-id tank) *shop-sent*))
                  (or (eq *mode* :local) (equal (tank-owner tank) *my-id*))))
           (game-tanks *game*)))

(defun shop-buy (id key)
  (ecase *mode*
    ((:local :host)
     (if (buy *game* id key)
         (progn (sound "coin") (haptic 0) (broadcast-state))
         (notify :error))
     (show-shop))
    (:guest
     (sound "coin")
     (send-host (list :buy :tank id :item key)))))

(defun shop-done (id)
  (sound "click")
  (ecase *mode*
    ((:local :host)
     (finish-shopping *game* id)
     (broadcast-state)
     (enter-phase))
    (:guest
     (push id *shop-sent*)
     (send-host (list :shop-done :tank id))
     (show-shop))))

;;; ------------------------------------------------------------------
;;; starting games

(defun start-local-game ()
  (let ((players (loop for kind in *slots*
                       for colour from 0
                       unless (eq kind :off)
                         collect (list :kind kind :colour colour))))
    (setf *mode* :local
          *game* (new-game :seed (random 1000000000) :players players :rounds *rounds*))
    (enter-phase)))

(defun lobby-has-room-p ()
  (and (eq *mode* :host) (eq *screen* :lobby) (< (length *lobby*) 4)))

(defun accept-invitation-p (device)
  "Host: let DEVICE connect?  While the lobby has room, anyone; once the game
has started, only a player coming back to it.  Called on the framework's
queue, so it only reads."
  (and (eq *mode* :host)
       (or (lobby-has-room-p)
           (and *game* (assoc device *dropped* :test #'equal) t))))

(defun advertise-game ()
  (advertise (format nil "Game ~a" *host-code*) :playing (and *game* t)))

(defun broadcast-lobby ()
  (broadcast (list :lobby :players *lobby* :rounds *rounds*)))

(defun host-start ()
  (setf *dropped* '())
  (let ((players (loop for entry in *lobby*
                       collect (list :kind (getf entry :kind)
                                     :owner (and (eq (getf entry :kind) :human) (getf entry :id))))))
    (setf *game* (new-game :seed (random 1000000000) :players players :rounds *rounds*))
    ;; Still advertised, now as a game in progress, so a player who drops out
    ;; can find it and come back.
    (advertise-game)
    (broadcast-state)
    (enter-phase)))

(defun reset-to-local ()
  (close-session)
  (cancel-steps)
  (keep-awake nil)
  (setf *game* nil *mode* :local *awaiting-shot* nil *lobby* '() *dropped* '()
        *join-note* nil))

(defun leave-game ()
  (when (eq *mode* :host) (broadcast (list :bye)))
  (reset-to-local)
  (show-title))

(defun host-left (&key ended)
  "Guest: the host is gone.  If it may only have dropped out of reach, offer
to look for the game again."
  (reset-to-local)
  (show-title)
  (if ended
      (show-modal "Game over" (format nil "The host has ended the game.")
                  (list (list "OK" (lambda ()))))
      (show-modal "Disconnected" (format nil "The host's phone is out of reach.~%Move closer and look for the game again.")
                  (list (list "Look again" #'join-game) (list "OK" (lambda ()))))))

;;; ------------------------------------------------------------------
;;; the network, taken in between turns

(defun pump-network ()
  (loop while (and *net-events* (not *busy*))
        do (destructuring-bind (kind . peer) (pop *net-events*)
             (handle-net-event kind peer)))
  (loop while (and *inbox* (not *busy*))
        do (destructuring-bind (message . from) (pop *inbox*)
             (handler-case
                 (ecase *mode*
                   (:host (host-message message from))
                   (:guest (guest-message message from))
                   (:local))
               (error (condition)
                 (format *error-output* "~&ridgefire: message ~s failed: ~a~%" (first message) condition)
                 (finish-output *error-output*))))))

(defun handle-net-event (kind peer)
  (case *mode*
    (:host
     (when (eq kind :disconnected)
       (cond (*game*
              (let ((dropped (remove-if-not (lambda (tank) (equal (tank-owner tank) peer))
                                            (coerce (game-tanks *game*) 'list))))
                (when dropped
                  (push (cons peer (mapcar #'tank-id dropped)) *dropped*)
                  (dolist (tank dropped)
                    (setf (tank-kind tank) :cpu (tank-owner tank) nil)
                    (when (eq (game-phase *game*) :shop)
                      (auto-shop tank)
                      (finish-shopping *game* (tank-id tank))))
                  (broadcast-state)
                  (enter-phase)
                  (banner (format nil "~{~a~^, ~} left: the computer takes over"
                                  (mapcar #'tank-name dropped))
                          :size 18 :y 260 :seconds 2d0))))
             (t
              (setf *lobby* (remove peer *lobby* :key (lambda (e) (getf e :id)) :test #'equal))
              (broadcast-lobby)
              (when (eq *screen* :lobby) (show-lobby))))))
    (:guest
     (case kind
       (:connected
        (stop-browsing)
        (send-host (list :hello :id *my-id*))
        (setf *lobby* '())
        (show-guest-lobby))
       (:disconnected
        (when (and *host-peer* (equal peer (peer-name *host-peer*)))
          (if (eq *screen* :join)
              ;; Never got in: refused, or out of reach.
              (progn (setf *host-peer* nil
                           *join-note* "Couldn't join: that game is full or has already started.")
                     (show-join))
              (host-left))))))))

(defun host-message (message from)
  (destructuring-bind (kind &rest plist) message
    (let ((id (getf plist :tank)))
      (case kind
        (:hello
         (when (and *game* (assoc from *dropped* :test #'equal))
           (rejoin from))
         (when (and (eq *screen* :lobby) (null *game*)
                    (not (find from *lobby* :key (lambda (e) (getf e :id)) :test #'equal))
                    (< (length *lobby*) 4))
           (setf *lobby* (append *lobby* (list (list :id from :kind :human))))
           (sound "turn")
           (broadcast-lobby)
           (show-lobby)
           (when *demo* (after 1.5d0 (lambda () (when (eq *screen* :lobby) (host-start)))))))
        (:aim
         (when (and *game* (eq (game-phase *game*) :aiming) (eql id (game-turn *game*))
                    (controls-p *game* id from))
           (let ((tank (active-tank *game*)))
             (set-aim tank :angle (getf plist :angle) :power (getf plist :power)
                           :weapon (getf plist :weapon))
             (point-barrel tank)
             (refresh-hud))
           (broadcast message :reliable nil)))
        (:fire
         (if (and *game*
                  (valid-shot-p *game* from :tank id :angle (getf plist :angle)
                                            :power (getf plist :power) :weapon (getf plist :weapon)))
             (progn
               (set-aim (active-tank *game*) :angle (getf plist :angle) :power (getf plist :power)
                                             :weapon (getf plist :weapon))
               (perform-fire))
             ;; Out of turn or out of date: send the truth so they catch up.
             (when *game* (broadcast-state))))
        (:buy
         (when (and *game* (controls-p *game* id from))
           (buy *game* id (getf plist :item))
           (broadcast-state)
           (when (eq *screen* :shop) (show-shop))))
        (:shop-done
         (when (and *game* (eq (game-phase *game*) :shop) (controls-p *game* id from))
           (finish-shopping *game* id)
           (broadcast-state)
           (enter-phase)))))))

(defun rejoin (device)
  "Host: DEVICE is back.  It takes its tanks back from the computer."
  (let* ((entry (assoc device *dropped* :test #'equal))
         (tanks (loop for id in (rest entry)
                      for tank = (tank-of id)
                      collect tank
                      do (setf (tank-kind tank) :human (tank-owner tank) device))))
    (setf *dropped* (remove entry *dropped*))
    (sound "turn")
    (broadcast-state)
    (enter-phase)
    (banner (format nil "~{~a~^, ~} ~:[is~;are~] back" (mapcar #'tank-name tanks) (rest tanks))
            :size 18 :y 260 :seconds 2d0)))

(defun guest-message (message from)
  (declare (ignore from))
  (destructuring-bind (kind &rest plist) message
    (case kind
      (:lobby
       (setf *lobby* (getf plist :players)
             *rounds* (getf plist :rounds))
       (when (eq *screen* :guest-lobby) (show-guest-lobby)))
      (:state
       (install-state (getf plist :game))
       (enter-phase))
      (:shot
       (let ((state (getf plist :game))
             (id (getf plist :tank)))
         (if (and *game* (eq (game-phase *game*) :aiming) (eql id (game-turn *game*))
                  (eq *screen* :battle))
             (let ((events (progn
                             (set-aim (active-tank *game*) :angle (getf plist :angle)
                                                           :power (getf plist :power)
                                                           :weapon (getf plist :weapon))
                             (point-barrel (active-tank *game*))
                             (fire *game*))))
               (install-state state)
               (play-turn events))
             (progn (install-state state) (enter-phase)))))
      (:aim
       (let ((id (getf plist :tank)))
         (when (and *game* (eq (game-phase *game*) :aiming) (eql id (game-turn *game*))
                    (not (local-control-p (tank-of id))))
           (set-aim (tank-of id) :angle (getf plist :angle) :power (getf plist :power)
                                 :weapon (getf plist :weapon))
           (point-barrel (tank-of id))
           (refresh-hud))))
      (:bye (host-left :ended t)))))
