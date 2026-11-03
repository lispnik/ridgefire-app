;;;; src/core/package.lisp

(defpackage #:ridgefire
  (:use #:cl)
  (:export
   ;; the world
   #:+world-width+ #:+world-height+ #:+tau+
   #:make-heights #:height-at #:column #:generate-heights
   #:carve-circle #:add-dirt #:flatten
   ;; randomness
   #:make-rng #:rng-next #:rng-below #:rng-range #:rng-float #:rng-state
   ;; weapons and the shop
   #:*weapons* #:weapon #:find-weapon #:weapon-key-p
   #:weapon-key #:weapon-name #:weapon-radius #:weapon-damage
   #:weapon-price #:weapon-bundle #:weapon-effect #:weapon-blurb
   #:*shield-price* #:*shield-strength* #:shop-items #:find-shop-item
   ;; tanks
   #:tank #:make-tank #:tank-id #:tank-name #:tank-colour #:tank-kind #:tank-owner
   #:tank-x #:tank-y #:tank-health #:tank-angle #:tank-power #:tank-weapon
   #:tank-inventory #:tank-money #:tank-shield #:tank-alive #:tank-wins
   #:tank-kills #:tank-earned #:tank-centre
   #:*colour-names* #:*starter-kit*
   #:ammo #:add-ammo #:has-weapon-p #:owned-weapons #:cycle-weapon
   #:set-aim #:barrel-tip #:launch-velocity #:radians
   #:*barrel-length* #:*turret-height*
   ;; the game
   #:game #:make-game #:new-game #:start-round
   #:game-heights #:game-tanks #:game-turn #:game-wind #:game-round #:game-rounds
   #:game-phase #:game-rng #:game-sky #:game-shop-ready #:game-round-winner #:game-shots
   #:active-tank #:alive-tanks
   #:trace-path #:fire #:buy #:auto-shop #:finish-shopping #:standings
   #:blast-damage #:fall-damage
   #:*money-per-damage* #:*kill-bonus* #:*survivor-bonus* #:*winner-bonus*
   #:*max-wind* #:*dt*
   ;; the computer player
   #:plan-shot #:pick-target
   ;; the wire
   #:game->plist #:plist->game #:copy-game-deeply
   #:encode-message #:decode-message #:plain-data-p
   #:controls-p #:valid-shot-p))
