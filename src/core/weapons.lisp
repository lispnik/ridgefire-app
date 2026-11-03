;;;; src/core/weapons.lisp -- what a tank can fire, and what the shop sells.

(in-package #:ridgefire)

(defstruct (weapon (:constructor make-weapon
                       (key name &key (radius 0) (damage 0) (price 0) (bundle 1)
                                      (effect :blast) (blurb ""))))
  key name radius damage price bundle effect blurb)

;;; EFFECT says how a shot behaves:
;;;   :blast  explodes where it lands
;;;   :split  bursts into five shells at the top of its arc
;;;   :roll   lands, then rolls downhill and explodes when it stops
;;;   :dirt   raises a mound of earth, hurting nobody
;;;   :dig    scoops out earth, hurting nobody

(defparameter *weapons*
  (list (make-weapon :pebble "Pebble" :radius 16 :damage 35
                     :blurb "A small shell. You never run out.")
        (make-weapon :boomer "Boomer" :radius 28 :damage 55 :price 1500 :bundle 3
                     :blurb "A heavy shell with a wider blast.")
        (make-weapon :splitter "Splitter" :radius 18 :damage 35 :price 3000 :bundle 2
                     :effect :split
                     :blurb "Bursts into five shells at the top of its arc.")
        (make-weapon :roller "Roller" :radius 24 :damage 50 :price 2000 :bundle 3
                     :effect :roll
                     :blurb "Lands, then rolls downhill until it stops.")
        (make-weapon :thunderhead "Thunderhead" :radius 60 :damage 95 :price 5000 :bundle 1
                     :blurb "An enormous blast. Mind your own tank.")
        (make-weapon :mudball "Mudball" :radius 36 :price 800 :bundle 3 :effect :dirt
                     :blurb "Throws up a hill of earth to hide behind.")
        (make-weapon :spade "Spade" :radius 40 :price 800 :bundle 3 :effect :dig
                     :blurb "Scoops out earth without hurting anyone."))
  "Every weapon, in the order the weapon selector cycles through them.")

(defparameter *shield-price* 2500)
(defparameter *shield-strength* 60
  "Damage a shield soaks up before the tank's own health is touched.")

(defun find-weapon (key)
  (or (find key *weapons* :key #'weapon-key)
      (error "No weapon called ~s." key)))

(defun weapon-key-p (key)
  (and (find key *weapons* :key #'weapon-key) t))

(defun shop-items ()
  "What the shop sells, as plists of :key :name :price :bundle :blurb."
  (append (loop for w in *weapons*
                when (plusp (weapon-price w))
                  collect (list :key (weapon-key w) :name (weapon-name w)
                                :price (weapon-price w) :bundle (weapon-bundle w)
                                :blurb (weapon-blurb w)))
          (list (list :key :shield :name "Shield" :price *shield-price* :bundle 1
                      :blurb (format nil "Soaks up ~d damage next round." *shield-strength*)))))

(defun find-shop-item (key)
  (find key (shop-items) :key (lambda (item) (getf item :key))))
