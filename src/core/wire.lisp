;;;; src/core/wire.lisp -- a game as data, and messages between phones.
;;;;
;;;; A Nearby game is held by the host.  Guests send it what they want to do;
;;;; the host checks it, applies it, and sends everyone the resulting game.
;;;; Both travel as printed S-expressions, read back with *READ-EVAL* off and
;;;; checked to hold nothing but numbers, strings, keywords and lists.

(in-package #:ridgefire)

(defparameter *wire-version* 1)

;;; ------------------------------------------------------------------
;;; a game as a plist

(defun tank->plist (tank)
  (list :id (tank-id tank) :name (tank-name tank) :colour (tank-colour tank)
        :kind (tank-kind tank) :owner (tank-owner tank)
        :x (tank-x tank) :y (tank-y tank) :health (tank-health tank)
        :angle (tank-angle tank) :power (tank-power tank) :weapon (tank-weapon tank)
        :inventory (loop for (key . count) in (tank-inventory tank) collect (list key count))
        :money (tank-money tank) :shield (tank-shield tank)
        :alive (if (tank-alive tank) 1 0)
        :wins (tank-wins tank) :kills (tank-kills tank) :earned (tank-earned tank)))

(defun plist->tank (plist)
  (make-tank :id (getf plist :id) :name (getf plist :name) :colour (getf plist :colour)
             :kind (getf plist :kind) :owner (getf plist :owner)
             :x (getf plist :x) :y (getf plist :y) :health (getf plist :health)
             :angle (getf plist :angle) :power (getf plist :power) :weapon (getf plist :weapon)
             :inventory (loop for (key count) in (getf plist :inventory) collect (cons key count))
             :money (getf plist :money) :shield (getf plist :shield)
             :alive (eql 1 (getf plist :alive))
             :wins (getf plist :wins) :kills (getf plist :kills) :earned (getf plist :earned)))

(defun game->plist (game)
  (list :version *wire-version*
        :heights (coerce (game-heights game) 'list)
        :tanks (map 'list #'tank->plist (game-tanks game))
        :turn (game-turn game) :wind (game-wind game)
        :round (game-round game) :rounds (game-rounds game)
        :phase (game-phase game) :rng (rng-state (game-rng game))
        :sky (game-sky game) :shop-ready (game-shop-ready game)
        :round-winner (game-round-winner game) :shots (game-shots game)))

(defun plist->game (plist)
  (unless (eql (getf plist :version) *wire-version*)
    (error "A game from version ~a; this is version ~a." (getf plist :version) *wire-version*))
  (let ((heights (make-heights)))
    (replace heights (getf plist :heights))
    (make-game :heights heights
               :tanks (map 'vector #'plist->tank (getf plist :tanks))
               :turn (getf plist :turn) :wind (getf plist :wind)
               :round (getf plist :round) :rounds (getf plist :rounds)
               :phase (getf plist :phase) :rng (%make-rng (getf plist :rng))
               :sky (getf plist :sky) :shop-ready (getf plist :shop-ready)
               :round-winner (getf plist :round-winner) :shots (getf plist :shots))))

(defun copy-game-deeply (game)
  (plist->game (game->plist game)))

;;; ------------------------------------------------------------------
;;; text

(defmacro with-wire-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*package* (find-package '#:keyword))
           (*read-eval* nil)
           (*print-readably* nil)
           (*print-pretty* nil)
           (*read-default-float-format* 'double-float))
       ,@body)))

(defun plain-data-p (form &optional (budget (list 200000)))
  "True if FORM is made only of numbers, strings, keywords, T, NIL and lists."
  (labels ((walk (x)
             (when (minusp (decf (car budget)))
               (return-from plain-data-p nil))
             (cond ((null x) t)
                   ((consp x) (and (walk (car x)) (walk (cdr x))))
                   ((or (integerp x) (floatp x) (stringp x) (keywordp x) (eq x t)) t)
                   (t (return-from plain-data-p nil)))))
    (walk form)))

(defun encode-message (message)
  "MESSAGE, a list beginning with a keyword, as a string."
  (with-wire-syntax (prin1-to-string message)))

(defun decode-message (text)
  "The message in TEXT, or nil if TEXT is not one."
  (handler-case
      (with-wire-syntax
        (let ((form (read-from-string text)))
          (and (consp form)
               (keywordp (first form))
               (plain-data-p form)
               (evenp (length (rest form)))
               form)))
    (error () nil)))

;;; ------------------------------------------------------------------
;;; what a guest may ask of the host

(defun controls-p (game id device)
  "True if DEVICE controls tank ID and it is a person, not the computer."
  (and (integerp id) (< -1 id (length (game-tanks game)))
       (let ((tank (tank game id)))
         (and (eq (tank-kind tank) :human)
              (equal (tank-owner tank) device)))))

(defun valid-shot-p (game device &key tank angle power weapon)
  (and (eq (game-phase game) :aiming)
       (eql tank (game-turn game))
       (controls-p game tank device)
       (integerp angle) (<= 0 angle 180)
       (integerp power) (<= 0 power 100)
       (weapon-key-p weapon)
       (has-weapon-p (active-tank game) weapon)))
