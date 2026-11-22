;;;; src/core/wire.lisp -- a game as data, and messages between phones.
;;;;
;;;; A Nearby game is held by the host.  Guests send it what they want to do;
;;;; the host checks it, applies it, and sends everyone the resulting game.
;;;; Both travel as printed S-expressions.  What arrives is from a stranger:
;;;; it is looked over a character at a time before the Lisp reader sees it,
;;;; read back with *READ-EVAL* off, checked to hold nothing but numbers,
;;;; strings, keywords and lists, and a game in it is checked field by field
;;;; before it becomes the game on this phone.

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

(defun plistp (x &optional (longest 64))
  "True if X is a list of keys and values, the keys keywords."
  (and (listp x)
       (loop for rest = x then (cddr rest)
             for n from 0
             do (cond ((null rest) (return t))
                      ((not (and (consp rest) (keywordp (car rest)) (consp (cdr rest))
                                 (< n longest)))
                       (return nil))))))

(defun list-of-p (x test &key (least 0) most)
  "True if X is a proper list of LEAST to MOST things that each pass TEST."
  (loop for rest = x then (cdr rest)
        for n from 0
        do (cond ((null rest) (return (>= n least)))
                 ((not (and (consp rest) (or (null most) (< n most))
                            (funcall test (car rest))))
                  (return nil)))))

(defun whole-p (x low high)
  (and (integerp x) (<= low x high)))

(defun tank-plist-p (plist id)
  "True if PLIST describes a tank this game could hold as tank ID."
  (and (plistp plist)
       (flet ((whole (key low high) (whole-p (getf plist key) low high)))
         (and (eql (getf plist :id) id)
              (stringp (getf plist :name)) (<= (length (getf plist :name)) 40)
              (whole :colour 0 (1- (length *colour-names*)))
              (member (getf plist :kind) '(:human :cpu))
              (let ((owner (getf plist :owner)))
                (or (null owner) (and (stringp owner) (<= (length owner) 64))))
              (whole :x 0 (1- +world-width+))
              (whole :y 0 +world-height+)
              (whole :health 0 100)
              (whole :angle 0 180)
              (whole :power 0 100)
              (weapon-key-p (getf plist :weapon))
              (list-of-p (getf plist :inventory)
                         (lambda (entry)
                           (and (list-of-p entry #'identity :least 2 :most 2)
                                (keywordp (first entry))
                                (whole-p (second entry) 0 1000000)))
                         :most 64)
              (whole :money 0 1000000000)
              (whole :shield 0 *shield-strength*)
              (member (getf plist :alive) '(0 1))
              (whole :wins 0 1000000)
              (whole :kills 0 1000000)
              (whole :earned 0 1000000000)))))

(defun game-plist-p (plist)
  "True if PLIST describes a game: every field of the kind and within the
bounds that the rules and the view take for granted."
  (and (plistp plist)
       (flet ((whole (key low high) (whole-p (getf plist key) low high)))
         (let* ((tanks (getf plist :tanks))
                (count (and (list-of-p tanks #'listp :least 2 :most 4) (length tanks))))
           (and count
                (loop for tank in tanks
                      for id from 0
                      always (tank-plist-p tank id))
                (list-of-p (getf plist :heights) (lambda (h) (whole-p h 0 *ceiling*))
                           :least +world-width+ :most +world-width+)
                (whole :turn 0 (1- count))
                (realp (getf plist :wind))
                (<= (- *max-wind*) (getf plist :wind) *max-wind*)
                (whole :round 0 1000000)
                (whole :rounds 1 1000000)
                (member (getf plist :phase) '(:aiming :shop :over))
                (whole :rng 0 #xFFFFFFFF)
                (whole :sky 0 1000000)
                (list-of-p (getf plist :shop-ready) (lambda (id) (whole-p id 0 (1- count)))
                           :most 16)
                (or (null (getf plist :round-winner)) (whole :round-winner 0 (1- count)))
                (whole :shots 0 1000000000))))))

(defun plist->game (plist)
  "The game PLIST describes.  An error if it does not describe one."
  (unless (and (plistp plist) (eql (getf plist :version) *wire-version*))
    (error "A game from version ~a; this is version ~a."
           (and (plistp plist) (getf plist :version)) *wire-version*))
  (unless (game-plist-p plist)
    (error "Not a game this version could have sent."))
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
             ;; Along a list by looping, into one by recursion: only nesting
             ;; costs stack, and LEGIBLE-P has bounded that.
             (loop
               (when (minusp (decf (car budget)))
                 (return-from plain-data-p nil))
               (cond ((null x) (return t))
                     ((consp x) (walk (car x)) (setf x (cdr x)))
                     ((or (integerp x) (floatp x) (stringp x) (keywordp x) (eq x t)) (return t))
                     (t (return-from plain-data-p nil))))))
    (walk form)))

(defun encode-message (message)
  "MESSAGE, a list beginning with a keyword, as a string."
  (with-wire-syntax (prin1-to-string message)))

(defparameter *deepest-message* 16
  "Lists in a message nest no deeper than this; a game is five deep.")

(defparameter *longest-token* 40
  "No number or keyword in a message is longer than this.")

(defun number-token-p (token)
  "True if TOKEN is an integer or a float as the printer writes them, with
an exponent of at most three digits."
  (let ((i 0) (n (length token)))
    (flet ((digits ()
             (loop while (and (< i n) (digit-char-p (char token i)))
                   count t do (incf i)))
           (peek (characters)
             (and (< i n) (find (char token i) characters :test #'char-equal))))
      (when (peek "+-") (incf i))
      (and (plusp (digits))
           (progn (when (peek ".") (incf i) (digits)) t)
           (or (= i n)
               (and (peek "edfsl")
                    (progn (incf i) (when (peek "+-") (incf i)) t)
                    (<= 1 (digits) 3)
                    (= i n)))))))

(defun token-p (token)
  "True if TOKEN is a number, T or NIL as the printer writes them here, or a
keyword this program already knows.  A new keyword is of no use to it, and
would be kept for good; and a bare T, read in the keyword package, is one."
  (cond ((char= (char token 0) #\:)
         (and (> (length token) 1)
              (not (find #\: token :start 1))
              (find-symbol (string-upcase (subseq token 1)) '#:keyword)
              t))
        ((member token '("COMMON-LISP:T" "COMMON-LISP:NIL") :test #'string-equal) t)
        (t (number-token-p token))))

(defun legible-p (text)
  "True if TEXT begins with a list that is safe to hand to the reader: only
lists, strings and the tokens of TOKEN-P, nested no deeper than
*DEEPEST-MESSAGE*.  The reader itself would follow a million open brackets
down a million stack frames, or work for a minute on one long number."
  (let ((i 0) (n (length text)) (depth 0))
    (flet ((blank-p (c) (member c '(#\Space #\Newline #\Tab #\Return)))
           (token-char-p (c)
             (and (< (char-code c) 128) (or (alphanumericp c) (find c ":.+-")))))
      (loop while (and (< i n) (blank-p (char text i))) do (incf i))
      (unless (and (< i n) (char= (char text i) #\())
        (return-from legible-p nil))
      (loop
        (when (>= i n) (return nil))
        (let ((c (char text i)))
          (cond ((blank-p c) (incf i))
                ((char= c #\()
                 (when (> (incf depth) *deepest-message*) (return nil))
                 (incf i))
                ((char= c #\))
                 (incf i)
                 (when (zerop (decf depth)) (return t)))
                ((char= c #\")
                 (incf i)
                 (loop
                   (when (>= i n) (return-from legible-p nil))
                   (let ((d (char text i)))
                     (cond ((char= d #\\) (incf i 2))
                           ((char= d #\") (incf i) (return))
                           (t (incf i))))))
                ((token-char-p c)
                 (let ((end (or (position-if-not #'token-char-p text :start i) n)))
                   (unless (and (<= (- end i) *longest-token*)
                                (token-p (subseq text i end)))
                     (return nil))
                   (setf i end)))
                (t (return nil))))))))

(defun decode-message (text)
  "The message in TEXT, or nil if TEXT is not one."
  (handler-case
      (and (legible-p text)
           (with-wire-syntax
             (let ((form (read-from-string text)))
               (and (consp form)
                    (keywordp (first form))
                    (plain-data-p form)
                    (evenp (length (rest form)))
                    form))))
    (error () nil)))

;;; ------------------------------------------------------------------
;;; before a game: who is who

(defun device-id-p (x)
  "True if X could be the id a phone gives itself."
  (and (stringp x) (<= 1 (length x) 40)))

(defun lobby-p (players rounds)
  "True if PLAYERS and ROUNDS are what a host's lobby could hold: up to four
players, each a person on some phone or the computer."
  (and (whole-p rounds 1 100)
       (list-of-p players
                  (lambda (entry)
                    (and (plistp entry)
                         (member (getf entry :kind) '(:human :cpu))
                         (or (null (getf entry :id)) (device-id-p (getf entry :id)))))
                  :most 4)))

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
