;;;; tests/fuzz-tests.lisp -- what a stranger on the network might send.
;;;;
;;;; A Nearby host listens to any phone in the room, and a guest believes the
;;;; host it joined.  Whatever arrives must be refused or be harmless: nothing
;;;; here may overflow the stack, run for seconds, or leave behind a game the
;;;; rules cannot play.  Every case comes from a seeded generator, so a
;;;; failure names the seed and the case that repeat it.

(in-package #:ridgefire/tests)

(def-suite fuzz :in all-tests)
(in-suite fuzz)

(defun attempt (thunk)
  "Call THUNK.  Returns what became of it -- :returned, :error (an ordinary
error, which the app catches) or :escaped (anything worse, which it does
not) -- then THUNK's value, and the seconds it took."
  (let* ((start (get-internal-real-time))
         (value nil)
         (outcome (handler-case (progn (setf value (funcall thunk)) :returned)
                    (error () :error)
                    (serious-condition () :escaped))))
    (values outcome value
            (/ (- (get-internal-real-time) start) internal-time-units-per-second))))

(defun keyword-count ()
  (let ((n 0))
    (do-symbols (s '#:keyword n) (incf n))))

(defun pick (rng sequence)
  (elt sequence (rng-below rng (length sequence))))

(defun sample-game (&optional (seed 5))
  (new-game :seed seed :players (list (list :kind :human :owner "abc")
                                      (list :kind :cpu :owner nil)
                                      (list :kind :human :owner "def"))))

;;; ------------------------------------------------------------------
;;; text

(defparameter *slow* 1 "Seconds: no message should take the decoder this long.")

(defun repeated (n string)
  (with-output-to-string (out)
    (loop repeat n do (write-string string out))))

(test hostile-text-is-refused-at-once
  ;; Each of these once took the reader down with it or kept it busy: the
  ;; first three overflowed the stack, the numbers ran for a minute.
  (let ((before (keyword-count)))
    (dolist (text (list (repeated 1000000 "(")
                        (format nil "(:x ~a 1 ~a)" (repeated 100000 "(") (repeated 100000 ")"))
                        (format nil "(:x ~a 1)" (repeated 100000 "'"))
                        (format nil "(:x ~a 1)" (repeated 100000 "`"))
                        (format nil "(:x ~a)" (repeated 1000000 "7"))
                        "(:x 1e999999999)"
                        "(:x 1d-999999999)"
                        (format nil "(:x 1.~a)" (repeated 1000000 "0"))
                        (format nil "(:x #~a)" (repeated 100000 "("))
                        (format nil "(:x #|~a" (repeated 100000 "#|"))
                        (format nil "(:x |~a" (repeated 1000000 "a"))
                        (format nil "(:x :~a)" (repeated 1000000 "a"))
                        (format nil "(:x \"~a" (repeated 1000000 "a"))
                        (format nil "(:x ~a)" (repeated 300000 ":a 1 "))
                        "(:x #1=(1 . #1#))"
                        "(:x #1=(#1# 1))"
                        "(:x 1/0)"
                        "(:x #.(loop))"
                        "(:x #2147483647A())"
                        "(:x #*10101)"
                        "(:x . #1=(:x . #1#))"))
      (multiple-value-bind (outcome value seconds) (attempt (lambda () (decode-message text)))
        (let ((start (subseq text 0 (min 24 (length text)))))
          (is (eq :returned outcome) "~a… ~a" start outcome)
          (is (null value) "~a… was taken for a message" start)
          (is (< seconds *slow*) "~a… took ~,1f seconds" start seconds))))
    (is (= before (keyword-count)) "Refused messages left keywords behind.")))

(test a-new-keyword-is-not-made
  (is (null (decode-message "(:x :a-keyword-nobody-has-written 1)")))
  (is (null (decode-message "(:another-keyword-nobody-has-written)")))
  (is (null (find-symbol "A-KEYWORD-NOBODY-HAS-WRITTEN" '#:keyword)))
  (is (null (find-symbol "ANOTHER-KEYWORD-NOBODY-HAS-WRITTEN" '#:keyword)))
  ;; Nor is a bare symbol read as one.
  (is (null (decode-message "(fire :tank 0)")))
  (is (null (decode-message "(:fire tank 0)")))
  (is (null (decode-message "(:x :flag t)")))
  (is (null (decode-message "(:x :flag nil)")))
  (is (equal '(:x :flag t :list nil) (decode-message "(:x :flag common-lisp:t :list common-lisp:nil)"))))

(test numbers-as-the-printer-writes-them-are-read
  (dolist (n (list 0 -3 1.5d0 -1.5d0 1d-7 1.2345678901234568d29 4.9d-324 1.7976931348623157d308
                   0.1d0 (/ 1d0 3) 1.5f0 123456789012345678901234567890))
    (is (equal (list :x :n n) (decode-message (encode-message (list :x :n n)))) "~s" n)))

(defparameter *scraps*
  '("(" ")" "(" ")" " " "\"" "\\" ":" "." "+" "-" "#" "'" "`" "," "|" ";" "/"
    "0" "1" "9" "e" "E" "d" "999999999" "1e9" "1.5" "-3" "#." "#1=" "#1#" "#(" "#|" "|#"
    ":FIRE" ":TANK" ":ANGLE" ":STATE" ":GAME" ":X" "T" "NIL" "COMMON-LISP:T" "CL-USER::X"
    "tank" "é" "💥" "
"))

(defun random-text (rng)
  (with-output-to-string (out)
    (loop repeat (rng-below rng 40)
          do (if (zerop (rng-below rng 12))
                 (write-char (code-char (rng-below rng 1024)) out)
                 (write-string (pick rng *scraps*) out)))))

(defun message-shaped-p (x)
  (and (consp x) (keywordp (first x)) (plain-data-p x) (evenp (length (rest x)))))

(test random-text-never-gets-past-the-decoder
  (let ((rng (make-rng 2024))
        (before (keyword-count))
        (taken 0))
    (loop for case from 1 to 30000
          for text = (random-text rng)
          do (multiple-value-bind (outcome value seconds) (attempt (lambda () (decode-message text)))
               (when value (incf taken))
               (unless (and (eq :returned outcome) (< seconds *slow*)
                            (or (null value) (message-shaped-p value)))
                 (fail "case ~d, ~s: ~a in ~,1f seconds" case text outcome seconds)
                 (return))))
    (is (= before (keyword-count)) "Random text left keywords behind.")
    ;; Not everything was refused: the generator does reach the reader.
    (is (plusp taken))))

(defun mutate-text (rng text)
  "TEXT with a few of the things that go wrong, or are made to, done to it."
  (let ((text (copy-seq text)))
    (loop repeat (1+ (rng-below rng 3))
          for n = (length text)
          for at = (rng-below rng (max 1 n))
          for end = (min n (+ at (rng-below rng 30)))
          do (setf text
                   (ecase (rng-below rng 6)
                     (0 (concatenate 'string (subseq text 0 at) (pick rng *scraps*) (subseq text at)))
                     (1 (concatenate 'string (subseq text 0 at) (subseq text end)))
                     (2 (concatenate 'string (subseq text 0 end) (subseq text at)))
                     (3 (subseq text 0 at))
                     (4 (if (< at n)
                            (concatenate 'string (subseq text 0 at)
                                         (string (char (pick rng *scraps*) 0))
                                         (subseq text (1+ at)))
                            text))
                     (5 (concatenate 'string (subseq text 0 at)
                                     (repeated (rng-below rng 60) (pick rng *scraps*))
                                     (subseq text at))))))
    text))

(defun shake (game)
  "Do to GAME what the app would on hearing of a shot or a purchase.  Returns
what became of it and how long it took, as ATTEMPT does."
  (multiple-value-bind (outcome value seconds)
      (attempt (lambda ()
                 (loop repeat 3
                       do (ecase (game-phase game)
                            (:aiming (fire game))
                            (:shop (loop for tank across (game-tanks game)
                                         do (buy game (tank-id tank) :boomer)
                                            (finish-shopping game (tank-id tank))))
                            (:over (return))))
                 (plist->game (game->plist game))))
    (declare (ignore value))
    (values outcome seconds)))

(test mangled-messages-are-refused-or-harmless
  (let* ((rng (make-rng 77))
         (game (sample-game))
         (messages (list (encode-message (list :state :game (game->plist game)))
                         (encode-message (list :shot :game (game->plist game)
                                                     :tank 0 :angle 45 :power 70 :weapon :pebble))
                         (encode-message (list :fire :tank 0 :angle 45 :power 70 :weapon :boomer))
                         (encode-message (list :aim :tank 1 :angle 90 :power 55 :weapon :pebble))
                         (encode-message (list :lobby :players (list (list :id "RF-ABCDEF" :kind :human)
                                                                     (list :id nil :kind :cpu))
                                                      :rounds 3))
                         (encode-message (list :hello :id "RF-ABCDEF"))))
         (taken 0) (games 0))
    (loop for case from 1 to 4000
          for text = (mutate-text rng (pick rng messages))
          do (multiple-value-bind (outcome value seconds) (attempt (lambda () (decode-message text)))
               (unless (and (eq :returned outcome) (< seconds *slow*)
                            (or (null value) (message-shaped-p value)))
                 (fail "case ~d: decoding ~a in ~,1f seconds" case outcome seconds)
                 (return))
               (when value
                 (incf taken)
                 (multiple-value-bind (outcome game)
                     (attempt (lambda () (plist->game (getf (rest value) :game))))
                   (when (eq outcome :escaped)
                     (fail "case ~d: a game escaped" case)
                     (return))
                   (when (and (eq outcome :returned) game)
                     (incf games)
                     (multiple-value-bind (outcome seconds) (shake game)
                       (unless (and (eq outcome :returned) (< seconds *slow*))
                         (fail "case ~d: playing it, ~a in ~,1f seconds" case outcome seconds)
                         (return))))))))
    ;; Some mangling is survivable; the test means little if none is.
    (is (plusp taken))
    (is (plusp games))))

;;; ------------------------------------------------------------------
;;; games

(defparameter *hostile-values*
  (list nil t -1 0 1 2 3 4 60 100 180 181 360 399 400 799 800 (expt 2 32) (expt 10 30)
        (- (expt 10 30)) 0.5d0 -0.5d0 1d300 -1d300 "" "abc" (make-string 5000 :initial-element #\a)
        :human :cpu :aiming :shop :over :pebble :boomer :shield :tank :x
        '(1 2) '((:pebble 1)) '((:boomer -1)) '(:id 0) '(nil) '((nil)) '(((1)))))

(defun conses (tree)
  (if (consp tree) (+ 1 (conses (car tree)) (conses (cdr tree))) 0))

(defun change-cons (tree k change)
  "A copy of TREE whose K-th cons, counting from the front, is what CHANGE
makes of it."
  (let ((n -1))
    (labels ((walk (x)
               (cond ((atom x) x)
                     ((= (incf n) k) (funcall change x))
                     (t (let ((a (walk (car x))))
                          (cons a (walk (cdr x))))))))
      (walk tree))))

(defun mutate-plist (rng plist &optional (within plist))
  "PLIST with one thing in it replaced, removed, repeated or cut short.
WITHIN narrows where: the change is made inside that part of PLIST."
  (let* ((k (if (eq within plist)
                (rng-below rng (conses plist))
                (+ (conses-before plist within) (rng-below rng (max 1 (conses within)))))))
    (change-cons plist k
                 (ecase (rng-below rng 6)
                   ((0 1 2) (let ((value (pick rng *hostile-values*)))
                              (lambda (cell) (cons value (cdr cell)))))
                   (3 #'cdr)
                   (4 (lambda (cell) (cons (car cell) cell)))
                   (5 (constantly nil))))))

(defun conses-before (tree part)
  "How many conses of TREE come before PART, which is in it."
  (let ((n 0))
    (labels ((walk (x)
               (cond ((eq x part) (throw 'found n))
                     ((consp x) (incf n) (walk (car x)) (walk (cdr x))))))
      (catch 'found (walk tree) n))))

(test mangled-games-are-refused-or-playable
  ;; What a guest is sent becomes the game on its phone, and the view draws it
  ;; on every frame: one the rules could not have made must not be let in.
  (let ((rng (make-rng 404))
        (refused 0) (played 0))
    (loop for case from 1 to 6000
          for plist = (game->plist (sample-game (1+ (mod case 7))))
          ;; Most of a game is ground; half the time, aim at the rest of it.
          for mangled = (let ((once (if (evenp case)
                                        (mutate-plist rng plist (member :tanks plist))
                                        (mutate-plist rng plist))))
                          (if (zerop (rng-below rng 3)) (mutate-plist rng once) once))
          do (multiple-value-bind (outcome game) (attempt (lambda () (plist->game mangled)))
               (ecase outcome
                 (:escaped (fail "case ~d: reading the game escaped" case) (return))
                 (:error (incf refused))
                 (:returned
                  (incf played)
                  (multiple-value-bind (outcome seconds) (shake game)
                    (unless (and (eq outcome :returned) (< seconds *slow*))
                      (fail "case ~d: playing it, ~a in ~,1f seconds" case outcome seconds)
                      (return)))))))
    (is (> refused 1000) "Only ~d refused" refused)
    (is (> played 100) "Only ~d played" played)))

(defun scramble (rng game)
  "Set a few of GAME's fields to anything the wire allows, however unlikely:
tanks in the air or underground, dead tanks with health, nobody left."
  (flet ((any (low high) (rng-range rng low high)))
    (loop repeat (1+ (rng-below rng 6))
          for tank = (pick rng (game-tanks game))
          do (ecase (rng-below rng 16)
               (0 (setf (tank-x tank) (any 0 (1- +world-width+))))
               (1 (setf (tank-y tank) (any 0 +world-height+)))
               (2 (setf (tank-health tank) (any 0 100)))
               (3 (setf (tank-alive tank) (zerop (any 0 1))))
               (4 (setf (tank-shield tank) (any 0 *shield-strength*)))
               (5 (setf (tank-angle tank) (any 0 180) (tank-power tank) (any 0 100)))
               (6 (setf (tank-weapon tank) (weapon-key (pick rng *weapons*))))
               (7 (setf (tank-inventory tank)
                        (list (cons (pick rng '(:boomer :shield :pebble :tank :x)) (any 0 1000000)))))
               (8 (setf (tank-money tank) (any 0 1000000000)))
               (9 (setf (game-turn game) (any 0 (1- (length (game-tanks game))))))
               (10 (setf (game-wind game) (any (- *max-wind*) *max-wind*)))
               (11 (setf (game-phase game) (pick rng '(:aiming :shop :over))))
               (12 (fill (game-heights game) (any 0 ridgefire::*ceiling*)
                         :start (any 0 400) :end (any 400 +world-width+)))
               (13 (setf (game-round game) (any 0 1000000) (game-rounds game) (any 1 1000000)))
               (14 (setf (game-shop-ready game)
                         (loop repeat (any 0 16) collect (any 0 (1- (length (game-tanks game)))))))
               (15 (loop for tank across (game-tanks game)
                         do (setf (tank-x tank) (tank-x (aref (game-tanks game) 0)))))))
    game))

(test any-game-the-wire-allows-can-be-played
  (let ((rng (make-rng 9)))
    (loop for case from 1 to 3000
          for game = (scramble rng (sample-game (1+ (mod case 5))))
          do (multiple-value-bind (outcome sent) (attempt (lambda () (plist->game (game->plist game))))
               (unless (eq outcome :returned)
                 (fail "case ~d: a scrambled game was refused" case)
                 (return))
               (multiple-value-bind (outcome seconds) (shake sent)
                 (unless (and (eq outcome :returned) (< seconds *slow*))
                   (fail "case ~d: playing it, ~a in ~,1f seconds" case outcome seconds)
                   (return)))))
    (pass)))

;;; ------------------------------------------------------------------
;;; what a guest asks of the host

(test a-guest-can-ask-for-nothing-harmful
  (let ((rng (make-rng 31))
        (game (sample-game)))
    (flet ((any () (pick rng *hostile-values*)))
      (loop for case from 1 to 20000
            for device = (if (zerop (rng-below rng 2)) "abc" (any))
            for tank = (if (zerop (rng-below rng 2)) (game-turn game) (any))
            do (multiple-value-bind (outcome)
                   (attempt (lambda ()
                              (valid-shot-p game device :tank tank :angle (any) :power (any)
                                                        :weapon (any))
                              (controls-p game tank device)
                              (when (controls-p game tank device)
                                (buy game tank (any)))))
                 (unless (eq outcome :returned)
                   (fail "case ~d: checking a request, ~a" case outcome)
                   (return)))
               ;; An aim is followed as it is dragged: wrong, it is an error
               ;; or it is clamped, and the tank still points somewhere.
               (let ((aiming (active-tank game)))
                 (multiple-value-bind (outcome)
                     (attempt (lambda () (set-aim aiming :angle (any) :power (any) :weapon (any))))
                   (unless (and (member outcome '(:returned :error))
                                (typep (tank-angle aiming) '(integer 0 180))
                                (typep (tank-power aiming) '(integer 0 100))
                                (has-weapon-p aiming (tank-weapon aiming)))
                     (fail "case ~d: aiming, ~a, left ~a at ~a" case outcome
                           (tank-angle aiming) (tank-power aiming))
                     (return))))))
    (is (null (problems game nil)))))
