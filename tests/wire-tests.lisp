;;;; tests/wire-tests.lisp

(in-package #:ridgefire/tests)

(def-suite wire :in all-tests)
(in-suite wire)

(test a-game-survives-the-wire
  (let* ((game (new-game :seed 5 :players (list (list :kind :human :owner "abc")
                                                (list :kind :cpu :owner nil)
                                                (list :kind :human :owner "def"))))
         (text (encode-message (list :state :game (game->plist game))))
         (back (plist->game (getf (rest (decode-message text)) :game))))
    (is (equal (game->plist game) (game->plist back)))
    (is (equalp (game-heights game) (game-heights back)))
    (is (equal (tank-owner (tank-of game 0)) (tank-owner (tank-of back 0))))
    (is (null (tank-owner (tank-of back 1))))
    (is (eq t (tank-alive (tank-of back 2))))
    ;; And plays on identically.
    (let ((a (fire game)) (b (fire back)))
      (is (equal (mapcar #'first a) (mapcar #'first b)))
      (is (equal (game->plist game) (game->plist back))))))

(test empty-lists-and-booleans-come-back
  (let ((message (list :x :list nil :flag t :n -3 :f 1.5d0 :s "hi \"there\"")))
    (is (equal message (decode-message (encode-message message))))))

(test junk-is-refused
  (is (null (decode-message "")))
  (is (null (decode-message "(")))
  (is (null (decode-message "42")))
  (is (null (decode-message "(\"not a keyword\")")))
  (is (null (decode-message "(:odd 1)")))
  (is (null (decode-message "(:x #.(error \"boom\"))")))
  (is (null (decode-message "(:x cl-user::foo)")))
  (is (null (decode-message "(:x #(1 2))"))))

(test only-the-owner-may-fire-and-only-in-turn
  (let ((game (new-game :seed 5 :players (list (list :kind :human :owner "abc")
                                               (list :kind :human :owner "def")))))
    (setf (game-turn game) 0)
    (is-true (valid-shot-p game "abc" :tank 0 :angle 45 :power 50 :weapon :pebble))
    (is (not (valid-shot-p game "def" :tank 0 :angle 45 :power 50 :weapon :pebble)))
    (is (not (valid-shot-p game "def" :tank 1 :angle 45 :power 50 :weapon :pebble)))
    (is (not (valid-shot-p game "abc" :tank 0 :angle 245 :power 50 :weapon :pebble)))
    (is (not (valid-shot-p game "abc" :tank 0 :angle 45 :power 50 :weapon :thunderhead)))
    (is (not (valid-shot-p game "abc" :tank 0 :angle 45 :power 50 :weapon :bogus)))
    (is-true (valid-shot-p game "abc" :tank 0 :angle 45 :power 50 :weapon :boomer))))

(test a-game-from-another-version-is-refused
  (let ((plist (game->plist (flat-game))))
    (setf (getf plist :version) 99)
    (signals error (plist->game plist))))

(test a-deep-copy-shares-nothing
  (let* ((game (flat-game))
         (copy (copy-game-deeply game)))
    (setf (aref (game-heights copy) 10) 333
          (tank-health (tank-of copy 0)) 1)
    (add-ammo (tank-of copy 0) :boomer 9)
    (is (= 100 (aref (game-heights game) 10)))
    (is (= 100 (tank-health (tank-of game 0))))
    (is (= 2 (ammo (tank-of game 0) :boomer)))))

(test a-game-in-the-shop-survives-the-wire
  (let* ((game (game-in-shop :kinds '(:human :cpu) :money 8000))
         (back (plist->game (getf (rest (decode-message
                                         (encode-message (list :state :game (game->plist game)))))
                                  :game))))
    (is (eq :shop (game-phase back)))
    (is (equal (game-shop-ready game) (game-shop-ready back)))
    (is (eql 0 (game-round-winner back)))
    (is (equal (tank-inventory (tank-of game 1)) (tank-inventory (tank-of back 1))))
    (is (not (tank-alive (tank-of back 1))))
    (is (= (tank-money (tank-of game 1)) (tank-money (tank-of back 1))))
    ;; The rng travels too: both start the same next round.
    (finish-shopping game 0)
    (finish-shopping back 0)
    (is (equal (game->plist game) (game->plist back)))))

(test messages-are-one-line-of-plain-text
  (let ((text (encode-message (list :state :game (game->plist (flat-game))))))
    (is (not (find #\Newline text)))
    (is (every (lambda (c) (< (char-code c) 128)) text))))

(test more-junk-is-refused
  (is (null (decode-message "(:x #\\a)")))
  (is (null (decode-message "(:x #:foo)")))
  (is (null (decode-message "(:x ridgefire::tank)")))
  (is (null (decode-message "(:x #p\"/etc/passwd\")")))
  (is (null (decode-message "(:x . 1)")))
  (is (null (decode-message "(:fire :tank 0 :angle)")) "A key with no value.")
  (is (null (decode-message ":fire")))
  (is (equal '(:fire :tank 0) (decode-message "(:fire :tank 0) trailing"))
      "Only the first form is read."))

(test an-enormous-message-is-refused
  (is (null (plain-data-p (make-list 300000 :initial-element 1))))
  (is-true (plain-data-p (make-list 1000 :initial-element 1))))

(test the-computer-cannot-be-steered-from-afar
  (let ((game (new-game :seed 5 :players (list (list :kind :human :owner "abc")
                                               (list :kind :cpu :owner "abc")))))
    (is-true (controls-p game 0 "abc"))
    (is (not (controls-p game 1 "abc")))
    (is (not (controls-p game 2 "abc")))
    (is (not (controls-p game -1 "abc")))
    (is (not (controls-p game "0" "abc")))
    (is (not (controls-p game 0 nil)))))

(test no-shots-out-of-turn-or-out-of-shape
  (let ((game (new-game :seed 5 :players (list (list :kind :human :owner "abc")
                                               (list :kind :human :owner "def")))))
    (setf (game-turn game) 0)
    (is (not (valid-shot-p game "abc" :tank 0 :angle 45.5 :power 50 :weapon :pebble)))
    (is (not (valid-shot-p game "abc" :tank 0 :angle 45 :power 101 :weapon :pebble)))
    (is (not (valid-shot-p game "abc" :tank 0 :angle -1 :power 50 :weapon :pebble)))
    (is (not (valid-shot-p game "abc" :tank 0 :angle 45 :power 50)))
    (is (not (valid-shot-p game "abc" :angle 45 :power 50 :weapon :pebble)))
    (is-true (valid-shot-p game "abc" :tank 0 :angle 0 :power 0 :weapon :pebble))
    (is-true (valid-shot-p game "abc" :tank 0 :angle 180 :power 100 :weapon :pebble))
    (setf (game-phase game) :shop)
    (is (not (valid-shot-p game "abc" :tank 0 :angle 45 :power 50 :weapon :pebble)))))
