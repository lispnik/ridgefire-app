;;;; tests/shop-tests.lisp

(in-package #:ridgefire/tests)

(def-suite shop :in all-tests)
(in-suite shop)

(defun game-in-shop (&rest options &key (money 0) &allow-other-keys)
  "A game whose first round has just been won by tank 0, every tank holding MONEY."
  (let* ((game (apply #'flat-game (progn (remf options :money) options)))
         (target (tank-of game 1)))
    (setf (tank-health target) 1)
    (loop for tank across (game-tanks game) do (setf (tank-money tank) money))
    (aim-at game 0 600)
    (fire game)
    (assert (eq :shop (game-phase game)))
    game))

(test the-shop-sells-everything-but-pebbles
  (let ((keys (mapcar (lambda (item) (getf item :key)) (shop-items))))
    (is (not (member :pebble keys)))
    (is (member :thunderhead keys))
    (is (member :shield keys))
    (is (every (lambda (item) (plusp (getf item :price))) (shop-items)))))

(test buying-costs-money-and-gives-a-bundle
  (let* ((game (game-in-shop))
         (buyer (tank-of game 0))
         (money (tank-money buyer)))
    (is-true (buy game 0 :boomer))
    (is (= (- money (weapon-price (find-weapon :boomer))) (tank-money buyer)))
    (is (= (+ 2 (weapon-bundle (find-weapon :boomer))) (ammo buyer :boomer)))))

(test you-cannot-buy-what-you-cannot-afford
  (let* ((game (game-in-shop))
         (buyer (tank-of game 1)))
    (setf (tank-money buyer) 100)
    (is (null (buy game 1 :thunderhead)))
    (is (= 100 (tank-money buyer)))
    (is (zerop (ammo buyer :thunderhead)))))

(test no-shopping-mid-round
  (let ((game (flat-game)))
    (setf (tank-money (tank-of game 0)) 100000)
    (is (null (buy game 0 :boomer)))))

(test the-next-round-starts-when-everyone-is-done
  (let ((game (game-in-shop)))
    (is (null (finish-shopping game 0)))
    (is (eq :shop (game-phase game)))
    (is-true (finish-shopping game 1))
    (is (eq :aiming (game-phase game)))
    (is (= 2 (game-round game)))
    (is (every #'tank-alive (game-tanks game)))
    (is (every (lambda (tank) (= 100 (tank-health tank))) (game-tanks game)))
    ;; Round wins carry over.
    (is (= 1 (tank-wins (tank-of game 0))))))

(test a-shield-bought-is-raised-next-round
  (let ((game (game-in-shop)))
    (buy game 0 :shield)
    (finish-shopping game 0)
    (finish-shopping game 1)
    (is (= *shield-strength* (tank-shield (tank-of game 0))))
    (is (zerop (ammo (tank-of game 0) :shield)))
    (is (zerop (tank-shield (tank-of game 1))))))

(test computers-shop-and-are-ready-at-once
  (let* ((game (game-in-shop :kinds '(:human :cpu) :money 10000))
         (cpu (tank-of game 1)))
    (is (member 1 (game-shop-ready game)))
    (is (plusp (ammo cpu :shield)))
    (is (>= (tank-money cpu) 0))
    (is-true (finish-shopping game 0))))
