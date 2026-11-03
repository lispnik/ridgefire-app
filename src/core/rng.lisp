;;;; src/core/rng.lisp -- a small seeded generator that lives in the game.
;;;;
;;;; Not CL:RANDOM: the state has to travel inside a saved or transmitted game,
;;;; and has to mean the same thing in SBCL (the tests) and ECL (the phone).

(in-package #:ridgefire)

(defstruct (rng (:constructor %make-rng (state)))
  (state 1 :type (unsigned-byte 32)))

(defun make-rng (seed)
  "A generator seeded from any integer."
  (let ((s (logand (logxor (* (logand seed #xFFFFFFFF) 2654435761) #x5BD1E995)
                   #xFFFFFFFF)))
    (%make-rng (if (zerop s) #x6D2B79F5 s))))

(defun rng-next (rng)
  "Advance RNG (xorshift32) and return the new 32-bit state."
  (let ((s (rng-state rng)))
    (setf s (logand (logxor s (ash s 13)) #xFFFFFFFF)
          s (logxor s (ash s -17))
          s (logand (logxor s (ash s 5)) #xFFFFFFFF))
    (setf (rng-state rng) s)))

(defun rng-below (rng n)
  "An integer in [0, N)."
  (if (<= n 1) 0 (mod (rng-next rng) n)))

(defun rng-range (rng low high)
  "An integer in [LOW, HIGH], both inclusive."
  (+ low (rng-below rng (1+ (- high low)))))

(defun rng-float (rng)
  "A double in [0, 1)."
  (/ (rng-next rng) 4294967296d0))
