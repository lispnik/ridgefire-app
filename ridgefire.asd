;;;; ridgefire.asd -- the rules of Ridgefire, independent of any phone.
;;;;
;;;; The iOS application is ridgefire-app.asd, a separate file on purpose:
;;;; :DEFSYSTEM-DEPENDS-ON is resolved when an .asd is READ, so keeping the
;;;; bundle out of this file lets the core and its tests load on a Mac with
;;;; nothing but a Lisp and FiveAM.

(asdf:defsystem #:ridgefire
  :description "An artillery game: tanks on destructible hills, taking turns to lob shells through the wind."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :components ((:module "core"
                :pathname "src/core"
                :serial t
                :components ((:file "package")
                             (:file "rng")
                             (:file "terrain")
                             (:file "weapons")
                             (:file "game")
                             (:file "ai")
                             (:file "wire"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:ridgefire/tests))))

(asdf:defsystem #:ridgefire/tests
  :description "FiveAM tests for the Ridgefire core."
  :depends-on (#:ridgefire #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "terrain-tests")
                             (:file "game-tests")
                             (:file "weapon-tests")
                             (:file "shop-tests")
                             (:file "ai-tests")
                             (:file "wire-tests")
                             (:file "rules-tests")
                             (:file "edge-tests")
                             (:file "play-tests"))))
  ;; FIVEAM:RUN! prints failures but returns NIL, and ASDF discards what a
  ;; TEST-OP returns -- which is how a suite goes green with failing tests.
  :perform (asdf:test-op (o c)
             (unless (uiop:symbol-call :ridgefire/tests :run-tests)
               (error "The test suite failed."))))
