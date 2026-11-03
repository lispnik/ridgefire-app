;;;; ridgefire-app.asd -- Ridgefire as an iOS application.
;;;;
;;;; Built with asdf-ios-app on ECL: make sim, make run-sim, make device,
;;;; make testflight.  The platforms and signing are read from the environment
;;;; when this file is read, so one declaration serves the simulator (no
;;;; identity), a phone, and an App Store build (RIDGEFIRE_DISTRIBUTION=1).

(defsystem "ridgefire-app"
  :defsystem-depends-on ("asdf-ios-app")
  :class :ios-app-system
  :build-operation "ios-app-op"
  :entry-point "ridgefire-ios:start"
  :description "Tanks on destructible hills taking turns to lob shells through the wind, for two to four players on one phone or on phones in the same room."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  ;; CFBundleVersion: the build number, which every upload must raise.
  :version #.(or (uiop:getenv "RIDGEFIRE_BUILD") "1.0.0")
  :bundle-short-version "1.0"
  :serial t
  :depends-on ("ridgefire" "objc/uikit")
  :components ((:module "ios"
                :pathname "src/ios"
                :serial t
                :components ((:file "package")
                             (:file "nodes")
                             (:file "widgets")
                             (:file "battle")
                             (:file "net")
                             (:file "flow")
                             (:file "screens")
                             (:file "app"))))

  ;; RIDGEFIRE_BUNDLE_ID gives a test build an identity of its own, for
  ;; example to be asked afresh for Local Network access.
  :bundle-identifier #.(or (uiop:getenv "RIDGEFIRE_BUNDLE_ID") "com.burnsidemk.ridgefire")
  :bundle-name "Ridgefire"
  :bundle-display-name "Ridgefire"
  :bundle-executable "ridgefire"
  :bundle-icon "res/Ridgefire.xcassets"
  ;; An artillery duel wants the width.  iPhone only: an iPad app must allow
  ;; every orientation for multitasking.
  :bundle-device-family (:iphone)
  :bundle-orientations (:landscape-left :landscape-right)
  :bundle-status-bar-hidden t
  :bundle-info-plist (("UIViewControllerBasedStatusBarAppearance" . :false)
                      ("ITSAppUsesNonExemptEncryption" . :false)
                      ("LSApplicationCategoryType" . "public.app-category.action-games")
                      ("GCSupportsGameMode" . :true)
                      ;; MultipeerConnectivity: the Bonjour service it uses,
                      ;; and why the app looks at the local network.
                      ("NSBonjourServices" . (:array "_ridgefire._tcp" "_ridgefire._udp"))
                      ("NSLocalNetworkUsageDescription"
                       . "Ridgefire looks for phones in the same room to play Nearby games with."))
  ;; tools/make-sounds.py synthesises these, into the bundle's root where
  ;; SpriteKit's playSoundFileNamed: looks.
  :bundle-resources (("res/PrivacyInfo.xcprivacy" . "PrivacyInfo.xcprivacy")
                     "res/sounds/fire.wav" "res/sounds/boom.wav" "res/sounds/bigboom.wav"
                     "res/sounds/thud.wav" "res/sounds/coin.wav" "res/sounds/click.wav"
                     "res/sounds/turn.wav" "res/sounds/win.wav")
  :bundle-frameworks ("UIKit" "Foundation" "CoreGraphics" "SpriteKit" "AVFoundation"
                      "MultipeerConnectivity")

  ;; A distribution build must not let a debugger attach.
  :get-task-allow #.(not (uiop:getenv "RIDGEFIRE_DISTRIBUTION"))
  :bundle-platforms #.(let ((identity (uiop:getenv "IOS_SIGNING_IDENTITY")))
                        (cond ((uiop:getenv "RIDGEFIRE_DISTRIBUTION") '(:device))
                              ((and identity (plusp (length identity)))
                               '(:simulator :device))
                              (t '(:simulator))))
  :code-signing-identity #.(let ((identity (uiop:getenv "IOS_SIGNING_IDENTITY")))
                             (if (and identity (plusp (length identity))) identity :automatic))
  :development-team #.(let ((team (uiop:getenv "IOS_DEVELOPMENT_TEAM")))
                        (and team (plusp (length team)) team))
  :provisioning-profile #.(let ((profile (uiop:getenv "IOS_PROVISIONING_PROFILE")))
                            (and profile (plusp (length profile)) profile)))
