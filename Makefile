# Makefile for Ridgefire
#
#   make test       run the FiveAM suite on the host, in SBCL
#   make test-ecl   the same suite in ECL, the Lisp the app is built with
#   make sim        build the simulator .app
#   make run-sim    build, install and launch it on the booted simulator
#   make demo-sim   the same, launched as a game between four computer players
#   make device     build for iphoneos, install on a connected iPhone and launch
#   make ipa        an App Store build, packaged as build/Ridgefire.ipa
#   make testflight make ipa, check it, validate it and upload it (doc/testflight.md)
#   make asc        once: register the App ID and make the App Store profile
#   make assets     redraw the icon and resynthesise the sounds (python3)
#   make deps       restore ocicl dependencies
#   make clean
#
# `make device' needs signing, which is personal and so lives in local.mk
# (not committed):
#
#   IOS_SIGNING_IDENTITY = Apple Development: Your Name (XXXXXXXXXX)
#   IOS_PROVISIONING_PROFILE = /path/to/profile.mobileprovision
#   IOS_DEVELOPMENT_TEAM = TEAMID1234
#
# and `make testflight' needs a distribution identity, an App Store profile
# and an App Store Connect API key, also in local.mk:
#
#   IOS_DISTRIBUTION_IDENTITY = Apple Distribution: Your Name (TEAMID1234)
#   IOS_DISTRIBUTION_PROFILE = /path/to/Ridgefire_App_Store.mobileprovision
#   ASC_KEY_ID = XXXXXXXXXX
#   ASC_ISSUER_ID = xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx

-include local.mk

# asdf-ios-app is not on ocicl; a checkout beside this one.  objc, FiveAM and
# the rest come from ./ocicl (see ocicl.csv).
IOS_APP_DIR ?= $(HOME)/Projects/common-lisp/asdf-ios-app
SBCL ?= sbcl
ECL ?= ecl
BUNDLE_ID = com.burnsidemk.ridgefire
# Simulator builds have an identity of their own.  macOS asks once, per
# identity, whether an app may use the local network; the simulator's answer
# is kept apart from the one a phone build gets, and can be started afresh by
# changing this.
SIM_BUNDLE_ID ?= com.burnsidemk.ridgefire.sim

# :IGNORE-INHERITED-CONFIGURATION so a missing dependency fails here rather
# than resolving to whatever a personal init file happens to point at.
REGISTRY = (asdf:initialize-source-registry \
              (list :source-registry \
                    (list :tree (truename "./")) \
                    (list :tree (truename "$(IOS_APP_DIR)")) \
                    :ignore-inherited-configuration))

# ECL in batch: any unhandled error exits 1 instead of waiting at a debugger
# prompt that nobody is there to answer.
define ecl
$(ECL) --norc \
  --eval '(require :asdf)' \
  --eval '$(REGISTRY)' \
  --eval '(handler-bind ((serious-condition (lambda (c) (format *error-output* "~&error: ~a~%" c) (finish-output *error-output*) (ext:quit 1)))) $(1))' \
  --eval '(ext:quit 0)'
endef

define sbcl
$(SBCL) --non-interactive --no-userinit --no-sysinit \
  --eval '(require :asdf)' \
  --eval '$(REGISTRY)' \
  --eval '$(1)'
endef

SIM_ENV = IOS_SIGNING_IDENTITY= IOS_PROVISIONING_PROFILE= RIDGEFIRE_BUNDLE_ID=$(SIM_BUNDLE_ID)
DEVICE_ENV = IOS_SIGNING_IDENTITY="$(IOS_SIGNING_IDENTITY)" \
             IOS_PROVISIONING_PROFILE="$(IOS_PROVISIONING_PROFILE)" \
             IOS_DEVELOPMENT_TEAM="$(IOS_DEVELOPMENT_TEAM)"

BUILD_AND_INSTALL_SIM = \
  (asdf:load-system "asdf-ios-app") \
  (uiop:symbol-call :asdf-ios-app "INSTALL-IN-SIMULATOR" \
    (first (uiop:symbol-call :asdf-ios-app "MAKE-APP" "ridgefire-app" :platforms (list :simulator))))

.PHONY: all test test-ecl sim run-sim demo-sim device ipa testflight asc assets deps clean

all: test sim

test:
	$(call sbcl,(asdf:test-system :ridgefire))

test-ecl:
	$(call ecl,(asdf:load-system :ridgefire/tests) (unless (uiop:symbol-call :ridgefire/tests :run-tests) (ext:quit 1)))

sim:
	$(SIM_ENV) $(call ecl,(asdf:make "ridgefire-app"))

run-sim:
	$(SIM_ENV) $(call ecl,$(BUILD_AND_INSTALL_SIM))
	-xcrun simctl terminate booted $(SIM_BUNDLE_ID)
	xcrun simctl launch booted $(SIM_BUNDLE_ID)

demo-sim:
	$(SIM_ENV) $(call ecl,$(BUILD_AND_INSTALL_SIM))
	-xcrun simctl terminate booted $(SIM_BUNDLE_ID)
	SIMCTL_CHILD_RIDGEFIRE_DEMO=1 xcrun simctl launch booted $(SIM_BUNDLE_ID)

device:
	@test -n "$(IOS_SIGNING_IDENTITY)" || { echo "error: set IOS_SIGNING_IDENTITY and IOS_PROVISIONING_PROFILE in local.mk" >&2; exit 1; }
	$(DEVICE_ENV) $(call ecl,\
	  (asdf:load-system "asdf-ios-app") \
	  (let* ((app (first (uiop:symbol-call :asdf-ios-app "MAKE-APP" "ridgefire-app" :platforms (list :device)))) \
	         (device (uiop:symbol-call :asdf-ios-app "INSTALL-ON-DEVICE" app))) \
	    (uiop:run-program (list "xcrun" "devicectl" "device" "process" "launch" "--device" device "$(BUNDLE_ID)") \
	                      :output t :error-output t :ignore-error-status t)))

# The build number every upload must raise: one per commit, so it never goes
# backwards and says which commit a tester is holding.  Builds 1.0.1 and 1.0.2
# went up before the first commit, hence the offset.  BUILD=1.0.20 to choose.
BUILD_OFFSET = 2
COMMITS := $(shell git rev-list --count HEAD 2>/dev/null || echo 0)
BUILD ?= 1.0.$(shell echo $$(( $(COMMITS) + $(BUILD_OFFSET) )))
IPA = build/Ridgefire.ipa

DIST_ENV = RIDGEFIRE_DISTRIBUTION=1 RIDGEFIRE_BUILD="$(BUILD)" \
           IOS_SIGNING_IDENTITY="$(IOS_DISTRIBUTION_IDENTITY)" \
           IOS_PROVISIONING_PROFILE="$(IOS_DISTRIBUTION_PROFILE)" \
           IOS_DEVELOPMENT_TEAM="$(IOS_DEVELOPMENT_TEAM)"

# From a clean build/iphoneos: a stale bundle can lack the compiled icon, and
# the upload is then refused for a missing CFBundleIconName.
ipa:
	@test -n "$(IOS_DISTRIBUTION_IDENTITY)" -a -n "$(IOS_DISTRIBUTION_PROFILE)" || { echo "error: set IOS_DISTRIBUTION_IDENTITY and IOS_DISTRIBUTION_PROFILE in local.mk" >&2; exit 1; }
	rm -rf build/iphoneos "$(IPA)"
	$(DIST_ENV) $(call ecl,\
	  (asdf:load-system "asdf-ios-app") \
	  (uiop:symbol-call :asdf-ios-app "EXPORT-IPA" \
	    (first (uiop:symbol-call :asdf-ios-app "MAKE-APP" "ridgefire-app" :platforms (list :device))) \
	    :output (merge-pathnames "$(IPA)" (uiop:getcwd))))

testflight: ipa
	ASC_KEY_ID="$(ASC_KEY_ID)" ASC_ISSUER_ID="$(ASC_ISSUER_ID)" tools/testflight.sh "$(IPA)"

asc:
	ASC_KEY_ID="$(ASC_KEY_ID)" ASC_ISSUER_ID="$(ASC_ISSUER_ID)" \
	  IOS_DISTRIBUTION_PROFILE="$(IOS_DISTRIBUTION_PROFILE)" tools/asc.py setup

assets:
	python3 tools/make-icon.py
	python3 tools/make-sounds.py

deps:
	ocicl install

clean:
	rm -rf build
	find . -name '*.fasl' -o -name '*.fas' -o -name '*.fasc' | xargs rm -f
