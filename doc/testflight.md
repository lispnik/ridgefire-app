# Getting Ridgefire into TestFlight

There is no Xcode project, so what Xcode would set up behind the scenes is
done once by `make asc` (through the App Store Connect API) and one form in a
browser. After that every build is `make testflight`.

## Once

Shared with the other apps under team `Q47YS469F2`: the **Apple
Distribution** certificate in the keychain and the **App Store Connect API
key** in `~/.appstoreconnect/private_keys/`. `local.mk` (not committed) names
them.

1. `make asc` registers the App ID `com.burnsidemk.ridgefire`, makes the
   **Ridgefire App Store** profile and writes it to
   `~/.appstoreconnect/Ridgefire_App_Store.mobileprovision`. (Done.)
2. **Create the app record** -- the one thing the API cannot do.
   [Apps](https://appstoreconnect.apple.com/apps) › **+** › New App: iOS;
   name `Ridgefire` (or whatever is free; the store name need not match the
   bundle name); language English; bundle ID `com.burnsidemk.ridgefire`;
   SKU `ridgefire`; full access. `tools/asc.py check` then finds it.

## Every build

```sh
make testflight              # clean device build, check, validate, upload
make testflight BUILD=1.0.9  # a build number of your own
make ipa                     # just build/Ridgefire.ipa
```

Build numbers are `1.0.<commits + 2>` (two builds went up before the first
commit); every upload must be higher than the last.
`CFBundleShortVersionString` stays `1.0`: raise `:bundle-short-version` in
`ridgefire-app.asd` for a new version.

`tools/testflight.sh` checks the signature is Apple Distribution, carries
`beta-reports-active` and not `get-task-allow`, then `altool` validates and
uploads.

## Then, in App Store Connect

Processing takes a few minutes; the build then appears under **TestFlight**,
and internal testers (your own team) can install it at once. External
testers need Beta App Review, and before that **App Privacy** ("Data Not
Collected": Nearby games talk only to phones in the room) and Test
Information. Export compliance is answered by
`ITSAppUsesNonExemptEncryption` = false.

Nearby play needs two phones: on first use iOS asks for Local Network
permission, which must be allowed on both.
