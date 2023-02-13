# obliterate
another obsidian attempt

# How to build and install on Android

Build the apk with:

> nix-build -A android.frontend -o result-android

Install the apk with:

> ./result-android/bin/deploy

# How to convince your phone to accept the debug apk

1. phone must be in developer mode
2. connect the phone via USB cable
3. set the USB connection options to "File transfer / Android Auto"
4. Enter the Developer Options menu, select "USB Debugging"
   1. optionally check "always allow USB debugging from this device" if you don't want to check this every time you reconnect the USB cable
