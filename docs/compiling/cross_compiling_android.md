original script: Renato Bordin -> renabor -> lazandroid module wizard
https://github.com/jmpessoa/lazandroidmodulewizard/blob/master/docs/linux/1-master-script.sh

# android arm cross-compiler

```bash
# you must resolve any bronken dependency first
# /etc/apt/sources.list
# for java 8
# deb http://ftp.us.debian.org/debian/ jessie main contrib non-free
# deb http://ftp.de.debian.org/debian/ jessie-backports main contrib non-free
# deb http://httpredir.debian.org/debian/ jessie-updates main contrib non-free 
# deb http://security.debian.org/ jessie/updates main contrib non-free 

# make sure your system is up-to-date
sudo apt-get update
sudo apt-get upgrade

# android dependencies
sudo apt-get install openjdk-8-jdk
sudo apt-get install libgtk2.0-dev libcairo2-dev libpango1.0-dev libgdk-pixbuf2.0-dev libatk1.0-dev libghc-x11-dev
sudo apt-get install android-tools-adb ant subversion freeglut3 freeglut3-dev
sudo apt-get install libgtk2-gladexml-perl libgtk2.0-bin libgtk2.0-cil  
sudo apt-get install libgtk2.0-dev libgdk-pixbuf2.0-dev libgpm-dev fakeroot libncurses5-dev libtinfo-dev
# sudo apt-get install libwxgtk3.0-0v5

# where android will live?
mkdir ~/android
cd ~/android

# Android NDK
wget https://dl.google.com/android/repository/android-ndk-r11c-linux-x86_64.zip
unzip android-ndk-r11c-linux-x86_64.zip
rm android-ndk-r11c-linux-x86_64.zip

# Android SDK
cd ~/android
wget https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz
tar xzvf android-sdk_r24.4.1-linux.tgz
rm android-ndk-r11c-linux-x86_64.zip

# create shortcut links
ln -s ~/android/android-ndk-r11c ~/android/ndk
ln -s ~/android/android-sdk-linux ~/android/sdk

# link linkers so that fpc can see it
cd /usr/bin
sudo ln -s ~/android/ndk/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-as arm-linux-androideabi-as
sudo ln -s ~/android/ndk/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/bin/arm-linux-androideabi-ld.bfd arm-linux-androideabi-ld
sudo ln -s /usr/bin/arm-linux-androideabi-as arm-linux-as
sudo ln -s /usr/bin/arm-linux-androideabi-ld arm-linux-ld

# download and install SDK packages for ALL TARGET ANDROID VERSIONS
# https://developer.android.com/sdk/installing/adding-packages.html
~/android/sdk/tools/android sdk

# configure your .bashrc
# ANDROID_HOME=~/android/sdk
# ANDROID_BIN=~/android/android-ndk-r11c/toolchains/x86-4.9/prebuilt/linux-x86_64/bin
# export PATH=$ANDROID_BIN:$ANDROID_HOME:$PATH

# compile the cross-compiler
cd /usr/share/fpcsrc/3.0.0
sudo make clean crossall OS_TARGET=android CPU_TARGET=arm
sudo make crossinstall OS_TARGET=android CPU_TARGET=arm CROSSOPT="-Cparmv7a -Cfvfpv3 -OpARMv7a -OoFastMath -O3 -XX -Xs -CX" OPT=-dFPC_ARMEL CROSSBINDIR=$ANDROID_BIN BINUTILSPREFIX=arm-linux-androideabi- INSTALL_PREFIX=/usr
sudo ln -s /usr/lib/fpc/3.0.0/ppcrossarm /usr/bin/ppcrossarm


# check the spec of the target phone, for example:
# http://www.gsmarena.com/alcatel_pixi_4_%284%29-7845.php

# configure your lazarus build accordingly
# -Tandroid -Parm -MObjFPC -Scghi -O1 -g -gl -l -vewnhibq -Filib/arm-android -Fl../../android/android-ndk-r11c/platforms/android-23/arch-arm/usr/lib -Fl../../android/android-ndk-r11c/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/lib/gcc/arm-linux-androideabi/4.9 -Fu../../.lazarus/lib/units/arm-android/customdrawn -Fu../../.lazarus/lib/LCLBase/units/arm-android -Fu../../.lazarus/lib/LazUtils/lib/arm-android -Fu../../.lazarus/lib/units/arm-android -Fu. -FUlib/arm-android -ohello_world_android_arm -dLCL -dLCLcustomdrawn -dANDROID

# compile your program
```

# make an apk for your program using ant (Apache Ant(TM) version 1.9.4 compiled on October 7 2014)

```bash
# make sure JAVA_HOME is properly set in /etc/environment and, if not already done,
# add the following line in your .bashrc
# source /etc/environment

# create your debug and release android keystores
keytool -genkey -v -keystore mykey.keystore -alias mykeystore -keyalg RSA -keysize 2048 -validity 10000
keytool -genkey -v -keystore debug.keystore -alias mydebugkey -storepass store123 -keypass key12345 -keyalg RSA -keysize 2048 -validity 10000

# copy and past the outputs and save your key finger-prints
keytool -list -v -keystore mykey.keystore -alias mykeystore -storepass store123 -keypass key12345
keytool -list -v -keystore debug.keystore -alias mydebugkey -storepass store123 -keypass key12345

# create an android project to generate a default .build.xml for ant
cd ~/android/sdk/tools
./android create project --target 7 --name helloworld --package free.pascal.demo --activity helloworld --path ~/git/android_hello_world/apk

#.configure your ant.properties file
# for release
#key.store=mykey.keystore
#key.alias=mykeystore
#key.store.password=store123
#key.alias.password=store123

# for debug j

# check if you build successfully
cd ~/git/android_hello_world/apk
ant debug
ant release
ant clean release

```

