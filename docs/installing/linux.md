# install libbass.so library
```
# open a terminal a navigate to stimulus control folder

# copy the library to system path
sudo cp libbass.so /usr/local/lib

# give permissions to the library
sudo chmod a+rx /usr/local/lib/libbass.so

# update linker configurations
sudo ldconfig
```

# build and install libzmq
```
// do not clone the main master
// git clone https://github.com/zeromq/libzmq.git

// must use 3.2.5 repository
// zeromq3-x is a git submodule of the stimulus_control repository 
// the following assumes you are in the dependency folder
sudo apt-get install libtool-bin
cd zeromq3-x
./autogen.sh
./configure --enable-static
make
sudo make install
sudo ldconfig
```

# authorize Kaleb Interface

```
# check your device specific constants
udevadm info --attribute-walk -n /dev/ttyUSB0

# create a new dev rule based on some constants
echo 'SUBSYSTEM=="tty", ATTRS{serial}=="A1004chl", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", MODE="0664"' | sudo tee /etc/udev/rules.d/50-usb-serial.rules > /dev/null 

# reload rules
sudo udevadm control --reload-rules

# re-add all devices
sudo udevadm trigger

# test
ls -al /dev/ttyUSB0

# you should reboot your system
sudo reboot
```

# optional

```
# to install the python bindings (necessary to the Pupil Server side)
# use a command that will not mess up with your custom zeromq setup:

# https://github.com/zeromq/pyzmq
pip install --no-use-wheel pyzmq
```