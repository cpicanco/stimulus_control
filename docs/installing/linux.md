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