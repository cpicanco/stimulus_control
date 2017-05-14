# cross compiling

## cross compile from debian8 to win32

```
# navigate to fpcsrc path
cd /usr/share/fpcsrc/3.0.0

# compile the cross-compiler
sudo make clean all OS_TARGET=win32 CPU_TARGET=i386

# install the cross-compiler
sudo make crossinstall OS_TARGET=win32 CPU_TARGET=i386 INSTALL_PREFIX=/usr

# link the cross compiler so that lazarus can found it
sudo ln -sf /usr/lib/fpc/3.0.0/ppcross386 /usr/bin/ppcross386
```

## cross compile from debian8 to win64
```
cd /usr/share/fpcsrc/3.0.0
sudo make clean all OS_TARGET=win64 CPU_TARGET=x86_64
sudo make crossinstall OS_TARGET=win64 CPU_TARGET=x86_64 INSTALL_PREFIX=/usr
sudo ln -sf /usr/lib/fpc/3.0.0/ppcross386 /usr/bin/ppcross386
```