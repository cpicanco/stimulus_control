
Running from source
===================

Dependencies Overview:

- Programs:
    - Git, C Tools, etc.   
    - [FPC, FPC-Source and Lazarus 1.6][lazarus-ide]

- Libraries:   
    - [VideoLan Player][videolan-player]   
    - [libzyre][zyre]
    - [libzmq][zmq]   
    - [lbass][bass]

- Bindings:
    - [LCLVLC][lazarus-ide]   
    - [delphizmq][zmq.pas]   
    - [fpc_zyre][zyre.pas]
    - [bass.pas][bass]

- Units:   
    - [msgpack-delphi][msgpack.pas]

- Optional:   
    - [zmq python binding][pyzmq]   
    - [PUPIL Capture][pupil] bundle (0.8.5+)

- Repositorie:   
    - [Stimulus Control][stimulus-control]

1. Dependencies installation in a 64 bits, Debian 8, machine

``` bash
# System update
sudo apt-get update

# Install FPC, FPC-Source and Lazarus 1.6
cd ~
cd Downloads
wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%201.6/fpc_3.0.0-151205_amd64.deb/download
wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%201.6/fpc-src_3.0.0-151205_amd64.deb/download
wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%201.6/lazarus_1.6-0_amd64.deb/download
sudo dpkg -i fpc-src_3.0.0-151205_amd64.deb fpc_3.0.0-151205_amd64.deb lazarus_1.6-0_amd64.deb

# Install Git, VideoLan Player, C tools, etc:
sudo apt-get install git-all vlc build-essential libtool pkg-config autotools-dev autoconf automake cmake uuid-dev
cd ~
mkdir git && cd git

# Build and install libzmq library (3.2.5+) from source:
git clone git://github.com/zeromq/libzmq.git && cd libzmq
./autogen.sh
./configure
make check
sudo make install
cd ..

# optionally, install zmq python binding
pip install --no-use-wheel pyzmq

# Clone the repository:
git clone https://github.com/cpicanco/stimulus_control.git && cd stimulus_control

# Install BASS library:
sudo cp /dependency/lbass/linux/64/libbass.so /usr/local/lib
sudo chmod a+rx /usr/local/lib/libbass.so

# Configure Dynamic Linker Run Time Bindings
sudo ldconfig

# Done.
```
___

2. Dependencies installation in a 32 bits, Windows, machine 

Windows 8, 8.1, 10
  
  Copy the 32bits version of the dlls from the dependency folder to the `.exe` folder.

  Done!

[lazarus-ide]:http://www.lazarus.freepascal.org/
[videolan-player]:http://www.videolan.org/
[zmq]:https://zeromq.org/
[pyzmq]:https://github.com/zeromq/pyzmq
[stimulus-control]:https://github.com/cpicanco/stimulus_control
[msgpack.pas]:https://github.com/cpicanco/msgpack-delphi/
[zmq.pas]:https://github.com/cpicanco/delphizmq/
[zyre]:https://github.com/zeromq/zyre
[zyre.pas]:https://github.com/cpicanco/fpc_zyre
[pupil]:https://github.com/pupil-labs/pupil
[bass]:http://www.un4seen.com/