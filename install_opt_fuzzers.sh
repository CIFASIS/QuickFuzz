_PKG_DIR="packages"

mkdir -p $_PKG_DIR
cd $_PKG_DIR

git clone https://github.com/CIFASIS/radamsa
cd radamsa
git pull
make install DESTDIR=$HOME/.local PREFIX=""
cd ..

git clone https://github.com/CIFASIS/zzuf
cd zzuf
./bootstrap
./configure --prefix=$HOME/.local
make install

cd ..
