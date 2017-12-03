# sudo apt-get install bnfc
cd src/Test/QuickFuzz/Gen/Bnfc/
bnfc --haskell Grammar.cf -p Test.QuickFuzz.Gen.Bnfc
mv Test/QuickFuzz/Gen/Bnfc/* .
rm -rf Test
cd ../../../../../

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

git clone https://github.com/CIFASIS/honggfuzz
cd honggfuzz
make
cp ./honggfuzz $HOME/.local/bin
