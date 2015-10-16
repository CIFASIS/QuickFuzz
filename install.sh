_PKG="yjsvg_haskell Juicy.Pixels svg-tree hogg tar language-javascript"
_PKG_DIR="packages"

# RECOMMENDED ########################
# Add this line to your ~/.bashrc file
# export PATH=$HOME/.cabal/bin:$PATH
######################################

cabal update
cabal --config-file=config710 install alex
cabal --config-file=config710 install happy

mkdir -p $_PKG_DIR
cd $_PKG_DIR

for i in $_PKG
do
    git clone https://github.com/CIFASIS/$i
    cd $i
    git pull
    cabal --config-file=config710 install
    cd ..
done

cd ..
cabal --config-file=config710 install
