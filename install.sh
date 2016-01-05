# Parse options

# This might need more time, but we should get a better compilation
GHC_OPT='--max-backjumps=360 --reorder-goals' 

while getopts "m" opt; do
    case $opt in
        m)
            _OPT_MIN=1
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            ;;
    esac
done

[ $_OPT_MIN ] && _MSG="minimal" || _MSG="complete"
echo "Starting ${_MSG} installation..."

# Sandbox config

[ -f cabal.sandbox.config ] && _OPT_SANDBOX=1
if [ $_OPT_SANDBOX ]; then
    echo "Gentlemen we are in presence of a sandbox!"
fi

# Define needed packages
[ $_OPT_MIN ] && _PKG="Juicy.Pixels" || _PKG="wavy Juicy.Pixels yjsvg_haskell svg-tree hogg tar language-javascript ttasm"
_PKG_DIR="packages"

# RECOMMENDED ########################
# Add this line to your ~/.bashrc file
export PATH=$HOME/.cabal/bin:$PATH
######################################

cabal update
# If the installation is complete then alex and happy should be added.
if ! [ $_OPT_MIN ]; then
# These are just in case
    if [ $_OPT_SANDBOX ]; then
        cabal --sandbox-config-file=cabal.sandbox.config install $GHC_OPT alex
        cabal --sandbox-config-file=cabal.sandbox.config install $GHC_OPT happy
    else
        cabal install $GHC_OPT alex
        cabal install $GHC_OPT happy
    fi
fi

# Clone and install forked packages

mkdir -p $_PKG_DIR
cd $_PKG_DIR

for i in $_PKG
do
    git clone --depth=1 https://github.com/CIFASIS/$i
    cd $i
    git pull
    if [ $_OPT_SANDBOX ]; then
        cabal --sandbox-config-file=../../cabal.sandbox.config install $GHC_OPT
    else
        cabal install $GHC_OPT
    fi
    cd ..
done

cd ..

# Install QuickFuzz

if [ $_OPT_MIN ]; then
    cabal configure -f minimal # Set minimal flag
fi

if [ $_OPT_SANDBOX ]; then
    cabal --sandbox-config-file=cabal.sandbox.config install $GHC_OPT
    cp --remove-destination .cabal-sandbox/bin/QuickFuzz .
else
    cabal install $GHC_OPT
fi
