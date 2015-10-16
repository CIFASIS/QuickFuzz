# Parse options

while getopts "sm" opt; do
    case $opt in
        m)
            _OPT_MIN=1
            ;;
        s) 
            _OPT_SANDBOX=1
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            ;;
    esac
done

[ $_OPT_MIN ] && _MSG="minimal" || _MSG="complete"
echo "Starting ${_MSG} installation..."

#Sandbox config

[ $_OPT_SANDBOX ] && _MSG_SAND="activated" || _MSG_SAND="not activated"
echo "Sandbox option ${_MSG_SAND}"

# Define needed packages

_PKG="Juicy.Pixels"
[ $_OPT_MIN ] && _PKG="$_PKG yjsvg_haskell svg-tree hogg tar language-javascript ttasm"
_PKG_DIR="packages"

# RECOMMENDED ########################
# Add this line to your ~/.bashrc file
# export PATH=$HOME/.cabal/bin:$PATH
######################################

cabal update
# if [ $_OPT_MIN ]; then
cabal --config-file=/home/martin/.cabal/config710 --sandbox-config-file=cabal.sandbox.config install alex
cabal --config-file=/home/martin/.cabal/config710 --sandbox-config-file=cabal.sandbox.config install happy
# fi

# Clone and install forked packages

mkdir -p $_PKG_DIR
cd $_PKG_DIR

for i in $_PKG
do
    git clone https://github.com/CIFASIS/$i
    cd $i
    git pull
    if [ $_OPT_SANDBOX ]; then
        cabal --config-file=/home/martin/.cabal/config710 --sandbox-config-file=../../cabal.sandbox.config install
    else
        cabal --config-file=/home/martin/.cabal/config710 install
    fi
    cd ..
done

cd ..

# Install QuickFuzz

if [ $_OPT_MIN ]; then
    cabal configure -f minimal # Set minimal flag
fi

if [ $_OPT_SANDBOX ]; then
    cabal --config-file=/home/martin/.cabal/config710 --sandbox-config-file=../cabal.sandbox.config install
else
    cabal --config-file=/home/martin/.cabal/config710 install
fi
