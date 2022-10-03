set -e 
set -o pipefail

THERE=`pwd`

mkdir -p e2e
rm -rf e2e/*
cd e2e

#DIR=`mktemp -d`
#cd $DIR

yarn init -y
yarn add file:$THERE 
spago init --tag psc-0.15.4-20220924

yarn run ts-bridge
cp $THERE/assets/packages-local.dhall ts-bridge/packages.dhall

spago build
cd ts-bridge; spago build --purs-args "--output ../output"