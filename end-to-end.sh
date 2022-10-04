set -e 
set -o pipefail
shopt -s extglob

SOURCE_DIR=`pwd`
TARGET_DIR="${TARGET_DIR:-$`mktemp -d`}"
RM_GLOB="${RM_GLOB:-$"*"}"

echo SOURCE_DIR $SOURCE_DIR
echo TARGET_DIR $TARGET_DIR

# Create or cleanup target dir
mkdir -p $TARGET_DIR
rm -rf -- $TARGET_DIR/$RM_GLOB


function main () {
  pushd $TARGET_DIR

  # Init fresh yarn project
  yarn init -y

  # Add purescript-ts-bridge-cli dependency
  yarn add file:$SOURCE_DIR

  # Init fresh spago project
  spago init --tag psc-0.15.4-20220924

  # Init tsc
  tsc --init

  cp $SOURCE_DIR/assets/SampleTypes.purs src

  # Generate ts-bridge project
  yarn run ts-bridge

  # Use local typescript-bridge dependency
  echo "with typescript-bridge = $SOURCE_DIR/spago.dhall as Location" \
  >> ts-bridge/packages.dhall

  # Build main project
  spago build

  {
    pushd ts-bridge;

    # Build ts-bridge project
    spago build --purs-args "--output ../output"

    popd
  }

  # Generate Typescript Types
  echo "import('./output/MyTsBridgeModules/index.js').then(x => x.main())" \
    > generate-ts-types.js

  node generate-ts-types.js --output-dir ts-types

  # Verify generated ts code
  tsc --skipLibCheck false

  cat ts-types/SampleTypes/index.d.ts
  
  popd
}

main