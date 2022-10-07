module TsBridgeGen.UIText where

import Prelude

cli :: _
cli =
  { header: "ts-bridge - Generates TypeScript types from PureScript code"
  , options:
      { classFile: "Path to the PureScript module file that will contain a TsBridge type class and some instances."
      , modulesFile: "Path to the PureScript module file that will contain the exported TypeScript definitions."
      , spagoFile: "Path to the spago project file."
      , packagesFile: "Path to the pckage set file."
      , allDepsFile: "Path containing all dependencies of the project."
      }
  }

init :: _
init =
  { unexpectedError: "Unexpected Error. Try DEBUG=true"
  }
