module TsBridge
  ( module Exp
  , tsModuleFile
  , tsModuleWithImports
  , tsProgram
  , tsTypeAlias
  , tsValue
  )
  where

import Prelude

import Control.Monad.Writer (listens, tell)
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, un)
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Undefined (undefined)
import Safe.Coerce (coerce)
import TsBridge.DTS (TsDeclVisibility(..), TsDeclaration(..), TsFilePath(..), TsImport(..), TsModule(..), TsModuleAlias(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsProgram(..), TsQualName(..), TsRecordField(..), TsType(..), TsTypeArgs(..), dtsFilePath)
import TsBridge.DTS as TsBridge.DTS
import TsBridge.Monad (TsBridgeAccum(..), TsBridgeM, defaultTsBridgeAccum, runTsBridgeM)
import TsBridge.DTS (PropModifiers, TsDeclVisibility(..), TsDeclaration(..), TsFilePath(..), TsFnArg(..), TsImport(..), TsModule(..), TsModuleAlias(..), TsModuleFile(..), TsModulePath(..), TsName(..), TsProgram(..), TsQualName(..), TsRecordField(..), TsType(..), TsTypeArgs(..), TsTypeArgsQuant(..), Wrap(..), dtsFilePath, mapQuantifier) as Exp
import TsBridge.ABC (A(..), B(..), C(..), D(..), E(..), F(..), G(..), H(..), I(..), J(..), K(..), L(..), M(..), N(..), O(..), P(..), Q(..), R(..), S(..), T(..), U(..), V(..), W(..), X(..), Y(..), Z(..)) as Exp
import TsBridge.Class (class GenRecord, class GenRecordRL, genRecord, genRecordRL) as Exp
import TsBridge.Monad (Scope, TsBridgeAccum(..), TsBridgeM(..), TsBridge_Monad_Wrap(..), defaultTsBridgeAccum, opaqueType, runTsBridgeM) as Exp
import TsBridge.Print (printTsDeclarations, printTsName, printTsProgram, printTsType) as Exp

tsModuleFile :: String -> Array (TsBridgeM (Array TsDeclaration)) -> Array TsModuleFile
tsModuleFile n xs =
  let
    (xs' /\ TsBridgeAccum { typeDefs, imports }) = runTsBridgeM $ join <$> sequence xs
  in
    typeDefs <> [ TsModuleFile (dtsFilePath n) (TsModule imports xs') ]

mergeModules :: Array TsModuleFile -> TsProgram
mergeModules xs =
  xs
    <#> (\(TsModuleFile mp m) -> mp /\ m)
    # Map.fromFoldableWith mergeModule
    # TsProgram

mergeModule :: TsModule -> TsModule -> TsModule
mergeModule (TsModule is1 ds1) (TsModule is2 ds2) =
  TsModule
    (is1 `Set.union` is2)
    (Array.nub (ds1 <> ds2))

tsModuleWithImports :: String -> Array TsImport -> Array (Array TsImport) -> TsModule
tsModuleWithImports = undefined

tsProgram :: Array (Array TsModuleFile) -> TsProgram
tsProgram xs = mergeModules $ join xs

-- tsTypeAlias :: forall a. ToTsBridge a => String -> Proxy a -> TsBridgeM (Array TsDeclaration)
-- tsTypeAlias n p = ado
--   x /\ scope <- listens (un TsBridgeAccum >>> _.scope) $ toTsBridge p
--   in [ TsDeclTypeDef (TsName n) Public (coerce scope.floating) x ]

tsTypeAlias :: String -> TsBridgeM TsType -> TsBridgeM (Array TsDeclaration)
tsTypeAlias n t = ado
  x /\ scope <- listens (un TsBridgeAccum >>> _.scope) t
  in [ TsDeclTypeDef (TsName n) Public (coerce scope.floating) x ]

tsValue :: String -> TsBridgeM TsType -> TsBridgeM (Array TsDeclaration)
tsValue n mt = do
  t <- mt
  pure [ TsDeclValueDef (TsName n) Public t ]

-- tsOpaqueType :: forall a. String -> String -> Array TsType -> a -> TsBridgeM TsType
-- tsOpaqueType m n args _ = do
--   --x /\ scope <- listens (un TsBridgeAccum >>> _.scope) $ toTsBridge p
--   let
--     moduleAlias = TsModuleAlias $ sanatizeName m
--     modulePath = TsModulePath ("~/" <> m)
--     imports = Set.singleton $ TsImport moduleAlias modulePath

--     opaqueType =
--       TsTypeRecord
--         ( TsRecordField
--             (TsName "brand")
--             { readonly: true, optional: false }
--             TsTypeUniqueSymbol
--             :
--               Array.mapWithIndex mkTypeArgField args
--         )

--     mkTypeArgField :: Int -> TsType -> TsRecordField
--     mkTypeArgField idx ty = TsRecordField
--       (TsName ("_arg" <> show idx))
--       { readonly: true, optional: false }
--       ty

--     typeDefs =
--       [ TsModuleFile
--           (TsFilePath (m <> "/index") "d.ts")
--           ( TsModule Set.empty
--               [ TsDeclTypeDef (TsName n) Public mempty opaqueType
--               ]
--           )
--       ]

--   tell
--     $ over TsBridgeAccum
--         _
--           { imports = imports
--           , typeDefs = typeDefs
--           }
--         defaultTsBridgeAccum
--   pure $ TsTypeConstructor
--     (TsQualName (Just $ moduleAlias) (TsName n))
--     (TsTypeArgs args)

sanatizeName :: String -> String
sanatizeName = Str.replaceAll (Pattern ".") (Replacement "_")