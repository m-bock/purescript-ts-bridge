// Generated by purs version 0.15.4
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Except_Trans from "../Control.Monad.Except.Trans/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Dodo from "../Dodo/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Class_Console from "../Effect.Class.Console/index.js";
import * as Heterogeneous_Mapping from "../Heterogeneous.Mapping/index.js";
import * as Node_Process from "../Node.Process/index.js";
import * as Options_Applicative_Builder from "../Options.Applicative.Builder/index.js";
import * as Options_Applicative_Builder_Internal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options_Applicative_Extra from "../Options.Applicative.Extra/index.js";
import * as Options_Applicative_Internal_Utils from "../Options.Applicative.Internal.Utils/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
import * as PureScript_CST from "../PureScript.CST/index.js";
import * as Record from "../Record/index.js";
import * as Record_Extra from "../Record.Extra/index.js";
import * as Safe_Coerce from "../Safe.Coerce/index.js";
import * as Tidy from "../Tidy/index.js";
import * as TsBridgeGen_Types from "../TsBridgeGen.Types/index.js";
import * as Type_Equality from "../Type.Equality/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
import * as TypedEnv from "../TypedEnv/index.js";
var error = /* #__PURE__ */ Effect_Class_Console.error(Effect_Class.monadEffectEffect);
var defaultFormatOptions = /* #__PURE__ */ Tidy.defaultFormatOptions(Tidy.formatErrorVoid);
var pure = /* #__PURE__ */ Control_Applicative.pure(Effect.applicativeEffect);
var bind = /* #__PURE__ */ Control_Bind.bind(Effect.bindEffect);
var $$try = /* #__PURE__ */ Control_Monad_Error_Class["try"](Control_Monad_Error_Class.monadErrorEffect);
var mapFlipped = /* #__PURE__ */ Data_Functor.mapFlipped(/* #__PURE__ */ Control_Monad_Except_Trans.functorExceptT(Effect.functorEffect));
var bind1 = /* #__PURE__ */ Control_Bind.bind(/* #__PURE__ */ Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect));
var liftEffect = /* #__PURE__ */ Effect_Class.liftEffect(/* #__PURE__ */ Control_Monad_Except_Trans.monadEffectExceptT(Effect_Class.monadEffectEffect));
var debugIsSymbol = {
    reflectSymbol: function () {
        return "debug";
    }
};
var union = /* #__PURE__ */ Record.union();
var classFileIsSymbol = {
    reflectSymbol: function () {
        return "classFile";
    }
};
var modulesFileIsSymbol = {
    reflectSymbol: function () {
        return "modulesFile";
    }
};
var optional = /* #__PURE__ */ Options_Applicative_Types.optional(Options_Applicative_Types.readMAlt)(Options_Applicative_Types.readMApplicative);
var fold = /* #__PURE__ */ Data_Foldable.fold(Data_Foldable.foldableArray);
var fold1 = /* #__PURE__ */ fold(Options_Applicative_Builder_Internal.modMonoid);
var $$long = /* #__PURE__ */ Options_Applicative_Builder["long"](Options_Applicative_Builder_Internal.optionFieldsHasName);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Options_Applicative_Builder_Internal.modMonoid);
var $$short = /* #__PURE__ */ Options_Applicative_Builder["short"](Options_Applicative_Builder_Internal.optionFieldsHasName);
var value = /* #__PURE__ */ Options_Applicative_Builder.value(Options_Applicative_Builder_Internal.optionFieldsHasValue);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(/* #__PURE__ */ Control_Monad_Except_Trans.applicativeExceptT(Effect.monadEffect));
var Wrap = function (x) {
    return x;
};
var MappingFromMaybeRecord = function (x) {
    return x;
};
var AppEnvVars = function (x) {
    return x;
};
var AppConfig = function (x) {
    return x;
};
var AppCliArgs = function (x) {
    return x;
};
var readMString = {
    readM: Options_Applicative_Builder.str
};
var mappingWithIndexMappingFr = function (dictIsSymbol) {
    var get = Record.get(dictIsSymbol)();
    return function () {
        return {
            mappingWithIndex: function (v) {
                return function (prop) {
                    return Data_Maybe.fromMaybe(get(prop)(v));
                };
            }
        };
    };
};
var readM = function (dict) {
    return dict.readM;
};
var quitWithError = function (msg) {
    return function __do() {
        error(msg)();
        return Node_Process.exit(1)();
    };
};
var printExpr = function (expr) {
    return Dodo.print(Dodo.plainText)(Dodo.twoSpaces)((function (v) {
        return v.doc;
    })(Tidy.formatExpr(defaultFormatOptions)(expr)));
};
var showPretty = function (dictShow) {
    var $134 = Data_Show.show(dictShow);
    return function ($135) {
        return (function (v) {
            if (v instanceof PureScript_CST.ParseSucceeded) {
                return printExpr(v.value0);
            };
            return "<invalid>";
        })(PureScript_CST.parseExpr($134($135)));
    };
};
var printError = /* #__PURE__ */ showPretty(TsBridgeGen_Types.showAppError);
var handleErrors = function (v) {
    if (v instanceof Data_Either.Left) {
        return quitWithError("Unexpected Error");
    };
    if (v instanceof Data_Either.Right && v.value0 instanceof Data_Either.Left) {
        return quitWithError(printError(v.value0.value0));
    };
    if (v instanceof Data_Either.Right && v.value0 instanceof Data_Either.Right) {
        return pure(v.value0.value0);
    };
    throw new Error("Failed pattern match at TsBridgeGen.Config (line 195, column 16 - line 198, column 28): " + [ v.constructor.name ]);
};
var runInitM = function (ma) {
    return bind($$try(Control_Monad_Except_Trans.runExceptT(ma)))(handleErrors);
};
var getEnvVars = /* #__PURE__ */ (function () {
    return mapFlipped(bind1(mapFlipped(liftEffect(Data_Functor.mapFlipped(Effect.functorEffect)(Node_Process.getEnv)(TypedEnv.fromEnv(TypedEnv.readEnvImpl()()(TypedEnv.readEnvFieldsCons({
        reflectSymbol: function () {
            return "assetsDir";
        }
    })({
        reflectSymbol: function () {
            return "ASSETS_DIR";
        }
    })()(TypedEnv.readEnvFieldsCons(debugIsSymbol)({
        reflectSymbol: function () {
            return "DEBUG";
        }
    })()(TypedEnv.readEnvFieldsNil(Type_Equality.refl))()()(TypedEnv.readValueOptional(TypedEnv.parseValueBoolean)))()()(TypedEnv.readValueRequired(TypedEnv.parseValueString)))()())(Type_Proxy["Proxy"].value))))(Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither)(TsBridgeGen_Types.ErrParseEnvVars.create)))(Control_Monad_Error_Class.liftEither(Control_Monad_Except_Trans.monadThrowExceptT(Effect.monadEffect))))((function () {
        var $136 = Safe_Coerce.coerce();
        return function ($137) {
            return AppEnvVars($136($137));
        };
    })());
})();
var fromMaybeRecord = function (dictHMapWithIndex) {
    var $138 = Heterogeneous_Mapping.hmapWithIndex(dictHMapWithIndex);
    return function ($139) {
        return $138(MappingFromMaybeRecord($139));
    };
};
var fromMaybeRecord1 = /* #__PURE__ */ fromMaybeRecord(/* #__PURE__ */ Heterogeneous_Mapping.hmapWithIndexRecord()(/* #__PURE__ */ Heterogeneous_Mapping.mapRecordWithIndexCons(classFileIsSymbol)(/* #__PURE__ */ mappingWithIndexMappingFr(classFileIsSymbol)())(/* #__PURE__ */ Heterogeneous_Mapping.mapRecordWithIndexCons(debugIsSymbol)(/* #__PURE__ */ mappingWithIndexMappingFr(debugIsSymbol)())(/* #__PURE__ */ Heterogeneous_Mapping.mapRecordWithIndexCons(modulesFileIsSymbol)(/* #__PURE__ */ mappingWithIndexMappingFr(modulesFileIsSymbol)())(Heterogeneous_Mapping.mapRecordWithIndexNil)()())()())()()));
var defaults = {
    modulesFile: "src/MyTsBridgeModules.purs",
    classFile: "src/MyTsBridgeClass.purs",
    debug: false
};
var mergeCfg = function (v) {
    return function (v1) {
        var optional1 = {
            modulesFile: v.modulesFile,
            classFile: v.classFile,
            debug: v1.debug
        };
        var mandatory = {
            assetsDir: v1.assetsDir
        };
        return union(mandatory)(fromMaybeRecord1(defaults)(optional1));
    };
};
var cliOption = function (dictReadM) {
    var readM1 = readM(dictReadM);
    return function (long1) {
        return function (short1) {
            return function (help) {
                return Options_Applicative_Builder.option(optional(readM1))(fold1([ $$long(long1), Data_Maybe.maybe(mempty)($$short)(short1), Options_Applicative_Builder.help(help), value(Data_Maybe.Nothing.value) ]));
            };
        };
    };
};
var cliOption1 = /* #__PURE__ */ cliOption(readMString);
var optParser = /* #__PURE__ */ (function () {
    return Data_Functor.map(Options_Applicative_Types.parserFunctor)(AppCliArgs)(Record_Extra.sequenceRecord()(Record_Extra.sequenceRecordCons(classFileIsSymbol)()(Options_Applicative_Types.parserApply)(Record_Extra.sequenceRecordSingle(modulesFileIsSymbol)()(Options_Applicative_Types.parserFunctor)()())()())({
        classFile: cliOption1("class-file")(Data_Maybe.Nothing.value)(".."),
        modulesFile: cliOption1("modules-file")(Data_Maybe.Nothing.value)("..")
    }));
})();
var getCliArgs = /* #__PURE__ */ (function () {
    var opts = Options_Applicative_Builder.info(Options_Applicative_Internal_Utils.apApplyFlipped(Options_Applicative_Types.parserApply)(optParser)(Options_Applicative_Extra.helper))(fold(Options_Applicative_Builder.infoModMonoid)([ Options_Applicative_Builder.fullDesc, Options_Applicative_Builder.progDesc("Print a greeting for TARGET"), Options_Applicative_Builder.header("hello - a test for purescript-optparse") ]));
    return liftEffect(Options_Applicative_Extra.execParser(opts));
})();
var getConfig = /* #__PURE__ */ bind1(getCliArgs)(function (cliArgs) {
    return bind1(getEnvVars)(function (envVars) {
        return pure1(mergeCfg(cliArgs)(envVars));
    });
});
export {
    AppConfig,
    getConfig,
    runInitM
};
