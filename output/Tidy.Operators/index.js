// Generated by purs version 0.15.4
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as PureScript_CST_Lexer from "../PureScript.CST.Lexer/index.js";
import * as PureScript_CST_TokenStream from "../PureScript.CST.TokenStream/index.js";
import * as PureScript_CST_Types from "../PureScript.CST.Types/index.js";
import * as Tidy_Precedence from "../Tidy.Precedence/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Data_Maybe.bindMaybe);
var pure = /* #__PURE__ */ Control_Applicative.pure(Data_Maybe.applicativeMaybe);
var ordMaybe = /* #__PURE__ */ Data_Maybe.ordMaybe(PureScript_CST_Types.ordModuleName);
var lookup = /* #__PURE__ */ Data_Map_Internal.lookup(ordMaybe);
var insertWith = /* #__PURE__ */ Data_Map_Internal.insertWith(ordMaybe);
var union = /* #__PURE__ */ Data_Map_Internal.union(/* #__PURE__ */ Data_Tuple.ordTuple(Tidy_Precedence.ordOperatorNamespace)(PureScript_CST_Types.ordOperator));
var foldl = /* #__PURE__ */ Data_Foldable.foldl(Data_Foldable.foldableArray);
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var resolveOperatorExports = function (precMap) {
    return function (v) {
        var remappedPrecMap = Tidy_Precedence.remapOperators(precMap)(v);
        var goExport = function (pm) {
            var $87 = Data_Maybe.fromMaybe(pm);
            return function ($88) {
                return $87((function (v1) {
                    if (v1 instanceof PureScript_CST_Types.ExportOp) {
                        return bind(Tidy_Precedence.lookupOperator(new Tidy_Precedence.QualifiedOperator(Data_Maybe.Nothing.value, Tidy_Precedence.OperatorValue.value, v1.value0.name))(remappedPrecMap))(function (prec) {
                            return pure(Tidy_Precedence.insertOperator(new Tidy_Precedence.QualifiedOperator(new Data_Maybe.Just(v.header.name.name), Tidy_Precedence.OperatorValue.value, v1.value0.name))(prec)(pm));
                        });
                    };
                    if (v1 instanceof PureScript_CST_Types.ExportTypeOp) {
                        return bind(Tidy_Precedence.lookupOperator(new Tidy_Precedence.QualifiedOperator(Data_Maybe.Nothing.value, Tidy_Precedence.OperatorType.value, v1.value1.name))(remappedPrecMap))(function (prec) {
                            return pure(Tidy_Precedence.insertOperator(new Tidy_Precedence.QualifiedOperator(new Data_Maybe.Just(v.header.name.name), Tidy_Precedence.OperatorType.value, v1.value1.name))(prec)(pm));
                        });
                    };
                    if (v1 instanceof PureScript_CST_Types.ExportModule) {
                        return bind(lookup(new Data_Maybe.Just(v1.value1.name))(remappedPrecMap))(function (prec) {
                            return pure(insertWith(union)(new Data_Maybe.Just(v.header.name.name))(prec)(pm));
                        });
                    };
                    return Data_Maybe.Nothing.value;
                })($88));
            };
        };
        var goDecl = function (pm) {
            return function (v1) {
                if (v1 instanceof PureScript_CST_Types.DeclFixity) {
                    if (v1.value0.operator instanceof PureScript_CST_Types.FixityValue) {
                        return Tidy_Precedence.insertOperator(new Tidy_Precedence.QualifiedOperator(new Data_Maybe.Just(v.header.name.name), Tidy_Precedence.OperatorValue.value, v1.value0.operator.value2.name))(v1.value0.prec.value1)(pm);
                    };
                    if (v1.value0.operator instanceof PureScript_CST_Types.FixityType) {
                        return Tidy_Precedence.insertOperator(new Tidy_Precedence.QualifiedOperator(new Data_Maybe.Just(v.header.name.name), Tidy_Precedence.OperatorType.value, v1.value0.operator.value3.name))(v1.value0.prec.value1)(pm);
                    };
                    throw new Error("Failed pattern match at Tidy.Operators (line 75, column 7 - line 79, column 84): " + [ v1.value0.operator.constructor.name ]);
                };
                return pm;
            };
        };
        if (v.header.exports instanceof Data_Maybe.Nothing) {
            return foldl(goDecl)(precMap)(v.body.decls);
        };
        if (v.header.exports instanceof Data_Maybe.Just) {
            return foldl(goExport)(precMap)(Data_Array.cons(v.header.exports.value0.value.head)(map(Data_Tuple.snd)(v.header.exports.value0.value.tail)));
        };
        throw new Error("Failed pattern match at Tidy.Operators (line 50, column 3 - line 54, column 62): " + [ v.header.exports.constructor.name ]);
    };
};
var parseOperatorPrec = /* #__PURE__ */ (function () {
    var tokenStreamToArray = (function () {
        var go = function (acc) {
            return function ($89) {
                return (function (v) {
                    if (v instanceof PureScript_CST_TokenStream.TokenEOF) {
                        return new Data_Either.Right(acc);
                    };
                    if (v instanceof PureScript_CST_TokenStream.TokenError) {
                        return new Data_Either.Left(v.value1);
                    };
                    if (v instanceof PureScript_CST_TokenStream.TokenCons) {
                        return go(Data_Array.snoc(acc)(v.value0.value))(v.value2);
                    };
                    throw new Error("Failed pattern match at Tidy.Operators (line 33, column 35 - line 39, column 43): " + [ v.constructor.name ]);
                })(PureScript_CST_TokenStream.step($89));
            };
        };
        return go([  ]);
    })();
    return function ($90) {
        return (function (v) {
            if (v instanceof Data_Either.Right && (v.value0.length === 2 && (v["value0"][0] instanceof PureScript_CST_Types.TokSymbolName && (v["value0"][1] instanceof PureScript_CST_Types.TokInt && v["value0"][1].value1 instanceof PureScript_CST_Types.SmallInt)))) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(new Tidy_Precedence.QualifiedOperator(v["value0"][0].value0, Tidy_Precedence.OperatorValue.value, v["value0"][0].value1), v["value0"][1].value1.value0));
            };
            if (v instanceof Data_Either.Right && (v.value0.length === 3 && (v["value0"][0] instanceof PureScript_CST_Types.TokSymbolName && (v["value0"][1] instanceof PureScript_CST_Types.TokLowerName && (v["value0"][1].value0 instanceof Data_Maybe.Nothing && (v["value0"][1].value1 === "type" && (v["value0"][2] instanceof PureScript_CST_Types.TokInt && v["value0"][2].value1 instanceof PureScript_CST_Types.SmallInt))))))) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(new Tidy_Precedence.QualifiedOperator(v["value0"][0].value0, Tidy_Precedence.OperatorType.value, v["value0"][0].value1), v["value0"][2].value1.value0));
            };
            return Data_Maybe.Nothing.value;
        })(tokenStreamToArray(PureScript_CST_Lexer.lex($90)));
    };
})();
var parseOperatorTable = /* #__PURE__ */ (function () {
    var $91 = Data_Foldable.foldr(Data_Foldable.foldableArray)(Data_Tuple.uncurry(Tidy_Precedence.insertOperator))(Data_Map_Internal.empty);
    var $92 = Data_Array.mapMaybe(parseOperatorPrec);
    return function ($93) {
        return $91($92($93));
    };
})();
export {
    parseOperatorTable,
    parseOperatorPrec,
    resolveOperatorExports
};