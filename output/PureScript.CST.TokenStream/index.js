// Generated by purs version 0.15.4
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Lazy from "../Data.Lazy/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as PureScript_CST_Layout from "../PureScript.CST.Layout/index.js";
import * as PureScript_CST_Types from "../PureScript.CST.Types/index.js";
var TokenEOF = /* #__PURE__ */ (function () {
    function TokenEOF(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TokenEOF.create = function (value0) {
        return function (value1) {
            return new TokenEOF(value0, value1);
        };
    };
    return TokenEOF;
})();
var TokenError = /* #__PURE__ */ (function () {
    function TokenError(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    TokenError.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new TokenError(value0, value1, value2, value3);
                };
            };
        };
    };
    return TokenError;
})();
var TokenCons = /* #__PURE__ */ (function () {
    function TokenCons(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    TokenCons.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new TokenCons(value0, value1, value2, value3);
                };
            };
        };
    };
    return TokenCons;
})();
var TokenStream = function (x) {
    return x;
};
var newtypeTokenStream = {
    Coercible0: function () {
        return undefined;
    }
};
var step = /* #__PURE__ */ (function () {
    var $32 = Data_Newtype.unwrap();
    return function ($33) {
        return Data_Lazy.force($32($33));
    };
})();
var unwindLayout = function (pos) {
    return function (eof) {
        var go = function (stk) {
            return Data_Lazy.defer(function (v) {
                if (stk instanceof Data_List_Types.Nil) {
                    return step(eof);
                };
                if (stk instanceof Data_List_Types.Cons) {
                    if (stk.value0.value1 instanceof PureScript_CST_Layout.LytRoot) {
                        return step(eof);
                    };
                    if (PureScript_CST_Layout.isIndented(stk.value0.value1)) {
                        return new TokenCons(PureScript_CST_Layout.lytToken(pos)(new PureScript_CST_Types.TokLayoutEnd(stk.value0.value0.column)), pos, go(stk.value1), stk.value1);
                    };
                    if (Data_Boolean.otherwise) {
                        return step(go(stk.value1));
                    };
                    throw new Error("Failed pattern match at PureScript.CST.TokenStream (line 59, column 7 - line 66, column 27): " + [ stk.value0.value1.constructor.name ]);
                };
                throw new Error("Failed pattern match at PureScript.CST.TokenStream (line 56, column 43 - line 66, column 27): " + [ stk.constructor.name ]);
            });
        };
        return go;
    };
};
var layoutStack = function (stream) {
    var v = step(stream);
    if (v instanceof TokenEOF) {
        return Data_List_Types.Nil.value;
    };
    if (v instanceof TokenError) {
        return v.value3;
    };
    if (v instanceof TokenCons) {
        return v.value3;
    };
    throw new Error("Failed pattern match at PureScript.CST.TokenStream (line 48, column 22 - line 51, column 29): " + [ v.constructor.name ]);
};
var consTokens = function (dictFoldable) {
    var go = function (v) {
        return function (v1) {
            return new Data_Tuple.Tuple(v.value0.range.start, Data_Lazy.defer(function (v2) {
                return new TokenCons(v.value0, v1.value0, v1.value1, v.value1);
            }));
        };
    };
    return Data_Function.flip(Data_Foldable.foldr(dictFoldable)(go));
};
export {
    TokenStream,
    TokenEOF,
    TokenError,
    TokenCons,
    step,
    consTokens,
    layoutStack,
    unwindLayout,
    newtypeTokenStream
};
