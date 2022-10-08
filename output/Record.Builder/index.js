// Generated by purs version 0.15.4
import * as $foreign from "./foreign.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Semigroupoid from "../Control.Semigroupoid/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Record_Unsafe_Union from "../Record.Unsafe.Union/index.js";
import * as Unsafe_Coerce from "../Unsafe.Coerce/index.js";
var Builder = function (x) {
    return x;
};
var union = function () {
    return function (r1) {
        return function (r2) {
            return Record_Unsafe_Union.unsafeUnionFn(r1, r2);
        };
    };
};
var semigroupoidBuilder = Control_Semigroupoid.semigroupoidFn;
var rename = function (dictIsSymbol) {
    var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
    return function (dictIsSymbol1) {
        var reflectSymbol1 = Data_Symbol.reflectSymbol(dictIsSymbol1);
        return function () {
            return function () {
                return function () {
                    return function () {
                        return function (l1) {
                            return function (l2) {
                                return function (r1) {
                                    return $foreign.unsafeRename(reflectSymbol(l1))(reflectSymbol1(l2))(r1);
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var nub = function () {
    return Unsafe_Coerce.unsafeCoerce;
};
var modify = function () {
    return function () {
        return function (dictIsSymbol) {
            var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
            return function (l) {
                return function (f) {
                    return function (r1) {
                        return $foreign.unsafeModify(reflectSymbol(l))(f)(r1);
                    };
                };
            };
        };
    };
};
var merge = function () {
    return function () {
        return function (r1) {
            return function (r2) {
                return Record_Unsafe_Union.unsafeUnionFn(r1, r2);
            };
        };
    };
};
var insert = function () {
    return function () {
        return function (dictIsSymbol) {
            var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
            return function (l) {
                return function (a) {
                    return function (r1) {
                        return $foreign.unsafeInsert(reflectSymbol(l))(a)(r1);
                    };
                };
            };
        };
    };
};
var disjointUnion = function () {
    return function () {
        return function (r1) {
            return function (r2) {
                return Record_Unsafe_Union.unsafeUnionFn(r1, r2);
            };
        };
    };
};
var $$delete = function (dictIsSymbol) {
    var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
    return function () {
        return function () {
            return function (l) {
                return function (r2) {
                    return $foreign.unsafeDelete(reflectSymbol(l))(r2);
                };
            };
        };
    };
};
var categoryBuilder = Control_Category.categoryFn;
var build = function (v) {
    return function (r1) {
        return v($foreign.copyRecord(r1));
    };
};
var buildFromScratch = /* #__PURE__ */ Data_Function.flip(build)({});
var flip = function (f) {
    return function (b) {
        return function (a) {
            return build(f(a))(b);
        };
    };
};
export {
    build,
    buildFromScratch,
    flip,
    insert,
    modify,
    $$delete as delete,
    rename,
    merge,
    union,
    disjointUnion,
    nub,
    semigroupoidBuilder,
    categoryBuilder
};