// Generated by purs version 0.15.4
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function_Memoize from "../Data.Function.Memoize/index.js";
import * as Data_NonEmpty from "../Data.NonEmpty/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var memoize2 = /* #__PURE__ */ Data_Function_Memoize.memoize2(Data_Function_Memoize.tabulateNat)(Data_Function_Memoize.tabulateNat);
var minimum = /* #__PURE__ */ Data_Semigroup_Foldable.minimum(Data_Ord.ordInt)(/* #__PURE__ */ Data_NonEmpty.foldable1NonEmpty(Data_Foldable.foldableArray));
var unsafeIndex = /* #__PURE__ */ Data_Array.unsafeIndex();
var editDistance = function (dictEq) {
    var eq = Data_Eq.eq(dictEq);
    return function (xs) {
        return function (ys) {
            var dist = function (v) {
                return function (v1) {
                    if (v === 0) {
                        return v1;
                    };
                    if (v1 === 0) {
                        return v;
                    };
                    return minimum(new Data_NonEmpty.NonEmpty($lazy_dist$prime(37)(v - 1 | 0)(v1) + 1 | 0, [ $lazy_dist$prime(38)(v)(v1 - 1 | 0) + 1 | 0, (function () {
                        var $14 = eq(unsafeIndex(xs)(v - 1 | 0))(unsafeIndex(ys)(v1 - 1 | 0));
                        if ($14) {
                            return $lazy_dist$prime(40)(v - 1 | 0)(v1 - 1 | 0);
                        };
                        return 1 + $lazy_dist$prime(41)(v - 1 | 0)(v1 - 1 | 0) | 0;
                    })() ]));
                };
            };
            var $lazy_dist$prime = $runtime_lazy("dist'", "Options.Applicative.Help.Levenshtein", function () {
                return memoize2(function (a) {
                    return function (b) {
                        return dist(a)(b);
                    };
                });
            });
            var dist$prime = $lazy_dist$prime(31);
            return dist$prime(Data_Array.length(xs))(Data_Array.length(ys));
        };
    };
};
export {
    editDistance
};
