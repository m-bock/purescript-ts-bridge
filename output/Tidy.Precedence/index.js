// Generated by purs version 0.15.4
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as PureScript_CST_Types from "../PureScript.CST.Types/index.js";
var compare = /* #__PURE__ */ Data_Ord.compare(Data_Ord.ordInt);
var append = /* #__PURE__ */ Data_Semigroup.append(Data_Array_NonEmpty_Internal.semigroupNonEmptyArray);
var eq = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(PureScript_CST_Types.eqModuleName));
var eq1 = /* #__PURE__ */ Data_Eq.eq(PureScript_CST_Types.eqOperator);
var ordMaybe = /* #__PURE__ */ Data_Maybe.ordMaybe(PureScript_CST_Types.ordModuleName);
var alter = /* #__PURE__ */ Data_Map_Internal.alter(ordMaybe);
var bind = /* #__PURE__ */ Control_Bind.bind(Data_Maybe.bindMaybe);
var lookup = /* #__PURE__ */ Data_Map_Internal.lookup(ordMaybe);
var pure = /* #__PURE__ */ Control_Applicative.pure(Data_Maybe.applicativeMaybe);
var compare1 = /* #__PURE__ */ Data_Ord.compare(ordMaybe);
var compare2 = /* #__PURE__ */ Data_Ord.compare(PureScript_CST_Types.ordOperator);
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var append1 = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var foldMap = /* #__PURE__ */ Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Monoid.monoidArray);
var foldl = /* #__PURE__ */ Data_Foldable.foldl(Data_Foldable.foldableArray);
var bindFlipped = /* #__PURE__ */ Control_Bind.bindFlipped(Data_Maybe.bindMaybe);
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Data_Array_NonEmpty_Internal.applicativeNonEmptyArray);
var pure2 = /* #__PURE__ */ Control_Applicative.pure(Data_List_Types.applicativeList);
var foldl1 = /* #__PURE__ */ Data_Foldable.foldl(Data_Array_NonEmpty_Internal.foldableNonEmptyArray);
var OperatorType = /* #__PURE__ */ (function () {
    function OperatorType() {

    };
    OperatorType.value = new OperatorType();
    return OperatorType;
})();
var OperatorValue = /* #__PURE__ */ (function () {
    function OperatorValue() {

    };
    OperatorValue.value = new OperatorValue();
    return OperatorValue;
})();
var QualifiedOperator = /* #__PURE__ */ (function () {
    function QualifiedOperator(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    QualifiedOperator.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new QualifiedOperator(value0, value1, value2);
            };
        };
    };
    return QualifiedOperator;
})();
var OpList = /* #__PURE__ */ (function () {
    function OpList(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    OpList.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new OpList(value0, value1, value2);
            };
        };
    };
    return OpList;
})();
var OpPure = /* #__PURE__ */ (function () {
    function OpPure(value0) {
        this.value0 = value0;
    };
    OpPure.create = function (value0) {
        return new OpPure(value0);
    };
    return OpPure;
})();
var OpHead = /* #__PURE__ */ (function () {
    function OpHead(value0) {
        this.value0 = value0;
    };
    OpHead.create = function (value0) {
        return new OpHead(value0);
    };
    return OpHead;
})();
var OpPrec = /* #__PURE__ */ (function () {
    function OpPrec(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    OpPrec.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new OpPrec(value0, value1, value2);
            };
        };
    };
    return OpPrec;
})();
var snoc = function (prevOps) {
    return function (nextPrec) {
        return function (nextOps) {
            var v = Data_Array_NonEmpty.unsnoc(prevOps);
            return Data_Array_NonEmpty["snoc$prime"](v.init)(new Data_Tuple.Tuple(v.last.value0, new OpList(v.last.value1, nextPrec, nextOps)));
        };
    };
};
var unwind = /* #__PURE__ */ (function () {
    var go = function ($copy_prec) {
        return function ($copy_ops) {
            return function ($copy_v) {
                var $tco_var_prec = $copy_prec;
                var $tco_var_ops = $copy_ops;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(prec, ops, v) {
                    if (v instanceof OpHead) {
                        $tco_done = true;
                        return new OpList(v.value0, prec, ops);
                    };
                    if (v instanceof OpPrec) {
                        $tco_var_prec = v.value1;
                        $tco_var_ops = snoc(v.value2)(prec)(ops);
                        $copy_v = v.value0;
                        return;
                    };
                    throw new Error("Failed pattern match at Tidy.Precedence (line 101, column 17 - line 104, column 46): " + [ v.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_prec, $tco_var_ops, $copy_v);
                };
                return $tco_result;
            };
        };
    };
    return function (v) {
        if (v instanceof OpHead) {
            return v.value0;
        };
        if (v instanceof OpPrec) {
            return go(v.value1)(v.value2)(v.value0);
        };
        throw new Error("Failed pattern match at Tidy.Precedence (line 97, column 10 - line 99, column 41): " + [ v.constructor.name ]);
    };
})();
var push = function ($copy_stk) {
    return function ($copy_chs) {
        var $tco_var_stk = $copy_stk;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(stk, chs) {
            if (chs instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return stk;
            };
            if (chs instanceof Data_List_Types.Cons && chs.value1 instanceof Data_List_Types.Nil) {
                if (stk instanceof OpHead) {
                    $tco_done = true;
                    return new OpPrec(stk, chs.value0.value0, chs.value0.value1);
                };
                if (stk instanceof OpPrec) {
                    var v = compare(chs.value0.value0)(stk.value1);
                    if (v instanceof Data_Ordering.EQ) {
                        $tco_done = true;
                        return new OpPrec(stk.value0, stk.value1, append(stk.value2)(chs.value0.value1));
                    };
                    if (v instanceof Data_Ordering.GT) {
                        $tco_done = true;
                        return new OpPrec(stk, chs.value0.value0, chs.value0.value1);
                    };
                    if (v instanceof Data_Ordering.LT) {
                        $tco_var_stk = stk.value0;
                        $copy_chs = new Data_List_Types.Cons(new Data_Tuple.Tuple(stk.value1, stk.value2), chs);
                        return;
                    };
                    throw new Error("Failed pattern match at Tidy.Precedence (line 82, column 9 - line 85, column 60): " + [ v.constructor.name ]);
                };
                throw new Error("Failed pattern match at Tidy.Precedence (line 78, column 5 - line 85, column 60): " + [ stk.constructor.name ]);
            };
            if (chs instanceof Data_List_Types.Cons) {
                if (stk instanceof OpHead) {
                    $tco_var_stk = new OpHead(new OpList(stk.value0, chs.value0.value0, chs.value0.value1));
                    $copy_chs = chs.value1;
                    return;
                };
                if (stk instanceof OpPrec) {
                    var v = compare(chs.value0.value0)(stk.value1);
                    if (v instanceof Data_Ordering.EQ) {
                        $tco_var_stk = new OpPrec(stk.value0, stk.value1, append(stk.value2)(chs.value0.value1));
                        $copy_chs = chs.value1;
                        return;
                    };
                    if (v instanceof Data_Ordering.GT) {
                        $tco_var_stk = new OpPrec(stk.value0, stk.value1, snoc(stk.value2)(chs.value0.value0)(chs.value0.value1));
                        $copy_chs = chs.value1;
                        return;
                    };
                    if (v instanceof Data_Ordering.LT) {
                        $tco_var_stk = stk.value0;
                        $copy_chs = new Data_List_Types.Cons(new Data_Tuple.Tuple(stk.value1, snoc(stk.value2)(chs.value0.value0)(chs.value0.value1)), chs.value1);
                        return;
                    };
                    throw new Error("Failed pattern match at Tidy.Precedence (line 91, column 9 - line 94, column 85): " + [ v.constructor.name ]);
                };
                throw new Error("Failed pattern match at Tidy.Precedence (line 87, column 5 - line 94, column 85): " + [ stk.constructor.name ]);
            };
            throw new Error("Failed pattern match at Tidy.Precedence (line 75, column 16 - line 94, column 85): " + [ chs.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_stk, $copy_chs);
        };
        return $tco_result;
    };
};
var eqOperatorNamespace = {
    eq: function (x) {
        return function (y) {
            if (x instanceof OperatorType && y instanceof OperatorType) {
                return true;
            };
            if (x instanceof OperatorValue && y instanceof OperatorValue) {
                return true;
            };
            return false;
        };
    }
};
var eq2 = /* #__PURE__ */ Data_Eq.eq(eqOperatorNamespace);
var eqQualifiedOperator = {
    eq: function (x) {
        return function (y) {
            return eq(x.value0)(y.value0) && eq2(x.value1)(y.value1) && eq1(x.value2)(y.value2);
        };
    }
};
var ordOperatorNamespace = {
    compare: function (x) {
        return function (y) {
            if (x instanceof OperatorType && y instanceof OperatorType) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof OperatorType) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof OperatorType) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof OperatorValue && y instanceof OperatorValue) {
                return Data_Ordering.EQ.value;
            };
            throw new Error("Failed pattern match at Tidy.Precedence (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqOperatorNamespace;
    }
};
var ordTuple = /* #__PURE__ */ Data_Tuple.ordTuple(ordOperatorNamespace)(PureScript_CST_Types.ordOperator);
var insert = /* #__PURE__ */ Data_Map_Internal.insert(ordTuple);
var lookup1 = /* #__PURE__ */ Data_Map_Internal.lookup(ordTuple);
var compare3 = /* #__PURE__ */ Data_Ord.compare(ordOperatorNamespace);
var union = /* #__PURE__ */ Data_Map_Internal.union(ordTuple);
var filterKeys = /* #__PURE__ */ Data_Map_Internal.filterKeys(ordTuple);
var insertOperator = function (v) {
    return function (prec) {
        var opKey = new Data_Tuple.Tuple(v.value1, v.value2);
        return alter(function (v1) {
            if (v1 instanceof Data_Maybe.Nothing) {
                return new Data_Maybe.Just(Data_Map_Internal.singleton(opKey)(prec));
            };
            if (v1 instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(insert(opKey)(prec)(v1.value0));
            };
            throw new Error("Failed pattern match at Tidy.Precedence (line 159, column 5 - line 163, column 41): " + [ v1.constructor.name ]);
        })(v.value0);
    };
};
var lookupOperator = function (v) {
    return function (precMap) {
        return bind(lookup(v.value0)(precMap))(lookup1(new Data_Tuple.Tuple(v.value1, v.value2)));
    };
};
var remapOperatorTo = function (newModName) {
    return function (v) {
        return function (precMap) {
            return Data_Maybe.fromMaybe(precMap)(bind(lookupOperator(v)(precMap))(function (prec) {
                return pure(insertOperator(new QualifiedOperator(newModName, v.value1, v.value2))(prec)(precMap));
            }));
        };
    };
};
var ordQualifiedOperator = {
    compare: function (x) {
        return function (y) {
            var v = compare1(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            var v1 = compare3(x.value1)(y.value1);
            if (v1 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v1 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return compare2(x.value2)(y.value2);
        };
    },
    Eq0: function () {
        return eqQualifiedOperator;
    }
};
var remapModuleTo = function (newModName) {
    return function (modName) {
        return function (precMap) {
            return Data_Maybe.fromMaybe(precMap)(bind(lookup(new Data_Maybe.Just(modName))(precMap))(function (ops) {
                return pure(alter(function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return new Data_Maybe.Just(ops);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return new Data_Maybe.Just(union(ops)(v.value0));
                    };
                    throw new Error("Failed pattern match at Tidy.Precedence (line 184, column 7 - line 188, column 38): " + [ v.constructor.name ]);
                })(newModName)(precMap));
            }));
        };
    };
};
var remapModuleToHiding = function (dictFoldable) {
    var elem = Data_Foldable.elem(dictFoldable)(eqQualifiedOperator);
    return function (hiding) {
        return function (newModName) {
            return function (modName) {
                return function (precMap) {
                    return Data_Maybe.fromMaybe(precMap)(bind(lookup(new Data_Maybe.Just(modName))(precMap))(function (ops) {
                        var filteredOps = filterKeys((function () {
                            var $212 = Data_Function.flip(elem)(hiding);
                            var $213 = Data_Tuple.uncurry(QualifiedOperator.create(new Data_Maybe.Just(modName)));
                            return function ($214) {
                                return !$212($213($214));
                            };
                        })())(ops);
                        return pure(alter(function (v) {
                            if (v instanceof Data_Maybe.Nothing) {
                                return new Data_Maybe.Just(filteredOps);
                            };
                            if (v instanceof Data_Maybe.Just) {
                                return new Data_Maybe.Just(union(filteredOps)(v.value0));
                            };
                            throw new Error("Failed pattern match at Tidy.Precedence (line 205, column 7 - line 209, column 46): " + [ v.constructor.name ]);
                        })(newModName)(precMap));
                    }));
                };
            };
        };
    };
};
var remapModuleToHiding1 = /* #__PURE__ */ remapModuleToHiding(Data_Foldable.foldableArray);
var remapOperators = /* #__PURE__ */ (function () {
    var goImport = function (modName) {
        return function (v) {
            if (v instanceof PureScript_CST_Types.ImportOp) {
                return [ new QualifiedOperator(new Data_Maybe.Just(modName), OperatorValue.value, v.value0.name) ];
            };
            if (v instanceof PureScript_CST_Types.ImportTypeOp) {
                return [ new QualifiedOperator(new Data_Maybe.Just(modName), OperatorType.value, v.value1.name) ];
            };
            return [  ];
        };
    };
    var goImportDecl = function (precMap) {
        return function (v) {
            var newModName = map(function (v1) {
                return v1.value1.name;
            })(v.qualified);
            if (v.names instanceof Data_Maybe.Nothing) {
                return remapModuleTo(newModName)(v.module.name)(precMap);
            };
            if (v.names instanceof Data_Maybe.Just) {
                var impOps = append1(goImport(v.module.name)(v.names.value0.value1.value.head))(foldMap((function () {
                    var $215 = goImport(v.module.name);
                    return function ($216) {
                        return $215(Data_Tuple.snd($216));
                    };
                })())(v.names.value0.value1.value.tail));
                var $169 = Data_Maybe.isJust(v.names.value0.value0);
                if ($169) {
                    return remapModuleToHiding1(impOps)(newModName)(v.module.name)(precMap);
                };
                return foldl(Data_Function.flip(remapOperatorTo(newModName)))(precMap)(impOps);
            };
            throw new Error("Failed pattern match at Tidy.Precedence (line 121, column 5 - line 132, column 67): " + [ v.names.constructor.name ]);
        };
    };
    var goDecl = function (modName) {
        return function (precMap) {
            return function (v) {
                if (v instanceof PureScript_CST_Types.DeclFixity) {
                    if (v.value0.operator instanceof PureScript_CST_Types.FixityValue) {
                        return insertOperator(new QualifiedOperator(new Data_Maybe.Just(modName), OperatorValue.value, v.value0.operator.value2.name))(v.value0.prec.value1)(insertOperator(new QualifiedOperator(Data_Maybe.Nothing.value, OperatorValue.value, v.value0.operator.value2.name))(v.value0.prec.value1)(precMap));
                    };
                    if (v.value0.operator instanceof PureScript_CST_Types.FixityType) {
                        return insertOperator(new QualifiedOperator(new Data_Maybe.Just(modName), OperatorType.value, v.value0.operator.value3.name))(v.value0.prec.value1)(insertOperator(new QualifiedOperator(Data_Maybe.Nothing.value, OperatorType.value, v.value0.operator.value3.name))(v.value0.prec.value1)(precMap));
                    };
                    throw new Error("Failed pattern match at Tidy.Precedence (line 144, column 7 - line 152, column 85): " + [ v.value0.operator.constructor.name ]);
                };
                return precMap;
            };
        };
    };
    var goModule = function (precMap) {
        return function (v) {
            return foldl(goDecl(v.header.name.name))(foldl(goImportDecl)(precMap)(v.header.imports))(v.body.decls);
        };
    };
    return goModule;
})();
var defaultPrecedence = 10;
var toOperatorTree = function (precMap) {
    return function (getOperator) {
        return function (init) {
            var go = function (stk) {
                return function (v) {
                    var v1 = getOperator(v.value0);
                    var prec = Data_Maybe.fromMaybe(defaultPrecedence)(bindFlipped(lookup1(new Data_Tuple.Tuple(v1.value1, v1.value2)))(lookup(v1.value0)(precMap)));
                    var opCh = pure1(new Data_Tuple.Tuple(v.value0, new OpPure(v.value1)));
                    return push(stk)(pure2(new Data_Tuple.Tuple(prec, opCh)));
                };
            };
            var $217 = foldl1(go)(new OpHead(new OpPure(init)));
            return function ($218) {
                return unwind($217($218));
            };
        };
    };
};
export {
    QualifiedOperator,
    OperatorType,
    OperatorValue,
    OpList,
    OpPure,
    defaultPrecedence,
    toOperatorTree,
    remapOperators,
    insertOperator,
    lookupOperator,
    remapOperatorTo,
    remapModuleTo,
    remapModuleToHiding,
    eqOperatorNamespace,
    ordOperatorNamespace,
    eqQualifiedOperator,
    ordQualifiedOperator
};
