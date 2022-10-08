// Generated by purs version 0.15.4
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_FoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data_Functor_Variant from "../Data.Functor.Variant/index.js";
import * as Data_Variant from "../Data.Variant/index.js";
import * as Record from "../Record/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var on = /* #__PURE__ */ Data_Variant.on();
var on1 = /* #__PURE__ */ Data_Functor_Variant.on();
var ConstFolding = function (x) {
    return x;
};
var hfoldlWithIndexRowListNil = {
    hfoldlWithIndex: function (v) {
        return function (x) {
            return function (v1) {
                return x;
            };
        };
    }
};
var hfoldlWithIndex = function (dict) {
    return dict.hfoldlWithIndex;
};
var hfoldlRowList = function (dictHFoldlWithIndex) {
    var hfoldlWithIndex1 = hfoldlWithIndex(dictHFoldlWithIndex);
    return {
        hfoldl: function (f) {
            return hfoldlWithIndex1(f);
        }
    };
};
var hfoldl = function (dict) {
    return dict.hfoldl;
};
var functionFoldingWithIndex = {
    foldingWithIndex: function (f) {
        return f;
    }
};
var functionFolding = {
    folding: function (f) {
        return f;
    }
};
var foldlVariantRowList = function (dict) {
    return dict.foldlVariantRowList;
};
var hfoldlVariant = function () {
    return function (dictFoldlVariant) {
        return {
            hfoldl: (function () {
                var $102 = foldlVariantRowList(dictFoldlVariant)(Type_Proxy["Proxy"].value);
                return function ($103) {
                    return $102(ConstFolding($103));
                };
            })()
        };
    };
};
var hfoldlVariantWithIndex = function () {
    return function (dictFoldlVariant) {
        return {
            hfoldlWithIndex: foldlVariantRowList(dictFoldlVariant)(Type_Proxy["Proxy"].value)
        };
    };
};
var foldlVariantNil = {
    foldlVariantRowList: function (v) {
        return function (v1) {
            return function (v2) {
                return Data_Variant.case_;
            };
        };
    }
};
var foldlVariantFRowList = function (dict) {
    return dict.foldlVariantFRowList;
};
var hfoldlVariantF = function () {
    return function (dictFoldlVariantF) {
        return {
            hfoldl: (function () {
                var $104 = foldlVariantFRowList(dictFoldlVariantF)(Type_Proxy["Proxy"].value);
                return function ($105) {
                    return $104(ConstFolding($105));
                };
            })()
        };
    };
};
var hfoldlVariantFWithIndex = function () {
    return function (dictFoldlVariantF) {
        return {
            hfoldlWithIndex: foldlVariantFRowList(dictFoldlVariantF)(Type_Proxy["Proxy"].value)
        };
    };
};
var foldlVariantFNil = {
    foldlVariantFRowList: function (v) {
        return function (v1) {
            return function (v2) {
                return Data_Functor_Variant.case_;
            };
        };
    }
};
var foldlRecordRowList = function (dict) {
    return dict.foldlRecordRowList;
};
var hfoldlRecord = function () {
    return function (dictFoldlRecord) {
        var foldlRecordRowList1 = foldlRecordRowList(dictFoldlRecord);
        return {
            hfoldl: function (f) {
                return function (x) {
                    return foldlRecordRowList1(f)(x)(Type_Proxy["Proxy"].value);
                };
            }
        };
    };
};
var hfoldlRecordWithIndex = function () {
    return function (dictFoldlRecord) {
        var foldlRecordRowList1 = foldlRecordRowList(dictFoldlRecord);
        return {
            hfoldlWithIndex: function (f) {
                return function (x) {
                    return foldlRecordRowList1(f)(x)(Type_Proxy["Proxy"].value);
                };
            }
        };
    };
};
var foldlRecordNil = {
    foldlRecordRowList: function (v) {
        return function (x) {
            return function (v1) {
                return function (v2) {
                    return x;
                };
            };
        };
    }
};
var foldingWithIndex = function (dict) {
    return dict.foldingWithIndex;
};
var foldlRecordCons = function (dictIsSymbol) {
    var get = Record.get(dictIsSymbol)();
    return function () {
        return function (dictFoldingWithIndex) {
            var foldingWithIndex1 = foldingWithIndex(dictFoldingWithIndex);
            return function (dictFoldlRecord) {
                var foldlRecordRowList1 = foldlRecordRowList(dictFoldlRecord);
                return {
                    foldlRecordRowList: function (f) {
                        return function (x) {
                            return function (v) {
                                return function (r) {
                                    return foldlRecordRowList1(f)(foldingWithIndex1(f)(Type_Proxy["Proxy"].value)(x)(get(Type_Proxy["Proxy"].value)(r)))(Type_Proxy["Proxy"].value)(r);
                                };
                            };
                        };
                    }
                };
            };
        };
    };
};
var foldlVariantCons = function (dictIsSymbol) {
    var on2 = on(dictIsSymbol);
    return function () {
        return function (dictFoldingWithIndex) {
            var foldingWithIndex1 = foldingWithIndex(dictFoldingWithIndex);
            return function (dictFoldlVariant) {
                var foldlVariantRowList1 = foldlVariantRowList(dictFoldlVariant);
                return {
                    foldlVariantRowList: function (v) {
                        return function (f) {
                            return function (x) {
                                return on2(Type_Proxy["Proxy"].value)(foldingWithIndex1(f)(Type_Proxy["Proxy"].value)(x))(foldlVariantRowList1(Type_Proxy["Proxy"].value)(f)(x));
                            };
                        };
                    }
                };
            };
        };
    };
};
var foldlVariantFCons = function (dictIsSymbol) {
    var on2 = on1(dictIsSymbol);
    return function () {
        return function (dictFoldingWithIndex) {
            var foldingWithIndex1 = foldingWithIndex(dictFoldingWithIndex);
            return function (dictFoldlVariantF) {
                var foldlVariantFRowList1 = foldlVariantFRowList(dictFoldlVariantF);
                return {
                    foldlVariantFRowList: function (v) {
                        return function (f) {
                            return function (x) {
                                return on2(Type_Proxy["Proxy"].value)(foldingWithIndex1(f)(Type_Proxy["Proxy"].value)(x))(foldlVariantFRowList1(Type_Proxy["Proxy"].value)(f)(x));
                            };
                        };
                    }
                };
            };
        };
    };
};
var hfoldlWithIndexApp = function (dictFoldableWithIndex) {
    var foldlWithIndex = Data_FoldableWithIndex.foldlWithIndex(dictFoldableWithIndex);
    return function (dictFoldingWithIndex) {
        var foldingWithIndex1 = foldingWithIndex(dictFoldingWithIndex);
        return {
            hfoldlWithIndex: function (f) {
                return function (x) {
                    return function (v) {
                        return foldlWithIndex(foldingWithIndex1(f))(x)(v);
                    };
                };
            }
        };
    };
};
var hfoldlWithIndexRowListCons = function (dictFoldingWithIndex) {
    var foldingWithIndex1 = foldingWithIndex(dictFoldingWithIndex);
    return function (dictHFoldlWithIndex) {
        var hfoldlWithIndex1 = hfoldlWithIndex(dictHFoldlWithIndex);
        return {
            hfoldlWithIndex: function (f) {
                return function (x) {
                    return function (v) {
                        return hfoldlWithIndex1(f)(foldingWithIndex1(f)(Type_Proxy["Proxy"].value)(x)(Type_Proxy["Proxy"].value))(Type_Proxy["Proxy"].value);
                    };
                };
            }
        };
    };
};
var folding = function (dict) {
    return dict.folding;
};
var hfoldlApp = function (dictFoldable) {
    var foldl = Data_Foldable.foldl(dictFoldable);
    return function (dictFolding) {
        var folding1 = folding(dictFolding);
        return {
            hfoldl: function (f) {
                return function (x) {
                    return function (v) {
                        return foldl(folding1(f))(x)(v);
                    };
                };
            }
        };
    };
};
var hfoldlEither = function (dictFolding) {
    var folding1 = folding(dictFolding);
    return function (dictFolding1) {
        var folding2 = folding(dictFolding1);
        return {
            hfoldl: function (f) {
                return function (x) {
                    return function (v) {
                        if (v instanceof Data_Either.Left) {
                            return folding1(f)(x)(v.value0);
                        };
                        if (v instanceof Data_Either.Right) {
                            return folding2(f)(x)(v.value0);
                        };
                        throw new Error("Failed pattern match at Heterogeneous.Folding (line 146, column 16 - line 148, column 29): " + [ v.constructor.name ]);
                    };
                };
            }
        };
    };
};
var hfoldlTuple = function (dictFolding) {
    var folding1 = folding(dictFolding);
    return function (dictFolding1) {
        var folding2 = folding(dictFolding1);
        return {
            hfoldl: function (f) {
                return function (x) {
                    return function (v) {
                        return folding2(f)(folding1(f)(x)(v.value0))(v.value1);
                    };
                };
            }
        };
    };
};
var constFolding = function (dictFolding) {
    var folding1 = folding(dictFolding);
    return {
        foldingWithIndex: function (v) {
            return function (v1) {
                return folding1(v);
            };
        }
    };
};
export {
    folding,
    foldingWithIndex,
    foldlRecordRowList,
    foldlVariantFRowList,
    foldlVariantRowList,
    hfoldl,
    hfoldlWithIndex,
    ConstFolding,
    functionFolding,
    functionFoldingWithIndex,
    constFolding,
    hfoldlApp,
    hfoldlWithIndexApp,
    hfoldlRowList,
    hfoldlWithIndexRowListCons,
    hfoldlWithIndexRowListNil,
    hfoldlRecord,
    hfoldlRecordWithIndex,
    foldlRecordCons,
    foldlRecordNil,
    hfoldlTuple,
    hfoldlEither,
    hfoldlVariant,
    hfoldlVariantWithIndex,
    foldlVariantCons,
    foldlVariantNil,
    hfoldlVariantF,
    hfoldlVariantFWithIndex,
    foldlVariantFCons,
    foldlVariantFNil
};