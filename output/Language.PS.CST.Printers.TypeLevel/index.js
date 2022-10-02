// Generated by purs version 0.15.4
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Dodo from "../Dodo/index.js";
import * as Dodo_Common from "../Dodo.Common/index.js";
import * as Dodo_Internal from "../Dodo.Internal/index.js";
import * as Language_PS_CST_Printers_Utils from "../Language.PS.CST.Printers.Utils/index.js";
import * as Language_PS_CST_ReservedNames from "../Language.PS.CST.ReservedNames/index.js";
import * as Language_PS_CST_Types_Declaration from "../Language.PS.CST.Types.Declaration/index.js";
import * as Language_PS_CST_Types_Leafs from "../Language.PS.CST.Types.Leafs/index.js";
import * as Language_PS_CST_Types_QualifiedName from "../Language.PS.CST.Types.QualifiedName/index.js";
var append = /* #__PURE__ */ Data_Semigroup.append(Dodo_Internal.semigroupDoc);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Data_Monoid.monoidString);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var paragraph = /* #__PURE__ */ Dodo.paragraph(Data_Array_NonEmpty_Internal.foldableNonEmptyArray);
var map = /* #__PURE__ */ Data_Functor.map(Data_Array_NonEmpty_Internal.functorNonEmptyArray);
var unwrapText = /* #__PURE__ */ Language_PS_CST_Printers_Utils.unwrapText();
var guard = /* #__PURE__ */ Data_Monoid.guard(Dodo_Internal.monoidDoc);
var foldWithSeparator = /* #__PURE__ */ Dodo.foldWithSeparator(Data_Array_NonEmpty_Internal.foldableNonEmptyArray);
var eqQualifiedName = /* #__PURE__ */ Language_PS_CST_Types_QualifiedName.eqQualifiedName(Language_PS_CST_Types_Leafs.eqOpName);
var notEq = /* #__PURE__ */ Data_Eq.notEq(eqQualifiedName);
var eq = /* #__PURE__ */ Data_Eq.eq(eqQualifiedName);
var foldWithSeparator1 = /* #__PURE__ */ Dodo.foldWithSeparator(Data_Foldable.foldableArray);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var fold = /* #__PURE__ */ Data_Foldable.fold(Data_Foldable.foldableMaybe)(Dodo_Internal.monoidDoc);
var map2 = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var wrap = function (open) {
    return function (close) {
        return function (d) {
            var appendSoftSpace = Dodo_Internal.bothNotEmpty(function (a) {
                return function (b) {
                    return append(a)(append(Language_PS_CST_Printers_Utils.softSpace)(b));
                };
            });
            var appendSoftBreak = Dodo_Internal.bothNotEmpty(function (a) {
                return function (b) {
                    return append(a)(append(Dodo.softBreak)(b));
                };
            });
            return Dodo.flexGroup(Dodo.alignCurrentColumn(appendSoftBreak(appendSoftSpace(Dodo.text(open))(d))(Dodo.text(close))));
        };
    };
};
var wrapEmpty = /* #__PURE__ */ wrap(mempty)(mempty);
var printQualifiedName_Ident = function (v) {
    if (v.qualModule instanceof Data_Maybe.Nothing) {
        return Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.qualName)));
    };
    if (v.qualModule instanceof Data_Maybe.Just) {
        return append(Language_PS_CST_Printers_Utils.printModuleName(v.qualModule.value0))(append(Dodo.text("."))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.qualName)))));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers.TypeLevel (line 33, column 58 - line 35, column 137): " + [ v.qualModule.constructor.name ]);
};
var printQualifiedName_AnyProperNameType = function (v) {
    if (v.qualModule instanceof Data_Maybe.Nothing) {
        return Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.qualName)));
    };
    if (v.qualModule instanceof Data_Maybe.Just) {
        return append(Language_PS_CST_Printers_Utils.printModuleName(v.qualModule.value0))(append(Dodo.text("."))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.qualName)))));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers.TypeLevel (line 38, column 70 - line 40, column 137): " + [ v.qualModule.constructor.name ]);
};
var printQualifiedName_AnyOpNameType = function (v) {
    if (v.qualModule instanceof Data_Maybe.Nothing) {
        return Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.qualName)));
    };
    if (v.qualModule instanceof Data_Maybe.Just) {
        return append(Language_PS_CST_Printers_Utils.printModuleName(v.qualModule.value0))(append(Dodo.text("."))(Language_PS_CST_Printers_Utils.parens(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.qualName))))));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers.TypeLevel (line 43, column 66 - line 45, column 146): " + [ v.qualModule.constructor.name ]);
};
var printFundep = function (v) {
    return Dodo.appendSpace(paragraph(map(function ($144) {
        return Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap($144)));
    })(v.value0)))(Dodo.appendSpace(Dodo.text("->"))(paragraph(map(unwrapText)(v.value1))));
};
var printFixity = function (v) {
    if (v instanceof Language_PS_CST_Types_Leafs.Infix) {
        return Dodo.text("infix");
    };
    if (v instanceof Language_PS_CST_Types_Leafs.Infixl) {
        return Dodo.text("infixl");
    };
    if (v instanceof Language_PS_CST_Types_Leafs.Infixr) {
        return Dodo.text("infixr");
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers.TypeLevel (line 23, column 1 - line 23, column 34): " + [ v.constructor.name ]);
};
var paren = /* #__PURE__ */ wrap("(")(")");
var parenIf = function (v) {
    if (v) {
        return paren;
    };
    if (!v) {
        return wrapEmpty;
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers.TypeLevel (line 192, column 1 - line 192, column 43): " + [ v.constructor.name ]);
};
var printTypeVarBinding = function (v) {
    if (v instanceof Language_PS_CST_Types_Declaration.TypeVarName) {
        return Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0)));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.TypeVarKinded) {
        return Language_PS_CST_Printers_Utils.parens(Dodo.appendSpace(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))))(Dodo.appendSpace(Dodo.text("::"))(printType$prime(false)(v.value1))));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers.TypeLevel (line 28, column 1 - line 28, column 50): " + [ v.constructor.name ]);
};
var printType$prime = function (v) {
    return function (v1) {
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeVar) {
            return unwrapText(v1.value0);
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeWildcard) {
            return Dodo.text("_");
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeConstructor) {
            return printQualifiedName_AnyProperNameType(v1.value0);
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeHole) {
            return append(Dodo.text("?"))(unwrapText(v1.value0));
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeString) {
            return append(Dodo.text("\""))(append(Dodo.text(v1.value0))(Dodo.text("\"")));
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeForall) {
            return append(Dodo.appendSpace(Dodo.text("forall"))(Dodo.flexGroup(Dodo.alignCurrentColumn(paragraph(map(printTypeVarBinding)(v1.value0))))))(append(Dodo.softBreak)(append(Language_PS_CST_Printers_Utils.softSpace)(append(Dodo.text(". "))(printType$prime(false)(v1.value1)))));
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeKinded) {
            return Language_PS_CST_Printers_Utils.printLabelledGroup(printType$prime(false)(v1.value0))(printType$prime(false)(v1.value1));
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeRow) {
            return printRow("(")(")")(v1.value0);
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeRecord) {
            return printRow("{")("}")(v1.value0);
        };
        var isWrapped = v;
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeApp) {
            var sep = Dodo.flexAlt(Dodo.space)(append(Dodo["break"])(append(Dodo.text("  "))(guard(!isWrapped)(Dodo.space))));
            var needsParen = function (v2) {
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeKinded) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeConstrained) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeArr) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeOp) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeForall) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeApp) {
                    return true;
                };
                return false;
            };
            var pp = function (t) {
                var isWrapped$prime = needsParen(t);
                return parenIf(isWrapped$prime)(printType$prime(isWrapped$prime)(t));
            };
            var collectApp = function ($copy_v2) {
                return function ($copy_acc) {
                    var $tco_var_v2 = $copy_v2;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(v2, acc) {
                        if (v2 instanceof Language_PS_CST_Types_Declaration.TypeApp) {
                            $tco_var_v2 = v2.value0;
                            $copy_acc = Data_Array_NonEmpty.cons(pp(v2.value1))(acc);
                            return;
                        };
                        $tco_done = true;
                        return Data_Array_NonEmpty.cons(pp(v2))(acc);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_v2, $copy_acc);
                    };
                    return $tco_result;
                };
            };
            var apps = collectApp(v1.value0)(Data_Array_NonEmpty.singleton(pp(v1.value1)));
            return foldWithSeparator(sep)(apps);
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeArr) {
            var sep = append(Dodo.spaceBreak)(Dodo.text("-> "));
            var needsParen = function (v2) {
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeKinded) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeConstrained) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeArr) {
                    return true;
                };
                return false;
            };
            var pp = function (t) {
                var isWrapped = needsParen(t);
                return parenIf(isWrapped)(printType$prime(isWrapped)(t));
            };
            var collectArr = function ($copy_v2) {
                return function ($copy_acc) {
                    var $tco_var_v2 = $copy_v2;
                    var $tco_done1 = false;
                    var $tco_result;
                    function $tco_loop(v2, acc) {
                        if (v2 instanceof Language_PS_CST_Types_Declaration.TypeArr) {
                            $tco_var_v2 = v2.value1;
                            $copy_acc = Data_Array_NonEmpty.snoc(acc)(pp(v2.value0));
                            return;
                        };
                        $tco_done1 = true;
                        return Data_Array_NonEmpty.snoc(acc)(pp(v2));
                    };
                    while (!$tco_done1) {
                        $tco_result = $tco_loop($tco_var_v2, $copy_acc);
                    };
                    return $tco_result;
                };
            };
            var arrs = collectArr(v1.value1)(Data_Array_NonEmpty.singleton(pp(v1.value0)));
            return foldWithSeparator(sep)(arrs);
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeOp) {
            var opDoc = printQualifiedName_AnyOpNameType(v1.value1);
            var sep = append(Dodo.spaceBreak)(Dodo.indent(append(opDoc)(Dodo.space)));
            var needsParen = function (v2) {
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeKinded) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeConstrained) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeArr) {
                    return true;
                };
                if (v2 instanceof Language_PS_CST_Types_Declaration.TypeOp) {
                    return notEq(v2.value1)(v1.value1);
                };
                return false;
            };
            var pp = function (t) {
                var isWrapped = needsParen(t);
                return parenIf(isWrapped)(printType$prime(isWrapped)(t));
            };
            var collectArr = function ($copy_v2) {
                return function ($copy_acc) {
                    var $tco_var_v2 = $copy_v2;
                    var $tco_done2 = false;
                    var $tco_result;
                    function $tco_loop(v2, acc) {
                        if (v2 instanceof Language_PS_CST_Types_Declaration.TypeOp && eq(v1.value1)(v2.value1)) {
                            $tco_var_v2 = v2.value2;
                            $copy_acc = Data_Array_NonEmpty.snoc(acc)(pp(v2.value0));
                            return;
                        };
                        $tco_done2 = true;
                        return Data_Array_NonEmpty.snoc(acc)(pp(v2));
                    };
                    while (!$tco_done2) {
                        $tco_result = $tco_loop($tco_var_v2, $copy_acc);
                    };
                    return $tco_result;
                };
            };
            var arrs = collectArr(v1.value2)(Data_Array_NonEmpty.singleton(pp(v1.value0)));
            return Dodo.flexGroup(foldWithSeparator(sep)(arrs));
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeConstrained) {
            return append(printConstraint(v1.value0))(append(Dodo.spaceBreak)(append(Dodo.text("=> "))(printType$prime(false)(v1.value1))));
        };
        throw new Error("Failed pattern match at Language.PS.CST.Printers.TypeLevel (line 50, column 1 - line 50, column 44): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var printRow = function (open) {
    return function (close) {
        return function (v) {
            var printTl = function (t) {
                return append(Dodo.text("| "))(printType$prime(false)(t));
            };
            var printE = function (v1) {
                return Language_PS_CST_Printers_Utils.printLabelledGroup(Language_PS_CST_Printers_Utils.dquotesIf(Language_PS_CST_Printers_Utils.labelNeedsQuotes(v1.label))(Dodo.text(v1.label)))(printType$prime(false)(v1.type_));
            };
            return Dodo.alignCurrentColumn(Dodo.encloseEmptyAlt(append(Dodo.text(open))(Dodo.space))(append(Dodo.spaceBreak)(Dodo.text(close)))(Dodo.text(open + close))(Language_PS_CST_Printers_Utils.appendSpaceBreakNoGroup(foldWithSeparator1(Dodo_Common.leadingComma)(map1(printE)(v.rowLabels)))(fold(map2(printTl)(v.rowTail)))));
        };
    };
};
var printConstraint = function (v) {
    var needsParen = function (v1) {
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeKinded) {
            return true;
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeConstrained) {
            return true;
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeArr) {
            return true;
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeForall) {
            return true;
        };
        if (v1 instanceof Language_PS_CST_Types_Declaration.TypeApp) {
            return true;
        };
        return false;
    };
    var pp = function (t) {
        return parenIf(needsParen(t))(printType$prime(true)(t));
    };
    var apps = Data_Array.cons(printQualifiedName_AnyProperNameType(v.className))(map1(pp)(v.args));
    return foldWithSeparator1(Dodo.spaceBreak)(apps);
};
var printConstraintList = function (cs) {
    return parenIf(Data_Array_NonEmpty.length(cs) > 1)(foldWithSeparator(Dodo_Common.leadingComma)(map(function ($145) {
        return Dodo.flexGroup(printConstraint($145));
    })(cs)));
};
var printType = /* #__PURE__ */ printType$prime(false);
var curlyBraces = /* #__PURE__ */ wrap("{")("}");
export {
    printFundep,
    printFixity,
    printTypeVarBinding,
    printQualifiedName_Ident,
    printQualifiedName_AnyProperNameType,
    printQualifiedName_AnyOpNameType,
    printType,
    printType$prime,
    printRow,
    printConstraint,
    printConstraintList,
    parenIf,
    wrapEmpty,
    paren,
    curlyBraces,
    wrap
};
