// Generated by purs version 0.15.4
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Dodo from "../Dodo/index.js";
import * as Dodo_Common from "../Dodo.Common/index.js";
import * as Dodo_Internal from "../Dodo.Internal/index.js";
import * as Language_PS_CST_Printers_PrintImports from "../Language.PS.CST.Printers.PrintImports/index.js";
import * as Language_PS_CST_Printers_PrintModuleModuleNameAndExports from "../Language.PS.CST.Printers.PrintModuleModuleNameAndExports/index.js";
import * as Language_PS_CST_Printers_TypeLevel from "../Language.PS.CST.Printers.TypeLevel/index.js";
import * as Language_PS_CST_Printers_Utils from "../Language.PS.CST.Printers.Utils/index.js";
import * as Language_PS_CST_ReservedNames from "../Language.PS.CST.ReservedNames/index.js";
import * as Language_PS_CST_Sugar_QualifiedName from "../Language.PS.CST.Sugar.QualifiedName/index.js";
import * as Language_PS_CST_Types_Declaration from "../Language.PS.CST.Types.Declaration/index.js";
import * as Language_PS_CST_Types_Leafs from "../Language.PS.CST.Types.Leafs/index.js";
import * as Language_PS_CST_Types_QualifiedName from "../Language.PS.CST.Types.QualifiedName/index.js";
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
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var append = /* #__PURE__ */ Data_Semigroup.append(Dodo_Internal.semigroupDoc);
var lines = /* #__PURE__ */ Dodo.lines(Data_Foldable.foldableArray);
var mapFlipped = /* #__PURE__ */ Data_Functor.mapFlipped(Data_Functor.functorArray);
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Dodo_Internal.monoidDoc);
var paragraph = /* #__PURE__ */ Dodo.paragraph(Data_Foldable.foldableArray);
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showBoolean);
var show1 = /* #__PURE__ */ Data_Show.show(Data_Show.showChar);
var show2 = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var show3 = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var show4 = /* #__PURE__ */ Data_Show.show(Data_Show.showNumber);
var foldWithSeparator = /* #__PURE__ */ Dodo.foldWithSeparator(Data_Foldable.foldableArray);
var $$null = /* #__PURE__ */ Data_Foldable["null"](Data_Foldable.foldableArray);
var foldWithSeparator1 = /* #__PURE__ */ Dodo.foldWithSeparator(Data_Array_NonEmpty_Internal.foldableNonEmptyArray);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Array_NonEmpty_Internal.functorNonEmptyArray);
var unwrapText = /* #__PURE__ */ Language_PS_CST_Printers_Utils.unwrapText();
var printAndConditionallyAddNewlinesBetween = /* #__PURE__ */ Language_PS_CST_Printers_Utils.printAndConditionallyAddNewlinesBetween(Data_Foldable.foldableArray);
var paragraph1 = /* #__PURE__ */ Dodo.paragraph(Data_Array_NonEmpty_Internal.foldableNonEmptyArray);
var any = /* #__PURE__ */ Data_Foldable.any(Data_Array_NonEmpty_Internal.foldableNonEmptyArray)(Data_HeytingAlgebra.heytingAlgebraBoolean);
var append1 = /* #__PURE__ */ Data_Semigroup.append(Data_Semigroup.semigroupArray);
var lines1 = /* #__PURE__ */ Dodo.lines(Data_Array_NonEmpty_Internal.foldableNonEmptyArray);
var printAndConditionallyAddNewlinesBetween1 = /* #__PURE__ */ Language_PS_CST_Printers_Utils.printAndConditionallyAddNewlinesBetween(Data_Array_NonEmpty_Internal.foldableNonEmptyArray);
var words = /* #__PURE__ */ Dodo.words(Data_Foldable.foldableArray);
var foldMap = /* #__PURE__ */ Data_Foldable.foldMap(Data_Foldable.foldableMaybe)(Dodo_Internal.monoidDoc);
var foldl = /* #__PURE__ */ Data_Foldable.foldl(Data_Foldable.foldableArray);
var map2 = /* #__PURE__ */ Data_Functor.map(Language_PS_CST_Types_QualifiedName.functorQualifiedName);
var foldl1 = /* #__PURE__ */ Data_Foldable.foldl(Data_Array_NonEmpty_Internal.foldableNonEmptyArray);
var fold = /* #__PURE__ */ Data_Foldable.fold(Data_Foldable.foldableMaybe)(Dodo_Internal.monoidDoc);
var map3 = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var guard = /* #__PURE__ */ Data_Monoid.guard(Dodo_Internal.monoidDoc);
var printRecordLabeled = function (v) {
    return function (v1) {
        if (v1 instanceof Language_PS_CST_Types_Leafs.RecordPun) {
            return Dodo.text(Language_PS_CST_ReservedNames.quoteIfReserved(unwrap(v1.value0)));
        };
        if (v1 instanceof Language_PS_CST_Types_Leafs.RecordField) {
            return Dodo.appendSpace(append(Language_PS_CST_Printers_Utils.dquotesIf(Language_PS_CST_Printers_Utils.labelNeedsQuotes(v1.value0))(Dodo.text(unwrap(v1.value0))))(Dodo.text(":")))(v(v1.value1));
        };
        throw new Error("Failed pattern match at Language.PS.CST.Printers (line 215, column 1 - line 215, column 75): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var printComments = function (v) {
    if (v instanceof Language_PS_CST_Types_Leafs.OneLineComments) {
        return lines(mapFlipped(v.value0)(function (x) {
            return Dodo.appendSpace(Dodo.text("-- |"))(Dodo.text(x));
        }));
    };
    if (v instanceof Language_PS_CST_Types_Leafs.BlockComments) {
        return append(Dodo.text("{-"))(append(Dodo["break"])(append(Dodo.indent(lines(map(Dodo.text)(v.value0))))(append(Dodo["break"])(Dodo.text("-}")))));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers (line 37, column 1 - line 37, column 38): " + [ v.constructor.name ]);
};
var printMaybeComments = function (comments) {
    return function (doc) {
        return lines([ Data_Maybe.maybe(mempty)(printComments)(comments), doc ]);
    };
};
var printBinder = function (v) {
    if (v instanceof Language_PS_CST_Types_Declaration.BinderWildcard) {
        return Dodo.text("_");
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderVar) {
        return Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0)));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderNamed) {
        return append(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0.ident))))(append(Dodo.text("@"))(Language_PS_CST_Printers_Utils.parens(printBinder(v.value0.binder))));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderConstructor && v.value0.args.length === 0) {
        return Language_PS_CST_Printers_TypeLevel.printQualifiedName_AnyProperNameType(v.value0.name);
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderConstructor) {
        return Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printQualifiedName_AnyProperNameType(v.value0.name))(paragraph(map(printBinder)(v.value0.args)));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderBoolean) {
        return Dodo.text(show(v.value0));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderChar) {
        return Dodo.text(show1(v.value0));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderString) {
        return Dodo.text(show2(v.value0));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderNumber && v.value0 instanceof Data_Either.Left) {
        return Dodo.text(show3(v.value0.value0));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderNumber && v.value0 instanceof Data_Either.Right) {
        return Dodo.text(show4(v.value0.value0));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderArray) {
        return append(Dodo.text("["))(append(foldWithSeparator(Dodo.text(", "))(map(printBinder)(v.value0)))(Dodo.text("]")));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderRecord) {
        return foldWithSeparator(Dodo.text(", "))(map(printRecordLabeled(printBinder))(v.value0));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderTyped) {
        return Dodo.appendSpace(printBinder(v.value0))(Dodo.appendSpace(Dodo.text("::"))(Language_PS_CST_Printers_TypeLevel.printType(v.value1)));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.BinderOp) {
        return Dodo.appendSpace(printBinder(v.value0))(Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printQualifiedName_AnyOpNameType(v.value1))(printBinder(v.value2)));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers (line 199, column 1 - line 199, column 34): " + [ v.constructor.name ]);
};
var printValueBindingFields = /* #__PURE__ */ (function () {
    var printBinderWithMaybeParens = function (v) {
        if (v instanceof Language_PS_CST_Types_Declaration.BinderConstructor && v.value0.args.length === 0) {
            return printBinder(v);
        };
        if (v instanceof Language_PS_CST_Types_Declaration.BinderConstructor) {
            return Language_PS_CST_Printers_Utils.parens(printBinder(v));
        };
        return printBinder(v);
    };
    return function (v) {
        var printedBinders = (function () {
            var $95 = $$null(v.binders);
            if ($95) {
                return mempty;
            };
            return append(paragraph(map(printBinderWithMaybeParens)(v.binders)))(Dodo.space);
        })();
        var printedHead = Dodo.appendSpace(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.name))))(append(printedBinders)(Dodo.text("=")));
        return printGuarded(printedHead)(v.guarded);
    };
})();
var printRecordUpdates = function (recordUpdates) {
    return Dodo.appendSpace(Dodo.text("{"))(Dodo.appendSpace(foldWithSeparator1(Dodo.text(","))(map1(printRecordUpdate)(recordUpdates)))(Dodo.text("}")));
};
var printRecordUpdate = function (v) {
    if (v instanceof Language_PS_CST_Types_Declaration.RecordUpdateLeaf) {
        return Dodo.appendSpace(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))))(Dodo.appendSpace(Dodo.text("="))($lazy_printExpr(327)(v.value1)));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.RecordUpdateBranch) {
        return Dodo.appendSpace(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))))(Dodo.appendSpace(Dodo.text("="))(printRecordUpdates(v.value1)));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers (line 326, column 1 - line 326, column 46): " + [ v.constructor.name ]);
};
var printLetBinding = function (v) {
    if (v instanceof Language_PS_CST_Types_Declaration.LetBindingSignature) {
        return Language_PS_CST_Printers_Utils.printLabelledGroup(unwrapText(v.value0.ident))(Language_PS_CST_Printers_TypeLevel.printType(v.value0.type_));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.LetBindingName) {
        return printValueBindingFields(v.value0);
    };
    if (v instanceof Language_PS_CST_Types_Declaration.LetBindingPattern) {
        return append(printBinder(v.value0.binder))(append(Dodo["break"])(append($lazy_printExpr(321)(v.value0.where_.expr))(append(Dodo.spaceBreak)(append(Dodo.text("where"))(append(Dodo.spaceBreak)(paragraph(map(printLetBinding)(v.value0.where_.whereBindings))))))));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers (line 316, column 1 - line 316, column 42): " + [ v.constructor.name ]);
};
var printGuarded = function (printedHead) {
    return function (guarded) {
        if (guarded instanceof Language_PS_CST_Types_Declaration.Unconditional) {
            if (guarded.value0.whereBindings.length === 0) {
                var $116 = Language_PS_CST_Printers_Utils.exprShouldBeOnNextLine(guarded.value0.expr);
                if ($116) {
                    return append(printedHead)(append(Dodo.spaceBreak)(Dodo.indent($lazy_printExpr(188)(guarded.value0.expr))));
                };
                return Dodo.appendSpace(printedHead)($lazy_printExpr(189)(guarded.value0.expr));
            };
            var printedBindings = Dodo.indent(append(Dodo.text("where"))(append(Dodo.spaceBreak)(printAndConditionallyAddNewlinesBetween(Language_PS_CST_Printers_Utils.shouldBeNoNewlineBetweenLetBindings)(printLetBinding)(guarded.value0.whereBindings))));
            var $119 = Language_PS_CST_Printers_Utils.exprShouldBeOnNextLine(guarded.value0.expr);
            if ($119) {
                return append(printedHead)(append(Dodo.spaceBreak)(append(Dodo.indent($lazy_printExpr(195)(guarded.value0.expr)))(append(Dodo.spaceBreak)(printedBindings))));
            };
            return Dodo.appendSpace(printedHead)(append($lazy_printExpr(196)(guarded.value0.expr))(append(Dodo.spaceBreak)(printedBindings)));
        };
        if (guarded instanceof Language_PS_CST_Types_Declaration.Guarded) {
            return mempty;
        };
        throw new Error("Failed pattern match at Language.PS.CST.Printers (line 183, column 3 - line 197, column 26): " + [ guarded.constructor.name ]);
    };
};
var printBranch = function (v) {
    var printedHead = Dodo.appendSpace(foldWithSeparator1(Dodo.text(", "))(map1(printBinder)(v.binders)))(Dodo.text("->"));
    return printGuarded(printedHead)(v.body);
};
var $lazy_printExpr = /* #__PURE__ */ $runtime_lazy("printExpr", "Language.PS.CST.Printers", function () {
    var processTopLevel = function (printExprImplementation$prime) {
        return function (expr) {
            if (expr instanceof Language_PS_CST_Types_Declaration.ExprApp) {
                return Dodo.flexGroup(printExprImplementation$prime(expr));
            };
            if (expr instanceof Language_PS_CST_Types_Declaration.ExprArray) {
                return Dodo.flexGroup(printExprImplementation$prime(expr));
            };
            if (expr instanceof Language_PS_CST_Types_Declaration.ExprRecord) {
                return Dodo.flexGroup(printExprImplementation$prime(expr));
            };
            return printExprImplementation$prime(expr);
        };
    };
    var printExprImplementation = function (v) {
        if (v instanceof Language_PS_CST_Types_Declaration.ExprHole) {
            return append(Dodo.text("?"))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprSection) {
            return Dodo.text("_");
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprIdent) {
            return Language_PS_CST_Printers_TypeLevel.printQualifiedName_Ident(v.value0);
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprConstructor) {
            return Language_PS_CST_Printers_TypeLevel.printQualifiedName_AnyProperNameType(v.value0);
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprBoolean) {
            return Dodo.text(show(v.value0));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprChar) {
            return Dodo.text(show1(v.value0));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprString) {
            return Dodo.text(show2(v.value0));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprNumber && v.value0 instanceof Data_Either.Left) {
            return Dodo.text(show3(v.value0.value0));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprNumber && v.value0 instanceof Data_Either.Right) {
            return Dodo.text(show4(v.value0.value0));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprArray) {
            return Dodo.alignCurrentColumn(Dodo_Common.pursSquares(foldWithSeparator(Dodo_Common.leadingComma)(map(processTopLevel(printExprImplementation))(v.value0))));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprRecord) {
            return Dodo.alignCurrentColumn(Dodo_Common.pursCurlies(foldWithSeparator(Dodo_Common.leadingComma)(map(printRecordLabeled(printExprImplementation))(v.value0))));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprTyped) {
            return Dodo.appendSpace(printExprImplementation(v.value0))(Dodo.appendSpace(Dodo.text("::"))(Language_PS_CST_Printers_TypeLevel.printType(v.value1)));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprInfix) {
            return Dodo.appendSpace(printExprImplementation(v.value0))(Dodo.appendSpace(printExprImplementation(v.value1))(printExprImplementation(v.value2)));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprOp) {
            return Dodo.appendSpace(printExprImplementation(v.value0))(Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printQualifiedName_AnyOpNameType(v.value1))(printExprImplementation(v.value2)));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprOpName) {
            return Language_PS_CST_Printers_TypeLevel.printQualifiedName_AnyOpNameType(v.value0);
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprNegate) {
            return append(Dodo.text("-"))(printExprImplementation(v.value0));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprRecordAccessor) {
            return append(printExprImplementation(v.value0.recExpr))(append(Dodo.text("."))(foldWithSeparator1(Language_PS_CST_Printers_Utils.dot)(map1(function (l) {
                return Language_PS_CST_Printers_Utils.dquotesIf(Language_PS_CST_Printers_Utils.labelNeedsQuotes(l))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(l))));
            })(v.value0.recPath))));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprRecordUpdate) {
            return Language_PS_CST_Printers_Utils.parens(Dodo.appendSpace(printExprImplementation(v.value0))(printRecordUpdates(v.value1)));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprApp) {
            var doWrapRight = (function () {
                if (v.value1 instanceof Language_PS_CST_Types_Declaration.ExprApp) {
                    return true;
                };
                if (v.value1 instanceof Language_PS_CST_Types_Declaration.ExprInfix) {
                    return true;
                };
                if (v.value1 instanceof Language_PS_CST_Types_Declaration.ExprOp) {
                    return true;
                };
                return false;
            })();
            return Dodo.alignCurrentColumn(foldWithSeparator(Dodo.spaceBreak)([ printExprImplementation(v.value0), Language_PS_CST_Printers_Utils.maybeWrapInParentheses(doWrapRight)(printExprImplementation(v.value1)) ]));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprLambda) {
            return Dodo.appendSpace(Language_PS_CST_Printers_Utils.parens(paragraph1(map1(printBinder)(v.value0.binders))))(Dodo.appendSpace(Dodo.text("="))(printExprImplementation(v.value0.body)));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprIf) {
            return foldWithSeparator(Dodo.spaceBreak)([ (function () {
                var $174 = Language_PS_CST_Printers_Utils.exprShouldBeOnNextLine(v.value0.cond);
                if ($174) {
                    return append(Dodo.text("if"))(append(Dodo["break"])(Dodo.indent(printExprImplementation(v.value0.cond))));
                };
                return Dodo.appendSpace(Dodo.text("if"))(printExprImplementation(v.value0.cond));
            })(), (function () {
                var $175 = Language_PS_CST_Printers_Utils.exprShouldBeOnNextLine(v.value0.true_);
                if ($175) {
                    return Dodo.indent(append(Dodo.text("then"))(append(Dodo["break"])(Dodo.indent(printExprImplementation(v.value0.true_)))));
                };
                return Dodo.indent(Dodo.appendSpace(Dodo.text("then"))(printExprImplementation(v.value0.true_)));
            })(), (function () {
                var $176 = Language_PS_CST_Printers_Utils.exprShouldBeOnNextLine(v.value0.false_);
                if ($176) {
                    return Dodo.indent(append(Dodo.text("else"))(append(Dodo["break"])(Dodo.indent(printExprImplementation(v.value0.false_)))));
                };
                return Dodo.indent(Dodo.appendSpace(Dodo.text("else"))(printExprImplementation(v.value0.false_)));
            })() ]);
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprCase) {
            var headShouldBeMultiline = Data_Function.flip(any)(v.value0.head)(function (v1) {
                if (v1 instanceof Language_PS_CST_Types_Declaration.ExprIf) {
                    return true;
                };
                if (v1 instanceof Language_PS_CST_Types_Declaration.ExprCase) {
                    return true;
                };
                if (v1 instanceof Language_PS_CST_Types_Declaration.ExprLet) {
                    return true;
                };
                if (v1 instanceof Language_PS_CST_Types_Declaration.ExprDo) {
                    return true;
                };
                if (v1 instanceof Language_PS_CST_Types_Declaration.ExprAdo) {
                    return true;
                };
                return false;
            });
            var headDocs = Data_Array_NonEmpty.toArray(map1(printExprImplementation)(v.value0.head));
            if (headShouldBeMultiline) {
                return foldWithSeparator(Dodo.spaceBreak)([ Dodo.text("case"), foldWithSeparator(Dodo.softBreak)(Data_Array.zipWith(append)(append1([ Dodo.text("  ") ])(Data_Array.replicate(Data_Array.length(headDocs) - 1 | 0)(Dodo.text(", "))))(map(Dodo.alignCurrentColumn)(headDocs))), Dodo.text("of"), Dodo.indent(lines1(map1(printBranch)(v.value0.branches))) ]);
            };
            return Dodo.appendSpace(Dodo.text("case"))(Dodo.appendSpace(foldWithSeparator(Dodo.text(", "))(headDocs))(append(Dodo.text("of"))(append(Dodo["break"])(Dodo.indent(lines1(map1(printBranch)(v.value0.branches)))))));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprLet) {
            return Dodo.alignCurrentColumn(foldWithSeparator(Dodo["break"])([ Dodo.text("let"), Dodo.indent(printAndConditionallyAddNewlinesBetween1(Language_PS_CST_Printers_Utils.shouldBeNoNewlineBetweenLetBindings)(printLetBinding)(v.value0.bindings)), Dodo.text(" in"), Dodo.indent(processTopLevel(printExprImplementation)(v.value0.body)) ]));
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprDo) {
            return mempty;
        };
        if (v instanceof Language_PS_CST_Types_Declaration.ExprAdo) {
            return mempty;
        };
        throw new Error("Failed pattern match at Language.PS.CST.Printers (line 230, column 5 - line 230, column 112): " + [ v.constructor.name ]);
    };
    return processTopLevel(printExprImplementation);
});
var printExpr = /* #__PURE__ */ $lazy_printExpr(220);
var printInstanceBinding = function (v) {
    if (v instanceof Language_PS_CST_Types_Declaration.InstanceBindingSignature) {
        return Language_PS_CST_Printers_Utils.printLabelledGroup(unwrapText(v.value0.ident))(Language_PS_CST_Printers_TypeLevel.printType(v.value0.type_));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.InstanceBindingName) {
        return printValueBindingFields(v.value0);
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers (line 160, column 1 - line 160, column 52): " + [ v.constructor.name ]);
};
var printAssignmentDecl = function (reservedWord) {
    return function (v) {
        return function (types) {
            var sep = append(Dodo.spaceBreak)(Dodo.text("| "));
            var printCtors = function (types$prime) {
                return append(Dodo.spaceBreak)(Dodo.indent(append(Dodo.text("= "))(foldWithSeparator1(sep)(map1((function () {
                    var $292 = Language_PS_CST_Printers_TypeLevel["printType$prime"](true);
                    return function ($293) {
                        return Dodo.flexGroup($292($293));
                    };
                })())(types$prime)))));
            };
            return Dodo.flexGroup(Dodo.appendSpace(Dodo.text(reservedWord))(append(Dodo.appendSpace(unwrapText(v.dataHdName))(words(map(Language_PS_CST_Printers_TypeLevel.printTypeVarBinding)(v.dataHdVars))))(foldMap(printCtors)(Data_Array_NonEmpty.fromArray(types)))));
        };
    };
};
var coerceProperName = function (v) {
    return v;
};
var dataCtorToType = function (v) {
    var initType = new Language_PS_CST_Types_Declaration.TypeConstructor(Language_PS_CST_Sugar_QualifiedName.nonQualifiedName(coerceProperName(v.dataCtorName)));
    return foldl(Language_PS_CST_Types_Declaration.TypeApp.create)(initType)(v.dataCtorFields);
};
var instanceHeadToType = function (inst) {
    var initType = new Language_PS_CST_Types_Declaration.TypeConstructor(map2(coerceProperName)(inst.instClass));
    return foldl1(Language_PS_CST_Types_Declaration.TypeApp.create)(initType)(inst.instTypes);
};
var printInstance = function (instance_) {
    var where_ = (function () {
        var $210 = $$null(instance_.body);
        if ($210) {
            return mempty;
        };
        return Dodo.text("where");
    })();
    var printedBody = printAndConditionallyAddNewlinesBetween(Language_PS_CST_Printers_Utils.shouldBeNoNewlineBetweenInstanceBindings)(function ($294) {
        return Dodo.indent(printInstanceBinding($294));
    })(instance_.body);
    return Dodo.appendBreak(Language_PS_CST_Printers_Utils.printLabelledGroup(Dodo.appendSpace(Dodo.text("instance"))(unwrapText(instance_.head.instName)))(Dodo.flexGroup(Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printType(instanceHeadToType(instance_.head)))(where_))))(printedBody);
};
var printDeclaration = function (v) {
    if (v instanceof Language_PS_CST_Types_Declaration.DeclData) {
        return printMaybeComments(v.value0.comments)(printAssignmentDecl("data")(v.value0.head)(map(dataCtorToType)(v.value0.constructors)));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclType) {
        return printMaybeComments(v.value0.comments)(printAssignmentDecl("type")(v.value0.head)([ v.value0.type_ ]));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclNewtype) {
        var dataCtor = {
            dataCtorName: v.value0.name,
            dataCtorFields: [ v.value0.type_ ]
        };
        return printMaybeComments(v.value0.comments)(printAssignmentDecl("newtype")(v.value0.head)([ dataCtorToType(dataCtor) ]));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclFixity) {
        var printFixityOp = function (v1) {
            if (v1 instanceof Language_PS_CST_Types_Declaration.FixityValue && v1.value0 instanceof Data_Either.Left) {
                return Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printQualifiedName_Ident(v1.value0.value0))(Dodo.appendSpace(Dodo.text("as"))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v1.value1)))));
            };
            if (v1 instanceof Language_PS_CST_Types_Declaration.FixityValue && v1.value0 instanceof Data_Either.Right) {
                return Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printQualifiedName_AnyProperNameType(v1.value0.value0))(Dodo.appendSpace(Dodo.text("as"))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v1.value1)))));
            };
            if (v1 instanceof Language_PS_CST_Types_Declaration.FixityType) {
                return Dodo.appendSpace(Dodo.text("type"))(Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printQualifiedName_AnyProperNameType(v1.value0))(Dodo.appendSpace(Dodo.text("as"))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v1.value1))))));
            };
            throw new Error("Failed pattern match at Language.PS.CST.Printers (line 66, column 5 - line 66, column 42): " + [ v1.constructor.name ]);
        };
        return printMaybeComments(v.value0.comments)(Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printFixity(v.value0.fixityFields.keyword))(Dodo.appendSpace(Dodo.text(show3(v.value0.fixityFields.precedence)))(printFixityOp(v.value0.fixityFields.operator))));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclForeign) {
        return printMaybeComments(v.value0.comments)((function () {
            if (v.value0.foreign_ instanceof Language_PS_CST_Types_Declaration.ForeignValue) {
                return Language_PS_CST_Printers_Utils.printLabelledGroup(Dodo.appendSpace(Dodo.text("foreign import"))(unwrapText(v.value0.foreign_.value0.ident)))(Dodo.flexGroup(Language_PS_CST_Printers_TypeLevel.printType(v.value0.foreign_.value0.type_)));
            };
            if (v.value0.foreign_ instanceof Language_PS_CST_Types_Declaration.ForeignData) {
                return Language_PS_CST_Printers_Utils.printLabelledGroup(Dodo.appendSpace(Dodo.text("foreign import data"))(unwrapText(v.value0.foreign_.value0.name)))(Dodo.flexGroup(Language_PS_CST_Printers_TypeLevel["printType$prime"](false)(v.value0.foreign_.value0.kind_)));
            };
            if (v.value0.foreign_ instanceof Language_PS_CST_Types_Declaration.ForeignKind) {
                return Dodo.appendSpace(Dodo.text("data"))(unwrapText(v.value0.foreign_.value0.name));
            };
            throw new Error("Failed pattern match at Language.PS.CST.Printers (line 73, column 3 - line 83, column 38): " + [ v.value0.foreign_.constructor.name ]);
        })());
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclDerive) {
        var instConstraint = {
            className: v.value0.head.instClass,
            args: Data_Array_NonEmpty.toArray(v.value0.head.instTypes)
        };
        var deriveType$prime = (function () {
            if (v.value0.deriveType instanceof Language_PS_CST_Types_Leafs.DeclDeriveType_Newtype) {
                return Dodo.text("newtype");
            };
            if (v.value0.deriveType instanceof Language_PS_CST_Types_Leafs.DeclDeriveType_Ordinary) {
                return mempty;
            };
            throw new Error("Failed pattern match at Language.PS.CST.Printers (line 105, column 7 - line 107, column 42): " + [ v.value0.deriveType.constructor.name ]);
        })();
        var label = words([ Dodo.text("derive"), deriveType$prime, Dodo.text("instance"), unwrapText(v.value0.head.instName) ]);
        var constraints$prime = fold(map3(Language_PS_CST_Printers_TypeLevel.printConstraintList)(Data_Array_NonEmpty.fromArray(v.value0.head.instConstraints)));
        var arrow = guard(!$$null(v.value0.head.instConstraints))(Dodo.text("=>"));
        return printMaybeComments(v.value0.comments)(Dodo.flexGroup(Language_PS_CST_Printers_Utils.appendSpaceBreakNoGroup(label)(Language_PS_CST_Printers_Utils.appendSpaceBreakNoGroup(Dodo.indent(Dodo.appendSpace(Dodo.text("::"))(Dodo.flexGroup(constraints$prime))))(Dodo.indent(Dodo.appendSpace(arrow)(Dodo.flexGroup(Dodo.alignCurrentColumn(Language_PS_CST_Printers_TypeLevel.printConstraint(instConstraint)))))))));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclClass) {
        var printedHeader = Dodo.flexGroup(foldWithSeparator(Dodo.space)([ Dodo.text("class"), (function () {
            if (v["value0"]["head"]["super"].length === 0) {
                return mempty;
            };
            if (v["value0"]["head"]["super"].length === 1) {
                return Dodo.appendSpace(Language_PS_CST_Printers_TypeLevel.printConstraint(v["value0"]["head"]["super"][0]))(Dodo.text("<="));
            };
            return Dodo.appendSpace(Dodo.alignCurrentColumn(Dodo_Common.pursParens(foldWithSeparator(Dodo_Common.leadingComma)(map(function ($295) {
                return Dodo.flexGroup(Language_PS_CST_Printers_TypeLevel.printConstraint($295));
            })(v["value0"]["head"]["super"])))))(Dodo.text("<="));
        })(), unwrapText(v.value0.head.name), (function () {
            if (v.value0.head.vars.length === 0) {
                return mempty;
            };
            return Dodo.alignCurrentColumn(Dodo.flexGroup(foldWithSeparator(Dodo.spaceBreak)(map(Language_PS_CST_Printers_TypeLevel.printTypeVarBinding)(v.value0.head.vars))));
        })(), (function () {
            if (v.value0.head.fundeps.length === 0) {
                return mempty;
            };
            return Dodo.appendSpace(Dodo.text("|"))(Dodo.alignCurrentColumn(Dodo.flexGroup(foldWithSeparator(Dodo.text(", "))(map(Language_PS_CST_Printers_TypeLevel.printFundep)(v.value0.head.fundeps)))));
        })() ]));
        var $265 = $$null(v.value0.methods);
        if ($265) {
            return printMaybeComments(v.value0.comments)(printedHeader);
        };
        return printMaybeComments(v.value0.comments)(Dodo.appendSpace(printedHeader)(append(Dodo.text("where"))(append(Dodo["break"])(Dodo.indent(paragraph(map(function (v1) {
            return Language_PS_CST_Printers_Utils.printLabelledGroup(unwrapText(v1.ident))(Language_PS_CST_Printers_TypeLevel.printType(v1.type_));
        })(v.value0.methods)))))));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclInstanceChain) {
        return printMaybeComments(v.value0.comments)(foldWithSeparator1(append(Dodo["break"])(append(Dodo["break"])(append(Dodo.text("else"))(append(Dodo["break"])(Dodo["break"])))))(map1(printInstance)(v.value0.instances)));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclSignature) {
        return printMaybeComments(v.value0.comments)(Language_PS_CST_Printers_Utils.printLabelledGroup(unwrapText(v.value0.ident))(Language_PS_CST_Printers_TypeLevel.printType(v.value0.type_)));
    };
    if (v instanceof Language_PS_CST_Types_Declaration.DeclValue) {
        return printMaybeComments(v.value0.comments)(printValueBindingFields(v.value0.valueBindingFields));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers (line 48, column 1 - line 48, column 44): " + [ v.constructor.name ]);
};
var printDeclarations = function (declarations) {
    return printAndConditionallyAddNewlinesBetween(Language_PS_CST_Printers_Utils.shouldBeNoNewlineBetweenDeclarations)(printDeclaration)(declarations);
};
var printModule = function (v) {
    return append(foldWithSeparator(append(Dodo["break"])(Dodo["break"]))([ Language_PS_CST_Printers_PrintModuleModuleNameAndExports.printModuleModuleNameAndExports(v.moduleName)(v.exports), Language_PS_CST_Printers_PrintImports.printImports(v.imports), printDeclarations(v.declarations) ]))(Dodo["break"]);
};
export {
    printModule,
    printDeclarations,
    printComments,
    printMaybeComments,
    printDeclaration,
    printInstance,
    printInstanceBinding,
    printValueBindingFields,
    printGuarded,
    printBinder,
    printRecordLabeled,
    printExpr,
    printBranch,
    printLetBinding,
    printRecordUpdates,
    printRecordUpdate,
    printAssignmentDecl,
    dataCtorToType,
    instanceHeadToType,
    coerceProperName
};
