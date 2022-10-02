// Generated by purs version 0.15.4
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
import * as Language_PS_CST_Types_Module from "../Language.PS.CST.Types.Module/index.js";
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Dodo_Internal.monoidDoc);
var append = /* #__PURE__ */ Data_Semigroup.append(Dodo_Internal.semigroupDoc);
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var foldWithSeparator = /* #__PURE__ */ Dodo.foldWithSeparator(Data_Foldable.foldableArray);
var paragraph = /* #__PURE__ */ Dodo.paragraph(Data_Foldable.foldableArray);
var lines = /* #__PURE__ */ Dodo.lines(Data_Foldable.foldableArray);
var printImportName = function (v) {
    if (v instanceof Language_PS_CST_Types_Module.ImportValue) {
        return Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0)));
    };
    if (v instanceof Language_PS_CST_Types_Module.ImportOp) {
        return Language_PS_CST_Printers_Utils.parens(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))));
    };
    if (v instanceof Language_PS_CST_Types_Module.ImportType) {
        var printedProperNameTypeConstructor = Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0)));
        var printedMaybeDataMembers = (function () {
            if (v.value1 instanceof Data_Maybe.Nothing) {
                return mempty;
            };
            if (v.value1 instanceof Data_Maybe.Just && v.value1.value0 instanceof Language_PS_CST_Types_Module.DataAll) {
                return Dodo.text("(..)");
            };
            if (v.value1 instanceof Data_Maybe.Just && v.value1.value0 instanceof Language_PS_CST_Types_Module.DataEnumerated) {
                return Language_PS_CST_Printers_Utils.parens(Language_PS_CST_Printers_Utils.printConstructors(v.value1.value0.value0));
            };
            throw new Error("Failed pattern match at Language.PS.CST.Printers.PrintImports (line 51, column 31 - line 54, column 86): " + [ v.value1.constructor.name ]);
        })();
        return append(printedProperNameTypeConstructor)(printedMaybeDataMembers);
    };
    if (v instanceof Language_PS_CST_Types_Module.ImportTypeOp) {
        return Dodo.appendSpace(Dodo.text("type"))(Language_PS_CST_Printers_Utils.parens(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0)))));
    };
    if (v instanceof Language_PS_CST_Types_Module.ImportClass) {
        return Dodo.appendSpace(Dodo.text("class"))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))));
    };
    if (v instanceof Language_PS_CST_Types_Module.ImportKind) {
        return Dodo.appendSpace(Dodo.text("kind"))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers.PrintImports (line 42, column 1 - line 42, column 38): " + [ v.constructor.name ]);
};
var printImport = function (v) {
    var qualification$prime = Data_Maybe.maybe(mempty)(function (qualificationModuleName) {
        return Dodo.appendSpace(Dodo.text(" as"))(Language_PS_CST_Printers_Utils.printModuleName(qualificationModuleName));
    })(v.qualification);
    var head = Dodo.appendSpace(Dodo.text("import"))(Language_PS_CST_Printers_Utils.printModuleName(v.moduleName));
    if (v.names.length === 0) {
        return append(head)(qualification$prime);
    };
    var exports = map(printImportName)(v.names);
    var exports$prime = Dodo_Common.pursParens(foldWithSeparator(Dodo_Common.leadingComma)(exports));
    return append(Dodo.flexGroup(paragraph([ head, Dodo.indent(Dodo.alignCurrentColumn(exports$prime)) ])))(qualification$prime);
};
var printImports = function (imports) {
    return lines(map(printImport)(imports));
};
export {
    printImports,
    printImport,
    printImportName
};
