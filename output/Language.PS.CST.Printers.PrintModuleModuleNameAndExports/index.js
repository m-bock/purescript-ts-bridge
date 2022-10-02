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
var foldWithSeparator = /* #__PURE__ */ Dodo.foldWithSeparator(Data_Foldable.foldableArray);
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var printExportName = function (v) {
    if (v instanceof Language_PS_CST_Types_Module.ExportValue) {
        return Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0)));
    };
    if (v instanceof Language_PS_CST_Types_Module.ExportOp) {
        return Language_PS_CST_Printers_Utils.parens(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))));
    };
    if (v instanceof Language_PS_CST_Types_Module.ExportType) {
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
            throw new Error("Failed pattern match at Language.PS.CST.Printers.PrintModuleModuleNameAndExports (line 31, column 31 - line 34, column 86): " + [ v.value1.constructor.name ]);
        })();
        return append(printedProperNameTypeConstructor)(printedMaybeDataMembers);
    };
    if (v instanceof Language_PS_CST_Types_Module.ExportTypeOp) {
        return Dodo.appendSpace(Dodo.text("type"))(Language_PS_CST_Printers_Utils.parens(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0)))));
    };
    if (v instanceof Language_PS_CST_Types_Module.ExportClass) {
        return Dodo.appendSpace(Dodo.text("class"))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))));
    };
    if (v instanceof Language_PS_CST_Types_Module.ExportKind) {
        return Dodo.appendSpace(Dodo.text("kind"))(Dodo.text(Language_PS_CST_ReservedNames.appendUnderscoreIfReserved(unwrap(v.value0))));
    };
    if (v instanceof Language_PS_CST_Types_Module.ExportModule) {
        return Dodo.appendSpace(Dodo.text("module"))(Language_PS_CST_Printers_Utils.printModuleName(v.value0));
    };
    throw new Error("Failed pattern match at Language.PS.CST.Printers.PrintModuleModuleNameAndExports (line 23, column 1 - line 23, column 38): " + [ v.constructor.name ]);
};
var printModuleModuleNameAndExports = function (moduleName) {
    return function (v) {
        if (v.length === 0) {
            return Dodo.appendSpace(Dodo.text("module"))(Dodo.appendSpace(Language_PS_CST_Printers_Utils.printModuleName(moduleName))(Dodo.text("where")));
        };
        var printedNames = Dodo_Common.pursParens(foldWithSeparator(Dodo_Common.leadingComma)(map(printExportName)(v)));
        return Dodo.flexGroup(Dodo.appendSpace(Dodo.text("module"))(append(Language_PS_CST_Printers_Utils.printModuleName(moduleName))(append(Dodo.softBreak)(Dodo.indent(Dodo.appendSpace(printedNames)(Dodo.text("where")))))));
    };
};
export {
    printModuleModuleNameAndExports,
    printExportName
};
