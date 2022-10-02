// Generated by purs version 0.15.4
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Language_PS_CST_Types_Declaration from "../Language.PS.CST.Types.Declaration/index.js";
import * as Language_PS_CST_Types_Leafs from "../Language.PS.CST.Types.Leafs/index.js";
var genericShowConstructor = /* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(Language_PS_CST_Types_Leafs.showIdent));
var genericShowConstructor1 = /* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(Language_PS_CST_Types_Leafs.showOpName));
var genericShowArgsArgument = /* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(Language_PS_CST_Types_Leafs.showProperName);
var genericShowConstructor2 = /* #__PURE__ */ Data_Show_Generic.genericShowConstructor(genericShowArgsArgument);
var showRecord = /* #__PURE__ */ Data_Show.showRecord()();
var moduleNameIsSymbol = {
    reflectSymbol: function () {
        return "moduleName";
    }
};
var eq = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Eq.eqArray(Language_PS_CST_Types_Leafs.eqProperName));
var eq1 = /* #__PURE__ */ Data_Eq.eq(Language_PS_CST_Types_Leafs.eqIdent);
var eq2 = /* #__PURE__ */ Data_Eq.eq(Language_PS_CST_Types_Leafs.eqOpName);
var eq3 = /* #__PURE__ */ Data_Eq.eq(Language_PS_CST_Types_Leafs.eqProperName);
var eq4 = /* #__PURE__ */ Data_Eq.eq(Language_PS_CST_Types_Leafs.eqModuleName);
var eq5 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(Language_PS_CST_Types_Leafs.eqModuleName));
var eq6 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Eq.eqArray(Language_PS_CST_Types_Declaration.eqDeclaration));
var compare = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Ord.ordArray(Language_PS_CST_Types_Leafs.ordProperName));
var compare1 = /* #__PURE__ */ Data_Ord.compare(Language_PS_CST_Types_Leafs.ordIdent);
var compare2 = /* #__PURE__ */ Data_Ord.compare(Language_PS_CST_Types_Leafs.ordOpName);
var compare3 = /* #__PURE__ */ Data_Ord.compare(Language_PS_CST_Types_Leafs.ordProperName);
var compare4 = /* #__PURE__ */ Data_Ord.compare(Language_PS_CST_Types_Leafs.ordModuleName);
var compare5 = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Maybe.ordMaybe(Language_PS_CST_Types_Leafs.ordModuleName));
var compare6 = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Ord.ordArray(Language_PS_CST_Types_Declaration.ordDeclaration));
var DataAll = /* #__PURE__ */ (function () {
    function DataAll() {

    };
    DataAll.value = new DataAll();
    return DataAll;
})();
var DataEnumerated = /* #__PURE__ */ (function () {
    function DataEnumerated(value0) {
        this.value0 = value0;
    };
    DataEnumerated.create = function (value0) {
        return new DataEnumerated(value0);
    };
    return DataEnumerated;
})();
var ExportValue = /* #__PURE__ */ (function () {
    function ExportValue(value0) {
        this.value0 = value0;
    };
    ExportValue.create = function (value0) {
        return new ExportValue(value0);
    };
    return ExportValue;
})();
var ExportOp = /* #__PURE__ */ (function () {
    function ExportOp(value0) {
        this.value0 = value0;
    };
    ExportOp.create = function (value0) {
        return new ExportOp(value0);
    };
    return ExportOp;
})();
var ExportType = /* #__PURE__ */ (function () {
    function ExportType(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ExportType.create = function (value0) {
        return function (value1) {
            return new ExportType(value0, value1);
        };
    };
    return ExportType;
})();
var ExportTypeOp = /* #__PURE__ */ (function () {
    function ExportTypeOp(value0) {
        this.value0 = value0;
    };
    ExportTypeOp.create = function (value0) {
        return new ExportTypeOp(value0);
    };
    return ExportTypeOp;
})();
var ExportClass = /* #__PURE__ */ (function () {
    function ExportClass(value0) {
        this.value0 = value0;
    };
    ExportClass.create = function (value0) {
        return new ExportClass(value0);
    };
    return ExportClass;
})();
var ExportKind = /* #__PURE__ */ (function () {
    function ExportKind(value0) {
        this.value0 = value0;
    };
    ExportKind.create = function (value0) {
        return new ExportKind(value0);
    };
    return ExportKind;
})();
var ExportModule = /* #__PURE__ */ (function () {
    function ExportModule(value0) {
        this.value0 = value0;
    };
    ExportModule.create = function (value0) {
        return new ExportModule(value0);
    };
    return ExportModule;
})();
var ImportValue = /* #__PURE__ */ (function () {
    function ImportValue(value0) {
        this.value0 = value0;
    };
    ImportValue.create = function (value0) {
        return new ImportValue(value0);
    };
    return ImportValue;
})();
var ImportOp = /* #__PURE__ */ (function () {
    function ImportOp(value0) {
        this.value0 = value0;
    };
    ImportOp.create = function (value0) {
        return new ImportOp(value0);
    };
    return ImportOp;
})();
var ImportType = /* #__PURE__ */ (function () {
    function ImportType(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ImportType.create = function (value0) {
        return function (value1) {
            return new ImportType(value0, value1);
        };
    };
    return ImportType;
})();
var ImportTypeOp = /* #__PURE__ */ (function () {
    function ImportTypeOp(value0) {
        this.value0 = value0;
    };
    ImportTypeOp.create = function (value0) {
        return new ImportTypeOp(value0);
    };
    return ImportTypeOp;
})();
var ImportClass = /* #__PURE__ */ (function () {
    function ImportClass(value0) {
        this.value0 = value0;
    };
    ImportClass.create = function (value0) {
        return new ImportClass(value0);
    };
    return ImportClass;
})();
var ImportKind = /* #__PURE__ */ (function () {
    function ImportKind(value0) {
        this.value0 = value0;
    };
    ImportKind.create = function (value0) {
        return new ImportKind(value0);
    };
    return ImportKind;
})();
var ImportDecl = function (x) {
    return x;
};
var Module = function (x) {
    return x;
};
var newtypeModule = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeImportDecl = {
    Coercible0: function () {
        return undefined;
    }
};
var genericModule = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var genericImportDecl = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var genericImport = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new ImportValue(x.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
            return new ImportOp(x.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl)) {
            return new ImportType(x.value0.value0.value0.value0, x.value0.value0.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl))) {
            return new ImportTypeOp(x.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))) {
            return new ImportClass(x.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr)))) {
            return new ImportKind(x.value0.value0.value0.value0.value0);
        };
        throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 51, column 1 - line 51, column 50): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof ImportValue) {
            return new Data_Generic_Rep.Inl(x.value0);
        };
        if (x instanceof ImportOp) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0));
        };
        if (x instanceof ImportType) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1))));
        };
        if (x instanceof ImportTypeOp) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0))));
        };
        if (x instanceof ImportClass) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)))));
        };
        if (x instanceof ImportKind) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(x.value0)))));
        };
        throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 51, column 1 - line 51, column 50): " + [ x.constructor.name ]);
    }
};
var genericExport = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new ExportValue(x.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
            return new ExportOp(x.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl)) {
            return new ExportType(x.value0.value0.value0.value0, x.value0.value0.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl))) {
            return new ExportTypeOp(x.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))) {
            return new ExportClass(x.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl))))) {
            return new ExportKind(x.value0.value0.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr))))) {
            return new ExportModule(x.value0.value0.value0.value0.value0.value0);
        };
        throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 64, column 1 - line 64, column 50): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof ExportValue) {
            return new Data_Generic_Rep.Inl(x.value0);
        };
        if (x instanceof ExportOp) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0));
        };
        if (x instanceof ExportType) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1))));
        };
        if (x instanceof ExportTypeOp) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0))));
        };
        if (x instanceof ExportClass) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)))));
        };
        if (x instanceof ExportKind) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0))))));
        };
        if (x instanceof ExportModule) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(x.value0))))));
        };
        throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 64, column 1 - line 64, column 50): " + [ x.constructor.name ]);
    }
};
var genericDataMembers = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return DataAll.value;
        };
        if (x instanceof Data_Generic_Rep.Inr) {
            return new DataEnumerated(x.value0);
        };
        throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 39, column 1 - line 39, column 60): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof DataAll) {
            return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
        };
        if (x instanceof DataEnumerated) {
            return new Data_Generic_Rep.Inr(x.value0);
        };
        throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 39, column 1 - line 39, column 60): " + [ x.constructor.name ]);
    }
};
var showDataMembers = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericDataMembers)(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsNoArguments)({
        reflectSymbol: function () {
            return "DataAll";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ Data_Show.showArray(Language_PS_CST_Types_Leafs.showProperName)))({
        reflectSymbol: function () {
            return "DataEnumerated";
        }
    })))
};
var genericShowConstructor3 = /* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsProduct(genericShowArgsArgument)(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ Data_Maybe.showMaybe(showDataMembers))));
var showExport = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericExport)(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor({
        reflectSymbol: function () {
            return "ExportValue";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor1({
        reflectSymbol: function () {
            return "ExportOp";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor3({
        reflectSymbol: function () {
            return "ExportType";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor1({
        reflectSymbol: function () {
            return "ExportTypeOp";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor2({
        reflectSymbol: function () {
            return "ExportClass";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor2({
        reflectSymbol: function () {
            return "ExportKind";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(Language_PS_CST_Types_Leafs.showModuleName))({
        reflectSymbol: function () {
            return "ExportModule";
        }
    }))))))))
};
var showImport = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericImport)(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor({
        reflectSymbol: function () {
            return "ImportValue";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor1({
        reflectSymbol: function () {
            return "ImportOp";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor3({
        reflectSymbol: function () {
            return "ImportType";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor1({
        reflectSymbol: function () {
            return "ImportTypeOp";
        }
    }))(/* #__PURE__ */ Data_Show_Generic.genericShowSum(/* #__PURE__ */ genericShowConstructor2({
        reflectSymbol: function () {
            return "ImportClass";
        }
    }))(/* #__PURE__ */ genericShowConstructor2({
        reflectSymbol: function () {
            return "ImportKind";
        }
    })))))))
};
var showImportDecl = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericImportDecl)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ showRecord(/* #__PURE__ */ Data_Show.showRecordFieldsCons(moduleNameIsSymbol)(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "names";
        }
    })(/* #__PURE__ */ Data_Show.showRecordFieldsConsNil({
        reflectSymbol: function () {
            return "qualification";
        }
    })(/* #__PURE__ */ Data_Maybe.showMaybe(Language_PS_CST_Types_Leafs.showModuleName)))(/* #__PURE__ */ Data_Show.showArray(showImport)))(Language_PS_CST_Types_Leafs.showModuleName))))({
        reflectSymbol: function () {
            return "ImportDecl";
        }
    }))
};
var showModule = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericModule)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ showRecord(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "declarations";
        }
    })(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "exports";
        }
    })(/* #__PURE__ */ Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "imports";
        }
    })(/* #__PURE__ */ Data_Show.showRecordFieldsConsNil(moduleNameIsSymbol)(Language_PS_CST_Types_Leafs.showModuleName))(/* #__PURE__ */ Data_Show.showArray(showImportDecl)))(/* #__PURE__ */ Data_Show.showArray(showExport)))(/* #__PURE__ */ Data_Show.showArray(Language_PS_CST_Types_Declaration.showDeclaration)))))({
        reflectSymbol: function () {
            return "Module";
        }
    }))
};
var eqDataMembers = {
    eq: function (x) {
        return function (y) {
            if (x instanceof DataAll && y instanceof DataAll) {
                return true;
            };
            if (x instanceof DataEnumerated && y instanceof DataEnumerated) {
                return eq(x.value0)(y.value0);
            };
            return false;
        };
    }
};
var eq7 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(eqDataMembers));
var eqExport = {
    eq: function (x) {
        return function (y) {
            if (x instanceof ExportValue && y instanceof ExportValue) {
                return eq1(x.value0)(y.value0);
            };
            if (x instanceof ExportOp && y instanceof ExportOp) {
                return eq2(x.value0)(y.value0);
            };
            if (x instanceof ExportType && y instanceof ExportType) {
                return eq3(x.value0)(y.value0) && eq7(x.value1)(y.value1);
            };
            if (x instanceof ExportTypeOp && y instanceof ExportTypeOp) {
                return eq2(x.value0)(y.value0);
            };
            if (x instanceof ExportClass && y instanceof ExportClass) {
                return eq3(x.value0)(y.value0);
            };
            if (x instanceof ExportKind && y instanceof ExportKind) {
                return eq3(x.value0)(y.value0);
            };
            if (x instanceof ExportModule && y instanceof ExportModule) {
                return eq4(x.value0)(y.value0);
            };
            return false;
        };
    }
};
var eq8 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Eq.eqArray(eqExport));
var eqImport = {
    eq: function (x) {
        return function (y) {
            if (x instanceof ImportValue && y instanceof ImportValue) {
                return eq1(x.value0)(y.value0);
            };
            if (x instanceof ImportOp && y instanceof ImportOp) {
                return eq2(x.value0)(y.value0);
            };
            if (x instanceof ImportType && y instanceof ImportType) {
                return eq3(x.value0)(y.value0) && eq7(x.value1)(y.value1);
            };
            if (x instanceof ImportTypeOp && y instanceof ImportTypeOp) {
                return eq2(x.value0)(y.value0);
            };
            if (x instanceof ImportClass && y instanceof ImportClass) {
                return eq3(x.value0)(y.value0);
            };
            if (x instanceof ImportKind && y instanceof ImportKind) {
                return eq3(x.value0)(y.value0);
            };
            return false;
        };
    }
};
var eq9 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Eq.eqArray(eqImport));
var eqImportDecl = {
    eq: function (x) {
        return function (y) {
            return eq4(x.moduleName)(y.moduleName) && eq9(x.names)(y.names) && eq5(x.qualification)(y.qualification);
        };
    }
};
var eq10 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Eq.eqArray(eqImportDecl));
var eqModule = {
    eq: function (x) {
        return function (y) {
            return eq6(x.declarations)(y.declarations) && eq8(x.exports)(y.exports) && eq10(x.imports)(y.imports) && eq4(x.moduleName)(y.moduleName);
        };
    }
};
var ordDataMembers = {
    compare: function (x) {
        return function (y) {
            if (x instanceof DataAll && y instanceof DataAll) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof DataAll) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof DataAll) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof DataEnumerated && y instanceof DataEnumerated) {
                return compare(x.value0)(y.value0);
            };
            throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqDataMembers;
    }
};
var compare7 = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Maybe.ordMaybe(ordDataMembers));
var ordExport = {
    compare: function (x) {
        return function (y) {
            if (x instanceof ExportValue && y instanceof ExportValue) {
                return compare1(x.value0)(y.value0);
            };
            if (x instanceof ExportValue) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ExportValue) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ExportOp && y instanceof ExportOp) {
                return compare2(x.value0)(y.value0);
            };
            if (x instanceof ExportOp) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ExportOp) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ExportType && y instanceof ExportType) {
                var v = compare3(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return compare7(x.value1)(y.value1);
            };
            if (x instanceof ExportType) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ExportType) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ExportTypeOp && y instanceof ExportTypeOp) {
                return compare2(x.value0)(y.value0);
            };
            if (x instanceof ExportTypeOp) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ExportTypeOp) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ExportClass && y instanceof ExportClass) {
                return compare3(x.value0)(y.value0);
            };
            if (x instanceof ExportClass) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ExportClass) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ExportKind && y instanceof ExportKind) {
                return compare3(x.value0)(y.value0);
            };
            if (x instanceof ExportKind) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ExportKind) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ExportModule && y instanceof ExportModule) {
                return compare4(x.value0)(y.value0);
            };
            throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqExport;
    }
};
var compare8 = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Ord.ordArray(ordExport));
var ordImport = {
    compare: function (x) {
        return function (y) {
            if (x instanceof ImportValue && y instanceof ImportValue) {
                return compare1(x.value0)(y.value0);
            };
            if (x instanceof ImportValue) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ImportValue) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ImportOp && y instanceof ImportOp) {
                return compare2(x.value0)(y.value0);
            };
            if (x instanceof ImportOp) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ImportOp) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ImportType && y instanceof ImportType) {
                var v = compare3(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return compare7(x.value1)(y.value1);
            };
            if (x instanceof ImportType) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ImportType) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ImportTypeOp && y instanceof ImportTypeOp) {
                return compare2(x.value0)(y.value0);
            };
            if (x instanceof ImportTypeOp) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ImportTypeOp) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ImportClass && y instanceof ImportClass) {
                return compare3(x.value0)(y.value0);
            };
            if (x instanceof ImportClass) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ImportClass) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ImportKind && y instanceof ImportKind) {
                return compare3(x.value0)(y.value0);
            };
            throw new Error("Failed pattern match at Language.PS.CST.Types.Module (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqImport;
    }
};
var compare9 = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Ord.ordArray(ordImport));
var ordImportDecl = {
    compare: function (x) {
        return function (y) {
            var v = compare4(x.moduleName)(y.moduleName);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            var v1 = compare9(x.names)(y.names);
            if (v1 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v1 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return compare5(x.qualification)(y.qualification);
        };
    },
    Eq0: function () {
        return eqImportDecl;
    }
};
var compare10 = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Ord.ordArray(ordImportDecl));
var ordModule = {
    compare: function (x) {
        return function (y) {
            var v = compare6(x.declarations)(y.declarations);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            var v1 = compare8(x.exports)(y.exports);
            if (v1 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v1 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            var v2 = compare10(x.imports)(y.imports);
            if (v2 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v2 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return compare4(x.moduleName)(y.moduleName);
        };
    },
    Eq0: function () {
        return eqModule;
    }
};
export {
    Module,
    ImportDecl,
    DataAll,
    DataEnumerated,
    ImportValue,
    ImportOp,
    ImportType,
    ImportTypeOp,
    ImportClass,
    ImportKind,
    ExportValue,
    ExportOp,
    ExportType,
    ExportTypeOp,
    ExportClass,
    ExportKind,
    ExportModule,
    newtypeModule,
    genericModule,
    eqModule,
    ordModule,
    showModule,
    newtypeImportDecl,
    genericImportDecl,
    eqImportDecl,
    ordImportDecl,
    showImportDecl,
    genericDataMembers,
    eqDataMembers,
    ordDataMembers,
    showDataMembers,
    genericImport,
    eqImport,
    ordImport,
    showImport,
    genericExport,
    eqExport,
    ordExport,
    showExport
};
