// Generated by purs version 0.15.4
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Void from "../Data.Void/index.js";
import * as PureScript_CST_Types from "../PureScript.CST.Types/index.js";
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var power = /* #__PURE__ */ Data_Monoid.power(Data_Monoid.monoidString);
var foldMap = /* #__PURE__ */ Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Monoid.monoidString);
var ShowLayout = /* #__PURE__ */ (function () {
    function ShowLayout() {

    };
    ShowLayout.value = new ShowLayout();
    return ShowLayout;
})();
var HideLayout = /* #__PURE__ */ (function () {
    function HideLayout() {

    };
    HideLayout.value = new HideLayout();
    return HideLayout;
})();
var printQualified = function (moduleName) {
    return function (name) {
        if (moduleName instanceof Data_Maybe.Nothing) {
            return name;
        };
        if (moduleName instanceof Data_Maybe.Just) {
            return unwrap(moduleName.value0) + ("." + name);
        };
        throw new Error("Failed pattern match at PureScript.CST.Print (line 124, column 34 - line 126, column 38): " + [ moduleName.constructor.name ]);
    };
};
var printTokenWithOption = function (option) {
    return function (v) {
        if (v instanceof PureScript_CST_Types.TokLeftParen) {
            return "(";
        };
        if (v instanceof PureScript_CST_Types.TokRightParen) {
            return ")";
        };
        if (v instanceof PureScript_CST_Types.TokLeftBrace) {
            return "{";
        };
        if (v instanceof PureScript_CST_Types.TokRightBrace) {
            return "}";
        };
        if (v instanceof PureScript_CST_Types.TokLeftSquare) {
            return "[";
        };
        if (v instanceof PureScript_CST_Types.TokRightSquare) {
            return "]";
        };
        if (v instanceof PureScript_CST_Types.TokLeftArrow) {
            if (v.value0 instanceof PureScript_CST_Types.ASCII) {
                return "<-";
            };
            if (v.value0 instanceof PureScript_CST_Types.Unicode) {
                return "\u2190";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 51, column 5 - line 53, column 21): " + [ v.value0.constructor.name ]);
        };
        if (v instanceof PureScript_CST_Types.TokRightArrow) {
            if (v.value0 instanceof PureScript_CST_Types.ASCII) {
                return "->";
            };
            if (v.value0 instanceof PureScript_CST_Types.Unicode) {
                return "\u2192";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 55, column 5 - line 57, column 21): " + [ v.value0.constructor.name ]);
        };
        if (v instanceof PureScript_CST_Types.TokRightFatArrow) {
            if (v.value0 instanceof PureScript_CST_Types.ASCII) {
                return "=>";
            };
            if (v.value0 instanceof PureScript_CST_Types.Unicode) {
                return "\u21d2";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 59, column 5 - line 61, column 21): " + [ v.value0.constructor.name ]);
        };
        if (v instanceof PureScript_CST_Types.TokDoubleColon) {
            if (v.value0 instanceof PureScript_CST_Types.ASCII) {
                return "::";
            };
            if (v.value0 instanceof PureScript_CST_Types.Unicode) {
                return "\u2237";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 63, column 5 - line 65, column 21): " + [ v.value0.constructor.name ]);
        };
        if (v instanceof PureScript_CST_Types.TokForall) {
            if (v.value0 instanceof PureScript_CST_Types.ASCII) {
                return "forall";
            };
            if (v.value0 instanceof PureScript_CST_Types.Unicode) {
                return "\u2200";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 67, column 5 - line 69, column 21): " + [ v.value0.constructor.name ]);
        };
        if (v instanceof PureScript_CST_Types.TokEquals) {
            return "=";
        };
        if (v instanceof PureScript_CST_Types.TokPipe) {
            return "|";
        };
        if (v instanceof PureScript_CST_Types.TokTick) {
            return "`";
        };
        if (v instanceof PureScript_CST_Types.TokDot) {
            return ".";
        };
        if (v instanceof PureScript_CST_Types.TokComma) {
            return ",";
        };
        if (v instanceof PureScript_CST_Types.TokUnderscore) {
            return "_";
        };
        if (v instanceof PureScript_CST_Types.TokBackslash) {
            return "\\";
        };
        if (v instanceof PureScript_CST_Types.TokAt) {
            return "@";
        };
        if (v instanceof PureScript_CST_Types.TokLowerName) {
            return printQualified(v.value0)(v.value1);
        };
        if (v instanceof PureScript_CST_Types.TokUpperName) {
            return printQualified(v.value0)(v.value1);
        };
        if (v instanceof PureScript_CST_Types.TokOperator) {
            return printQualified(v.value0)(v.value1);
        };
        if (v instanceof PureScript_CST_Types.TokSymbolName) {
            return printQualified(v.value0)("(" + (v.value1 + ")"));
        };
        if (v instanceof PureScript_CST_Types.TokSymbolArrow) {
            if (v.value0 instanceof PureScript_CST_Types.ASCII) {
                return "(->)";
            };
            if (v.value0 instanceof PureScript_CST_Types.Unicode) {
                return "(\u2192)";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 95, column 5 - line 97, column 23): " + [ v.value0.constructor.name ]);
        };
        if (v instanceof PureScript_CST_Types.TokHole) {
            return "?" + v.value0;
        };
        if (v instanceof PureScript_CST_Types.TokChar) {
            return "'" + (v.value0 + "'");
        };
        if (v instanceof PureScript_CST_Types.TokString) {
            return "\"" + (v.value0 + "\"");
        };
        if (v instanceof PureScript_CST_Types.TokRawString) {
            return "\"\"\"" + (v.value0 + "\"\"\"");
        };
        if (v instanceof PureScript_CST_Types.TokInt) {
            return v.value0;
        };
        if (v instanceof PureScript_CST_Types.TokNumber) {
            return v.value0;
        };
        if (v instanceof PureScript_CST_Types.TokLayoutStart) {
            if (option instanceof ShowLayout) {
                return "{";
            };
            if (option instanceof HideLayout) {
                return "";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 111, column 5 - line 113, column 23): " + [ option.constructor.name ]);
        };
        if (v instanceof PureScript_CST_Types.TokLayoutSep) {
            if (option instanceof ShowLayout) {
                return ";";
            };
            if (option instanceof HideLayout) {
                return "";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 115, column 5 - line 117, column 23): " + [ option.constructor.name ]);
        };
        if (v instanceof PureScript_CST_Types.TokLayoutEnd) {
            if (option instanceof ShowLayout) {
                return "}";
            };
            if (option instanceof HideLayout) {
                return "";
            };
            throw new Error("Failed pattern match at PureScript.CST.Print (line 119, column 5 - line 121, column 23): " + [ option.constructor.name ]);
        };
        throw new Error("Failed pattern match at PureScript.CST.Print (line 37, column 31 - line 121, column 23): " + [ v.constructor.name ]);
    };
};
var printToken = /* #__PURE__ */ (function () {
    return printTokenWithOption(HideLayout.value);
})();
var printLineFeed = function (v) {
    if (v instanceof PureScript_CST_Types.LF) {
        return "\x0a";
    };
    if (v instanceof PureScript_CST_Types.CRLF) {
        return "\x0d\x0a";
    };
    throw new Error("Failed pattern match at PureScript.CST.Print (line 135, column 17 - line 137, column 17): " + [ v.constructor.name ]);
};
var printComment = function (k) {
    return function (v) {
        if (v instanceof PureScript_CST_Types.Comment) {
            return v.value0;
        };
        if (v instanceof PureScript_CST_Types.Space) {
            return power(" ")(v.value0);
        };
        if (v instanceof PureScript_CST_Types.Line) {
            return power(k(v.value0))(v.value1);
        };
        throw new Error("Failed pattern match at PureScript.CST.Print (line 129, column 18 - line 132, column 28): " + [ v.constructor.name ]);
    };
};
var printSourceTokenWithOption = function (option) {
    return function (tok) {
        return foldMap(printComment(printLineFeed))(tok.leadingComments) + (printTokenWithOption(option)(tok.value) + foldMap(printComment(Data_Void.absurd))(tok.trailingComments));
    };
};
var printSourceToken = /* #__PURE__ */ (function () {
    return printSourceTokenWithOption(HideLayout.value);
})();
export {
    printToken,
    printSourceToken,
    ShowLayout,
    HideLayout,
    printTokenWithOption,
    printSourceTokenWithOption,
    printComment,
    printLineFeed,
    printQualified
};
