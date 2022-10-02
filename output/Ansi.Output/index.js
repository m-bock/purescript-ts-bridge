// Generated by purs version 0.15.4
import * as Ansi_Codes from "../Ansi.Codes/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
var pure = /* #__PURE__ */ Control_Applicative.pure(Data_List_Types.applicativeNonEmptyList);
var withGraphics = function (params) {
    return function (text) {
        return Ansi_Codes.escapeCodeToString(new Ansi_Codes.Graphics(params)) + (text + Ansi_Codes.escapeCodeToString(new Ansi_Codes.Graphics(pure(Ansi_Codes.Reset.value))));
    };
};
var underline = /* #__PURE__ */ (function () {
    return pure(new Ansi_Codes.PMode(Ansi_Codes.Underline.value));
})();
var strikethrough = /* #__PURE__ */ (function () {
    return pure(new Ansi_Codes.PMode(Ansi_Codes.Strikethrough.value));
})();
var italic = /* #__PURE__ */ (function () {
    return pure(new Ansi_Codes.PMode(Ansi_Codes.Italic.value));
})();
var inverse = /* #__PURE__ */ (function () {
    return pure(new Ansi_Codes.PMode(Ansi_Codes.Inverse.value));
})();
var foreground = function (c) {
    return pure(new Ansi_Codes.PForeground(c));
};
var dim = /* #__PURE__ */ (function () {
    return pure(new Ansi_Codes.PMode(Ansi_Codes.Dim.value));
})();
var bold = /* #__PURE__ */ (function () {
    return pure(new Ansi_Codes.PMode(Ansi_Codes.Bold.value));
})();
var background = function (c) {
    return pure(new Ansi_Codes.PBackground(c));
};
export {
    withGraphics,
    bold,
    dim,
    italic,
    underline,
    inverse,
    strikethrough,
    foreground,
    background
};
