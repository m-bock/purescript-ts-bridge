// Generated by purs version 0.15.4
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Lens_Lens from "../Data.Lens.Lens/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var united = function (dictStrong) {
    return Data_Lens_Lens.lens(Data_Function["const"](Data_Unit.unit))(Data_Function["const"])(dictStrong);
};
export {
    united
};
