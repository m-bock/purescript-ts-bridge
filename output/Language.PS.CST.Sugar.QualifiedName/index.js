// Generated by purs version 0.15.4
import * as Data_Maybe from "../Data.Maybe/index.js";
var qualifiedName = function (moduleName) {
    return function (a) {
        return {
            qualModule: new Data_Maybe.Just(moduleName),
            qualName: a
        };
    };
};
var nonQualifiedName = function (a) {
    return {
        qualModule: Data_Maybe.Nothing.value,
        qualName: a
    };
};
export {
    nonQualifiedName,
    qualifiedName
};
