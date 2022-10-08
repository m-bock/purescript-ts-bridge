// Generated by purs version 0.15.4
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Show from "../Data.Show/index.js";
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var Uid = function (x) {
    return x;
};
var Pid = function (x) {
    return x;
};
var Gid = function (x) {
    return x;
};
var showUid = {
    show: function (v) {
        return "(Uid " + (show(v) + ")");
    }
};
var showPid = {
    show: function (v) {
        return "(Pid " + (show(v) + ")");
    }
};
var showGid = {
    show: function (v) {
        return "(Gid " + (show(v) + ")");
    }
};
var ordUid = Data_Ord.ordInt;
var ordPid = Data_Ord.ordInt;
var ordGid = Data_Ord.ordInt;
var newtypeUid = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypePid = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeGid = {
    Coercible0: function () {
        return undefined;
    }
};
var eqUid = Data_Eq.eqInt;
var eqPid = Data_Eq.eqInt;
var eqGid = Data_Eq.eqInt;
export {
    Pid,
    Gid,
    Uid,
    newtypePid,
    eqPid,
    ordPid,
    showPid,
    newtypeGid,
    eqGid,
    ordGid,
    showGid,
    newtypeUid,
    eqUid,
    ordUid,
    showUid
};