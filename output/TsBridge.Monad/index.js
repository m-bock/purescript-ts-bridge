// Generated by purs version 0.15.4
import * as Control_Monad_Writer from "../Control.Monad.Writer/index.js";
import * as Control_Monad_Writer_Trans from "../Control.Monad.Writer.Trans/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Set from "../Data.Set/index.js";
import * as Data_Set_Ordered from "../Data.Set.Ordered/index.js";
import * as TsBridge_DTS from "../TsBridge.DTS/index.js";
var semigroupRecord = /* #__PURE__ */ Data_Semigroup.semigroupRecord();
var importsIsSymbol = {
    reflectSymbol: function () {
        return "imports";
    }
};
var scopeIsSymbol = {
    reflectSymbol: function () {
        return "scope";
    }
};
var typeDefsIsSymbol = {
    reflectSymbol: function () {
        return "typeDefs";
    }
};
var fixedIsSymbol = {
    reflectSymbol: function () {
        return "fixed";
    }
};
var floatingIsSymbol = {
    reflectSymbol: function () {
        return "floating";
    }
};
var monoidRecord = /* #__PURE__ */ Data_Monoid.monoidRecord();
var monoidSet = /* #__PURE__ */ Data_Set.monoidSet(TsBridge_DTS.ordTsImport);
var TsBridge_Monad_Wrap = function (x) {
    return x;
};
var TsBridgeAccum = function (x) {
    return x;
};
var TsBridgeM = function (x) {
    return x;
};
var semigroupTsBridge_Monad_W = function (dictEq) {
    return Data_Set_Ordered.semigroupOSet(dictEq);
};
var semigroupTsBridge_Monad_W1 = /* #__PURE__ */ semigroupTsBridge_Monad_W(TsBridge_DTS.eqTsName);
var semigroupTsBridgeAccum = /* #__PURE__ */ semigroupRecord(/* #__PURE__ */ Data_Semigroup.semigroupRecordCons(importsIsSymbol)()(/* #__PURE__ */ Data_Semigroup.semigroupRecordCons(scopeIsSymbol)()(/* #__PURE__ */ Data_Semigroup.semigroupRecordCons(typeDefsIsSymbol)()(Data_Semigroup.semigroupRecordNil)(Data_Semigroup.semigroupArray))(/* #__PURE__ */ semigroupRecord(/* #__PURE__ */ Data_Semigroup.semigroupRecordCons(fixedIsSymbol)()(/* #__PURE__ */ Data_Semigroup.semigroupRecordCons(floatingIsSymbol)()(Data_Semigroup.semigroupRecordNil)(semigroupTsBridge_Monad_W1))(semigroupTsBridge_Monad_W1))))(/* #__PURE__ */ Data_Set.semigroupSet(TsBridge_DTS.ordTsImport)));
var newtypeTsBridge_Monad_Wra = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeTsBridgeAccum_ = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidTsBridge_Monad_Wrap = function (dictEq) {
    var semigroupTsBridge_Monad_W2 = semigroupTsBridge_Monad_W(dictEq);
    return {
        mempty: Data_Set_Ordered.empty,
        Semigroup0: function () {
            return semigroupTsBridge_Monad_W2;
        }
    };
};
var monoidTsBridge_Monad_Wrap1 = /* #__PURE__ */ monoidTsBridge_Monad_Wrap(TsBridge_DTS.eqTsName);
var monoidRecord1 = /* #__PURE__ */ monoidRecord(/* #__PURE__ */ Data_Monoid.monoidRecordCons(fixedIsSymbol)(monoidTsBridge_Monad_Wrap1)()(/* #__PURE__ */ Data_Monoid.monoidRecordCons(floatingIsSymbol)(monoidTsBridge_Monad_Wrap1)()(Data_Monoid.monoidRecordNil)));
var monoidTsBridgeAccum = /* #__PURE__ */ monoidRecord(/* #__PURE__ */ Data_Monoid.monoidRecordCons(importsIsSymbol)(monoidSet)()(/* #__PURE__ */ Data_Monoid.monoidRecordCons(scopeIsSymbol)(monoidRecord1)()(/* #__PURE__ */ Data_Monoid.monoidRecordCons(typeDefsIsSymbol)(Data_Monoid.monoidArray)()(Data_Monoid.monoidRecordNil))));
var monadWriterTsBridgeAccumT = /* #__PURE__ */ Control_Monad_Writer_Trans.monadWriterWriterT(monoidTsBridgeAccum)(Data_Identity.monadIdentity);
var monadTsBridgeM = /* #__PURE__ */ Control_Monad_Writer_Trans.monadWriterT(monoidTsBridgeAccum)(Data_Identity.monadIdentity);
var monadTellTsBridgeAccumTsB = /* #__PURE__ */ Control_Monad_Writer_Trans.monadTellWriterT(monoidTsBridgeAccum)(Data_Identity.monadIdentity);
var functorTsBridgeM = /* #__PURE__ */ Control_Monad_Writer_Trans.functorWriterT(Data_Identity.functorIdentity);
var bindTsBridgeM = /* #__PURE__ */ Control_Monad_Writer_Trans.bindWriterT(semigroupTsBridgeAccum)(Data_Identity.bindIdentity);
var applyTsBridgeM = /* #__PURE__ */ Control_Monad_Writer_Trans.applyWriterT(semigroupTsBridgeAccum)(Data_Identity.applyIdentity);
var applicativeTsBridgeM = /* #__PURE__ */ Control_Monad_Writer_Trans.applicativeWriterT(monoidTsBridgeAccum)(Data_Identity.applicativeIdentity);
var runTsBridgeM = function (v) {
    return Control_Monad_Writer.runWriter(v);
};
var defaultTsBridgeAccum = {
    typeDefs: /* #__PURE__ */ Data_Monoid.mempty(Data_Monoid.monoidArray),
    imports: /* #__PURE__ */ Data_Monoid.mempty(monoidSet),
    scope: /* #__PURE__ */ Data_Monoid.mempty(monoidRecord1)
};
export {
    TsBridgeAccum,
    TsBridgeM,
    TsBridge_Monad_Wrap,
    defaultTsBridgeAccum,
    runTsBridgeM,
    newtypeTsBridge_Monad_Wra,
    semigroupTsBridge_Monad_W,
    monoidTsBridge_Monad_Wrap,
    newtypeTsBridgeAccum_,
    monoidTsBridgeAccum,
    semigroupTsBridgeAccum,
    monadTellTsBridgeAccumTsB,
    monadWriterTsBridgeAccumT,
    monadTsBridgeM,
    bindTsBridgeM,
    functorTsBridgeM,
    applyTsBridgeM,
    applicativeTsBridgeM
};
