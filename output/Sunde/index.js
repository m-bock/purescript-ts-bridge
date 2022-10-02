// Generated by purs version 0.15.4
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Posix_Signal from "../Data.Posix.Signal/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Ref from "../Effect.Ref/index.js";
import * as Node_ChildProcess from "../Node.ChildProcess/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
import * as Node_Stream from "../Node.Stream/index.js";
var $$void = /* #__PURE__ */ Data_Functor["void"](Effect.functorEffect);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(/* #__PURE__ */ Effect.monoidEffect(Data_Monoid.monoidUnit));
var pure1 = /* #__PURE__ */ Control_Applicative.pure(Data_Either.applicativeEither);
var spawn$prime = function (encoding) {
    return function (killSignal) {
        return function (v) {
            return function (options) {
                return Effect_Aff.makeAff(function (cb) {
                    return function __do() {
                        var stdoutRef = Effect_Ref["new"]("")();
                        var stderrRef = Effect_Ref["new"]("")();
                        var process = Node_ChildProcess.spawn(v.cmd)(v.args)(options)();
                        (function () {
                            if (v.stdin instanceof Data_Maybe.Just) {
                                var write = Node_ChildProcess.stdin(process);
                                return $$void(Node_Stream.writeString(write)(Node_Encoding.UTF8.value)(v.stdin.value0)(function (v1) {
                                    return Node_Stream.end(write)(function (v2) {
                                        return mempty;
                                    });
                                }))();
                            };
                            if (v.stdin instanceof Data_Maybe.Nothing) {
                                return Data_Unit.unit;
                            };
                            throw new Error("Failed pattern match at Sunde (line 41, column 3 - line 46, column 25): " + [ v.stdin.constructor.name ]);
                        })();
                        Node_Stream.onDataString(Node_ChildProcess.stdout(process))(encoding)(function (string) {
                            return Effect_Ref.modify_(function (v1) {
                                return v1 + string;
                            })(stdoutRef);
                        })();
                        Node_Stream.onDataString(Node_ChildProcess.stderr(process))(encoding)(function (string) {
                            return Effect_Ref.modify_(function (v1) {
                                return v1 + string;
                            })(stderrRef);
                        })();
                        Node_ChildProcess.onError(process)(function ($24) {
                            return cb(Data_Either.Left.create(Node_ChildProcess.toStandardError($24)));
                        })();
                        Node_ChildProcess.onExit(process)(function (exit) {
                            return function __do() {
                                var stdout = Effect_Ref.read(stdoutRef)();
                                var stderr = Effect_Ref.read(stderrRef)();
                                return cb(pure1({
                                    stdout: stdout,
                                    stderr: stderr,
                                    exit: exit
                                }))();
                            };
                        })();
                        return Effect_Aff.effectCanceler($$void(Node_ChildProcess.kill(killSignal)(process)));
                    };
                });
            };
        };
    };
};
var spawn = /* #__PURE__ */ (function () {
    return spawn$prime(Node_Encoding.UTF8.value)(Data_Posix_Signal.SIGTERM.value);
})();
export {
    spawn,
    spawn$prime
};
