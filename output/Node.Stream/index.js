// Generated by purs version 0.15.4
import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Exception from "../Effect.Exception/index.js";
import * as Node_Buffer from "../Node.Buffer/index.js";
import * as Node_Buffer_Class from "../Node.Buffer.Class/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
var show = /* #__PURE__ */ Data_Show.show(Node_Encoding.showEncoding);
var pure = /* #__PURE__ */ Control_Applicative.pure(Effect.applicativeEffect);
var map = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect);
var toString = /* #__PURE__ */ Node_Buffer_Class.toString(Node_Buffer.mutableBufferEffect);
var composeKleisliFlipped = /* #__PURE__ */ Control_Bind.composeKleisliFlipped(Effect.bindEffect);
var writeString = function (w) {
    return function (enc) {
        return function (s) {
            return function (cb) {
                return $foreign.writeStringImpl(w)(show(enc))(s)(function ($20) {
                    return cb(Data_Nullable.toMaybe($20))();
                });
            };
        };
    };
};
var write = function (w) {
    return function (b) {
        return function (cb) {
            return $foreign.writeImpl(w)(b)(function ($21) {
                return cb(Data_Nullable.toMaybe($21))();
            });
        };
    };
};
var setEncoding = function (r) {
    return function (enc) {
        return $foreign.setEncodingImpl(r)(show(enc));
    };
};
var setDefaultEncoding = function (r) {
    return function (enc) {
        return $foreign.setDefaultEncodingImpl(r)(show(enc));
    };
};
var readChunk = /* #__PURE__ */ (function () {
    return $foreign.readChunkImpl(Data_Either.Left.create)(Data_Either.Right.create);
})();
var readEither = function (r) {
    return function (size) {
        return $foreign.readImpl(readChunk)(Data_Maybe.Nothing.value)(Data_Maybe.Just.create)(r)(Data_Maybe.fromMaybe($foreign["undefined"])(size));
    };
};
var readString = function (r) {
    return function (size) {
        return function (enc) {
            return function __do() {
                var v = readEither(r)(size)();
                if (v instanceof Data_Maybe.Nothing) {
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Left) {
                    return Effect_Exception["throw"]("Stream encoding should not be set")();
                };
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Right) {
                    return map(Data_Maybe.Just.create)(toString(enc)(v.value0.value0))();
                };
                throw new Error("Failed pattern match at Node.Stream (line 123, column 3 - line 126, column 60): " + [ v.constructor.name ]);
            };
        };
    };
};
var read = function (r) {
    return function (size) {
        return function __do() {
            var v = readEither(r)(size)();
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Left) {
                return Effect_Exception["throw"]("Stream encoding should not be set")();
            };
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Either.Right) {
                return new Data_Maybe.Just(v.value0.value0);
            };
            throw new Error("Failed pattern match at Node.Stream (line 110, column 3 - line 113, column 36): " + [ v.constructor.name ]);
        };
    };
};
var onDataEither = function (r) {
    return function (cb) {
        return $foreign.onDataEitherImpl(readChunk)(r)(cb);
    };
};
var onData = function (r) {
    return function (cb) {
        var fromEither = function (x) {
            if (x instanceof Data_Either.Left) {
                return Effect_Exception["throw"]("Stream encoding should not be set");
            };
            if (x instanceof Data_Either.Right) {
                return pure(x.value0);
            };
            throw new Error("Failed pattern match at Node.Stream (line 97, column 5 - line 101, column 17): " + [ x.constructor.name ]);
        };
        return onDataEither(r)(composeKleisliFlipped(cb)(fromEither));
    };
};
var onDataString = function (r) {
    return function (enc) {
        return function (cb) {
            return onData(r)(composeKleisliFlipped(cb)(toString(enc)));
        };
    };
};
var end = function (w) {
    return function (cb) {
        return $foreign.endImpl(w)(function ($22) {
            return cb(Data_Nullable.toMaybe($22))();
        });
    };
};
export {
    onReadable,
    onEnd,
    onFinish,
    onClose,
    onError,
    resume,
    pause,
    isPaused,
    pipe,
    unpipe,
    unpipeAll,
    cork,
    uncork,
    destroy,
    destroyWithError
} from "./foreign.js";
export {
    onData,
    onDataString,
    onDataEither,
    setEncoding,
    read,
    readString,
    readEither,
    write,
    writeString,
    setDefaultEncoding,
    end
};
