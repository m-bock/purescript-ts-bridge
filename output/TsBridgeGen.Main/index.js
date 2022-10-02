// Generated by purs version 0.15.4
import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Aff_Class from "../Effect.Aff.Class/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
import * as Node_ChildProcess from "../Node.ChildProcess/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
import * as Node_FS_Aff from "../Node.FS.Aff/index.js";
import * as Node_Glob_Basic from "../Node.Glob.Basic/index.js";
import * as Sunde from "../Sunde/index.js";
import * as TsBridgeGen_Cli from "../TsBridgeGen.Cli/index.js";
import * as TsBridgeGen_Config from "../TsBridgeGen.Config/index.js";
import * as TsBridgeGen_Monad from "../TsBridgeGen.Monad/index.js";
import * as TsBridgeGen_Types from "../TsBridgeGen.Types/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(TsBridgeGen_Monad.bindAppM);
var liftAff = /* #__PURE__ */ Effect_Aff_Class.liftAff(TsBridgeGen_Monad.monadAffAppM);
var pure = /* #__PURE__ */ Control_Applicative.pure(TsBridgeGen_Monad.applicativeAppM);
var throwError = /* #__PURE__ */ Control_Monad_Error_Class.throwError(TsBridgeGen_Monad.monadThrowAppErrorAppM);
var mapFlipped = /* #__PURE__ */ Data_Functor.mapFlipped(Effect_Aff.functorAff);
var $$try = /* #__PURE__ */ Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff);
var lmap = /* #__PURE__ */ Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither);
var liftEither = /* #__PURE__ */ Control_Monad_Error_Class.liftEither(TsBridgeGen_Monad.monadThrowAppErrorAppM);
var liftEffect = /* #__PURE__ */ Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
var app = /* #__PURE__ */ TsBridgeGen_Cli.app(TsBridgeGen_Monad.monadAppAppM);
var spawn = function (cmd) {
    return function (args) {
        return bind(liftAff(Sunde.spawn({
            cmd: cmd,
            args: args,
            stdin: Data_Maybe.Nothing.value
        })(Node_ChildProcess.defaultSpawnOptions)))(function (v) {
            if (v.exit instanceof Node_ChildProcess.Normally && v.exit.value0 === 0) {
                return pure({
                    stderr: v.stderr,
                    stdout: v.stdout
                });
            };
            return throwError(new TsBridgeGen_Types.ErrSpawn(cmd, args));
        });
    };
};
var liftAffWithErr = function (mkError) {
    return function (ma) {
        return bind(liftAff(mapFlipped($$try(ma))(lmap(mkError))))(liftEither);
    };
};
var readTextFile = function (path) {
    return liftAffWithErr(Data_Function["const"](new TsBridgeGen_Types.ErrReadFile(path)))(Node_FS_Aff.readTextFile(Node_Encoding.UTF8.value)(path));
};
var runPrettier = function (str) {
    return liftAffWithErr(Data_Function["const"](new TsBridgeGen_Types.ErrLiteral("Prettier error")))(liftEffect($foreign.runPrettierImpl(str)));
};
var writeTextFile = function (path) {
    return function (content) {
        return liftAffWithErr(Data_Function["const"](new TsBridgeGen_Types.ErrWriteFile(path)))(Node_FS_Aff.writeTextFile(Node_Encoding.UTF8.value)(path)(content));
    };
};
var expandGlobsCwd = function (globs) {
    return liftAffWithErr(Data_Function["const"](TsBridgeGen_Types.ErrExpandGlobs.value))(Node_Glob_Basic.expandGlobsCwd(globs));
};
var main = function __do() {
    var config = TsBridgeGen_Config.runInitM(TsBridgeGen_Config.getConfig)();
    var appEnv = {
        config: config,
        capabilities: {
            spawn: spawn,
            writeTextFile: writeTextFile,
            readTextFile: readTextFile,
            expandGlobsCwd: expandGlobsCwd,
            runPrettier: runPrettier
        }
    };
    return TsBridgeGen_Monad.runAppM(appEnv)(app)();
};
export {
    runPrettierImpl
} from "./foreign.js";
export {
    main,
    expandGlobsCwd,
    writeTextFile,
    readTextFile,
    runPrettier,
    spawn,
    liftAffWithErr
};
