// Generated by purs version 0.15.4
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Effect_Aff from "../Effect.Aff/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Class_Console from "../Effect.Class.Console/index.js";
import * as Effect_Exception from "../Effect.Exception/index.js";
import * as Node_ChildProcess from "../Node.ChildProcess/index.js";
import * as Node_Encoding from "../Node.Encoding/index.js";
import * as Node_FS_Aff from "../Node.FS.Aff/index.js";
import * as Node_FS_Perms from "../Node.FS.Perms/index.js";
import * as Node_Path from "../Node.Path/index.js";
import * as Options_Applicative_Builder from "../Options.Applicative.Builder/index.js";
import * as Options_Applicative_Builder_Internal from "../Options.Applicative.Builder.Internal/index.js";
import * as Options_Applicative_Extra from "../Options.Applicative.Extra/index.js";
import * as Options_Applicative_Internal_Utils from "../Options.Applicative.Internal.Utils/index.js";
import * as Options_Applicative_Types from "../Options.Applicative.Types/index.js";
import * as Sunde from "../Sunde/index.js";
import * as TsBridge_Print from "../TsBridge.Print/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Effect_Aff.bindAff);
var pure = /* #__PURE__ */ Control_Applicative.pure(Effect_Aff.applicativeAff);
var throwError = /* #__PURE__ */ Control_Monad_Error_Class.throwError(Effect_Aff.monadThrowAff);
var liftEffect = /* #__PURE__ */ Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
var toUnfoldable = /* #__PURE__ */ Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff);
var for_ = /* #__PURE__ */ Data_Foldable.for_(Effect_Aff.applicativeAff)(Data_Foldable.foldableArray);
var log = /* #__PURE__ */ Effect_Class_Console.log(Effect_Aff.monadEffectAff);
var $$void = /* #__PURE__ */ Data_Functor["void"](Effect_Aff.functorAff);
var spawn = function (cmd) {
    return function (args) {
        return bind(Sunde.spawn({
            cmd: cmd,
            args: args,
            stdin: Data_Maybe.Nothing.value
        })(Node_ChildProcess.defaultSpawnOptions))(function (v) {
            if (v.exit instanceof Node_ChildProcess.Normally && v.exit.value0 === 0) {
                return pure({
                    stderr: v.stderr,
                    stdout: v.stdout
                });
            };
            return throwError(Effect_Exception.error("Command " + (cmd + " failed")));
        });
    };
};
var parserTsBridgeCliOpts = /* #__PURE__ */ Data_Functor.map(Options_Applicative_Types.parserFunctor)(function (v) {
    return {
        outputDir: v
    };
})(/* #__PURE__ */ Options_Applicative_Builder.strOption(/* #__PURE__ */ Data_Foldable.fold(Data_Foldable.foldableArray)(Options_Applicative_Builder_Internal.modMonoid)([ /* #__PURE__ */ Options_Applicative_Builder["long"](Options_Applicative_Builder_Internal.optionFieldsHasName)("output-dir"), /* #__PURE__ */ Options_Applicative_Builder.metavar(Options_Applicative_Builder_Internal.optionFieldsHasMetavar)("OUTPUT_DIR"), /* #__PURE__ */ Options_Applicative_Builder.help("Dictionary the CLI will write the output d.ts files to.") ])));
var parserInfoTsBridgeCliOpts = /* #__PURE__ */ Options_Applicative_Builder.info(/* #__PURE__ */ Options_Applicative_Internal_Utils.apApplyFlipped(Options_Applicative_Types.parserApply)(parserTsBridgeCliOpts)(Options_Applicative_Extra.helper))(/* #__PURE__ */ Data_Monoid.mempty(Options_Applicative_Builder.infoModMonoid));
var mkTypeGenCliAff = function (tsProg) {
    return bind(liftEffect(Options_Applicative_Extra.execParser(parserInfoTsBridgeCliOpts)))(function (cliOpts) {
        var files = toUnfoldable(TsBridge_Print.printTsProgram(tsProg));
        return discard(for_(files)(function (v) {
            var filePath = cliOpts.outputDir + ("/" + v.value0);
            return discard(log(filePath))(function () {
                return discard(Node_FS_Aff["mkdir$prime"](Node_Path.dirname(filePath))({
                    recursive: true,
                    mode: Node_FS_Perms.mkPerms(Node_FS_Perms.all)(Node_FS_Perms.all)(Node_FS_Perms.all)
                }))(function () {
                    return Node_FS_Aff.writeTextFile(Node_Encoding.UTF8.value)(filePath)(v.value1);
                });
            });
        }))(function () {
            return $$void(spawn("prettier")([ "--write", cliOpts.outputDir + "/**/*.d.ts" ]));
        });
    });
};
var mkTypeGenCli = function (tsProg) {
    return Effect_Aff.launchAff_(mkTypeGenCliAff(tsProg));
};
export {
    mkTypeGenCli
};
