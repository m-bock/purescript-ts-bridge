// Generated by purs version 0.15.4
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Comonad_Cofree from "../Control.Comonad.Cofree/index.js";
import * as Control_Comonad_Env_Trans from "../Control.Comonad.Env.Trans/index.js";
import * as Control_Comonad_Store_Trans from "../Control.Comonad.Store.Trans/index.js";
import * as Control_Comonad_Traced_Trans from "../Control.Comonad.Traced.Trans/index.js";
import * as Control_Monad_Except_Trans from "../Control.Monad.Except.Trans/index.js";
import * as Control_Monad_Free from "../Control.Monad.Free/index.js";
import * as Control_Monad_Maybe_Trans from "../Control.Monad.Maybe.Trans/index.js";
import * as Control_Monad_RWS_Trans from "../Control.Monad.RWS.Trans/index.js";
import * as Control_Monad_Reader_Trans from "../Control.Monad.Reader.Trans/index.js";
import * as Control_Monad_State_Trans from "../Control.Monad.State.Trans/index.js";
import * as Control_Monad_Writer_Trans from "../Control.Monad.Writer.Trans/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Coyoneda from "../Data.Coyoneda/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Yoneda from "../Data.Yoneda/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorFn);
var over = /* #__PURE__ */ Data_Newtype.over()();
var lmap = /* #__PURE__ */ Data_Bifunctor.lmap(Data_Bifunctor.bifunctorTuple);
var foldFree = /* #__PURE__ */ Control_Monad_Free.foldFree(Control_Monad_Free.freeMonadRec);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Tuple.functorTuple);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var mfunctorYoneda = {
    hoist: function (dictMonad) {
        return Data_Yoneda.hoistYoneda;
    }
};
var mmonadYoneda = {
    embed: function (dictMonad) {
        return function (f) {
            return map(f)(Data_Yoneda.lowerYoneda);
        };
    },
    MFunctor0: function () {
        return mfunctorYoneda;
    },
    MonadTrans1: function () {
        return Data_Yoneda.monadTransYoneda;
    }
};
var mfunctorWriterT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return nat(Control_Monad_Writer_Trans.runWriterT(m));
            };
        };
    }
};
var mmonadWriterT = function (dictMonoid) {
    var append = Data_Semigroup.append(dictMonoid.Semigroup0());
    var monadTransWriterT = Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid);
    return {
        embed: function (dictMonad) {
            var bind = Control_Bind.bind(dictMonad.Bind1());
            var pure = Control_Applicative.pure(dictMonad.Applicative0());
            return function (f) {
                return function (m) {
                    return bind(Control_Monad_Writer_Trans.runWriterT(f(Control_Monad_Writer_Trans.runWriterT(m))))(function (v) {
                        return pure(new Data_Tuple.Tuple(v.value0.value0, append(v.value0.value1)(v.value1)));
                    });
                };
            };
        },
        MFunctor0: function () {
            return mfunctorWriterT;
        },
        MonadTrans1: function () {
            return monadTransWriterT;
        }
    };
};
var mfunctorTracedT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return over(Control_Comonad_Traced_Trans.TracedT)(nat);
        };
    }
};
var mfunctorStoreT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return over(Control_Comonad_Store_Trans.StoreT)(lmap(nat));
        };
    }
};
var mfunctorStateT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return function (s) {
                    return nat(Control_Monad_State_Trans.runStateT(m)(s));
                };
            };
        };
    }
};
var mfunctorReaderT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return function (i) {
                    return nat(Control_Monad_Reader_Trans.runReaderT(m)(i));
                };
            };
        };
    }
};
var mmonadReaderT = {
    embed: function (dictMonad) {
        return function (f) {
            return function (m) {
                return function (i) {
                    return Control_Monad_Reader_Trans.runReaderT(f(Control_Monad_Reader_Trans.runReaderT(m)(i)))(i);
                };
            };
        };
    },
    MFunctor0: function () {
        return mfunctorReaderT;
    },
    MonadTrans1: function () {
        return Control_Monad_Reader_Trans.monadTransReaderT;
    }
};
var mfunctorRWS = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return function (r) {
                    return function (s) {
                        return nat(Control_Monad_RWS_Trans.runRWST(m)(r)(s));
                    };
                };
            };
        };
    }
};
var mfunctorProduct = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (v) {
                return new Data_Tuple.Tuple(v.value0, nat(v.value1));
            };
        };
    }
};
var mfunctorMaybe = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return nat(Control_Monad_Maybe_Trans.runMaybeT(m));
            };
        };
    }
};
var mmonadMaybeT = {
    embed: function (dictMonad) {
        var bind = Control_Bind.bind(dictMonad.Bind1());
        var pure = Control_Applicative.pure(dictMonad.Applicative0());
        return function (f) {
            return function (m) {
                return bind(Control_Monad_Maybe_Trans.runMaybeT(f(Control_Monad_Maybe_Trans.runMaybeT(m))))(function (x) {
                    return pure((function () {
                        if (x instanceof Data_Maybe.Nothing) {
                            return Data_Maybe.Nothing.value;
                        };
                        if (x instanceof Data_Maybe.Just && x.value0 instanceof Data_Maybe.Nothing) {
                            return Data_Maybe.Nothing.value;
                        };
                        if (x instanceof Data_Maybe.Just && x.value0 instanceof Data_Maybe.Just) {
                            return new Data_Maybe.Just(x.value0.value0);
                        };
                        throw new Error("Failed pattern match at Control.Monad.Morph (line 130, column 10 - line 133, column 30): " + [ x.constructor.name ]);
                    })());
                });
            };
        };
    },
    MFunctor0: function () {
        return mfunctorMaybe;
    },
    MonadTrans1: function () {
        return Control_Monad_Maybe_Trans.monadTransMaybeT;
    }
};
var mfunctorFree = {
    hoist: function (dictMonad) {
        return Control_Monad_Free.hoistFree;
    }
};
var mmonadFree = {
    embed: function (dictMonad) {
        return foldFree;
    },
    MFunctor0: function () {
        return mfunctorFree;
    },
    MonadTrans1: function () {
        return Control_Monad_Free.freeMonadTrans;
    }
};
var mfunctorExceptT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return nat(Control_Monad_Except_Trans.runExceptT(m));
            };
        };
    }
};
var mmonadExceptT = {
    embed: function (dictMonad) {
        var bind = Control_Bind.bind(dictMonad.Bind1());
        var pure = Control_Applicative.pure(dictMonad.Applicative0());
        return function (f) {
            return function (m) {
                return bind(Control_Monad_Except_Trans.runExceptT(f(Control_Monad_Except_Trans.runExceptT(m))))(function (x) {
                    return pure((function () {
                        if (x instanceof Data_Either.Left) {
                            return new Data_Either.Left(x.value0);
                        };
                        if (x instanceof Data_Either.Right && x.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(x.value0.value0);
                        };
                        if (x instanceof Data_Either.Right && x.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(x.value0.value0);
                        };
                        throw new Error("Failed pattern match at Control.Monad.Morph (line 122, column 10 - line 125, column 33): " + [ x.constructor.name ]);
                    })());
                });
            };
        };
    },
    MFunctor0: function () {
        return mfunctorExceptT;
    },
    MonadTrans1: function () {
        return Control_Monad_Except_Trans.monadTransExceptT;
    }
};
var mfunctorEnvT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return over(Control_Comonad_Env_Trans.EnvT)(map1(nat));
        };
    }
};
var mfunctorCoyoneda = {
    hoist: function (dictMonad) {
        return Data_Coyoneda.hoistCoyoneda;
    }
};
var mfunctorCompose = function (dictFunctor) {
    var map2 = Data_Functor.map(dictFunctor);
    return {
        hoist: function (dictMonad) {
            return function (nat) {
                return function (v) {
                    return map2(nat)(v);
                };
            };
        }
    };
};
var mfunctorCofree = {
    hoist: function (dictMonad) {
        return Control_Comonad_Cofree.hoistCofree(((dictMonad.Bind1()).Apply0()).Functor0());
    }
};
var hoist = function (dict) {
    return dict.hoist;
};
var generalize = function (dictMonad) {
    var $97 = Control_Applicative.pure(dictMonad.Applicative0());
    return function ($98) {
        return $97(unwrap($98));
    };
};
var embed = function (dict) {
    return dict.embed;
};
var flipEmbed = function (dictMMonad) {
    var embed1 = embed(dictMMonad);
    return function (dictMonad) {
        var embed2 = embed1(dictMonad);
        return function (t) {
            return function (f) {
                return embed2(f)(t);
            };
        };
    };
};
var squash = function (dictMonad) {
    return function (dictMMonad) {
        return embed(dictMMonad)(dictMonad)(identity);
    };
};
var composeKleisliRight = function (dictMMonad) {
    var embed1 = embed(dictMMonad);
    return function (dictMonad) {
        var embed2 = embed1(dictMonad);
        return function (f) {
            return function (g) {
                return function (m) {
                    return embed2(g)(f(m));
                };
            };
        };
    };
};
var composeKleisliLeft = function (dictMMonad) {
    var embed1 = embed(dictMMonad);
    return function (dictMonad) {
        var embed2 = embed1(dictMonad);
        return function (g) {
            return function (f) {
                return function (m) {
                    return embed2(g)(f(m));
                };
            };
        };
    };
};
export {
    embed,
    hoist,
    generalize,
    squash,
    composeKleisliRight,
    composeKleisliLeft,
    flipEmbed,
    mfunctorExceptT,
    mfunctorMaybe,
    mfunctorReaderT,
    mfunctorWriterT,
    mfunctorStateT,
    mfunctorRWS,
    mfunctorCompose,
    mfunctorProduct,
    mfunctorYoneda,
    mfunctorCoyoneda,
    mfunctorFree,
    mfunctorCofree,
    mfunctorEnvT,
    mfunctorTracedT,
    mfunctorStoreT,
    mmonadExceptT,
    mmonadMaybeT,
    mmonadReaderT,
    mmonadWriterT,
    mmonadFree,
    mmonadYoneda
};
