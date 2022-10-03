
*Note: This is still a work in progress. Not yet recommended to use. th following instructions may not work yet*

# purescript-typescript-bridge

- A PureScript library for type class based TypeScript type generation.
- A CLI that generates boilerplate code for the library usage


# Install

```
spago init
spago install typescript-bridge
```

```
yarn add purescript-typescript-bridge-cli
```

Add some types and values to a module. E.g. to `src/Sample.purs`:

```hs
module Sample (State, Foo, Id) where

data State = On | Off Int | Loading

type Foo = {
    name :: String,
    age :: Int,
    loggedIn :: Boolean
}

newtype Id = Id String

user :: User
user = {
    name: "Me",
    age: 23,
    loggedIn: true
}

myFn :: Int -> String -> Boolean
myFn n str = true
```


# Sample Project

[Link](https://github.com/


# TODO

- [ ] Tidy the whole doc aftercode genration
- [ ] Apply prettier to JSON
- [ ] Use "unsupported reason" ADT
- [ ] Add Text to CLI Options
- [ ] Allow passing globs to CLI, make spago for this purpose optional
- [ ] Publish CLI to NPM (Add Pipeline)
- [ ] Fix SourcePosition calculation
- [ ] Use error counter (read/write) and reflect in exit code