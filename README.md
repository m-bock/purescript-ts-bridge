_Note: This is still a work in progress. Not yet recommended to use. th following instructions may not work yet_

# purescript-typescript-bridge

- A PureScript library for type class based TypeScript type generation.
- A CLI that generates boilerplate code for the library usage

## How It works

## Prior Projects

## Getting started

You can either follow the instructions below or check out the [sample project](https://github.com/thought2/purescript-typescript-bridge.sample-project) that contains a fully working pipeline.

1. Project Setup

   - If you start a new project run `spago init`, otherwise `cd` to an existing project.

   - Make sure you have a `package.json` in your project. E.g. run `yarn init -y` to create one.

2. Installation of `purs-ts-bridge-cli` CLI

   ```
   yarn add purs-ts-bridge-cli
   # for now: yarn add https://github.com/thought2/purescript-typescript-bridge#dist
   ```

3. Add some PureScript types

   - Add some PureScript types and values to a module. E.g. to

     `./src/Sample.purs`:

     ```hs
     module Sample (State, Foo, Id) where

     data State = On | Off Int | Loading
     ```

   - Build your project

     ```
     spago build
     ```

4. Generate `ts-bridge` project

   - Run the CLI

     ```sh
     yarn run purs-ts-bridge
     ```

     If you used the default options, you should now see a new subfolder called `ts-bridge` inside your project directory. It conatins a full spago project that will be able to generate TypeScript types.

   - Compile

     We should now built this project with the following command:

     ```bash
     bash -c 'cd ts-bridge; spago build --purs-args "--output ../output"'
     ```

     This uses the output folder of your main project for better compiling performance and IDE support.

5. Generate TypeScript types

   - Create a launcher script
     E.g:

     `./generate-ts-types.js`

     ```js
     import("./output/MyTsBridgeModules/index.js").then((x) => x.main());
     ```

   - Run the launcher

     ```sh
     node generate-ts-types --output-dir generated-ts-types
     ```

     Inside the directory `generated-types` you should now find two `.d.ts` files containing the generated types. In many cases you'd generate those files directly to you output to have them side by side to the actual `.js` files.

6. Verify result

   - Make sure you have a like:

     `./tsconfig.json`

     ```json
     {
       "compilerOptions": {
         "paths": {
           "~/*": ["./generated-ts-types/*"]
         },
         "strict": true,
         "skipLibCheck": false
       }
     }
     ```

   - Run `tsc`

# Sample Project

[Link](https://github.com/thought2/purescript-typescript-bridge.sample-project)

# TODO

- [x] Tidy the whole doc aftercode generation
- [x] Apply prettier to JSON
- [ ] Use "unsupported reason" ADT
- [x] Implement `tsUnsupported` function
- [x] Add Text to CLI Options
- [ ] Allow passing globs to CLI, make spago for this purpose optional
- [ ] Publish CLI to NPM (Add Pipeline)
- [x] Fix SourcePosition calculation
- [x] Use error counter (read/write) and reflect in exit code
- [ ] Manage exports
- [x] Write full end to end test

Bugs
- [ ] Check globbing e.g. [ "Sample" ]
- [ ] Add .gitignore to ts-bridge

Instance printing

- [x] Data types (Opaque)
- [ ] Newtypes
- [ ] Multiple Arguments
- [ ] Foreign imports

Module prinitng

- [x] Data types (Opaque)
- [ ] Values
- [ ] Type Alias
- [ ] Newtypes
- [ ] Data Types
- [ ] Multiple Arguments
- [ ] Foreign imports

CI

- [ ] Improve CI pipeline

Code Quality
- Remove `ErrLiteral` and `LogLiteral`
- Remove warnings