# README

TODO: description

## Features

TODO

## Syntax

TODO

## Developement

### Prerequisites

Note that these are all installed during setup.

- `purescript`
- `spago`
- `parcel`

### Setup

To get an instance of `shape-lang-purs` built from scratch, do the following:

```
git clone https://github.com/Riib11/shape-lang-purs.git
cd shape-lang-purs
npm install
npm run build
```

### The real building instructions:

```
npm run build ; npm run bundle ; npm run serve
```

### Building

Build the purescript `spago` project via:

```
npm run build
```

To use sourcemaps, first set the environment variable
```
export NODE_OPTIONS=--enable-source-maps
```
or whatever windows uses for environment variable syntax.

### Serving

Serve the app via `parcel` using:

```
npm run serve
```

### Testing 

(For testing changes)

1. Set `module_` to the correct `test*` in `Language.Shape.Stlc.Initial`. For example, `test1`.
2. `npm run build`
3. `npm run serve`
4. Open the webapp. Perform the changes you want until a bug or whatever else
5. In the console, copy the log that appears directly after "===[ changeHistory ]===" logging header. You now have the  `ChangeHistory`, as a string, that results in the current state.
6. In `Test.ChangeHistory`, add a new element to the array defining `changeHistories`. The format is `<label> /\ <initial module> /\ <change history>`. So, pick a `<label>`, and use the same `Initial.test*` that you specified in step 1. For the `<change history>` paste the text you copied in step 5.
7. In `Test.Main`, change the `main` function has a line `ChangeHistory.runChangeHistory $ Map.lookup' <label> ChangeHistory.changeHistories`. Change the `<label>`> to the one you specified in step 6.
7.5. For stacktraces: On windows, type $Env:NODE_OPTIONS = "--enable-source-maps". On unix, type...
7.6. Also for stacktraces: Run `npm run build`
8. Run `npm run test` in the terminal. This should output some logging information corresponding to running your change history.
9. Add loggers to `Test.ChangeHistory.runChangeHistory` if you want to print out special information in your test.

## Deployment

To deploy the GitHub pages powered web-app, run the following commands:
```sh
git fetch
git pull
npm install
npm run deploy
git add ./docs/
git commit -m"deploy"
git push
```


## Similar Projects

The following are some projects that have some relevant overlaping goals with
Shape:
- [Hazel](https://hazel.org/)
  - a type-sensitive structural editor built on gradual typing
- [Alfa](https://cth.altocumulus.org/~hallgren/Alfa/index.html)
- [Meta Programming System (MPS)](https://www.jetbrains.com/mps/)
  - a framework for defining DSLs and automatically generating a UI
  - user study: https://dl.acm.org/doi/pdf/10.1145/2950290.2950315
- [Lambdu](http://www.lamdu.org)
- [Scratch](https://scratch.mit.edu)
  - UI design for a structural editor
 - aimed at teaching programming
- [awesome-structure-editors](https://github.com/yairchu/awesome-structure-editors)
  - collection of structure editors
- [Tylr](https://tylr.fun)
  - the first _tile-based_ editor
- [Holbert](http://liamoc.net/holbert)
  - mathematical notation
