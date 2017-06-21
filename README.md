# glucose
`glucose` is a wrapper around different opengl APIs which allows you to write
graphics code in a platform generic way. The goal here is to improve
cross-platform graphics development as well as "write once, run anywhere"
Haskell code. `glucose` code is meant to automatically choose a backend at
compile time based on cabal flags. It currently has three compilation targets:

* opengl + ghc
* webgl + ghc (jsaddle)
* webgl + ghcjs

The second target will allow you to work with code bound for ghcjs while still
using ghci based tools (intero, ghcid + cabal repl, etc).

## methodology
`glucose` uses a mixture of the "[scrap your
typeclasses](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)"
record strategy along with (oddly enough) a typeclass to keep you from having
to pass around an unruly record with 24 type variables. The record abstracts
over the gl API while the typeclass abstracts over the record.

Most graphics code will look almost identical to its C counterpart (if you're
used to looking at Haskell and C), given a few assumptions like using
`RecordWildCards` to bring the gl functions into scope.

## example code
```haskell
compileShader
  :: forall a. IsGLES a
  => a
  -> GLEnum a
  -> String
  -> (M a) (Either String (GLShader a))
compileShader gl shtype src = do
  let GLES{..} = gles gl
  s <- glCreateShader shtype
  glShaderSource s src
  glCompileShader s
  glGetShaderParameter s gl_COMPILE_STATUS >>= \case
    Right _ -> return $ Left "Glucose library error, this should never happen."
    Left success
      | success == false -> Left <$> glGetShaderInfoLog s
      | otherwise -> return $ Right s
```
## building
Building should be simple and depends on stack.

_opengl + ghc_

    stack build

_webgl + ghc_

    stack build --flags glucose:webgl

_webgl + ghcjs_

    stack build --stack-yaml ghcjs-stack.yaml

That's it!

## my list of things that are needed

- [ ] abstract over GLSL to write once, run anywhere

- [x] add VAO extensions to the main GLES type

  - [x] w/ opengl 3   implementation

  - [x] w/ webgl  ext implementation

  - [ ] w/ webgl  2   implementation

- [ ] add `TypedArray` instances to `jsaddle` to make marshalling easier, or
      convert all array types to
      `(Foldable f, Functor f, Storable a) => Vector (f a)`.
