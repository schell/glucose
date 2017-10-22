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
`glucose` uses the "[scrap your
typeclasses](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)"
record strategy along with open type families on an explicitly kinded type
variable (`a :: GLBackend`) to abstract over the gl API.

Most graphics code will look almost identical to its C counterpart (if you're
used to looking at Haskell and C), given a few assumptions like using
`RecordWildCards` to bring the gl functions into scope.

## example code
```haskell
compileShader
  :: forall m (a :: GLBackend). (Monad m, Eq (GLBoolean a))
  => GLES m a
  -> GLEnum a
  -> String
  -> m (Either String (GLShader a))
compileShader GLES{..} shtype src = do
  s <- glCreateShader shtype
  glShaderSource s src
  glCompileShader s
  glGetShaderParameter s gl_COMPILE_STATUS >>= \case
    Right _ -> return $ Left "Glucose library error, this should never happen."
    Left success
      | success == false -> Left <$> glGetShaderInfoLog s
      | otherwise -> return $ Right s
```

It wraps both OpenGL (targeting 3.2+) and WebGL in a "least common denominator"
fasion. Functions that exist in both APIs exist in glucose and functions from one
API that can be written in terms of the other also exist, though they might
exist in a slightly different form, either for simplicity or common sense's sake.

## building
Building should be simple and depends on stack.

_opengl + ghc_

    stack build

_webgl + ghc_

    stack build --flags glucose:webgl

_webgl + ghcjs_

    stack build --stack-yaml ghcjs-stack.yaml

That's it!
