# gristle
`gristle` is a shallow embeddeding of the OpenGL Shading Language in Haskell. It
aims to look as close to actual glsl shader code as possibly, while providing
better compile-time safety. Currently writing shader code in `gristle`'s
@IxShader@ monad will catch variable assignment mismatchs, multiplication
mismatches and some other common errors. It also builds a description of your
shader at the type level to use downstream during buffering and uniform updates.
Lastly, it abstracts over shader code written for opengl and webgl.

## example

```haskell
vertex
  :: forall ctx. HasContext ctx
  => IxShader ctx '[] '[ In      "vec2" "position"
                       , In      "vec4" "color"
                       , Uniform "mat4" "projection"
                       , Uniform "mat4" "modelview"
                       , Out     "vec4" "fcolor"
                       , Out     "vec4" "gl_Position"
                       ] ()
vertex = do
  pos    <- in_ @"vec2" @"position"
  color  <- in_ @"vec4" @"color"

  proj   <- uniform_ @"mat4" @"projection"
  modl   <- uniform_ @"mat4" @"modelview"

  fcolor <- out_ @"vec4" @"fcolor"
  glPos  <- gl_Position

  main_ $ do
    fcolor .= color
    glPos  .= proj .* modl .* mkvec4 (x pos) (y pos) (f 0.0) (f 1.0)

fragment
  :: forall ctx. (HasContext ctx, KnownSymbol (GLFragName ctx))
  => IxShader ctx '[] '[ In  "vec4" "fcolor"
                       , Out "vec4" (GLFragName ctx)
                       ] ()

main = do
  putStrLn "First OpenGL:"
  putSrcLn $ vertex @'OpenGLContext
  putStrLn "\nThen WebGL:"
{-
First OpenGL:
in vec2 position;
in vec4 color;
uniform mat4 projection;
uniform mat4 modelview;
out vec4 fcolor;
void main ()
{ fcolor = color;
  gl_Position = projection * modelview * vec4 (position.x, position.y, 0.0, 1.0);
}

Then WebGL:
attribute vec2 position;
attribute vec4 color;
uniform mat4 projection;
uniform mat4 modelview;
varying vec4 fcolor;
void main ()
{ fcolor = color;
  gl_Position = projection * modelview * vec4 (position.x, position.y, 0.0, 1.0);
}
-}
```
