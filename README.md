# glucose and gristle
`glucose` and `gristle` abstract over opengl/webgl and glsl, respectively.
`glucose` is the gl portion and `gristle` is the glsl portion. Together they
allow write once, run "anywhere" opengl code.

## glucose
The interesting thing about `glucose` is that it uses a gigantic record to wrap
the “least common denominator” of the opengl/webgl apis, but then uses a
typeclass to keep you from having to pass that horrible record around, lol.

## gristle
The interesting thing about gristle is that it uses indexed monads to build up
a shader's source statement by statement. It only typechecks certain things
(currently uniforms, gpu inputs, gpu outputs, variable assignment, scalar and
vector matrix multiplication). The library started with no statements being
typechecked and can gradually be updated to typecheck more and more.

It’s literally like the stupidest indexed monad ever.

You can see the shaders being built [here](https://github.com/schell/glucose/blob/master/glucose-example/glucose-example-shared/src/Graphics/Glucose/Shared/Shaders.hs)

## my list of things that are needed

- [x] abstract over GLSL to write once, run anywhere

- [x] add VAO extensions to the main GLES type

  - [x] w/ opengl 3   implementation

  - [x] w/ webgl  ext implementation

  - [ ] w/ webgl  2   implementation

- [ ] add `TypedArray` instances to `jsaddle` to make marshalling easier, or
      convert all array types to
      `(Foldable f, Functor f, Storable a) => Vector (f a)`.
