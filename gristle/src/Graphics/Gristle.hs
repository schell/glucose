{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Graphics.Gristle where

data Vec2
data Vec4
data Mat4

data VertexShaderType
data FragmentShaderType

data Version a where
  MkVersion :: Integer -> Integer -> Integer -> String -> Version ()

data GetVar typ
data SetVar typ
data GetSetVar typ


data GlobalDeclaration
       (uniforms :: [*])
       (input    :: [*])
       (output   :: [*])
       a
  where
    GDUni :: typ -> String -> GlobalDeclaration '[typ] '[]    '[]    (GetVar typ)
    GDIn  :: typ -> String -> GlobalDeclaration '[]    '[typ] '[]    (GetVar typ)
    GDOut :: typ -> String -> GlobalDeclaration '[]    '[]    '[typ] (SetVar typ)

--type VertexShader   = Shader VertexShaderType
--type FragmentShader = Shader FragmentShaderType
--
--
--
--vertex :: VertexShader '[Vec2, Vec4] '[Mat4, Mat4] '[Vec4]
--vertex = do
--  glPos  <- gl_Position
--
--  pos    <- in_  vec2 "position"
--  color  <- in_  vec4 "color"
--
--  fcolor <- out_ vec4 "fcolor"
--
--  proj   <- uniform_ mat4 "projection"
--  modl   <- uniform_ mat4 "modelview"
--  main () $ do
--    fcolor $= color
--    glPos  $= projection * modelview * mkVec4 (pos.xy)
--
--fragment :: FragmentShader '[Vec4] '[] '[Vec4]
--fragment = do
--  version_ 3 3 0 core
--  fcolor <- in_ Vec4 "fcolor"
--  glFrag <- gl_FragColor
--  main () $ glFrag $= fcolor
