{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Glucose where

import           Data.Bits            (Bits)
import           Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import           Data.Word            (Word32)
import           Linear               (M22, M33, M44, V2, V3, V4)

--------------------------------------------------------------------------------
data GLBackend = GLBackendOpenGL
               | GLBackendWebGL

data ShaderPrecisionFormat a = ShaderPrecisionFormat { spfRangeMin  :: a
                                                     , spfRangeMax  :: a
                                                     , spfPrecision :: a
                                                     }

data ActiveInfo a = ActiveInfo { aiSize :: a
                               , aiType :: a
                               , aiName :: String
                               }

data TypedVector = FloatVector (Vector Float)
                 | IntVector   (Vector Int)
                 | UintVector  (Vector Word32)

class Storable a => ToTypedVector a where
  toTypedVector :: Vector a -> TypedVector

instance ToTypedVector Float where
  toTypedVector = FloatVector

instance ToTypedVector Double where
  toTypedVector = FloatVector . V.map realToFrac

instance ToTypedVector Int where
  toTypedVector = IntVector

instance ToTypedVector Word32 where
  toTypedVector = UintVector

instance ToTypedVector Word where
  toTypedVector = UintVector . V.map fromIntegral


--class ( Monad m
--      , Eq (GLGLBoolean A a)
--      , Num (GLFloat a), Num (GLEnum a), Num (GLClampf a), Num (GLInt a)
--      , Num (GLUint a) , Num (GLSizei a), Num (GLIntptr a)
--      , GLInt aegral (GLEnum a)
--      , Bits (GLEnum a), Show (GLEnum a)
--      , Storable (GLFloat a), Storable (GLInt a), Storable (GLUint a)
--      ) => IsGLES (m :: * -> *) a where

type family GLProgram (a :: GLBackend)
type family GLShader (a :: GLBackend)
type family GLTexture (a :: GLBackend)
type family GLUniformLocation (a :: GLBackend)
type family GLClampf (a :: GLBackend)
type family GLFloat (a :: GLBackend)
type family GLEnum (a :: GLBackend)
type family GLUint (a :: GLBackend)
type family GLInt (a :: GLBackend)
type family GLIntptr (a :: GLBackend)
type family GLBoolean (a :: GLBackend)
type family GLSizei (a :: GLBackend)
type family GLPtr (a :: GLBackend)
type family GLImageData (a :: GLBackend)
type family GLBuffer (a :: GLBackend)
type family GLFramebuffer (a :: GLBackend)
type family GLRenderbuffer (a :: GLBackend)
type family GLVertexArrayObject (a :: GLBackend)
type family GLExtension (a :: GLBackend)

data GLES (m :: * -> *) (a :: GLBackend) =
  GLES { glActiveTexture :: GLEnum a -> m ()
       , glAttachShader :: GLProgram a -> GLShader a -> m ()
       , glBindAttribLocation :: GLProgram a -> GLUint a -> String -> m ()
       , glBindBuffer :: GLEnum a -> GLBuffer a -> m ()
       , glBindFramebuffer :: GLEnum a -> GLFramebuffer a -> m ()
       , glBindRenderbuffer :: GLEnum a -> GLRenderbuffer a -> m ()
       , glBindTexture :: GLEnum a -> GLTexture a -> m ()
       , glBindVertexArray :: GLVertexArrayObject a -> m ()
       , noVertexArray :: GLVertexArrayObject a
       , glBlendColor :: GLClampf a -> GLClampf a -> GLClampf a -> GLClampf a -> m ()
       , glBlendEquation :: GLEnum a -> m ()
       , glBlendEquationSeparate :: GLEnum a -> GLEnum a -> m ()
       , glBlendFunc :: GLEnum a -> GLEnum a -> m ()
       , glBlendFuncSeparate :: GLEnum a -> GLEnum a -> GLEnum a -> GLEnum a -> m ()
       , glBufferData
           :: forall v. (Storable v, ToTypedVector v)
           => GLEnum a -> Vector v -> GLEnum a -> m ()
       , glBufferSubData
           :: forall v. (Storable v, ToTypedVector v)
           => GLEnum a -> GLIntptr a -> Vector v -> m ()
       , glCheckFramebufferStatus :: GLEnum a -> m (GLEnum a)
       , glClear :: Word32 -> m ()
       , glClearColor :: GLClampf a -> GLClampf a -> GLClampf a -> GLClampf a -> m ()
       , glClearDepth :: GLClampf a -> m ()
       , glClearStencil :: GLInt a -> m ()
       , glColorMask :: GLBoolean a -> GLBoolean a -> GLBoolean a -> GLBoolean a -> m ()
       , glCompileShader :: GLShader a -> m ()
         -- | Copies pixels from the current @framebuffer@ GLInt ao a 2D (GLTexture a) image.
       , glCopyTexImage2D
          :: GLEnum a
          -- ^ target
          -- A GLGLEnum a specifying the binding point target of the active texture. Possible values:
          --   * gl_TEXTURE_2D: A two-dimensional texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_X: Positive X face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_X: Negative X face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Y: Positive Y face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Y: Negative Y face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Z: Positive Z face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Z: Negative Z face for a cube-mapped texture.
          -> GLInt a
          -- ^ level
          -- A GLint specifying the level of detail. Level 0 is the base image level
          -- and level n is the nth mipmap reduction level.
          -> GLEnum a
          -- ^ GLInt aernal format
          -- A GLint specifying the color components in the texture. Possible values:
          --   * gl_ALPHA: Discards the red, green and blue components and reads the alpha component.
          --   * gl_RGB: Discards the alpha components and reads the red, green and blue components.
          --   * gl_RGBA: Red, green, blue and alpha components are read from the color buffer.
          --   * gl_LUMINANCE: Each color component is a luminance component, alpha is 1.0.
          --   * gl_LUMINANCE_ALPHA: Each component is a luminance/alpha component.
          -> GLInt a
          -- ^ A GLint specifying the x coordinate of the lower left corner where to
          -- start copying.
          -> GLInt a
          -- ^ A GLint specifying the y coordinate of the lower left corner where to
          -- start copying.
          -> GLSizei a
          -- ^ A GLsizei specifying the width of the texture.
          -> GLSizei a
          -- ^ A GLsizei specifying the height of the texture.
          -> GLInt a
          -- ^ This parameter is ignored, but the WebGL docs say this:
          -- A GLint specifying the width of the border. Must be 0.
          -> m ()
       , glCopyTexSubImage2D
          :: GLEnum a
          -- ^ target
          -- A GLGLEnum a specifying the binding point target of the active texture. Possible values:
          --   * gl_TEXTURE_2D: A two-dimensional texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_X: Positive X face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_X: Negative X face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Y: Positive Y face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Y: Negative Y face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Z: Positive Z face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Z: Negative Z face for a cube-mapped texture.
          -> GLInt a
          -- ^ level
          -- A GLint specifying the level of detail. Level 0 is the base image level
          -- and level n is the nth mipmap reduction level.
          -> GLInt a
          -- ^ xoffset
          -- A GLint specifying the horizontal offset within the (GLTexture a) image.
          -> GLInt a
          -- ^ yoffset
          -- A GLint specifying the vertical offset within the (GLTexture a) image.
          -> GLInt a
          -- ^ A GLint specifying the x coordinate of the lower left corner where to
          -- start copying.
          -> GLInt a
          -- ^ A GLint specifying the y coordinate of the lower left corner where to
          -- start copying.
          -> GLSizei a
          -- ^ A GLsizei specifying the width of the texture.
          -> GLSizei a
          -- ^ A GLsizei specifying the height of the texture.
          -> m ()
       , glCreateBuffer :: m (GLBuffer a)
       , glCreateFramebuffer :: m (GLFramebuffer a)
       , glCreateProgram :: m (GLProgram a)
       , glCreateRenderbuffer :: m (GLRenderbuffer a)
       , glCreateShader :: GLEnum a -> m (GLShader a)
       , glCreateTexture :: m (GLTexture a)
       , glCreateVertexArray :: m (GLVertexArrayObject a)
       , glCullFace
          :: GLEnum a
          -- ^ mode
          -- A GLGLEnum a specifying whether front- or back-facing polygons are candidates
          -- for culling. The default value is gl.BACK. Possible values are:
          --   * gl_FRONT
          --   * gl_BACK
          --   * gl_FRONT_AND_BACK
          -> m ()

       , glDeleteBuffer :: GLBuffer a -> m ()
       , glDeleteFramebuffer :: GLFramebuffer a -> m ()
       , glDeleteProgram :: GLProgram a -> m ()
       , glDeleteRenderbuffer :: GLRenderbuffer a -> m ()
       , glDeleteShader :: GLShader a -> m ()
       , glDeleteTexture :: GLTexture a -> m ()
       , glDeleteVertexArray :: GLVertexArrayObject a -> m ()
         -- | Specifies a function that compares incoming pixel depth to the current
         -- depth (GLBuffer a) value
       , glDepthFunc
          :: GLEnum a
          -- ^ func
          -- A GLGLEnum a specifying the depth comparison function, which sets the
          -- conditions under which the pixel will be drawn. The default value is
          -- gl_LESS. Possible values are:
          --   * gl_NEVER (never pass)
          --   * gl_LESS (pass if the incoming value is less than the depth (GLBuffer a) value)
          --   * gl_EQUAL (pass if the incoming value equals the the depth (GLBuffer a) value)
          --   * gl_LEQUAL (pass if the incoming value is less than or equal to the depth (GLBuffer a) value)
          --   * gl_GREATER (pass if the incoming value is greater than the depth (GLBuffer a) value)
          --   * gl_NOTEQUAL (pass if the incoming value is not equal to the depth (GLBuffer a) value)
          --   * gl_GEQUAL (pass if the incoming value is greater than or equal to the depth (GLBuffer a) value)
          --   * gl_ALWAYS (always pass)
          -> m ()

         -- | Sets whether writing (GLInt a)o the depth (GLBuffer a) is enabled or disabled.
       , glDepthMask
          :: GLBoolean a
          -- ^ flag
          -- A GLGLBoolean a specifying whether or not writing (GLInt a)o the depth (GLBuffer a) is
          -- enabled. Default value: true, meaning that writing is enabled.
          -> m ()

         -- | Specifies the depth range mapping from normalized device coordinates to
         -- window or viewport coordinates.
       , glDepthRange
          :: GLClampf a
          -- ^ zNear
          -- A @clampf@ specifying the mapping of the near clipping plane to window
          -- or viewport coordinates. Clamped to the range 0 to 1 and must be less
          -- than or equal to zFar. The default value is 0.
          -> GLClampf a
          -- ^ zFar
          -- A @clampf@ specifying the mapping of the far clipping plane to window or
          -- viewport coordinates. Clamped to the range 0 to 1. The default value is 1.
          -> m ()

         -- | Detaches a previously attached @GLShader a@ from a @GLProgram a@.
       , glDetachShader
          :: GLProgram a
          -- ^ The (GLProgram a) to detach the (GLShader a) from.
          -> GLShader a
          -- ^ The fragment or vertex (GLShader a) to detach.
          -> m ()

       , glDisable :: GLEnum a -> m ()
       , glDisableVertexAttribArray :: GLUint a -> m ()
       , glDrawArrays :: GLEnum a -> GLInt a -> GLSizei a -> m ()
       , glDrawElements :: GLEnum a -> GLSizei a -> GLEnum a -> GLPtr a -> m ()
       , glEnable :: GLEnum a -> m ()
       , glEnableVertexAttribArray :: GLUint a -> m ()
       , glFinish ::  m ()
       , glFlush ::  m ()
       , glFramebufferRenderbuffer :: GLEnum a -> GLEnum a -> GLEnum a -> GLRenderbuffer a -> m ()
       , glFramebufferTexture2D :: GLEnum a -> GLEnum a -> GLEnum a -> GLTexture a -> GLInt a -> m ()
       , glFrontFace :: GLEnum a -> m ()
       , glGenerateMipmap :: GLEnum a -> m ()
       , glGetActiveAttrib :: GLProgram a -> GLUint a -> m (ActiveInfo (GLInt a))
       , glGetActiveUniform :: GLProgram a -> GLUint a -> m (ActiveInfo (GLInt a))
       , glGetAttachedShaders :: GLProgram a -> m [GLShader a]
       , glGetAttribLocation :: GLProgram a -> String -> m (GLInt a)
       , glGetBufferParameter :: GLEnum a -> GLEnum a -> m (Either (GLInt a) (GLEnum a))
       , glGetError ::  m (GLEnum a)
       , glGetExtension :: String -> m (Maybe (GLExtension a))
       , glGetFramebufferAttachmentParameter :: GLEnum a -> GLEnum a -> GLEnum a -> m (Maybe (GLEnum a), Maybe (GLTexture a), Maybe (GLInt a))
       , glGetProgramParameter :: GLProgram a -> GLEnum a -> m (Either (GLBoolean a) (GLInt a))
       , glGetProgramInfoLog :: GLProgram a -> m String
       , glGetRenderbufferParameter :: GLEnum a -> GLEnum a -> m (Either (GLInt a) (GLEnum a))
       , glGetShaderParameter :: GLShader a -> GLEnum a -> m (Either (GLBoolean a) (GLEnum a))
       , glGetShaderInfoLog :: GLShader a -> m String
       , glGetShaderPrecisionFormat :: GLEnum a -> GLEnum a -> m (ShaderPrecisionFormat (GLInt a))
       , glGetShaderSource :: GLShader a -> m String
       , glGetSupportedExtensions ::  m [String]
       , glGetTexParameter :: GLEnum a -> GLEnum a -> m (Maybe (GLEnum a), Maybe (GLBoolean a), Maybe (GLUint a), Maybe (GLInt a), Maybe (GLFloat a))
       , glGetUniformfv
           :: forall v. (Storable v, Fractional v)
           => GLProgram a -> GLUniformLocation a -> Int -> m (Vector v)
       , glGetUniformiv
           :: forall v. (Storable v, Integral v)
           => GLProgram a -> GLUniformLocation a -> Int -> m (Vector v)
       , glGetUniformLocation :: GLProgram a -> String -> m (GLUniformLocation a)
       , glGetVertexAttribfv
           :: forall v. (Storable v, Fractional v)
           => GLUint a -> GLEnum a -> Int -> m (Vector v)
       , glGetVertexAttribiv
           :: forall v. (Storable v, Integral v)
           => GLUint a -> GLEnum a -> Int -> m (Vector v)
       , glHint :: GLEnum a -> GLEnum a -> m ()
       , glIsBuffer :: GLBuffer a -> m (GLBoolean a)
       , glIsContextLost ::  m (GLBoolean a)
       , glIsEnabled :: GLEnum a -> m (GLBoolean a)
       , glIsFramebuffer :: GLFramebuffer a -> m (GLBoolean a)
       , glIsProgram :: GLProgram a -> m (GLBoolean a)
       , glIsRenderbuffer :: GLRenderbuffer a -> m (GLBoolean a)
       , glIsShader :: GLShader a -> m (GLBoolean a)
       , glIsTexture :: GLTexture a -> m (GLBoolean a)
       , glIsVertexArray :: GLVertexArrayObject a -> m (GLBoolean a)
       , glLineWidth :: GLFloat a -> m ()
       , glLinkProgram :: GLProgram a -> m ()
       , glPixelStorei :: GLEnum a -> GLInt a -> m ()
       , glPolygonOffset :: GLFloat a -> GLFloat a -> m ()
       , glReleaseShaderCompiler ::  m ()
       , glRenderbufferStorage :: GLEnum a -> GLEnum a -> GLSizei a -> GLSizei a -> m ()
       , glSampleCoverage :: GLClampf a -> GLBoolean a -> m ()
       , glScissor :: GLInt a -> GLInt a -> GLSizei a -> GLSizei a -> m ()
       , glShaderSource :: GLShader a -> String -> m ()
       , glStencilFunc :: GLEnum a -> GLInt a -> GLUint a -> m ()
       , glStencilFuncSeparate :: GLEnum a -> GLEnum a -> GLInt a -> GLUint a -> m ()
       , glStencilMask :: GLUint a -> m ()
       , glStencilMaskSeparate :: GLEnum a -> GLUint a -> m ()
       , glStencilOp :: GLEnum a -> GLEnum a -> GLEnum a -> m ()
       , glStencilOpSeparate :: GLEnum a -> GLEnum a -> GLEnum a -> GLEnum a -> m ()
       , glTexParameterf :: GLEnum a -> GLEnum a -> GLFloat a -> m ()
       , glTexParameteri :: GLEnum a -> GLEnum a -> GLInt a -> m ()
       --, glTexImage2DData :: GLEnum a -> GLInt a -> GLEnum a -> GLEnum a -> GLEnum a -> GLImageData a -> m ()
       , glTexImage2D :: GLEnum a -> GLInt a -> GLInt a -> GLEnum a -> GLEnum a -> GLImageData a -> m ()
       --, glTexImage2DCanvas :: GLEnum a -> GLInt a -> GLEnum a -> GLEnum a -> GLEnum a -> HTMLCanvasElement -> m ()
       --, glTexImage2DVideo :: GLEnum a -> GLInt a -> GLEnum a -> GLEnum a -> GLEnum a -> HTMLVideoElement -> m ()
       --, glTexSubImage2DData :: GLEnum a -> GLInt a -> GLInt a -> GLInt a -> GLEnum a -> GLEnum a -> GLImageData a -> m ()
       , glTexSubImage2D :: GLEnum a -> GLInt a -> GLInt a -> GLInt a -> GLEnum a -> GLEnum a -> GLImageData a -> m ()
       --, glTexSubImage2DCanvas :: GLEnum a -> GLInt a -> GLInt a -> GLInt a -> GLEnum a -> GLEnum a -> HTMLCanvasElement -> m ()
       --, glTexSubImage2DVideo :: GLEnum a -> GLInt a -> GLInt a -> GLInt a -> GLEnum a -> GLEnum a -> HTMLVideoElement -> m ()
       , glUniform1f :: GLUniformLocation a -> GLFloat a -> m ()
       , glUniform1fv
           :: forall v. (Real v, Storable v)
           => GLUniformLocation a -> Vector v -> m ()
       , glUniform1i :: GLUniformLocation a -> GLInt a -> m ()
       , glUniform1iv
           :: forall v. (Integral v, Storable v)
           => GLUniformLocation a -> Vector v -> m ()
       , glUniform2f :: GLUniformLocation a -> GLFloat a -> GLFloat a -> m ()
       , glUniform2fv :: forall v. (Real v, Storable v) => GLUniformLocation a -> Vector (V2 v) -> m ()
       , glUniform2i :: GLUniformLocation a -> GLInt a -> GLInt a -> m ()
       , glUniform2iv :: forall v. (Integral v, Storable v) => GLUniformLocation a -> Vector (V2 v) -> m ()
       , glUniform3f :: GLUniformLocation a -> GLFloat a -> GLFloat a -> GLFloat a -> m ()
       , glUniform3fv :: forall v. (Real v, Storable v) => GLUniformLocation a -> Vector (V3 v) -> m ()
       , glUniform3i :: GLUniformLocation a -> GLInt a -> GLInt a -> GLInt a -> m ()
       , glUniform3iv :: forall v. (Integral v, Storable v) => GLUniformLocation a -> Vector (V3 v) -> m ()
       , glUniform4f :: GLUniformLocation a -> GLFloat a -> GLFloat a -> GLFloat a -> GLFloat a -> m ()
       , glUniform4fv :: forall v. (Real v, Storable v) => GLUniformLocation a -> Vector (V4 v) -> m ()
       , glUniform4i :: GLUniformLocation a -> GLInt a -> GLInt a -> GLInt a -> GLInt a -> m ()
       , glUniform4iv :: forall v. (Integral v, Storable v) => GLUniformLocation a -> Vector (V4 v) -> m ()
       , glUniformMatrix2fv :: forall v. (Real v, Storable v) => GLUniformLocation a -> GLBoolean a -> Vector (M22 v) -> m ()
       , glUniformMatrix3fv :: forall v. (Real v, Storable v) => GLUniformLocation a -> GLBoolean a -> Vector (M33 v) -> m ()
       , glUniformMatrix4fv :: forall v. (Real v, Storable v) => GLUniformLocation a -> GLBoolean a -> Vector (M44 v) -> m ()
       , glUseProgram :: GLProgram a -> m ()
       , glValidateProgram :: GLProgram a -> m ()
       , glVertexAttrib1f :: GLUint a -> GLFloat a -> m ()
       --, glVertexAttrib1fv :: forall a. (Real a, Storable a) => GLUint a -> Vector a -> m ()
       , glVertexAttrib2f :: GLUint a -> GLFloat a -> GLFloat a -> m ()
       --, glVertexAttrib2fv :: forall a. (Real a, Storable a) => GLUint a -> Vector a -> m ()
       , glVertexAttrib3f :: GLUint a -> GLFloat a -> GLFloat a -> GLFloat a -> m ()
       --, glVertexAttrib3fv :: forall a. (Real a, Storable a) => GLUint a -> Vector a -> m ()
       , glVertexAttrib4f :: GLUint a -> GLFloat a -> GLFloat a -> GLFloat a -> GLFloat a -> m ()
       --, glVertexAttrib4fv :: forall a. (Real a, Storable a) => GLUint a -> Vector a -> m ()
       , glVertexAttribPointer :: GLUint a -> GLInt a -> GLEnum a -> GLBoolean a -> GLSizei a -> GLIntptr a -> m ()
       , glViewport :: GLInt a -> GLInt a -> GLSizei a -> GLSizei a -> m ()

       , gl_DEPTH_BUFFER_BIT :: GLEnum a
       , gl_STENCIL_BUFFER_BIT :: GLEnum a
       , gl_COLOR_BUFFER_BIT :: GLEnum a
       , gl_POINTS :: GLEnum a
       , gl_LINES :: GLEnum a
       , gl_LINE_LOOP :: GLEnum a
       , gl_LINE_STRIP :: GLEnum a
       , gl_TRIANGLES :: GLEnum a
       , gl_TRIANGLE_STRIP :: GLEnum a
       , gl_TRIANGLE_FAN :: GLEnum a
       , gl_ZERO :: GLEnum a
       , gl_ONE :: GLEnum a
       , gl_SRC_COLOR :: GLEnum a
       , gl_ONE_MINUS_SRC_COLOR :: GLEnum a
       , gl_SRC_ALPHA :: GLEnum a
       , gl_ONE_MINUS_SRC_ALPHA :: GLEnum a
       , gl_DST_ALPHA :: GLEnum a
       , gl_ONE_MINUS_DST_ALPHA :: GLEnum a
       , gl_DST_COLOR :: GLEnum a
       , gl_ONE_MINUS_DST_COLOR :: GLEnum a
       , gl_SRC_ALPHA_SATURATE :: GLEnum a
       , gl_FUNC_ADD :: GLEnum a
       --, gl_BLEND_EQUATION :: GLEnum a
       , gl_BLEND_EQUATION_RGB :: GLEnum a
       , gl_BLEND_EQUATION_ALPHA :: GLEnum a
       , gl_FUNC_SUBTRACT :: GLEnum a
       , gl_FUNC_REVERSE_SUBTRACT :: GLEnum a
       , gl_BLEND_DST_RGB :: GLEnum a
       , gl_BLEND_SRC_RGB :: GLEnum a
       , gl_BLEND_DST_ALPHA :: GLEnum a
       , gl_BLEND_SRC_ALPHA :: GLEnum a
       , gl_CONSTANT_COLOR :: GLEnum a
       , gl_ONE_MINUS_CONSTANT_COLOR :: GLEnum a
       , gl_CONSTANT_ALPHA :: GLEnum a
       , gl_ONE_MINUS_CONSTANT_ALPHA :: GLEnum a
       , gl_BLEND_COLOR :: GLEnum a
       , gl_ARRAY_BUFFER :: GLEnum a
       , gl_ELEMENT_ARRAY_BUFFER :: GLEnum a
       , gl_ARRAY_BUFFER_BINDING :: GLEnum a
       , gl_ELEMENT_ARRAY_BUFFER_BINDING :: GLEnum a
       , gl_STREAM_DRAW :: GLEnum a
       , gl_STATIC_DRAW :: GLEnum a
       , gl_DYNAMIC_DRAW :: GLEnum a
       , gl_BUFFER_SIZE :: GLEnum a
       , gl_BUFFER_USAGE :: GLEnum a
       , gl_CURRENT_VERTEX_ATTRIB :: GLEnum a
       , gl_FRONT :: GLEnum a
       , gl_BACK :: GLEnum a
       , gl_FRONT_AND_BACK :: GLEnum a
       , gl_TEXTURE_2D :: GLEnum a
       , gl_CULL_FACE :: GLEnum a
       , gl_BLEND :: GLEnum a
       , gl_DITHER :: GLEnum a
       , gl_STENCIL_TEST :: GLEnum a
       , gl_DEPTH_TEST :: GLEnum a
       , gl_SCISSOR_TEST :: GLEnum a
       , gl_POLYGON_OFFSET_FILL :: GLEnum a
       , gl_SAMPLE_ALPHA_TO_COVERAGE :: GLEnum a
       , gl_SAMPLE_COVERAGE :: GLEnum a
       , gl_NO_ERROR :: GLEnum a
       , gl_INVALID_ENUM :: GLEnum a
       , gl_INVALID_VALUE :: GLEnum a
       , gl_INVALID_OPERATION :: GLEnum a
       , gl_OUT_OF_MEMORY :: GLEnum a
       , gl_CW :: GLEnum a
       , gl_CCW :: GLEnum a
       , gl_LINE_WIDTH :: GLEnum a
       , gl_ALIASED_POINT_SIZE_RANGE :: GLEnum a
       , gl_ALIASED_LINE_WIDTH_RANGE :: GLEnum a
       , gl_CULL_FACE_MODE :: GLEnum a
       , gl_FRONT_FACE :: GLEnum a
       , gl_DEPTH_RANGE :: GLEnum a
       , gl_DEPTH_WRITEMASK :: GLEnum a
       , gl_DEPTH_CLEAR_VALUE :: GLEnum a
       , gl_DEPTH_FUNC :: GLEnum a
       , gl_STENCIL_CLEAR_VALUE :: GLEnum a
       , gl_STENCIL_FUNC :: GLEnum a
       , gl_STENCIL_FAIL :: GLEnum a
       , gl_STENCIL_PASS_DEPTH_FAIL :: GLEnum a
       , gl_STENCIL_PASS_DEPTH_PASS :: GLEnum a
       , gl_STENCIL_REF :: GLEnum a
       , gl_STENCIL_VALUE_MASK :: GLEnum a
       , gl_STENCIL_WRITEMASK :: GLEnum a
       , gl_STENCIL_BACK_FUNC :: GLEnum a
       , gl_STENCIL_BACK_FAIL :: GLEnum a
       , gl_STENCIL_BACK_PASS_DEPTH_FAIL :: GLEnum a
       , gl_STENCIL_BACK_PASS_DEPTH_PASS :: GLEnum a
       , gl_STENCIL_BACK_REF :: GLEnum a
       , gl_STENCIL_BACK_VALUE_MASK :: GLEnum a
       , gl_STENCIL_BACK_WRITEMASK :: GLEnum a
       , gl_VIEWPORT :: GLEnum a
       , gl_SCISSOR_BOX :: GLEnum a
       , gl_COLOR_CLEAR_VALUE :: GLEnum a
       , gl_COLOR_WRITEMASK :: GLEnum a
       , gl_UNPACK_ALIGNMENT :: GLEnum a
       , gl_PACK_ALIGNMENT :: GLEnum a
       , gl_MAX_TEXTURE_SIZE :: GLEnum a
       , gl_MAX_VIEWPORT_DIMS :: GLEnum a
       , gl_SUBPIXEL_BITS :: GLEnum a
       , gl_RED_BITS :: GLEnum a
       , gl_GREEN_BITS :: GLEnum a
       , gl_BLUE_BITS :: GLEnum a
       , gl_ALPHA_BITS :: GLEnum a
       , gl_DEPTH_BITS :: GLEnum a
       , gl_STENCIL_BITS :: GLEnum a
       , gl_POLYGON_OFFSET_UNITS :: GLEnum a
       , gl_POLYGON_OFFSET_FACTOR :: GLEnum a
       , gl_TEXTURE_BINDING_2D :: GLEnum a
       , gl_SAMPLE_BUFFERS :: GLEnum a
       , gl_SAMPLES :: GLEnum a
       , gl_SAMPLE_COVERAGE_VALUE :: GLEnum a
       , gl_SAMPLE_COVERAGE_INVERT :: GLEnum a
       , gl_COMPRESSED_TEXTURE_FORMATS :: GLEnum a
       , gl_DONT_CARE :: GLEnum a
       , gl_FASTEST :: GLEnum a
       , gl_NICEST :: GLEnum a
       , gl_GENERATE_MIPMAP_HINT :: GLEnum a
       , gl_BYTE :: GLEnum a
       , gl_UNSIGNED_BYTE :: GLEnum a
       , gl_SHORT :: GLEnum a
       , gl_UNSIGNED_SHORT :: GLEnum a
       , gl_INT :: GLEnum a
       , gl_UNSIGNED_INT :: GLEnum a
       , gl_FLOAT :: GLEnum a
       , gl_DEPTH_COMPONENT :: GLEnum a
       , gl_ALPHA :: GLEnum a
       , gl_RGB :: GLEnum a
       , gl_RGBA :: GLEnum a
       , gl_LUMINANCE :: GLEnum a
       , gl_LUMINANCE_ALPHA :: GLEnum a
       , gl_UNSIGNED_SHORT_4_4_4_4 :: GLEnum a
       , gl_UNSIGNED_SHORT_5_5_5_1 :: GLEnum a
       , gl_UNSIGNED_SHORT_5_6_5 :: GLEnum a
       , gl_FRAGMENT_SHADER :: GLEnum a
       , gl_VERTEX_SHADER :: GLEnum a
       , gl_MAX_VERTEX_ATTRIBS :: GLEnum a
       , gl_MAX_VERTEX_UNIFORM_VECTORS :: GLEnum a
       , gl_MAX_VARYING_VECTORS :: GLEnum a
       , gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: GLEnum a
       , gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: GLEnum a
       , gl_MAX_TEXTURE_IMAGE_UNITS :: GLEnum a
       , gl_MAX_FRAGMENT_UNIFORM_VECTORS :: GLEnum a
       , gl_SHADER_TYPE :: GLEnum a
       , gl_DELETE_STATUS :: GLEnum a
       , gl_LINK_STATUS :: GLEnum a
       , gl_VALIDATE_STATUS :: GLEnum a
       , gl_ATTACHED_SHADERS :: GLEnum a
       , gl_ACTIVE_UNIFORMS :: GLEnum a
       , gl_ACTIVE_ATTRIBUTES :: GLEnum a
       , gl_SHADING_LANGUAGE_VERSION :: GLEnum a
       , gl_CURRENT_PROGRAM :: GLEnum a
       , gl_NEVER :: GLEnum a
       , gl_LESS :: GLEnum a
       , gl_EQUAL :: GLEnum a
       , gl_LEQUAL :: GLEnum a
       , gl_GREATER :: GLEnum a
       , gl_NOTEQUAL :: GLEnum a
       , gl_GEQUAL :: GLEnum a
       , gl_ALWAYS :: GLEnum a
       , gl_KEEP :: GLEnum a
       , gl_REPLACE :: GLEnum a
       , gl_INCR :: GLEnum a
       , gl_DECR :: GLEnum a
       , gl_INVERT :: GLEnum a
       , gl_INCR_WRAP :: GLEnum a
       , gl_DECR_WRAP :: GLEnum a
       , gl_VENDOR :: GLEnum a
       , gl_RENDERER :: GLEnum a
       , gl_VERSION :: GLEnum a
       , gl_NEAREST :: GLEnum a
       , gl_LINEAR :: GLEnum a
       , gl_NEAREST_MIPMAP_NEAREST :: GLEnum a
       , gl_LINEAR_MIPMAP_NEAREST :: GLEnum a
       , gl_NEAREST_MIPMAP_LINEAR :: GLEnum a
       , gl_LINEAR_MIPMAP_LINEAR :: GLEnum a
       , gl_TEXTURE_MAG_FILTER :: GLEnum a
       , gl_TEXTURE_MIN_FILTER :: GLEnum a
       , gl_TEXTURE_WRAP_S :: GLEnum a
       , gl_TEXTURE_WRAP_T :: GLEnum a
       , gl_TEXTURE :: GLEnum a
       , gl_TEXTURE_CUBE_MAP :: GLEnum a
       , gl_TEXTURE_BINDING_CUBE_MAP :: GLEnum a
       , gl_TEXTURE_CUBE_MAP_POSITIVE_X :: GLEnum a
       , gl_TEXTURE_CUBE_MAP_NEGATIVE_X :: GLEnum a
       , gl_TEXTURE_CUBE_MAP_POSITIVE_Y :: GLEnum a
       , gl_TEXTURE_CUBE_MAP_NEGATIVE_Y :: GLEnum a
       , gl_TEXTURE_CUBE_MAP_POSITIVE_Z :: GLEnum a
       , gl_TEXTURE_CUBE_MAP_NEGATIVE_Z :: GLEnum a
       , gl_MAX_CUBE_MAP_TEXTURE_SIZE :: GLEnum a
       , gl_TEXTURE0 :: GLEnum a
       , gl_TEXTURE1 :: GLEnum a
       , gl_TEXTURE2 :: GLEnum a
       , gl_TEXTURE3 :: GLEnum a
       , gl_TEXTURE4 :: GLEnum a
       , gl_TEXTURE5 :: GLEnum a
       , gl_TEXTURE6 :: GLEnum a
       , gl_TEXTURE7 :: GLEnum a
       , gl_TEXTURE8 :: GLEnum a
       , gl_TEXTURE9 :: GLEnum a
       , gl_TEXTURE10 :: GLEnum a
       , gl_TEXTURE11 :: GLEnum a
       , gl_TEXTURE12 :: GLEnum a
       , gl_TEXTURE13 :: GLEnum a
       , gl_TEXTURE14 :: GLEnum a
       , gl_TEXTURE15 :: GLEnum a
       , gl_TEXTURE16 :: GLEnum a
       , gl_TEXTURE17 :: GLEnum a
       , gl_TEXTURE18 :: GLEnum a
       , gl_TEXTURE19 :: GLEnum a
       , gl_TEXTURE20 :: GLEnum a
       , gl_TEXTURE21 :: GLEnum a
       , gl_TEXTURE22 :: GLEnum a
       , gl_TEXTURE23 :: GLEnum a
       , gl_TEXTURE24 :: GLEnum a
       , gl_TEXTURE25 :: GLEnum a
       , gl_TEXTURE26 :: GLEnum a
       , gl_TEXTURE27 :: GLEnum a
       , gl_TEXTURE28 :: GLEnum a
       , gl_TEXTURE29 :: GLEnum a
       , gl_TEXTURE30 :: GLEnum a
       , gl_TEXTURE31 :: GLEnum a
       , gl_ACTIVE_TEXTURE :: GLEnum a
       , gl_REPEAT :: GLEnum a
       , gl_CLAMP_TO_EDGE :: GLEnum a
       , gl_MIRRORED_REPEAT :: GLEnum a
       , gl_FLOAT_VEC2 :: GLEnum a
       , gl_FLOAT_VEC3 :: GLEnum a
       , gl_FLOAT_VEC4 :: GLEnum a
       , gl_INT_VEC2 :: GLEnum a
       , gl_INT_VEC3 :: GLEnum a
       , gl_INT_VEC4 :: GLEnum a
       , gl_BOOL :: GLEnum a
       , gl_BOOL_VEC2 :: GLEnum a
       , gl_BOOL_VEC3 :: GLEnum a
       , gl_BOOL_VEC4 :: GLEnum a
       , gl_FLOAT_MAT2 :: GLEnum a
       , gl_FLOAT_MAT3 :: GLEnum a
       , gl_FLOAT_MAT4 :: GLEnum a
       , gl_SAMPLER_2D :: GLEnum a
       , gl_SAMPLER_CUBE :: GLEnum a
       , gl_VERTEX_ATTRIB_ARRAY_ENABLED :: GLEnum a
       , gl_VERTEX_ATTRIB_ARRAY_SIZE :: GLEnum a
       , gl_VERTEX_ATTRIB_ARRAY_STRIDE :: GLEnum a
       , gl_VERTEX_ATTRIB_ARRAY_TYPE :: GLEnum a
       , gl_VERTEX_ATTRIB_ARRAY_NORMALIZED :: GLEnum a
       , gl_VERTEX_ATTRIB_ARRAY_POINTER :: GLEnum a
       , gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: GLEnum a
       , gl_IMPLEMENTATION_COLOR_READ_TYPE :: GLEnum a
       , gl_IMPLEMENTATION_COLOR_READ_FORMAT :: GLEnum a
       , gl_COMPILE_STATUS :: GLEnum a
       , gl_LOW_FLOAT :: GLEnum a
       , gl_MEDIUM_FLOAT :: GLEnum a
       , gl_HIGH_FLOAT :: GLEnum a
       , gl_LOW_INT :: GLEnum a
       , gl_MEDIUM_INT :: GLEnum a
       , gl_HIGH_INT :: GLEnum a
       , gl_FRAMEBUFFER :: GLEnum a
       , gl_RENDERBUFFER :: GLEnum a
       , gl_RGBA4 :: GLEnum a
       , gl_RGB5_A1 :: GLEnum a
       , gl_RGB565 :: GLEnum a
       , gl_DEPTH_COMPONENT16 :: GLEnum a
       , gl_STENCIL_INDEX :: GLEnum a
       , gl_STENCIL_INDEX8 :: GLEnum a
       , gl_DEPTH_STENCIL :: GLEnum a
       , gl_RENDERBUFFER_WIDTH :: GLEnum a
       , gl_RENDERBUFFER_HEIGHT :: GLEnum a
       , gl_RENDERBUFFER_INTERNAL_FORMAT :: GLEnum a
       , gl_RENDERBUFFER_RED_SIZE :: GLEnum a
       , gl_RENDERBUFFER_GREEN_SIZE :: GLEnum a
       , gl_RENDERBUFFER_BLUE_SIZE :: GLEnum a
       , gl_RENDERBUFFER_ALPHA_SIZE :: GLEnum a
       , gl_RENDERBUFFER_DEPTH_SIZE :: GLEnum a
       , gl_RENDERBUFFER_STENCIL_SIZE :: GLEnum a
       , gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: GLEnum a
       , gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: GLEnum a
       , gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: GLEnum a
       , gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: GLEnum a
       , gl_COLOR_ATTACHMENT0 :: GLEnum a
       , gl_DEPTH_ATTACHMENT :: GLEnum a
       , gl_STENCIL_ATTACHMENT :: GLEnum a
       , gl_DEPTH_STENCIL_ATTACHMENT :: GLEnum a
       , gl_NONE :: GLEnum a
       , gl_FRAMEBUFFER_COMPLETE :: GLEnum a
       , gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: GLEnum a
       , gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: GLEnum a
       , gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: GLEnum a
       , gl_FRAMEBUFFER_UNSUPPORTED :: GLEnum a
       , gl_FRAMEBUFFER_BINDING :: GLEnum a
       , gl_RENDERBUFFER_BINDING :: GLEnum a
       , gl_MAX_RENDERBUFFER_SIZE :: GLEnum a
       , gl_INVALID_FRAMEBUFFER_OPERATION :: GLEnum a
       --, gl_UNPACK_FLIP_Y_WEBGL :: GLEnum a
       --, gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: GLEnum a
       --, gl_CONTEXT_LOST_WEBGL :: GLEnum a
       --, gl_UNPACK_COLORSPACE_CONVERSION_WEBGL :: GLEnum a
       --, gl_BROWSER_DEFAULT_WEBGL :: GLEnum a

         -- | A true value.
       , true  :: GLBoolean a
         -- | A false value.
       , false :: GLBoolean a

       }
