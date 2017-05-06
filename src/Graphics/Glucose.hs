{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
module Graphics.Glucose where

import GHC.Exts (Constraint)

data ShaderPrecisionFormat a = ShaderPrecisionFormat { spfRangeMin  :: GLint a
                                                     , spfRangeMax  :: GLint a
                                                     , spfPrecision :: GLint a
                                                     }

data ActiveInfo a = ActiveInfo { aiSize :: GLint a
                               , aiType :: GLint a
                               , aiName :: String
                               }

-- | An sum of all possible GL types, for use in functions where the return type
-- would usually be cast.
data AnyGLESType a where
  GLESProgram :: Program a -> AnyGLESType a
  GLESShader :: Shader a -> AnyGLESType a
  GLESTexture :: Texture a -> AnyGLESType a
  GLESUniformLocation :: UniformLocation a -> AnyGLESType a
  GLESGLclampf :: GLclampf a -> AnyGLESType a
  GLESGLfloat :: GLfloat a -> AnyGLESType a
  GLESGLenum :: GLenum a -> AnyGLESType a
  GLESGLuint :: GLuint a -> AnyGLESType a
  GLESGLint :: GLint a -> AnyGLESType a
  GLESGLintptr :: GLintptr a -> AnyGLESType a
  GLESGLbitfield :: GLbitfield a -> AnyGLESType a
  GLESGLboolean :: GLboolean a -> AnyGLESType a
  GLESGLsizei :: GLsizei a -> AnyGLESType a
  GLESGLstring :: GLstring a -> AnyGLESType a
  GLESGLptr :: GLptr a -> AnyGLESType a
  GLESBufferableData :: BufferableData a -> AnyGLESType a
  GLESImageData :: ImageData a -> AnyGLESType a
  GLESBuffer :: Buffer a -> AnyGLESType a
  GLESFramebuffer :: Framebuffer a -> AnyGLESType a
  GLESRenderbuffer :: Renderbuffer a -> AnyGLESType a
  GLESFloatArray :: FloatArray a -> AnyGLESType a
  GLESIntArray :: IntArray a -> AnyGLESType a
  GLESExtension :: Extension a -> AnyGLESType a
  GLESNone :: AnyGLESType a

class GLES a where
  type C a :: (* -> *) -> Constraint

  type Program         a
  type Shader          a
  type Texture         a
  type UniformLocation a

  type GLclampf        a
  type GLfloat         a
  type GLenum          a
  type GLuint          a
  type GLint           a
  type GLintptr        a
  type GLbitfield      a
  type GLboolean       a
  type GLsizei         a
  type GLstring        a
  type GLptr           a

  type BufferableData  a
  type ImageData       a

  type Buffer          a
  type Framebuffer     a
  type Renderbuffer    a
  type FloatArray      a
  type IntArray        a
  type UintArray       a
  type Extension       a

  glActiveTexture :: (C a) m => GLenum a -> m ()
  glAttachShader :: (C a) m => Program a -> Shader a -> m ()
  glBindAttribLocation :: (C a) m => Program a -> GLuint a -> GLstring a -> m ()
  glBindBuffer :: (C a) m => GLenum a -> Buffer a -> m ()
  glBindFramebuffer :: (C a) m => GLenum a -> Framebuffer a -> m ()
  glBindRenderbuffer :: (C a) m => GLenum a -> Renderbuffer a -> m ()
  glBindTexture :: (C a) m => GLenum a -> Texture a -> m ()
  glBlendColor :: (C a) m => GLclampf a -> GLclampf a -> GLclampf a -> GLclampf a -> m ()
  glBlendEquation :: (C a) m => GLenum a -> m ()
  glBlendEquationSeparate :: (C a) m => GLenum a -> GLenum a -> m ()
  glBlendFunc :: (C a) m => GLenum a -> GLenum a -> m ()
  glBlendFuncSeparate :: (C a) m => GLenum a -> GLenum a -> GLenum a -> GLenum a -> m ()
  glBufferData :: (C a) m => () => GLenum a -> BufferableData a -> GLenum a -> m ()
  glBufferSubData :: (C a) m => () => GLenum a -> GLintptr a -> BufferableData a -> m ()
  glCheckFramebufferStatus :: (C a) m => GLenum a -> m (GLenum a)
  glClear :: (C a) m => GLbitfield a -> m ()
  glClearColor :: (C a) m => GLclampf a -> GLclampf a -> GLclampf a -> GLclampf a -> m ()
  glClearDepth :: (C a) m => GLclampf a -> m ()
  glClearStencil :: (C a) m => GLint a -> m ()
  glColorMask :: (C a) m => GLboolean a -> GLboolean a -> GLboolean a -> GLboolean a -> m ()
  glCompileShader :: (C a) m => Shader a -> m ()
  -- | Copies pixels from the current @Framebuffer a@ into a 2D texture image.
  glCopyTexImage2D
    :: (C a) m
    => GLenum a
    -- ^ target
    -- A GLenum specifying the binding point (target) of the active texture. Possible values:
    --   * gl_TEXTURE_2D: A two-dimensional texture.
    --   * gl_TEXTURE_CUBE_MAP_POSITIVE_X: Positive X face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_X: Negative X face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Y: Positive Y face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Y: Negative Y face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Z: Positive Z face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Z: Negative Z face for a cube-mapped texture.
    -> GLint a
    -- ^ level
    -- A GLint specifying the level of detail. Level 0 is the base image level
    -- and level n is the nth mipmap reduction level.
    -> GLenum a
    -- ^ internal format
    -- A GLint specifying the color components in the texture. Possible values:
    --   * gl_ALPHA: Discards the red, green and blue components and reads the alpha component.
    --   * gl_RGB: Discards the alpha components and reads the red, green and blue components.
    --   * gl_RGBA: Red, green, blue and alpha components are read from the color buffer.
    --   * gl_LUMINANCE: Each color component is a luminance component, alpha is 1.0.
    --   * gl_LUMINANCE_ALPHA: Each component is a luminance/alpha component.
    -> GLint a
    -- ^ A GLint specifying the x coordinate of the lower left corner where to
    -- start copying.
    -> GLint a
    -- ^ A GLint specifying the y coordinate of the lower left corner where to
    -- start copying.
    -> GLsizei a
    -- ^ A GLsizei specifying the width of the texture.
    -> GLsizei a
    -- ^ A GLsizei specifying the height of the texture.
    -> GLint a
    -- ^ This parameter is ignored, but the WebGL docs say this:
    -- A GLint specifying the width of the border. Must be 0.
    -> m ()
  glCopyTexSubImage2D
    :: (C a) m
    => GLenum a
    -- ^ target
    -- A GLenum specifying the binding point (target) of the active texture. Possible values:
    --   * gl_TEXTURE_2D: A two-dimensional texture.
    --   * gl_TEXTURE_CUBE_MAP_POSITIVE_X: Positive X face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_X: Negative X face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Y: Positive Y face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Y: Negative Y face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Z: Positive Z face for a cube-mapped texture.
    --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Z: Negative Z face for a cube-mapped texture.
    -> GLint a
    -- ^ level
    -- A GLint specifying the level of detail. Level 0 is the base image level
    -- and level n is the nth mipmap reduction level.
    -> GLint a
    -- ^ xoffset
    -- A GLint specifying the horizontal offset within the texture image.
    -> GLint a
    -- ^ yoffset
    -- A GLint specifying the vertical offset within the texture image.
    -> GLint a
    -- ^ A GLint specifying the x coordinate of the lower left corner where to
    -- start copying.
    -> GLint a
    -- ^ A GLint specifying the y coordinate of the lower left corner where to
    -- start copying.
    -> GLsizei a
    -- ^ A GLsizei specifying the width of the texture.
    -> GLsizei a
    -- ^ A GLsizei specifying the height of the texture.
    -> m ()
  glCreateBuffer :: (C a) m =>  m (Buffer a)
  glCreateFramebuffer :: (C a) m =>  m (Framebuffer a)
  glCreateProgram :: (C a) m =>  m (Program a)
  glCreateRenderbuffer :: (C a) m =>  m (Renderbuffer a)
  glCreateShader :: (C a) m => GLenum a -> m (Shader a)
  glCreateTexture :: (C a) m =>  m (Texture a)
  glCullFace
    :: (C a) m
    => GLenum a
    -- ^ mode
    -- A GLenum specifying whether front- or back-facing polygons are candidates
    -- for culling. The default value is gl.BACK. Possible values are:
    --   * gl_FRONT
    --   * gl_BACK
    --   * gl_FRONT_AND_BACK
    -> m ()

  glDeleteBuffer :: (C a) m => Buffer a -> m ()
  glDeleteFramebuffer :: (C a) m => Framebuffer a -> m ()
  glDeleteProgram :: (C a) m => Program a -> m ()
  glDeleteRenderbuffer :: (C a) m => Renderbuffer a -> m ()
  glDeleteShader :: (C a) m => Shader a -> m ()
  glDeleteTexture :: (C a) m => Texture a -> m ()

  -- | Specifies a function that compares incoming pixel depth to the current
  -- depth buffer value
  glDepthFunc
    :: (C a) m
    => GLenum a
    -- ^ func
    -- A GLenum specifying the depth comparison function, which sets the
    -- conditions under which the pixel will be drawn. The default value is
    -- gl_LESS. Possible values are:
    --   * gl_NEVER (never pass)
    --   * gl_LESS (pass if the incoming value is less than the depth buffer value)
    --   * gl_EQUAL (pass if the incoming value equals the the depth buffer value)
    --   * gl_LEQUAL (pass if the incoming value is less than or equal to the depth buffer value)
    --   * gl_GREATER (pass if the incoming value is greater than the depth buffer value)
    --   * gl_NOTEQUAL (pass if the incoming value is not equal to the depth buffer value)
    --   * gl_GEQUAL (pass if the incoming value is greater than or equal to the depth buffer value)
    --   * gl_ALWAYS (always pass)
    -> m ()

  -- | Sets whether writing into the depth buffer is enabled or disabled.
  glDepthMask
    :: (C a) m
    => GLboolean a
    -- ^ flag
    -- A GLboolean specifying whether or not writing into the depth buffer is
    -- enabled. Default value: true, meaning that writing is enabled.
    -> m ()

  -- | Specifies the depth range mapping from normalized device coordinates to
  -- window or viewport coordinates.
  glDepthRange
    :: (C a) m
    => GLclampf a
    -- ^ zNear
    -- A @GLclampf a@ specifying the mapping of the near clipping plane to window
    -- or viewport coordinates. Clamped to the range 0 to 1 and must be less
    -- than or equal to zFar. The default value is 0.
    -> GLclampf a
    -- ^ zFar
    -- A @GLclampf a@ specifying the mapping of the far clipping plane to window or
    -- viewport coordinates. Clamped to the range 0 to 1. The default value is 1.
    -> m ()

  -- | Detaches a previously attached @Shader a@ from a @Program a@.
  glDetachShader
    :: (C a) m
    => Program a
    -- ^ The program to detach the shader from.
    -> Shader a
    -- ^ The fragment or vertex shader to detach.
    -> m ()

  glDisable :: (C a) m => GLenum a -> m ()
  glDisableVertexAttribArray :: (C a) m => GLuint a -> m ()
  glDrawArrays :: (C a) m => GLenum a -> GLint a -> GLsizei a -> m ()
  glDrawElements :: (C a) m => GLenum a -> GLsizei a -> GLenum a -> GLptr a -> m ()
  glEnable :: (C a) m => GLenum a -> m ()
  glEnableVertexAttribArray :: (C a) m => GLuint a -> m ()
  glFinish :: (C a) m =>  m ()
  glFlush :: (C a) m =>  m ()
  glFramebufferRenderbuffer :: (C a) m => GLenum a -> GLenum a -> GLenum a -> Renderbuffer a -> m ()
  glFramebufferTexture2D :: (C a) m => GLenum a -> GLenum a -> GLenum a -> Texture a -> GLint a -> m ()
  glFrontFace :: (C a) m => GLenum a -> m ()
  glGenerateMipmap :: (C a) m => GLenum a -> m ()
  glGetActiveAttrib :: (C a) m => Program a -> GLuint a -> m (ActiveInfo a)
  glGetActiveUniform :: (C a) m => Program a -> GLuint a -> m (ActiveInfo a)
  glGetAttachedShaders :: (C a) m => Program a -> m [Shader a]
  glGetAttribLocation :: (C a) m => Program a -> String -> m (GLint a)
  glGetBufferParameter :: (C a) m => GLenum a -> GLenum a -> m (Either (GLint a) (GLenum a))
  glGetError :: (C a) m =>  m (GLenum a)
  glGetExtension :: (C a) m => GLstring a -> m (Maybe (Extension a))
  glGetFramebufferAttachmentParameter
    :: (C a) m => GLenum a -> GLenum a -> GLenum a -> m (AnyGLESType a)
  glGetProgramParameter :: (C a) m => Program a -> GLenum a -> m (Either (GLboolean a) (GLint a))
  glGetProgramInfoLog :: (C a) m => Program a -> m String
  glGetRenderbufferParameter :: (C a) m => GLenum a -> GLenum a -> m (Either (GLint a) (GLenum a))
  glGetShaderParameter :: (C a) m => Shader a -> GLenum a -> m (Either (GLboolean a) (GLenum a))
  glGetShaderInfoLog :: (C a) m => Shader a -> m String
  glGetShaderPrecisionFormat :: (C a) m => GLenum a -> GLenum a -> m (ShaderPrecisionFormat a)
  glGetShaderSource :: (C a) m => Shader a -> m String
  glGetSupportedExtensions :: (C a) m =>  m [String]
  glGetTexParameter :: (C a) m => GLenum a -> GLenum a -> m (AnyGLESType a)
  glGetUniformfv :: (C a) m => Program a -> UniformLocation a -> FloatArray a -> m (Maybe (FloatArray a))
  glGetUniformiv :: (C a) m => Program a -> UniformLocation a -> IntArray a -> m (Maybe (IntArray a))
  glGetUniformLocation :: (C a) m => Program a -> GLstring a -> m (UniformLocation a)
  glGetVertexAttribfv :: (C a) m => GLuint a -> GLenum a -> FloatArray a -> m (Maybe (FloatArray a))
  glGetVertexAttribiv :: (C a) m => GLuint a -> GLenum a -> IntArray a -> m (Maybe (IntArray a))
  glHint :: (C a) m => GLenum a -> GLenum a -> m ()
  glIsBuffer :: (C a) m => Buffer a -> m (GLboolean a)
  glIsContextLost :: (C a) m =>  m (GLboolean a)
  glIsEnabled :: (C a) m => GLenum a -> m (GLboolean a)
  glIsFramebuffer :: (C a) m => Framebuffer a -> m (GLboolean a)
  glIsProgram :: (C a) m => Program a -> m (GLboolean a)
  glIsRenderbuffer :: (C a) m => Renderbuffer a -> m (GLboolean a)
  glIsShader :: (C a) m => Shader a -> m (GLboolean a)
  glIsTexture :: (C a) m => Texture a -> m (GLboolean a)
  glLineWidth :: (C a) m => GLfloat a -> m ()
  glLinkProgram :: (C a) m => Program a -> m ()
  glPixelStorei :: (C a) m => GLenum a -> GLint a -> m ()
  glPolygonOffset :: (C a) m => GLfloat a -> GLfloat a -> m ()
  glReleaseShaderCompiler :: (C a) m =>  m ()
  glRenderbufferStorage :: (C a) m => GLenum a -> GLenum a -> GLsizei a -> GLsizei a -> m ()
  glSampleCoverage :: (C a) m => GLclampf a -> GLboolean a -> m ()
  glScissor :: (C a) m => GLint a -> GLint a -> GLsizei a -> GLsizei a -> m ()
  glShaderSource :: (C a) m => Shader a -> String -> m ()
  glStencilFunc :: (C a) m => GLenum a -> GLint a -> GLuint a -> m ()
  glStencilFuncSeparate :: (C a) m => GLenum a -> GLenum a -> GLint a -> GLuint a -> m ()
  glStencilMask :: (C a) m => GLuint a -> m ()
  glStencilMaskSeparate :: (C a) m => GLenum a -> GLuint a -> m ()
  glStencilOp :: (C a) m => GLenum a -> GLenum a -> GLenum a -> m ()
  glStencilOpSeparate :: (C a) m => GLenum a -> GLenum a -> GLenum a -> GLenum a -> m ()
  glTexParameterf :: (C a) m => GLenum a -> GLenum a -> GLfloat a -> m ()
  glTexParameteri :: (C a) m => GLenum a -> GLenum a -> GLint a -> m ()
  --glTexImage2DData :: (C a) m => GLenum a -> GLint a -> GLenum a -> GLenum a -> GLenum a -> ImageData a -> m ()
  glTexImage2D :: (C a) m => GLenum a -> GLint a -> GLint a -> GLenum a -> GLenum a -> ImageData a -> m ()
  --glTexImage2DCanvas :: (C a) m => GLenum a -> GLint a -> GLenum a -> GLenum a -> GLenum a -> HTMLCanvasElement -> m ()
  --glTexImage2DVideo :: (C a) m => GLenum a -> GLint a -> GLenum a -> GLenum a -> GLenum a -> HTMLVideoElement -> m ()
  --glTexSubImage2DData :: (C a) m => GLenum a -> GLint a -> GLint a -> GLint a -> GLenum a -> GLenum a -> ImageData a -> m ()
  glTexSubImage2D :: (C a) m => GLenum a -> GLint a -> GLint a -> GLint a -> GLenum a -> GLenum a -> ImageData a -> m ()
  --glTexSubImage2DCanvas :: (C a) m => GLenum a -> GLint a -> GLint a -> GLint a -> GLenum a -> GLenum a -> HTMLCanvasElement -> m ()
  --glTexSubImage2DVideo :: (C a) m => GLenum a -> GLint a -> GLint a -> GLint a -> GLenum a -> GLenum a -> HTMLVideoElement -> m ()
  glUniform1f :: (C a) m => UniformLocation a -> GLfloat a -> m ()
  glUniform1fv :: (C a) m => UniformLocation a -> FloatArray a -> m ()
  glUniform1i :: (C a) m => UniformLocation a -> GLint a -> m ()
  glUniform1iv :: (C a) m => UniformLocation a -> IntArray a -> m ()
  glUniform2f :: (C a) m => UniformLocation a -> GLfloat a -> GLfloat a -> m ()
  glUniform2fv :: (C a) m => UniformLocation a -> FloatArray a -> m ()
  glUniform2i :: (C a) m => UniformLocation a -> GLint a -> GLint a -> m ()
  glUniform2iv :: (C a) m => UniformLocation a -> IntArray a -> m ()
  glUniform3f :: (C a) m => UniformLocation a -> GLfloat a -> GLfloat a -> GLfloat a -> m ()
  glUniform3fv :: (C a) m => UniformLocation a -> FloatArray a -> m ()
  glUniform3i :: (C a) m => UniformLocation a -> GLint a -> GLint a -> GLint a -> m ()
  glUniform3iv :: (C a) m => UniformLocation a -> IntArray a -> m ()
  glUniform4f :: (C a) m => UniformLocation a -> GLfloat a -> GLfloat a -> GLfloat a -> GLfloat a -> m ()
  glUniform4fv :: (C a) m => UniformLocation a -> FloatArray a -> m ()
  glUniform4i :: (C a) m => UniformLocation a -> GLint a -> GLint a -> GLint a -> GLint a -> m ()
  glUniform4iv :: (C a) m => UniformLocation a -> IntArray a -> m ()
  glUniformMatrix2fv :: (C a) m => UniformLocation a -> GLboolean a -> FloatArray a -> m ()
  glUniformMatrix3fv :: (C a) m => UniformLocation a -> GLboolean a -> FloatArray a -> m ()
  glUniformMatrix4fv :: (C a) m => UniformLocation a -> GLboolean a -> FloatArray a -> m ()
  glUseProgram :: (C a) m => Program a -> m ()
  glValidateProgram :: (C a) m => Program a -> m ()
  glVertexAttrib1f :: (C a) m => GLuint a -> GLfloat a -> m ()
  glVertexAttrib1fv :: (C a) m => GLuint a -> FloatArray a -> m ()
  glVertexAttrib2f :: (C a) m => GLuint a -> GLfloat a -> GLfloat a -> m ()
  glVertexAttrib2fv :: (C a) m => GLuint a -> FloatArray a -> m ()
  glVertexAttrib3f :: (C a) m => GLuint a -> GLfloat a -> GLfloat a -> GLfloat a -> m ()
  glVertexAttrib3fv :: (C a) m => GLuint a -> FloatArray a -> m ()
  glVertexAttrib4f :: (C a) m => GLuint a -> GLfloat a -> GLfloat a -> GLfloat a -> GLfloat a -> m ()
  glVertexAttrib4fv :: (C a) m => GLuint a -> FloatArray a -> m ()
  glVertexAttribPointer :: (C a) m => GLuint a -> GLint a -> GLenum a -> GLboolean a -> GLsizei a -> GLintptr a -> m ()
  glViewport :: (C a) m => GLint a -> GLint a -> GLsizei a -> GLsizei a -> m ()

  gl_DEPTH_BUFFER_BIT :: GLenum a
  gl_STENCIL_BUFFER_BIT :: GLenum a
  gl_COLOR_BUFFER_BIT :: GLenum a
  gl_POINTS :: GLenum a
  gl_LINES :: GLenum a
  gl_LINE_LOOP :: GLenum a
  gl_LINE_STRIP :: GLenum a
  gl_TRIANGLES :: GLenum a
  gl_TRIANGLE_STRIP :: GLenum a
  gl_TRIANGLE_FAN :: GLenum a
  gl_ZERO :: GLenum a
  gl_ONE :: GLenum a
  gl_SRC_COLOR :: GLenum a
  gl_ONE_MINUS_SRC_COLOR :: GLenum a
  gl_SRC_ALPHA :: GLenum a
  gl_ONE_MINUS_SRC_ALPHA :: GLenum a
  gl_DST_ALPHA :: GLenum a
  gl_ONE_MINUS_DST_ALPHA :: GLenum a
  gl_DST_COLOR :: GLenum a
  gl_ONE_MINUS_DST_COLOR :: GLenum a
  gl_SRC_ALPHA_SATURATE :: GLenum a
  gl_FUNC_ADD :: GLenum a
  --gl_BLEND_EQUATION :: GLenum a
  gl_BLEND_EQUATION_RGB :: GLenum a
  gl_BLEND_EQUATION_ALPHA :: GLenum a
  gl_FUNC_SUBTRACT :: GLenum a
  gl_FUNC_REVERSE_SUBTRACT :: GLenum a
  gl_BLEND_DST_RGB :: GLenum a
  gl_BLEND_SRC_RGB :: GLenum a
  gl_BLEND_DST_ALPHA :: GLenum a
  gl_BLEND_SRC_ALPHA :: GLenum a
  gl_CONSTANT_COLOR :: GLenum a
  gl_ONE_MINUS_CONSTANT_COLOR :: GLenum a
  gl_CONSTANT_ALPHA :: GLenum a
  gl_ONE_MINUS_CONSTANT_ALPHA :: GLenum a
  gl_BLEND_COLOR :: GLenum a
  gl_ARRAY_BUFFER :: GLenum a
  gl_ELEMENT_ARRAY_BUFFER :: GLenum a
  gl_ARRAY_BUFFER_BINDING :: GLenum a
  gl_ELEMENT_ARRAY_BUFFER_BINDING :: GLenum a
  gl_STREAM_DRAW :: GLenum a
  gl_STATIC_DRAW :: GLenum a
  gl_DYNAMIC_DRAW :: GLenum a
  gl_BUFFER_SIZE :: GLenum a
  gl_BUFFER_USAGE :: GLenum a
  gl_CURRENT_VERTEX_ATTRIB :: GLenum a
  gl_FRONT :: GLenum a
  gl_BACK :: GLenum a
  gl_FRONT_AND_BACK :: GLenum a
  gl_TEXTURE_2D :: GLenum a
  gl_CULL_FACE :: GLenum a
  gl_BLEND :: GLenum a
  gl_DITHER :: GLenum a
  gl_STENCIL_TEST :: GLenum a
  gl_DEPTH_TEST :: GLenum a
  gl_SCISSOR_TEST :: GLenum a
  gl_POLYGON_OFFSET_FILL :: GLenum a
  gl_SAMPLE_ALPHA_TO_COVERAGE :: GLenum a
  gl_SAMPLE_COVERAGE :: GLenum a
  gl_NO_ERROR :: GLenum a
  gl_INVALID_ENUM :: GLenum a
  gl_INVALID_VALUE :: GLenum a
  gl_INVALID_OPERATION :: GLenum a
  gl_OUT_OF_MEMORY :: GLenum a
  gl_CW :: GLenum a
  gl_CCW :: GLenum a
  gl_LINE_WIDTH :: GLenum a
  gl_ALIASED_POINT_SIZE_RANGE :: GLenum a
  gl_ALIASED_LINE_WIDTH_RANGE :: GLenum a
  gl_CULL_FACE_MODE :: GLenum a
  gl_FRONT_FACE :: GLenum a
  gl_DEPTH_RANGE :: GLenum a
  gl_DEPTH_WRITEMASK :: GLenum a
  gl_DEPTH_CLEAR_VALUE :: GLenum a
  gl_DEPTH_FUNC :: GLenum a
  gl_STENCIL_CLEAR_VALUE :: GLenum a
  gl_STENCIL_FUNC :: GLenum a
  gl_STENCIL_FAIL :: GLenum a
  gl_STENCIL_PASS_DEPTH_FAIL :: GLenum a
  gl_STENCIL_PASS_DEPTH_PASS :: GLenum a
  gl_STENCIL_REF :: GLenum a
  gl_STENCIL_VALUE_MASK :: GLenum a
  gl_STENCIL_WRITEMASK :: GLenum a
  gl_STENCIL_BACK_FUNC :: GLenum a
  gl_STENCIL_BACK_FAIL :: GLenum a
  gl_STENCIL_BACK_PASS_DEPTH_FAIL :: GLenum a
  gl_STENCIL_BACK_PASS_DEPTH_PASS :: GLenum a
  gl_STENCIL_BACK_REF :: GLenum a
  gl_STENCIL_BACK_VALUE_MASK :: GLenum a
  gl_STENCIL_BACK_WRITEMASK :: GLenum a
  gl_VIEWPORT :: GLenum a
  gl_SCISSOR_BOX :: GLenum a
  gl_COLOR_CLEAR_VALUE :: GLenum a
  gl_COLOR_WRITEMASK :: GLenum a
  gl_UNPACK_ALIGNMENT :: GLenum a
  gl_PACK_ALIGNMENT :: GLenum a
  gl_MAX_TEXTURE_SIZE :: GLenum a
  gl_MAX_VIEWPORT_DIMS :: GLenum a
  gl_SUBPIXEL_BITS :: GLenum a
  gl_RED_BITS :: GLenum a
  gl_GREEN_BITS :: GLenum a
  gl_BLUE_BITS :: GLenum a
  gl_ALPHA_BITS :: GLenum a
  gl_DEPTH_BITS :: GLenum a
  gl_STENCIL_BITS :: GLenum a
  gl_POLYGON_OFFSET_UNITS :: GLenum a
  gl_POLYGON_OFFSET_FACTOR :: GLenum a
  gl_TEXTURE_BINDING_2D :: GLenum a
  gl_SAMPLE_BUFFERS :: GLenum a
  gl_SAMPLES :: GLenum a
  gl_SAMPLE_COVERAGE_VALUE :: GLenum a
  gl_SAMPLE_COVERAGE_INVERT :: GLenum a
  gl_COMPRESSED_TEXTURE_FORMATS :: GLenum a
  gl_DONT_CARE :: GLenum a
  gl_FASTEST :: GLenum a
  gl_NICEST :: GLenum a
  gl_GENERATE_MIPMAP_HINT :: GLenum a
  gl_BYTE :: GLenum a
  gl_UNSIGNED_BYTE :: GLenum a
  gl_SHORT :: GLenum a
  gl_UNSIGNED_SHORT :: GLenum a
  gl_INT :: GLenum a
  gl_UNSIGNED_INT :: GLenum a
  gl_FLOAT :: GLenum a
  gl_DEPTH_COMPONENT :: GLenum a
  gl_ALPHA :: GLenum a
  gl_RGB :: GLenum a
  gl_RGBA :: GLenum a
  gl_LUMINANCE :: GLenum a
  gl_LUMINANCE_ALPHA :: GLenum a
  gl_UNSIGNED_SHORT_4_4_4_4 :: GLenum a
  gl_UNSIGNED_SHORT_5_5_5_1 :: GLenum a
  gl_UNSIGNED_SHORT_5_6_5 :: GLenum a
  gl_FRAGMENT_SHADER :: GLenum a
  gl_VERTEX_SHADER :: GLenum a
  gl_MAX_VERTEX_ATTRIBS :: GLenum a
  gl_MAX_VERTEX_UNIFORM_VECTORS :: GLenum a
  gl_MAX_VARYING_VECTORS :: GLenum a
  gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: GLenum a
  gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: GLenum a
  gl_MAX_TEXTURE_IMAGE_UNITS :: GLenum a
  gl_MAX_FRAGMENT_UNIFORM_VECTORS :: GLenum a
  gl_SHADER_TYPE :: GLenum a
  gl_DELETE_STATUS :: GLenum a
  gl_LINK_STATUS :: GLenum a
  gl_VALIDATE_STATUS :: GLenum a
  gl_ATTACHED_SHADERS :: GLenum a
  gl_ACTIVE_UNIFORMS :: GLenum a
  gl_ACTIVE_ATTRIBUTES :: GLenum a
  gl_SHADING_LANGUAGE_VERSION :: GLenum a
  gl_CURRENT_PROGRAM :: GLenum a
  gl_NEVER :: GLenum a
  gl_LESS :: GLenum a
  gl_EQUAL :: GLenum a
  gl_LEQUAL :: GLenum a
  gl_GREATER :: GLenum a
  gl_NOTEQUAL :: GLenum a
  gl_GEQUAL :: GLenum a
  gl_ALWAYS :: GLenum a
  gl_KEEP :: GLenum a
  gl_REPLACE :: GLenum a
  gl_INCR :: GLenum a
  gl_DECR :: GLenum a
  gl_INVERT :: GLenum a
  gl_INCR_WRAP :: GLenum a
  gl_DECR_WRAP :: GLenum a
  gl_VENDOR :: GLenum a
  gl_RENDERER :: GLenum a
  gl_VERSION :: GLenum a
  gl_NEAREST :: GLenum a
  gl_LINEAR :: GLenum a
  gl_NEAREST_MIPMAP_NEAREST :: GLenum a
  gl_LINEAR_MIPMAP_NEAREST :: GLenum a
  gl_NEAREST_MIPMAP_LINEAR :: GLenum a
  gl_LINEAR_MIPMAP_LINEAR :: GLenum a
  gl_TEXTURE_MAG_FILTER :: GLenum a
  gl_TEXTURE_MIN_FILTER :: GLenum a
  gl_TEXTURE_WRAP_S :: GLenum a
  gl_TEXTURE_WRAP_T :: GLenum a
  gl_TEXTURE :: GLenum a
  gl_TEXTURE_CUBE_MAP :: GLenum a
  gl_TEXTURE_BINDING_CUBE_MAP :: GLenum a
  gl_TEXTURE_CUBE_MAP_POSITIVE_X :: GLenum a
  gl_TEXTURE_CUBE_MAP_NEGATIVE_X :: GLenum a
  gl_TEXTURE_CUBE_MAP_POSITIVE_Y :: GLenum a
  gl_TEXTURE_CUBE_MAP_NEGATIVE_Y :: GLenum a
  gl_TEXTURE_CUBE_MAP_POSITIVE_Z :: GLenum a
  gl_TEXTURE_CUBE_MAP_NEGATIVE_Z :: GLenum a
  gl_MAX_CUBE_MAP_TEXTURE_SIZE :: GLenum a
  gl_TEXTURE0 :: GLenum a
  gl_TEXTURE1 :: GLenum a
  gl_TEXTURE2 :: GLenum a
  gl_TEXTURE3 :: GLenum a
  gl_TEXTURE4 :: GLenum a
  gl_TEXTURE5 :: GLenum a
  gl_TEXTURE6 :: GLenum a
  gl_TEXTURE7 :: GLenum a
  gl_TEXTURE8 :: GLenum a
  gl_TEXTURE9 :: GLenum a
  gl_TEXTURE10 :: GLenum a
  gl_TEXTURE11 :: GLenum a
  gl_TEXTURE12 :: GLenum a
  gl_TEXTURE13 :: GLenum a
  gl_TEXTURE14 :: GLenum a
  gl_TEXTURE15 :: GLenum a
  gl_TEXTURE16 :: GLenum a
  gl_TEXTURE17 :: GLenum a
  gl_TEXTURE18 :: GLenum a
  gl_TEXTURE19 :: GLenum a
  gl_TEXTURE20 :: GLenum a
  gl_TEXTURE21 :: GLenum a
  gl_TEXTURE22 :: GLenum a
  gl_TEXTURE23 :: GLenum a
  gl_TEXTURE24 :: GLenum a
  gl_TEXTURE25 :: GLenum a
  gl_TEXTURE26 :: GLenum a
  gl_TEXTURE27 :: GLenum a
  gl_TEXTURE28 :: GLenum a
  gl_TEXTURE29 :: GLenum a
  gl_TEXTURE30 :: GLenum a
  gl_TEXTURE31 :: GLenum a
  gl_ACTIVE_TEXTURE :: GLenum a
  gl_REPEAT :: GLenum a
  gl_CLAMP_TO_EDGE :: GLenum a
  gl_MIRRORED_REPEAT :: GLenum a
  gl_FLOAT_VEC2 :: GLenum a
  gl_FLOAT_VEC3 :: GLenum a
  gl_FLOAT_VEC4 :: GLenum a
  gl_INT_VEC2 :: GLenum a
  gl_INT_VEC3 :: GLenum a
  gl_INT_VEC4 :: GLenum a
  gl_BOOL :: GLenum a
  gl_BOOL_VEC2 :: GLenum a
  gl_BOOL_VEC3 :: GLenum a
  gl_BOOL_VEC4 :: GLenum a
  gl_FLOAT_MAT2 :: GLenum a
  gl_FLOAT_MAT3 :: GLenum a
  gl_FLOAT_MAT4 :: GLenum a
  gl_SAMPLER_2D :: GLenum a
  gl_SAMPLER_CUBE :: GLenum a
  gl_VERTEX_ATTRIB_ARRAY_ENABLED :: GLenum a
  gl_VERTEX_ATTRIB_ARRAY_SIZE :: GLenum a
  gl_VERTEX_ATTRIB_ARRAY_STRIDE :: GLenum a
  gl_VERTEX_ATTRIB_ARRAY_TYPE :: GLenum a
  gl_VERTEX_ATTRIB_ARRAY_NORMALIZED :: GLenum a
  gl_VERTEX_ATTRIB_ARRAY_POINTER :: GLenum a
  gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: GLenum a
  gl_IMPLEMENTATION_COLOR_READ_TYPE :: GLenum a
  gl_IMPLEMENTATION_COLOR_READ_FORMAT :: GLenum a
  gl_COMPILE_STATUS :: GLenum a
  gl_LOW_FLOAT :: GLenum a
  gl_MEDIUM_FLOAT :: GLenum a
  gl_HIGH_FLOAT :: GLenum a
  gl_LOW_INT :: GLenum a
  gl_MEDIUM_INT :: GLenum a
  gl_HIGH_INT :: GLenum a
  gl_FRAMEBUFFER :: GLenum a
  gl_RENDERBUFFER :: GLenum a
  gl_RGBA4 :: GLenum a
  gl_RGB5_A1 :: GLenum a
  gl_RGB565 :: GLenum a
  gl_DEPTH_COMPONENT16 :: GLenum a
  gl_STENCIL_INDEX :: GLenum a
  gl_STENCIL_INDEX8 :: GLenum a
  gl_DEPTH_STENCIL :: GLenum a
  gl_RENDERBUFFER_WIDTH :: GLenum a
  gl_RENDERBUFFER_HEIGHT :: GLenum a
  gl_RENDERBUFFER_INTERNAL_FORMAT :: GLenum a
  gl_RENDERBUFFER_RED_SIZE :: GLenum a
  gl_RENDERBUFFER_GREEN_SIZE :: GLenum a
  gl_RENDERBUFFER_BLUE_SIZE :: GLenum a
  gl_RENDERBUFFER_ALPHA_SIZE :: GLenum a
  gl_RENDERBUFFER_DEPTH_SIZE :: GLenum a
  gl_RENDERBUFFER_STENCIL_SIZE :: GLenum a
  gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: GLenum a
  gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: GLenum a
  gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: GLenum a
  gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: GLenum a
  gl_COLOR_ATTACHMENT0 :: GLenum a
  gl_DEPTH_ATTACHMENT :: GLenum a
  gl_STENCIL_ATTACHMENT :: GLenum a
  gl_DEPTH_STENCIL_ATTACHMENT :: GLenum a
  gl_NONE :: GLenum a
  gl_FRAMEBUFFER_COMPLETE :: GLenum a
  gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: GLenum a
  gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: GLenum a
  gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: GLenum a
  gl_FRAMEBUFFER_UNSUPPORTED :: GLenum a
  gl_FRAMEBUFFER_BINDING :: GLenum a
  gl_RENDERBUFFER_BINDING :: GLenum a
  gl_MAX_RENDERBUFFER_SIZE :: GLenum a
  gl_INVALID_FRAMEBUFFER_OPERATION :: GLenum a
  --gl_UNPACK_FLIP_Y_WEBGL :: GLenum a
  --gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: GLenum a
  --gl_CONTEXT_LOST_WEBGL :: GLenum a
  --gl_UNPACK_COLORSPACE_CONVERSION_WEBGL :: GLenum a
  --gl_BROWSER_DEFAULT_WEBGL :: GLenum a
