{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Glucose where

import           Control.Monad        (forM_)
import           Data.Bits            (Bits)
import           Data.Proxy
import           Data.Vector.Storable
import           Data.Word            (Word32)

#ifdef ghcjs_HOST_OS
import           JSDOM.Types          (ToJSVal)
#endif

data ShaderPrecisionFormat a = ShaderPrecisionFormat { spfRangeMin  :: a
                                                     , spfRangeMax  :: a
                                                     , spfPrecision :: a
                                                     }

data ActiveInfo a = ActiveInfo { aiSize :: a
                               , aiType :: a
                               , aiName :: String
                               }

class BufferableData m from to where
  withBufferable :: from -> (to -> m a) -> m a

data GLES
  m
  program
  shader
  texture
  uniformlocation
  clampf
  float
  enum
  uint
  int
  intptr
  boolean
  sizei
  ptr
  bufferabledata
  imagedata
  buffer
  framebuffer
  renderbuffer
  floatarray
  intarray
  uintarray
  vertexarrayobject
  extension =

  GLES { glActiveTexture :: enum -> m ()
       , glAttachShader :: program -> shader -> m ()
       , glBindAttribLocation :: program -> uint -> String -> m ()
       , glBindBuffer :: enum -> buffer -> m ()
       , glBindFramebuffer :: enum -> framebuffer -> m ()
       , glBindRenderbuffer :: enum -> renderbuffer -> m ()
       , glBindTexture :: enum -> texture -> m ()
       , glBindVertexArray :: vertexarrayobject -> m ()
       , glBlendColor :: clampf -> clampf -> clampf -> clampf -> m ()
       , glBlendEquation :: enum -> m ()
       , glBlendEquationSeparate :: enum -> enum -> m ()
       , glBlendFunc :: enum -> enum -> m ()
       , glBlendFuncSeparate :: enum -> enum -> enum -> enum -> m ()
       , glBufferData :: forall from. BufferableData m from bufferabledata => enum -> from -> enum -> m ()
       , glBufferSubData :: forall from. BufferableData m from bufferabledata => enum -> intptr -> from -> m ()
       , glCheckFramebufferStatus :: enum -> m enum
       , glClear :: Word32 -> m ()
       , glClearColor :: clampf -> clampf -> clampf -> clampf -> m ()
       , glClearDepth :: clampf -> m ()
       , glClearStencil :: int -> m ()
       , glColorMask :: boolean -> boolean -> boolean -> boolean -> m ()
       , glCompileShader :: shader -> m ()
         -- | Copies pixels from the current @framebuffer@ into a 2D texture image.
       , glCopyTexImage2D
          :: enum
          -- ^ target
          -- A GLenum specifying the binding point target of the active texture. Possible values:
          --   * gl_TEXTURE_2D: A two-dimensional texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_X: Positive X face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_X: Negative X face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Y: Positive Y face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Y: Negative Y face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Z: Positive Z face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Z: Negative Z face for a cube-mapped texture.
          -> int
          -- ^ level
          -- A GLint specifying the level of detail. Level 0 is the base image level
          -- and level n is the nth mipmap reduction level.
          -> enum
          -- ^ internal format
          -- A GLint specifying the color components in the texture. Possible values:
          --   * gl_ALPHA: Discards the red, green and blue components and reads the alpha component.
          --   * gl_RGB: Discards the alpha components and reads the red, green and blue components.
          --   * gl_RGBA: Red, green, blue and alpha components are read from the color buffer.
          --   * gl_LUMINANCE: Each color component is a luminance component, alpha is 1.0.
          --   * gl_LUMINANCE_ALPHA: Each component is a luminance/alpha component.
          -> int
          -- ^ A GLint specifying the x coordinate of the lower left corner where to
          -- start copying.
          -> int
          -- ^ A GLint specifying the y coordinate of the lower left corner where to
          -- start copying.
          -> sizei
          -- ^ A GLsizei specifying the width of the texture.
          -> sizei
          -- ^ A GLsizei specifying the height of the texture.
          -> int
          -- ^ This parameter is ignored, but the WebGL docs say this:
          -- A GLint specifying the width of the border. Must be 0.
          -> m ()
       , glCopyTexSubImage2D
          :: enum
          -- ^ target
          -- A GLenum specifying the binding point target of the active texture. Possible values:
          --   * gl_TEXTURE_2D: A two-dimensional texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_X: Positive X face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_X: Negative X face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Y: Positive Y face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Y: Negative Y face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_POSITIVE_Z: Positive Z face for a cube-mapped texture.
          --   * gl_TEXTURE_CUBE_MAP_NEGATIVE_Z: Negative Z face for a cube-mapped texture.
          -> int
          -- ^ level
          -- A GLint specifying the level of detail. Level 0 is the base image level
          -- and level n is the nth mipmap reduction level.
          -> int
          -- ^ xoffset
          -- A GLint specifying the horizontal offset within the texture image.
          -> int
          -- ^ yoffset
          -- A GLint specifying the vertical offset within the texture image.
          -> int
          -- ^ A GLint specifying the x coordinate of the lower left corner where to
          -- start copying.
          -> int
          -- ^ A GLint specifying the y coordinate of the lower left corner where to
          -- start copying.
          -> sizei
          -- ^ A GLsizei specifying the width of the texture.
          -> sizei
          -- ^ A GLsizei specifying the height of the texture.
          -> m ()
       , glCreateBuffer ::  m buffer
       , glCreateFramebuffer ::  m framebuffer
       , glCreateProgram ::  m program
       , glCreateRenderbuffer ::  m renderbuffer
       , glCreateShader :: enum -> m shader
       , glCreateTexture ::  m texture
       , glCreateVertexArray :: m vertexarrayobject
       , glCullFace
          :: enum
          -- ^ mode
          -- A GLenum specifying whether front- or back-facing polygons are candidates
          -- for culling. The default value is gl.BACK. Possible values are:
          --   * gl_FRONT
          --   * gl_BACK
          --   * gl_FRONT_AND_BACK
          -> m ()

       , glDeleteBuffer :: buffer -> m ()
       , glDeleteFramebuffer :: framebuffer -> m ()
       , glDeleteProgram :: program -> m ()
       , glDeleteRenderbuffer :: renderbuffer -> m ()
       , glDeleteShader :: shader -> m ()
       , glDeleteTexture :: texture -> m ()
       , glDeleteVertexArray :: vertexarrayobject -> m ()
         -- | Specifies a function that compares incoming pixel depth to the current
         -- depth buffer value
       , glDepthFunc
          :: enum
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
       , glDepthMask
          :: boolean
          -- ^ flag
          -- A GLboolean specifying whether or not writing into the depth buffer is
          -- enabled. Default value: true, meaning that writing is enabled.
          -> m ()

         -- | Specifies the depth range mapping from normalized device coordinates to
         -- window or viewport coordinates.
       , glDepthRange
          :: clampf
          -- ^ zNear
          -- A @clampf@ specifying the mapping of the near clipping plane to window
          -- or viewport coordinates. Clamped to the range 0 to 1 and must be less
          -- than or equal to zFar. The default value is 0.
          -> clampf
          -- ^ zFar
          -- A @clampf@ specifying the mapping of the far clipping plane to window or
          -- viewport coordinates. Clamped to the range 0 to 1. The default value is 1.
          -> m ()

         -- | Detaches a previously attached @shader@ from a @program@.
       , glDetachShader
          :: program
          -- ^ The program to detach the shader from.
          -> shader
          -- ^ The fragment or vertex shader to detach.
          -> m ()

       , glDisable :: enum -> m ()
       , glDisableVertexAttribArray :: uint -> m ()
       , glDrawArrays :: enum -> int -> sizei -> m ()
       , glDrawElements :: enum -> sizei -> enum -> ptr -> m ()
       , glEnable :: enum -> m ()
       , glEnableVertexAttribArray :: uint -> m ()
       , glFinish ::  m ()
       , glFlush ::  m ()
       , glFramebufferRenderbuffer :: enum -> enum -> enum -> renderbuffer -> m ()
       , glFramebufferTexture2D :: enum -> enum -> enum -> texture -> int -> m ()
       , glFrontFace :: enum -> m ()
       , glGenerateMipmap :: enum -> m ()
       , glGetActiveAttrib :: program -> uint -> m (ActiveInfo int)
       , glGetActiveUniform :: program -> uint -> m (ActiveInfo int)
       , glGetAttachedShaders :: program -> m [shader]
       , glGetAttribLocation :: program -> String -> m int
       , glGetBufferParameter :: enum -> enum -> m (Either int enum)
       , glGetError ::  m enum
       , glGetExtension :: String -> m (Maybe extension)
       , glGetFramebufferAttachmentParameter :: enum -> enum -> enum -> m (Maybe enum, Maybe texture, Maybe int)
       , glGetProgramParameter :: program -> enum -> m (Either boolean int)
       , glGetProgramInfoLog :: program -> m String
       , glGetRenderbufferParameter :: enum -> enum -> m (Either int enum)
       , glGetShaderParameter :: shader -> enum -> m (Either boolean enum)
       , glGetShaderInfoLog :: shader -> m String
       , glGetShaderPrecisionFormat :: enum -> enum -> m (ShaderPrecisionFormat int)
       , glGetShaderSource :: shader -> m String
       , glGetSupportedExtensions ::  m [String]
       , glGetTexParameter :: enum -> enum -> m (Maybe enum, Maybe boolean, Maybe uint, Maybe int, Maybe float)
       , glGetUniformfv :: program -> uniformlocation -> floatarray -> m (Maybe floatarray)
       , glGetUniformiv :: program -> uniformlocation -> intarray -> m (Maybe intarray)
       , glGetUniformLocation :: program -> String -> m uniformlocation
       , glGetVertexAttribfv :: uint -> enum -> floatarray -> m (Maybe floatarray)
       , glGetVertexAttribiv :: uint -> enum -> intarray -> m (Maybe intarray)
       , glHint :: enum -> enum -> m ()
       , glIsBuffer :: buffer -> m boolean
       , glIsContextLost ::  m boolean
       , glIsEnabled :: enum -> m boolean
       , glIsFramebuffer :: framebuffer -> m boolean
       , glIsProgram :: program -> m boolean
       , glIsRenderbuffer :: renderbuffer -> m boolean
       , glIsShader :: shader -> m boolean
       , glIsTexture :: texture -> m boolean
       , glIsVertexArray :: vertexarrayobject -> m boolean
       , glLineWidth :: float -> m ()
       , glLinkProgram :: program -> m ()
       , glPixelStorei :: enum -> int -> m ()
       , glPolygonOffset :: float -> float -> m ()
       , glReleaseShaderCompiler ::  m ()
       , glRenderbufferStorage :: enum -> enum -> sizei -> sizei -> m ()
       , glSampleCoverage :: clampf -> boolean -> m ()
       , glScissor :: int -> int -> sizei -> sizei -> m ()
       , glShaderSource :: shader -> String -> m ()
       , glStencilFunc :: enum -> int -> uint -> m ()
       , glStencilFuncSeparate :: enum -> enum -> int -> uint -> m ()
       , glStencilMask :: uint -> m ()
       , glStencilMaskSeparate :: enum -> uint -> m ()
       , glStencilOp :: enum -> enum -> enum -> m ()
       , glStencilOpSeparate :: enum -> enum -> enum -> enum -> m ()
       , glTexParameterf :: enum -> enum -> float -> m ()
       , glTexParameteri :: enum -> enum -> int -> m ()
       --, glTexImage2DData :: enum -> int -> enum -> enum -> enum -> imagedata -> m ()
       , glTexImage2D :: enum -> int -> int -> enum -> enum -> imagedata -> m ()
       --, glTexImage2DCanvas :: enum -> int -> enum -> enum -> enum -> HTMLCanvasElement -> m ()
       --, glTexImage2DVideo :: enum -> int -> enum -> enum -> enum -> HTMLVideoElement -> m ()
       --, glTexSubImage2DData :: enum -> int -> int -> int -> enum -> enum -> imagedata -> m ()
       , glTexSubImage2D :: enum -> int -> int -> int -> enum -> enum -> imagedata -> m ()
       --, glTexSubImage2DCanvas :: enum -> int -> int -> int -> enum -> enum -> HTMLCanvasElement -> m ()
       --, glTexSubImage2DVideo :: enum -> int -> int -> int -> enum -> enum -> HTMLVideoElement -> m ()
       , glUniform1f :: uniformlocation -> float -> m ()
       , glUniform1fv :: uniformlocation -> floatarray -> m ()
       , glUniform1i :: uniformlocation -> int -> m ()
       , glUniform1iv :: uniformlocation -> intarray -> m ()
       , glUniform2f :: uniformlocation -> float -> float -> m ()
       , glUniform2fv :: uniformlocation -> floatarray -> m ()
       , glUniform2i :: uniformlocation -> int -> int -> m ()
       , glUniform2iv :: uniformlocation -> intarray -> m ()
       , glUniform3f :: uniformlocation -> float -> float -> float -> m ()
       , glUniform3fv :: uniformlocation -> floatarray -> m ()
       , glUniform3i :: uniformlocation -> int -> int -> int -> m ()
       , glUniform3iv :: uniformlocation -> intarray -> m ()
       , glUniform4f :: uniformlocation -> float -> float -> float -> float -> m ()
       , glUniform4fv :: uniformlocation -> floatarray -> m ()
       , glUniform4i :: uniformlocation -> int -> int -> int -> int -> m ()
       , glUniform4iv :: uniformlocation -> intarray -> m ()
       , glUniformMatrix2fv :: uniformlocation -> boolean -> floatarray -> m ()
       , glUniformMatrix3fv :: uniformlocation -> boolean -> floatarray -> m ()
       , glUniformMatrix4fv :: uniformlocation -> boolean -> floatarray -> m ()
       , glUseProgram :: program -> m ()
       , glValidateProgram :: program -> m ()
       , glVertexAttrib1f :: uint -> float -> m ()
       , glVertexAttrib1fv :: uint -> floatarray -> m ()
       , glVertexAttrib2f :: uint -> float -> float -> m ()
       , glVertexAttrib2fv :: uint -> floatarray -> m ()
       , glVertexAttrib3f :: uint -> float -> float -> float -> m ()
       , glVertexAttrib3fv :: uint -> floatarray -> m ()
       , glVertexAttrib4f :: uint -> float -> float -> float -> float -> m ()
       , glVertexAttrib4fv :: uint -> floatarray -> m ()
       , glVertexAttribPointer :: uint -> int -> enum -> boolean -> sizei -> intptr -> m ()
       , glViewport :: int -> int -> sizei -> sizei -> m ()

       , gl_DEPTH_BUFFER_BIT :: enum
       , gl_STENCIL_BUFFER_BIT :: enum
       , gl_COLOR_BUFFER_BIT :: enum
       , gl_POINTS :: enum
       , gl_LINES :: enum
       , gl_LINE_LOOP :: enum
       , gl_LINE_STRIP :: enum
       , gl_TRIANGLES :: enum
       , gl_TRIANGLE_STRIP :: enum
       , gl_TRIANGLE_FAN :: enum
       , gl_ZERO :: enum
       , gl_ONE :: enum
       , gl_SRC_COLOR :: enum
       , gl_ONE_MINUS_SRC_COLOR :: enum
       , gl_SRC_ALPHA :: enum
       , gl_ONE_MINUS_SRC_ALPHA :: enum
       , gl_DST_ALPHA :: enum
       , gl_ONE_MINUS_DST_ALPHA :: enum
       , gl_DST_COLOR :: enum
       , gl_ONE_MINUS_DST_COLOR :: enum
       , gl_SRC_ALPHA_SATURATE :: enum
       , gl_FUNC_ADD :: enum
       --, gl_BLEND_EQUATION :: enum
       , gl_BLEND_EQUATION_RGB :: enum
       , gl_BLEND_EQUATION_ALPHA :: enum
       , gl_FUNC_SUBTRACT :: enum
       , gl_FUNC_REVERSE_SUBTRACT :: enum
       , gl_BLEND_DST_RGB :: enum
       , gl_BLEND_SRC_RGB :: enum
       , gl_BLEND_DST_ALPHA :: enum
       , gl_BLEND_SRC_ALPHA :: enum
       , gl_CONSTANT_COLOR :: enum
       , gl_ONE_MINUS_CONSTANT_COLOR :: enum
       , gl_CONSTANT_ALPHA :: enum
       , gl_ONE_MINUS_CONSTANT_ALPHA :: enum
       , gl_BLEND_COLOR :: enum
       , gl_ARRAY_BUFFER :: enum
       , gl_ELEMENT_ARRAY_BUFFER :: enum
       , gl_ARRAY_BUFFER_BINDING :: enum
       , gl_ELEMENT_ARRAY_BUFFER_BINDING :: enum
       , gl_STREAM_DRAW :: enum
       , gl_STATIC_DRAW :: enum
       , gl_DYNAMIC_DRAW :: enum
       , gl_BUFFER_SIZE :: enum
       , gl_BUFFER_USAGE :: enum
       , gl_CURRENT_VERTEX_ATTRIB :: enum
       , gl_FRONT :: enum
       , gl_BACK :: enum
       , gl_FRONT_AND_BACK :: enum
       , gl_TEXTURE_2D :: enum
       , gl_CULL_FACE :: enum
       , gl_BLEND :: enum
       , gl_DITHER :: enum
       , gl_STENCIL_TEST :: enum
       , gl_DEPTH_TEST :: enum
       , gl_SCISSOR_TEST :: enum
       , gl_POLYGON_OFFSET_FILL :: enum
       , gl_SAMPLE_ALPHA_TO_COVERAGE :: enum
       , gl_SAMPLE_COVERAGE :: enum
       , gl_NO_ERROR :: enum
       , gl_INVALID_ENUM :: enum
       , gl_INVALID_VALUE :: enum
       , gl_INVALID_OPERATION :: enum
       , gl_OUT_OF_MEMORY :: enum
       , gl_CW :: enum
       , gl_CCW :: enum
       , gl_LINE_WIDTH :: enum
       , gl_ALIASED_POINT_SIZE_RANGE :: enum
       , gl_ALIASED_LINE_WIDTH_RANGE :: enum
       , gl_CULL_FACE_MODE :: enum
       , gl_FRONT_FACE :: enum
       , gl_DEPTH_RANGE :: enum
       , gl_DEPTH_WRITEMASK :: enum
       , gl_DEPTH_CLEAR_VALUE :: enum
       , gl_DEPTH_FUNC :: enum
       , gl_STENCIL_CLEAR_VALUE :: enum
       , gl_STENCIL_FUNC :: enum
       , gl_STENCIL_FAIL :: enum
       , gl_STENCIL_PASS_DEPTH_FAIL :: enum
       , gl_STENCIL_PASS_DEPTH_PASS :: enum
       , gl_STENCIL_REF :: enum
       , gl_STENCIL_VALUE_MASK :: enum
       , gl_STENCIL_WRITEMASK :: enum
       , gl_STENCIL_BACK_FUNC :: enum
       , gl_STENCIL_BACK_FAIL :: enum
       , gl_STENCIL_BACK_PASS_DEPTH_FAIL :: enum
       , gl_STENCIL_BACK_PASS_DEPTH_PASS :: enum
       , gl_STENCIL_BACK_REF :: enum
       , gl_STENCIL_BACK_VALUE_MASK :: enum
       , gl_STENCIL_BACK_WRITEMASK :: enum
       , gl_VIEWPORT :: enum
       , gl_SCISSOR_BOX :: enum
       , gl_COLOR_CLEAR_VALUE :: enum
       , gl_COLOR_WRITEMASK :: enum
       , gl_UNPACK_ALIGNMENT :: enum
       , gl_PACK_ALIGNMENT :: enum
       , gl_MAX_TEXTURE_SIZE :: enum
       , gl_MAX_VIEWPORT_DIMS :: enum
       , gl_SUBPIXEL_BITS :: enum
       , gl_RED_BITS :: enum
       , gl_GREEN_BITS :: enum
       , gl_BLUE_BITS :: enum
       , gl_ALPHA_BITS :: enum
       , gl_DEPTH_BITS :: enum
       , gl_STENCIL_BITS :: enum
       , gl_POLYGON_OFFSET_UNITS :: enum
       , gl_POLYGON_OFFSET_FACTOR :: enum
       , gl_TEXTURE_BINDING_2D :: enum
       , gl_SAMPLE_BUFFERS :: enum
       , gl_SAMPLES :: enum
       , gl_SAMPLE_COVERAGE_VALUE :: enum
       , gl_SAMPLE_COVERAGE_INVERT :: enum
       , gl_COMPRESSED_TEXTURE_FORMATS :: enum
       , gl_DONT_CARE :: enum
       , gl_FASTEST :: enum
       , gl_NICEST :: enum
       , gl_GENERATE_MIPMAP_HINT :: enum
       , gl_BYTE :: enum
       , gl_UNSIGNED_BYTE :: enum
       , gl_SHORT :: enum
       , gl_UNSIGNED_SHORT :: enum
       , gl_INT :: enum
       , gl_UNSIGNED_INT :: enum
       , gl_FLOAT :: enum
       , gl_DEPTH_COMPONENT :: enum
       , gl_ALPHA :: enum
       , gl_RGB :: enum
       , gl_RGBA :: enum
       , gl_LUMINANCE :: enum
       , gl_LUMINANCE_ALPHA :: enum
       , gl_UNSIGNED_SHORT_4_4_4_4 :: enum
       , gl_UNSIGNED_SHORT_5_5_5_1 :: enum
       , gl_UNSIGNED_SHORT_5_6_5 :: enum
       , gl_FRAGMENT_SHADER :: enum
       , gl_VERTEX_SHADER :: enum
       , gl_MAX_VERTEX_ATTRIBS :: enum
       , gl_MAX_VERTEX_UNIFORM_VECTORS :: enum
       , gl_MAX_VARYING_VECTORS :: enum
       , gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: enum
       , gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: enum
       , gl_MAX_TEXTURE_IMAGE_UNITS :: enum
       , gl_MAX_FRAGMENT_UNIFORM_VECTORS :: enum
       , gl_SHADER_TYPE :: enum
       , gl_DELETE_STATUS :: enum
       , gl_LINK_STATUS :: enum
       , gl_VALIDATE_STATUS :: enum
       , gl_ATTACHED_SHADERS :: enum
       , gl_ACTIVE_UNIFORMS :: enum
       , gl_ACTIVE_ATTRIBUTES :: enum
       , gl_SHADING_LANGUAGE_VERSION :: enum
       , gl_CURRENT_PROGRAM :: enum
       , gl_NEVER :: enum
       , gl_LESS :: enum
       , gl_EQUAL :: enum
       , gl_LEQUAL :: enum
       , gl_GREATER :: enum
       , gl_NOTEQUAL :: enum
       , gl_GEQUAL :: enum
       , gl_ALWAYS :: enum
       , gl_KEEP :: enum
       , gl_REPLACE :: enum
       , gl_INCR :: enum
       , gl_DECR :: enum
       , gl_INVERT :: enum
       , gl_INCR_WRAP :: enum
       , gl_DECR_WRAP :: enum
       , gl_VENDOR :: enum
       , gl_RENDERER :: enum
       , gl_VERSION :: enum
       , gl_NEAREST :: enum
       , gl_LINEAR :: enum
       , gl_NEAREST_MIPMAP_NEAREST :: enum
       , gl_LINEAR_MIPMAP_NEAREST :: enum
       , gl_NEAREST_MIPMAP_LINEAR :: enum
       , gl_LINEAR_MIPMAP_LINEAR :: enum
       , gl_TEXTURE_MAG_FILTER :: enum
       , gl_TEXTURE_MIN_FILTER :: enum
       , gl_TEXTURE_WRAP_S :: enum
       , gl_TEXTURE_WRAP_T :: enum
       , gl_TEXTURE :: enum
       , gl_TEXTURE_CUBE_MAP :: enum
       , gl_TEXTURE_BINDING_CUBE_MAP :: enum
       , gl_TEXTURE_CUBE_MAP_POSITIVE_X :: enum
       , gl_TEXTURE_CUBE_MAP_NEGATIVE_X :: enum
       , gl_TEXTURE_CUBE_MAP_POSITIVE_Y :: enum
       , gl_TEXTURE_CUBE_MAP_NEGATIVE_Y :: enum
       , gl_TEXTURE_CUBE_MAP_POSITIVE_Z :: enum
       , gl_TEXTURE_CUBE_MAP_NEGATIVE_Z :: enum
       , gl_MAX_CUBE_MAP_TEXTURE_SIZE :: enum
       , gl_TEXTURE0 :: enum
       , gl_TEXTURE1 :: enum
       , gl_TEXTURE2 :: enum
       , gl_TEXTURE3 :: enum
       , gl_TEXTURE4 :: enum
       , gl_TEXTURE5 :: enum
       , gl_TEXTURE6 :: enum
       , gl_TEXTURE7 :: enum
       , gl_TEXTURE8 :: enum
       , gl_TEXTURE9 :: enum
       , gl_TEXTURE10 :: enum
       , gl_TEXTURE11 :: enum
       , gl_TEXTURE12 :: enum
       , gl_TEXTURE13 :: enum
       , gl_TEXTURE14 :: enum
       , gl_TEXTURE15 :: enum
       , gl_TEXTURE16 :: enum
       , gl_TEXTURE17 :: enum
       , gl_TEXTURE18 :: enum
       , gl_TEXTURE19 :: enum
       , gl_TEXTURE20 :: enum
       , gl_TEXTURE21 :: enum
       , gl_TEXTURE22 :: enum
       , gl_TEXTURE23 :: enum
       , gl_TEXTURE24 :: enum
       , gl_TEXTURE25 :: enum
       , gl_TEXTURE26 :: enum
       , gl_TEXTURE27 :: enum
       , gl_TEXTURE28 :: enum
       , gl_TEXTURE29 :: enum
       , gl_TEXTURE30 :: enum
       , gl_TEXTURE31 :: enum
       , gl_ACTIVE_TEXTURE :: enum
       , gl_REPEAT :: enum
       , gl_CLAMP_TO_EDGE :: enum
       , gl_MIRRORED_REPEAT :: enum
       , gl_FLOAT_VEC2 :: enum
       , gl_FLOAT_VEC3 :: enum
       , gl_FLOAT_VEC4 :: enum
       , gl_INT_VEC2 :: enum
       , gl_INT_VEC3 :: enum
       , gl_INT_VEC4 :: enum
       , gl_BOOL :: enum
       , gl_BOOL_VEC2 :: enum
       , gl_BOOL_VEC3 :: enum
       , gl_BOOL_VEC4 :: enum
       , gl_FLOAT_MAT2 :: enum
       , gl_FLOAT_MAT3 :: enum
       , gl_FLOAT_MAT4 :: enum
       , gl_SAMPLER_2D :: enum
       , gl_SAMPLER_CUBE :: enum
       , gl_VERTEX_ATTRIB_ARRAY_ENABLED :: enum
       , gl_VERTEX_ATTRIB_ARRAY_SIZE :: enum
       , gl_VERTEX_ATTRIB_ARRAY_STRIDE :: enum
       , gl_VERTEX_ATTRIB_ARRAY_TYPE :: enum
       , gl_VERTEX_ATTRIB_ARRAY_NORMALIZED :: enum
       , gl_VERTEX_ATTRIB_ARRAY_POINTER :: enum
       , gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: enum
       , gl_IMPLEMENTATION_COLOR_READ_TYPE :: enum
       , gl_IMPLEMENTATION_COLOR_READ_FORMAT :: enum
       , gl_COMPILE_STATUS :: enum
       , gl_LOW_FLOAT :: enum
       , gl_MEDIUM_FLOAT :: enum
       , gl_HIGH_FLOAT :: enum
       , gl_LOW_INT :: enum
       , gl_MEDIUM_INT :: enum
       , gl_HIGH_INT :: enum
       , gl_FRAMEBUFFER :: enum
       , gl_RENDERBUFFER :: enum
       , gl_RGBA4 :: enum
       , gl_RGB5_A1 :: enum
       , gl_RGB565 :: enum
       , gl_DEPTH_COMPONENT16 :: enum
       , gl_STENCIL_INDEX :: enum
       , gl_STENCIL_INDEX8 :: enum
       , gl_DEPTH_STENCIL :: enum
       , gl_RENDERBUFFER_WIDTH :: enum
       , gl_RENDERBUFFER_HEIGHT :: enum
       , gl_RENDERBUFFER_INTERNAL_FORMAT :: enum
       , gl_RENDERBUFFER_RED_SIZE :: enum
       , gl_RENDERBUFFER_GREEN_SIZE :: enum
       , gl_RENDERBUFFER_BLUE_SIZE :: enum
       , gl_RENDERBUFFER_ALPHA_SIZE :: enum
       , gl_RENDERBUFFER_DEPTH_SIZE :: enum
       , gl_RENDERBUFFER_STENCIL_SIZE :: enum
       , gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: enum
       , gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: enum
       , gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: enum
       , gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: enum
       , gl_COLOR_ATTACHMENT0 :: enum
       , gl_DEPTH_ATTACHMENT :: enum
       , gl_STENCIL_ATTACHMENT :: enum
       , gl_DEPTH_STENCIL_ATTACHMENT :: enum
       , gl_NONE :: enum
       , gl_FRAMEBUFFER_COMPLETE :: enum
       , gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: enum
       , gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: enum
       , gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: enum
       , gl_FRAMEBUFFER_UNSUPPORTED :: enum
       , gl_FRAMEBUFFER_BINDING :: enum
       , gl_RENDERBUFFER_BINDING :: enum
       , gl_MAX_RENDERBUFFER_SIZE :: enum
       , gl_INVALID_FRAMEBUFFER_OPERATION :: enum
       --, gl_UNPACK_FLIP_Y_WEBGL :: enum
       --, gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: enum
       --, gl_CONTEXT_LOST_WEBGL :: enum
       --, gl_UNPACK_COLORSPACE_CONVERSION_WEBGL :: enum
       --, gl_BROWSER_DEFAULT_WEBGL :: enum

         -- | A true value.
       , true  :: boolean
         -- | A false value.
       , false :: boolean

       , withFloatArray :: forall a b. (Storable a, Real a)     => Vector a -> (floatarray -> m b) -> m b
       , withIntArray   :: forall a b. (Storable a, Integral a) => Vector a -> (intarray   -> m b) -> m b
       , withUintArray  :: forall a b. (Storable a, Integral a) => Vector a -> (uintarray  -> m b) -> m b

       , fromFloatArray :: floatarray -> m (Vector Float)
       , fromIntArray   :: intarray   -> m (Vector Int)
       , fromUintArray  :: uintarray  -> m (Vector Word32)

       }

class ( Monad (M a)
      , Eq (GLBoolean a)
      , Num (GLFloat a), Num (GLEnum a), Num (GLClampf a), Num (GLInt a)
      , Num (GLUint a) , Num (GLSizei a), Num (GLIntptr a)
      , Integral (GLEnum a), Integral (GLBoolean a)
      , Bits (GLEnum a), Show (GLEnum a)
      , Storable (GLFloat a), Storable (GLInt a), Storable (GLUint a)
      ) => IsGLES a where
  type M a :: * -> *
  type GLProgram a
  type GLShader a
  type GLTexture a
  type GLUniformlocation a
  type GLClampf a
  type GLFloat a
  type GLEnum a
  type GLUint a
  type GLInt a
  type GLIntptr a
  type GLBoolean a
  type GLSizei a
  type GLPtr a
  type GLBufferabledata a
  type GLImagedata a
  type GLBuffer a
  type GLFramebuffer a
  type GLRenderbuffer a
  type GLFloatarray a
  type GLIntarray a
  type GLUintarray a
  type GLVertexArrayObject a
  type GLExtension a

  gles :: a
       -> GLES (M a)
               (GLProgram a)
               (GLShader a)
               (GLTexture a)
               (GLUniformlocation a)
               (GLClampf a)
               (GLFloat a)
               (GLEnum a)
               (GLUint a)
               (GLInt a)
               (GLIntptr a)
               (GLBoolean a)
               (GLSizei a)
               (GLPtr a)
               (GLBufferabledata a)
               (GLImagedata a)
               (GLBuffer a)
               (GLFramebuffer a)
               (GLRenderbuffer a)
               (GLFloatarray a)
               (GLIntarray a)
               (GLUintarray a)
               (GLVertexArrayObject a)
               (GLExtension a)
