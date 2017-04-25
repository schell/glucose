{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Glucose.WebGL where

--import           Data.Vector            (Vector)
--import qualified Data.Vector            as V
--import           Foreign.C.String
--import           Foreign.Marshal.Array               (allocaArray, peekArray,
--                                                      withArray)
--import           Foreign.Marshal.Utils               (with)
--import           Foreign.Ptr                         (Ptr, intPtrToPtr, nullPtr)
--import           Foreign.Storable                    (Storable)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, ask, asks)
import           GHCJS.DOM.Types                     as GL
import           GHCJS.DOM.WebGLRenderingContextBase as GL

import           Graphics.Glucose

data WebGL = WebGL

--createWith :: (Storable b, Num a) => (a -> Ptr b -> IO ()) -> IO b
--createWith f = do
--  [x] <- allocaArray 1 $ \ptr -> do
--    f 1 ptr
--    peekArray 1 ptr
--  return x
--
--deleteWith :: (Storable b, Num a) => (a -> Ptr b -> IO ()) -> b -> IO ()
--deleteWith f b = withArray [b] $ f 1
--
--getActiveThing f program index =
--  allocaArray 1 $ \lenPtr ->
--    allocaArray 1 $ \szPtr ->
--      allocaArray 1 $ \typPtr ->
--        withCString (replicate 64 ' ') $ \cStr -> do
--          f program index 64 lenPtr szPtr typPtr cStr
--          [len] <- peekArray 1 lenPtr
--          [sz]  <- peekArray 1 szPtr
--          [typ] <- peekArray 1 typPtr
--          str   <- peekCString cStr
--          return (len, sz, typ, str)

class (MonadIO m, MonadJSM m) => WebGLConstraint m where
  readCtx :: m WebGLRenderingContextBase

instance GLES WebGL where
  type C WebGL = WebGLConstraint

  type Program         WebGL = GL.WebGLProgram
  type Shader          WebGL = GL.WebGLShader
  type Texture         WebGL = GL.WebGLTexture
  type UniformLocation WebGL = GL.WebGLUniformLocation

  type GLclampf        WebGL = GL.GLclampf
  type GLfloat         WebGL = GL.GLfloat
  type GLenum          WebGL = GL.GLenum
  type GLuint          WebGL = GL.GLuint
  type GLint           WebGL = GL.GLint
  type GLintptr        WebGL = GL.GLintptr
  type GLbitfield      WebGL = GL.GLbitfield
  type GLboolean       WebGL = GL.GLboolean
  type GLsizei         WebGL = GL.GLsizei
  type GLstring        WebGL = String
  type GLptr           WebGL = GL.GLintptr

  type BufferableData  WebGL = GL.ArrayBuffer
  type ImageData       WebGL = GL.ImageData

  type Buffer          WebGL = GL.GLuint
  type Framebuffer     WebGL = GL.GLuint
  type Renderbuffer    WebGL = GL.GLuint
  type FloatArray      WebGL = GL.Float32Array
  type IntArray        WebGL = GL.Int32Array
  type UintArray       WebGL = GL.Uint32Array
  type Extension       WebGL = ()

  glActiveTexture texture = readCtx >>= \ctx -> GL.activeTexture ctx texture
  glAttachShader program shader = readCtx >>= \ctx ->
    GL.attachShader ctx (Just program) (Just shader)
  glBindAttribLocation p n s = withCString s $ GL.bindAttribLocation p n
  glBindBuffer = GL.bindBuffer
  glBindFramebuffer = GL.bindFramebuffer
  glBindRenderbuffer = GL.bindRenderbuffer
  glBindTexture = GL.bindTexture
  glBlendColor = GL.blendColor
  glBlendEquation = GL.blendEquation
  glBlendEquationSeparate = GL.blendEquationSeparate
  glBlendFunc = GL.blendFunc
  glBlendFuncSeparate = GL.blendFuncSeparate
  glBufferData target = uncurry $ GL.bufferData target
  glBufferSubData target offset = uncurry $ GL.bufferSubData target offset
  glCheckFramebufferStatus = GL.checkFramebufferStatus
  glClear = GL.clear
  glClearColor = GL.clearColor
  glClearDepth = GL.clearDepth . realToFrac
  glClearStencil = GL.clearStencil
  glColorMask = GL.colorMask
  glCompileShader = GL.compileShader
  glCopyTexImage2D = GL.copyTexImage2D
  glCopyTexSubImage2D = GL.copyTexSubImage2D
  glCreateBuffer = createWith GL.createBuffer
  glCreateFramebuffer = createWith GL.createFramebuffer
  glCreateProgram = GL.createProgram
  glCreateRenderbuffer = createWith GL.createRenderbuffer
  glCreateShader = GL.createShader
  glCreateTexture = createWith GL.createTexture
  glCullFace = GL.cullFace
  glDeleteBuffer = deleteWith GL.deleteBuffer
  glDeleteFramebuffer = deleteWith GL.deleteFramebuffer
  glDeleteProgram = GL.deleteProgram
  glDeleteRenderbuffer = deleteWith GL.deleteRenderbuffer
  glDeleteShader = GL.deleteShader
  glDeleteTexture = deleteWith GL.deleteTexture
  glDepthFunc = GL.depthFunc
  glDepthMask = GL.depthMask
  glDepthRange x y = GL.depthRange (realToFrac x) (realToFrac y)
  glDetachShader = GL.detachShader
  glDisable = GL.disable
  glDisableVertexAttribArray = GL.disableVertexAttribArray
  glDrawArrays = GL.drawArrays
  glDrawElements = GL.drawElements
  glEnable = GL.enable
  glEnableVertexAttribArray = GL.enableVertexAttribArray
  glFinish = GL.finish
  glFlush = GL.flush
  glFramebufferRenderbuffer = GL.framebufferRenderbuffer
  glFramebufferTexture2D = GL.framebufferTexture2D
  glFrontFace = GL.frontFace
  glGenerateMipmap = GL.generateMipmap
  glGetActiveAttrib = getActiveThing GL.getActiveAttrib
  glGetActiveUniform = getActiveThing GL.getActiveUniform
  glGetAttachedShaders program =
    allocaArray 1 $ \countPtr ->
      allocaArray 256 $ \shadersPtr -> do
        GL.getAttachedShaders program 256 countPtr shadersPtr
        [count] <- peekArray 1 countPtr
        peekArray (fromIntegral count) shadersPtr
  glGetAttribLocation program name =
    withCString name $ GL.getAttribLocation program
  glGetBufferParameter target pname = do
    [i] <- allocaArray 1 $ \ptr -> do
      GL.getBufferParameter target pname ptr
      peekArray 1 ptr
    return $ if target == GL.BUFFER_SIZE
      then Left  $ fromIntegral i
      else Right $ fromIntegral i
  glGetError = GL.getError
  glGetExtension = const $ return Nothing
  glGetFramebufferAttachmentParameter target attachment pname
    | pname `elem` [ GL.FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
                   , GL.FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
                   ] =
      GLESGLenum . fromIntegral <$> getVal
    | pname == GL.FRAMEBUFFER_ATTACHMENT_OBJECT_NAME =
      GLESTexture . fromIntegral <$> getVal
    | pname == GL.FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL =
      GLESGLint . fromIntegral <$> getVal
    | otherwise = return GLESNone
    where getVal = allocaArray 1 $ \ptr -> do
            GL.getFramebufferAttachmentParameter target attachment pname ptr
            [val] <- peekArray 1 ptr
            return val
  -- | TODO: Flesh this out
  --glGetParameter pname
  --  --https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGet.xml
  --  -- Float32Array (with 2 elements)
  --  |    pname == GL.ALIASED_LINE_WIDTH_RANGE
  --    || pname == GL.ALIASED_POINT_SIZE_RANGE
  --    || pname == GL.DEPTH_RANGE = allocaArray 2 $ \ptr -> do
  --    GL.getFloatv pname ptr
  --    return $ GLESFloatArray (2, ptr)
  --  -- Float32Array (with 4 values)
  --  |    pname == GL.BLEND_COLOR
  --    || pname == GL.COLOR_CLEAR_VALUE = allocaArray 4 $ \ptr -> do
  --    GL.getFloatv pname ptr
  --    return $ GLESFloatArray
  --  GLboolean | pname == GL.BLEND
  --  GLboolean || pname == GL.CULL_FACE
  --  GLboolean || pname == GL.DEPTH_TEST
  --  GLboolean || pname == GL.DEPTH_WRITEMASK
  --  GLboolean || pname == GL.DITHER
  --  GLboolean || pname == GL.POLYGON_OFFSET_FILL
  --  GLboolean || pname == GL.SAMPLE_COVERAGE_INVERT
  --  GLboolean || pname == GL.SCISSOR_TEST
  --  GLboolean || pname == GL.STENCIL_TEST
  --  GLboolean || pname == GL.UNPACK_FLIP_Y_WEBGL
  --  GLboolean || pname == GL.UNPACK_PREMULTIPLY_ALPHA_WEBGL
  --  GLenum | pname == GL.ACTIVE_TEXTURE
  --  GLenum || pname == GL.BLEND_DST_ALPHA
  --  GLenum || pname == GL.BLEND_DST_RGB
  --  GLenum || pname == GL.BLEND_EQUATION
  --  GLenum || pname == GL.BLEND_EQUATION_ALPHA
  --  GLenum || pname == GL.BLEND_EQUATION_RGB
  --  GLenum || pname == GL.BLEND_SRC_ALPHA
  --  GLenum || pname == GL.BLEND_SRC_RGB
  --  GLenum || pname == GL.CULL_FACE_MODE
  --  GLenum || pname == GL.DEPTH_FUNC
  --  GLenum || pname == GL.FRONT_FACE
  --  GLenum || pname == GL.GENERATE_MIPMAP_HINT
  --  GLenum || pname == GL.IMPLEMENTATION_COLOR_READ_FORMAT
  --  GLenum || pname == GL.IMPLEMENTATION_COLOR_READ_TYPE
  --  GLenum || pname == GL.STENCIL_BACK_FAIL
  --  GLenum || pname == GL.STENCIL_BACK_FUNC
  --  GLenum || pname == GL.STENCIL_BACK_PASS_DEPTH_FAIL
  --  GLenum || pname == GL.STENCIL_BACK_PASS_DEPTH_PASS
  --  GLenum || pname == GL.STENCIL_FAIL
  --  GLenum || pname == GL.STENCIL_FUNC
  --  GLenum || pname == GL.STENCIL_PASS_DEPTH_FAIL
  --  GLenum || pname == GL.STENCIL_PASS_DEPTH_PASS
  --  GLenum || pname == GL.UNPACK_COLORSPACE_CONVERSION_WEBGL
  --  GLfloat | pname == GL.DEPTH_CLEAR_VALUE
  --  GLfloat || pname == GL.LINE_WIDTH
  --  GLfloat || pname == GL.POLYGON_OFFSET_FACTOR
  --  GLfloat || pname == GL.POLYGON_OFFSET_UNITS
  --  GLfloat || pname == GL.SAMPLE_COVERAGE_VALUE
  --  GLint | pname == GL.ALPHA_BITS
  --  GLint || pname == GL.BLUE_BITS
  --  GLint || pname == GL.DEPTH_BITS
  --  GLint || pname == GL.GREEN_BITS
  --  GLint || pname == GL.MAX_COMBINED_TEXTURE_IMAGE_UNITS
  --  GLint || pname == GL.MAX_CUBE_MAP_TEXTURE_SIZE
  --  GLint || pname == GL.MAX_FRAGMENT_UNIFORM_VECTORS
  --  GLint || pname == GL.MAX_RENDERBUFFER_SIZE
  --  GLint || pname == GL.MAX_TEXTURE_IMAGE_UNITS
  --  GLint || pname == GL.MAX_TEXTURE_SIZE
  --  GLint || pname == GL.MAX_VARYING_VECTORS
  --  GLint || pname == GL.MAX_VERTEX_ATTRIBS
  --  GLint || pname == GL.MAX_VERTEX_TEXTURE_IMAGE_UNITS
  --  GLint || pname == GL.MAX_VERTEX_UNIFORM_VECTORS
  --  GLint || pname == GL.PACK_ALIGNMENT
  --  GLint || pname == GL.RED_BITS
  --  GLint || pname == GL.SAMPLES
  --  GLint || pname == GL.SAMPLE_BUFFERS
  --  GLint || pname == GL.STENCIL_BACK_REF
  --  GLint || pname == GL.STENCIL_BITS
  --  GLint || pname == GL.STENCIL_CLEAR_VALUE
  --  GLint || pname == GL.STENCIL_REF
  --  GLint || pname == GL.SUBPIXEL_BITS
  --  GLint || pname == GL.UNPACK_ALIGNMENT
  --  GLuint | pname == GL.STENCIL_BACK_VALUE_MASK
  --  GLuint || pname == GL.STENCIL_BACK_WRITEMASK
  --  GLuint || pname == GL.STENCIL_VALUE_MASK
  --  GLuint || pname == GL.STENCIL_WRITEMASK
  --  Int32Array (with 2 elements) | pname == GL.MAX_VIEWPORT_DIMS
  --  Int32Array (with 4 elements) || pname == GL.SCISSOR_BOX
  --  Int32Array (with 4 elements)| pname == GL.VIEWPORT
  --  Uint32Array | pname == GL.COMPRESSED_TEXTURE_FORMATS
  --  WebGLBuffer | pname == GL.ARRAY_BUFFER_BINDING
  --  WebGLBuffer || pname == GL.ELEMENT_ARRAY_BUFFER_BINDING
  --  WebGLFramebuffer | pname == GL.FRAMEBUFFER_BINDING
  --  WebGLProgram | pname == GL.CURRENT_PROGRAM
  --  WebGLRenderbuffer | pname == GL.RENDERBUFFER_BINDING
  --  WebGLTexture | pname == GL.TEXTURE_BINDING_2D
  --  WebGLTexture || pname == GL.TEXTURE_BINDING_CUBE_MAP
  --  sequence<GLboolean> (with 4 values) | pname == GL.COLOR_WRITEMASK
  --  | otherwise = return GLESNone
  glGetProgramParameter program pname = do
    glint <- GL.getProgramParameter program pname
    let intValues =
            -- Returns a GLint indicating the number of attached shaders to a program.
          [ GL.ATTACHED_SHADERS
            -- Returns a GLint indicating the number of active attribute variables to a program.
          , GL.ACTIVE_ATTRIBUTES
            -- Returns a GLint indicating the number of active uniform variables to a program.
          , GL.ACTIVE_UNIFORMS
          ]
    return $ if pname `elem` intValues
               then Right glint
               else Left $ fromIntegral glint

  glGetProgramInfoLog program = withCString (replicate 1024 ' ') $ \ptr -> do
    GL.getProgramInfoLog program 1024 nullPtr ptr
    peekCString ptr
  glGetRenderbufferParameter target pname = allocaArray 1 $ \ptr -> do
    GL.getRenderbufferParameter target pname ptr
    [params] <- peekArray 1 ptr
    return $
      if pname == GL.RENDERBUFFER_INTERNAL_FORMAT
      then Right $ fromIntegral params
      else Left params
  glGetShaderParameter shader pname = readCtx >>= \ctx ->
    gl.getShaderParameter ctx shader pname
  glGetShaderInfoLog shader = readCtx >>= \ctx -> GL.getShaderInfoLog ctx shader
  glGetShaderPrecisionFormat shaderType precisionType =
    allocaArray 1 $ \precisionPtr -> allocaArray 2 $ \rangePtr -> do
      GL.getShaderPrecisionFormat shaderType precisionType rangePtr precisionPtr
      [precision]          <- peekArray 1 precisionPtr
      [rangeMin, rangeMax] <- peekArray 2 rangePtr
      return ShaderPrecisionFormat { spfRangeMin  = rangeMin
                                   , spfRangeMax  = rangeMax
                                   , spfPrecision = precision
                                   }
  glGetShaderSource shader = GL.getShaderSource
  glGetSupportedExtensions = GL.getSupportedExtensions
  glGetTexParameter target pname = readCtx >>= \ctx -> do
    jsval <- GL.getTexParameter target pname
    return $ case () of
      () | pname `elem` [ GL.TEXTURE_MAG_FILTER
                        , GL.TEXTURE_MIN_FILTER
                        , GL.TEXTURE_WRAP_S
                        , GL.TEXTURE_WRAP_T
                        ] -> GLESGLenum $ fromJSval jsval

         -- | pname == GL.TEXTURE_IMMUTABLE_FORMAT =
         --     allocaArray 1 $ \ptr -> do
         --       GL.getTextureParameteriv target pname ptr
         --       [bool] <- peekArray 1 ptr
         --       return $ GLESGLboolean $ fromIntegral bool

         -- | pname == GL.TEXTURE_IMMUTABLE_LEVELS =
         --   allocaArray 1 $ \ptr -> do
         --     GL.getTextureParameterIuiv target pname ptr
         --     [gluint] <- peekArray 1 ptr
         --     return $ GLESGLuint gluint

         -- | pname `elem` [ GL.TEXTURE_BASE_LEVEL
         --                , GL.TEXTURE_MAX_LEVEL
         --                ] = -- GLint
         --   allocaArray 1 $ \ptr -> do
         --     GL.getTextureParameteriv target pname ptr
         --     [glint] <- peekArray 1 ptr
         --     return $ GLESGLint glint

         -- | pname `elem` [ GL.TEXTURE_MAX_LOD
         --                , GL.TEXTURE_MIN_LOD
         --                ] = -- GLfloat
         --   allocaArray 1 $ \ptr -> do
         --     GL.getTextureParameterfv target pname ptr
         --     [glfloat] <- peekArray 1 ptr
         --     return $ GLESGLfloat glfloat
         | otherwise -> return GLESNone
  glGetUniformfv program location outArray = readCtx >>= \ctx -> do
    jsval <- GL.getUniform ctx (Just program) (Just location)
    mf32array <- liftJSM $ ((pFromJSVal :: _) jsval :: _)

    case mf32array of
        Just f32array -> return f32array
        Nothing       -> return ()
  glGetUniformiv program location _ = readCtx >>= \ctx ->
    GL.getUniform program location
  glGetUniformLocation program str =
    withCString str $ GL.getUniformLocation program
  glGetVertexAttribfv index pname = GL.getVertexAttrib index pname . snd
  glGetVertexAttribiv index pname = GL.getVertexAttrib index pname . snd
  glHint = GL.hint
  glIsBuffer = GL.isBuffer
  glIsContextLost = return 0
  glIsEnabled = GL.isEnabled
  glIsFramebuffer = GL.isFramebuffer
  glIsProgram = GL.isProgram
  glIsRenderbuffer = GL.isRenderbuffer
  glIsShader = GL.isShader
  glIsTexture = GL.isTexture
  glLineWidth = GL.lineWidth
  glLinkProgram = GL.linkProgram
  glPixelStorei = GL.pixelStorei
  glPolygonOffset = GL.polygonOffset
  glReleaseShaderCompiler = GL.releaseShaderCompiler
  glRenderbufferStorage = GL.renderbufferStorage
  glSampleCoverage = GL.sampleCoverage
  glScissor = GL.scissor
  glShaderSource shader src = withCString src $ \ptr ->
    with ptr $ \ptrptr -> GL.shaderSource shader 1 ptrptr nullPtr
  glStencilFunc = GL.stencilFunc
  glStencilFuncSeparate = GL.stencilFuncSeparate
  glStencilMask = GL.stencilMask
  glStencilMaskSeparate = GL.stencilMaskSeparate
  glStencilOp = GL.stencilOp
  glStencilOpSeparate = GL.stencilOpSeparate
  glTexParameterf = GL.texParameterf
  glTexParameteri = GL.texParameteri
  glTexImage2D target level internalFormat format typ ((w, h), dat) =
    GL.texImage2D target level internalFormat (fromIntegral w) (fromIntegral h) 0 format typ dat
  glTexSubImage2D target level x y format typ ((w, h), dat) =
    GL.texSubImage2D target level x y (fromIntegral w) (fromIntegral h) format typ dat
  glUniform1f  = GL.getUniform
  glUniform1fv loc (sz, dat) = GL.getUniform loc sz dat
  glUniform1i  = GL.getUniform
  glUniform1iv loc (sz, dat) = GL.getUniform loc sz dat
  glUniform2f  = GL.getUniform
  glUniform2fv loc (sz, dat) = GL.getUniform loc sz dat
  glUniform2i  = GL.getUniform
  glUniform2iv loc (sz, dat) = GL.getUniform loc sz dat
  glUniform3f  = GL.getUniform
  glUniform3fv loc (sz, dat) = GL.getUniform loc sz dat
  glUniform3i  = GL.getUniform
  glUniform3iv loc (sz, dat) = GL.getUniform loc sz dat
  glUniform4f  = GL.getUniform
  glUniform4fv loc (sz, dat) = GL.getUniform loc sz dat
  glUniform4i  = GL.getUniform
  glUniform4iv loc (sz, dat) = GL.getUniform loc sz dat
  glUniformMatrix2fv loc trans (sz, dat) = GL.uniformMatrix2fv loc sz trans dat
  glUniformMatrix3fv loc trans (sz, dat) = GL.uniformMatrix3fv loc sz trans dat
  glUniformMatrix4fv loc trans (sz, dat) = GL.uniformMatrix4fv loc sz trans dat
  glUseProgram = GL.useProgram
  glValidateProgram = GL.validateProgram
  glVertexAttrib1f = GL.vertexAttrib1f
  glVertexAttrib1fv loc (_, dat) = GL.vertexAttrib1fv loc dat
  glVertexAttrib2f = GL.vertexAttrib2f
  glVertexAttrib2fv loc (_, dat) = GL.vertexAttrib2fv loc dat
  glVertexAttrib3f = GL.vertexAttrib3f
  glVertexAttrib3fv loc (_, dat) = GL.vertexAttrib3fv loc dat
  glVertexAttrib4f = GL.vertexAttrib4f
  glVertexAttrib4fv loc (_, dat) = GL.vertexAttrib4fv loc dat
  glVertexAttribPointer index sz typ normed stride n =
    GL.vertexAttribPointer index sz typ normed stride (intPtrToPtr $ fromIntegral n)
  glViewport = GL.viewport

  gl_DEPTH_BUFFER_BIT  = GL.DEPTH_BUFFER_BIT
  gl_STENCIL_BUFFER_BIT  = GL.STENCIL_BUFFER_BIT
  gl_COLOR_BUFFER_BIT  = GL.COLOR_BUFFER_BIT
  gl_POINTS  = GL.POINTS
  gl_LINES  = GL.LINES
  gl_LINE_LOOP  = GL.LINE_LOOP
  gl_LINE_STRIP  = GL.LINE_STRIP
  gl_TRIANGLES  = GL.TRIANGLES
  gl_TRIANGLE_STRIP  = GL.TRIANGLE_STRIP
  gl_TRIANGLE_FAN  = GL.TRIANGLE_FAN
  gl_ZERO  = GL.ZERO
  gl_ONE  = GL.ONE
  gl_SRC_COLOR  = GL.SRC_COLOR
  gl_ONE_MINUS_SRC_COLOR  = GL.ONE_MINUS_SRC_COLOR
  gl_SRC_ALPHA  = GL.SRC_ALPHA
  gl_ONE_MINUS_SRC_ALPHA  = GL.ONE_MINUS_SRC_ALPHA
  gl_DST_ALPHA  = GL.DST_ALPHA
  gl_ONE_MINUS_DST_ALPHA  = GL.ONE_MINUS_DST_ALPHA
  gl_DST_COLOR  = GL.DST_COLOR
  gl_ONE_MINUS_DST_COLOR  = GL.ONE_MINUS_DST_COLOR
  gl_SRC_ALPHA_SATURATE  = GL.SRC_ALPHA_SATURATE
  gl_FUNC_ADD  = GL.FUNC_ADD
  --gl_BLEND_EQUATION  = GL.BLEND_EQUATION
  gl_BLEND_EQUATION_RGB  = GL.BLEND_EQUATION_RGB
  gl_BLEND_EQUATION_ALPHA  = GL.BLEND_EQUATION_ALPHA
  gl_FUNC_SUBTRACT  = GL.FUNC_SUBTRACT
  gl_FUNC_REVERSE_SUBTRACT  = GL.FUNC_REVERSE_SUBTRACT
  gl_BLEND_DST_RGB  = GL.BLEND_DST_RGB
  gl_BLEND_SRC_RGB  = GL.BLEND_SRC_RGB
  gl_BLEND_DST_ALPHA  = GL.BLEND_DST_ALPHA
  gl_BLEND_SRC_ALPHA  = GL.BLEND_SRC_ALPHA
  gl_CONSTANT_COLOR  = GL.CONSTANT_COLOR
  gl_ONE_MINUS_CONSTANT_COLOR  = GL.ONE_MINUS_CONSTANT_COLOR
  gl_CONSTANT_ALPHA  = GL.CONSTANT_ALPHA
  gl_ONE_MINUS_CONSTANT_ALPHA  = GL.ONE_MINUS_CONSTANT_ALPHA
  gl_BLEND_COLOR  = GL.BLEND_COLOR
  gl_ARRAY_BUFFER  = GL.ARRAY_BUFFER
  gl_ELEMENT_ARRAY_BUFFER  = GL.ELEMENT_ARRAY_BUFFER
  gl_ARRAY_BUFFER_BINDING  = GL.ARRAY_BUFFER_BINDING
  gl_ELEMENT_ARRAY_BUFFER_BINDING  = GL.ELEMENT_ARRAY_BUFFER_BINDING
  gl_STREAM_DRAW  = GL.STREAM_DRAW
  gl_STATIC_DRAW  = GL.STATIC_DRAW
  gl_DYNAMIC_DRAW  = GL.DYNAMIC_DRAW
  gl_BUFFER_SIZE  = GL.BUFFER_SIZE
  gl_BUFFER_USAGE  = GL.BUFFER_USAGE
  gl_CURRENT_VERTEX_ATTRIB  = GL.CURRENT_VERTEX_ATTRIB
  gl_FRONT  = GL.FRONT
  gl_BACK  = GL.BACK
  gl_FRONT_AND_BACK  = GL.FRONT_AND_BACK
  gl_TEXTURE_2D  = GL.TEXTURE_2D
  gl_CULL_FACE  = GL.CULL_FACE
  gl_BLEND  = GL.BLEND
  gl_DITHER  = GL.DITHER
  gl_STENCIL_TEST  = GL.STENCIL_TEST
  gl_DEPTH_TEST  = GL.DEPTH_TEST
  gl_SCISSOR_TEST  = GL.SCISSOR_TEST
  gl_POLYGON_OFFSET_FILL  = GL.POLYGON_OFFSET_FILL
  gl_SAMPLE_ALPHA_TO_COVERAGE  = GL.SAMPLE_ALPHA_TO_COVERAGE
  gl_SAMPLE_COVERAGE  = GL.SAMPLE_COVERAGE
  gl_NO_ERROR  = GL.NO_ERROR
  gl_INVALID_ENUM  = GL.INVALID_ENUM
  gl_INVALID_VALUE  = GL.INVALID_VALUE
  gl_INVALID_OPERATION  = GL.INVALID_OPERATION
  gl_OUT_OF_MEMORY  = GL.OUT_OF_MEMORY
  gl_CW  = GL.CW
  gl_CCW  = GL.CCW
  gl_LINE_WIDTH  = GL.LINE_WIDTH
  gl_ALIASED_POINT_SIZE_RANGE  = GL.ALIASED_POINT_SIZE_RANGE
  gl_ALIASED_LINE_WIDTH_RANGE  = GL.ALIASED_LINE_WIDTH_RANGE
  gl_CULL_FACE_MODE  = GL.CULL_FACE_MODE
  gl_FRONT_FACE  = GL.FRONT_FACE
  gl_DEPTH_RANGE  = GL.DEPTH_RANGE
  gl_DEPTH_WRITEMASK  = GL.DEPTH_WRITEMASK
  gl_DEPTH_CLEAR_VALUE  = GL.DEPTH_CLEAR_VALUE
  gl_DEPTH_FUNC  = GL.DEPTH_FUNC
  gl_STENCIL_CLEAR_VALUE  = GL.STENCIL_CLEAR_VALUE
  gl_STENCIL_FUNC  = GL.STENCIL_FUNC
  gl_STENCIL_FAIL  = GL.STENCIL_FAIL
  gl_STENCIL_PASS_DEPTH_FAIL  = GL.STENCIL_PASS_DEPTH_FAIL
  gl_STENCIL_PASS_DEPTH_PASS  = GL.STENCIL_PASS_DEPTH_PASS
  gl_STENCIL_REF  = GL.STENCIL_REF
  gl_STENCIL_VALUE_MASK  = GL.STENCIL_VALUE_MASK
  gl_STENCIL_WRITEMASK  = GL.STENCIL_WRITEMASK
  gl_STENCIL_BACK_FUNC  = GL.STENCIL_BACK_FUNC
  gl_STENCIL_BACK_FAIL  = GL.STENCIL_BACK_FAIL
  gl_STENCIL_BACK_PASS_DEPTH_FAIL  = GL.STENCIL_BACK_PASS_DEPTH_FAIL
  gl_STENCIL_BACK_PASS_DEPTH_PASS  = GL.STENCIL_BACK_PASS_DEPTH_PASS
  gl_STENCIL_BACK_REF  = GL.STENCIL_BACK_REF
  gl_STENCIL_BACK_VALUE_MASK  = GL.STENCIL_BACK_VALUE_MASK
  gl_STENCIL_BACK_WRITEMASK  = GL.STENCIL_BACK_WRITEMASK
  gl_VIEWPORT  = GL.VIEWPORT
  gl_SCISSOR_BOX  = GL.SCISSOR_BOX
  gl_COLOR_CLEAR_VALUE  = GL.COLOR_CLEAR_VALUE
  gl_COLOR_WRITEMASK  = GL.COLOR_WRITEMASK
  gl_UNPACK_ALIGNMENT  = GL.UNPACK_ALIGNMENT
  gl_PACK_ALIGNMENT  = GL.PACK_ALIGNMENT
  gl_MAX_TEXTURE_SIZE  = GL.MAX_TEXTURE_SIZE
  gl_MAX_VIEWPORT_DIMS  = GL.MAX_VIEWPORT_DIMS
  gl_SUBPIXEL_BITS  = GL.SUBPIXEL_BITS
  gl_RED_BITS  = GL.RED_BITS
  gl_GREEN_BITS  = GL.GREEN_BITS
  gl_BLUE_BITS  = GL.BLUE_BITS
  gl_ALPHA_BITS  = GL.ALPHA_BITS
  gl_DEPTH_BITS  = GL.DEPTH_BITS
  gl_STENCIL_BITS  = GL.STENCIL_BITS
  gl_POLYGON_OFFSET_UNITS  = GL.POLYGON_OFFSET_UNITS
  gl_POLYGON_OFFSET_FACTOR  = GL.POLYGON_OFFSET_FACTOR
  gl_TEXTURE_BINDING_2D  = GL.TEXTURE_BINDING_2D
  gl_SAMPLE_BUFFERS  = GL.SAMPLE_BUFFERS
  gl_SAMPLES  = GL.SAMPLES
  gl_SAMPLE_COVERAGE_VALUE  = GL.SAMPLE_COVERAGE_VALUE
  gl_SAMPLE_COVERAGE_INVERT  = GL.SAMPLE_COVERAGE_INVERT
  gl_COMPRESSED_TEXTURE_FORMATS  = GL.COMPRESSED_TEXTURE_FORMATS
  gl_DONT_CARE  = GL.DONT_CARE
  gl_FASTEST  = GL.FASTEST
  gl_NICEST  = GL.NICEST
  gl_GENERATE_MIPMAP_HINT  = GL.GENERATE_MIPMAP_HINT
  gl_BYTE  = GL.BYTE
  gl_UNSIGNED_BYTE  = GL.UNSIGNED_BYTE
  gl_SHORT  = GL.SHORT
  gl_UNSIGNED_SHORT  = GL.UNSIGNED_SHORT
  gl_INT  = GL.INT
  gl_UNSIGNED_INT  = GL.UNSIGNED_INT
  gl_FLOAT  = GL.FLOAT
  gl_DEPTH_COMPONENT  = GL.DEPTH_COMPONENT
  gl_ALPHA  = GL.ALPHA
  gl_RGB  = GL.RGB
  gl_RGBA  = GL.RGBA
  gl_LUMINANCE  = GL.LUMINANCE
  gl_LUMINANCE_ALPHA  = GL.LUMINANCE_ALPHA
  gl_UNSIGNED_SHORT_4_4_4_4  = GL.UNSIGNED_SHORT_4_4_4_4
  gl_UNSIGNED_SHORT_5_5_5_1  = GL.UNSIGNED_SHORT_5_5_5_1
  gl_UNSIGNED_SHORT_5_6_5  = GL.UNSIGNED_SHORT_5_6_5
  gl_FRAGMENT_SHADER  = GL.FRAGMENT_SHADER
  gl_VERTEX_SHADER  = GL.VERTEX_SHADER
  gl_MAX_VERTEX_ATTRIBS  = GL.MAX_VERTEX_ATTRIBS
  gl_MAX_VERTEX_UNIFORM_VECTORS  = GL.MAX_VERTEX_UNIFORM_VECTORS
  gl_MAX_VARYING_VECTORS  = GL.MAX_VARYING_VECTORS
  gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS  = GL.MAX_COMBINED_TEXTURE_IMAGE_UNITS
  gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS  = GL.MAX_VERTEX_TEXTURE_IMAGE_UNITS
  gl_MAX_TEXTURE_IMAGE_UNITS  = GL.MAX_TEXTURE_IMAGE_UNITS
  gl_MAX_FRAGMENT_UNIFORM_VECTORS  = GL.MAX_FRAGMENT_UNIFORM_VECTORS
  gl_SHADER_TYPE  = GL.SHADER_TYPE
  gl_DELETE_STATUS  = GL.DELETE_STATUS
  gl_LINK_STATUS  = GL.LINK_STATUS
  gl_VALIDATE_STATUS  = GL.VALIDATE_STATUS
  gl_ATTACHED_SHADERS  = GL.ATTACHED_SHADERS
  gl_ACTIVE_UNIFORMS  = GL.ACTIVE_UNIFORMS
  gl_ACTIVE_ATTRIBUTES  = GL.ACTIVE_ATTRIBUTES
  gl_SHADING_LANGUAGE_VERSION  = GL.SHADING_LANGUAGE_VERSION
  gl_CURRENT_PROGRAM  = GL.CURRENT_PROGRAM
  gl_NEVER  = GL.NEVER
  gl_LESS  = GL.LESS
  gl_EQUAL  = GL.EQUAL
  gl_LEQUAL  = GL.LEQUAL
  gl_GREATER  = GL.GREATER
  gl_NOTEQUAL  = GL.NOTEQUAL
  gl_GEQUAL  = GL.GEQUAL
  gl_ALWAYS  = GL.ALWAYS
  gl_KEEP  = GL.KEEP
  gl_REPLACE  = GL.REPLACE
  gl_INCR  = GL.INCR
  gl_DECR  = GL.DECR
  gl_INVERT  = GL.INVERT
  gl_INCR_WRAP  = GL.INCR_WRAP
  gl_DECR_WRAP  = GL.DECR_WRAP
  gl_VENDOR  = GL.VENDOR
  gl_RENDERER  = GL.RENDERER
  gl_VERSION  = GL.VERSION
  gl_NEAREST  = GL.NEAREST
  gl_LINEAR  = GL.LINEAR
  gl_NEAREST_MIPMAP_NEAREST  = GL.NEAREST_MIPMAP_NEAREST
  gl_LINEAR_MIPMAP_NEAREST  = GL.LINEAR_MIPMAP_NEAREST
  gl_NEAREST_MIPMAP_LINEAR  = GL.NEAREST_MIPMAP_LINEAR
  gl_LINEAR_MIPMAP_LINEAR  = GL.LINEAR_MIPMAP_LINEAR
  gl_TEXTURE_MAG_FILTER  = GL.TEXTURE_MAG_FILTER
  gl_TEXTURE_MIN_FILTER  = GL.TEXTURE_MIN_FILTER
  gl_TEXTURE_WRAP_S  = GL.TEXTURE_WRAP_S
  gl_TEXTURE_WRAP_T  = GL.TEXTURE_WRAP_T
  gl_TEXTURE  = GL.TEXTURE
  gl_TEXTURE_CUBE_MAP  = GL.TEXTURE_CUBE_MAP
  gl_TEXTURE_BINDING_CUBE_MAP  = GL.TEXTURE_BINDING_CUBE_MAP
  gl_TEXTURE_CUBE_MAP_POSITIVE_X  = GL.TEXTURE_CUBE_MAP_POSITIVE_X
  gl_TEXTURE_CUBE_MAP_NEGATIVE_X  = GL.TEXTURE_CUBE_MAP_NEGATIVE_X
  gl_TEXTURE_CUBE_MAP_POSITIVE_Y  = GL.TEXTURE_CUBE_MAP_POSITIVE_Y
  gl_TEXTURE_CUBE_MAP_NEGATIVE_Y  = GL.TEXTURE_CUBE_MAP_NEGATIVE_Y
  gl_TEXTURE_CUBE_MAP_POSITIVE_Z  = GL.TEXTURE_CUBE_MAP_POSITIVE_Z
  gl_TEXTURE_CUBE_MAP_NEGATIVE_Z  = GL.TEXTURE_CUBE_MAP_NEGATIVE_Z
  gl_MAX_CUBE_MAP_TEXTURE_SIZE  = GL.MAX_CUBE_MAP_TEXTURE_SIZE
  gl_TEXTURE0  = GL.TEXTURE0
  gl_TEXTURE1  = GL.TEXTURE1
  gl_TEXTURE2  = GL.TEXTURE2
  gl_TEXTURE3  = GL.TEXTURE3
  gl_TEXTURE4  = GL.TEXTURE4
  gl_TEXTURE5  = GL.TEXTURE5
  gl_TEXTURE6  = GL.TEXTURE6
  gl_TEXTURE7  = GL.TEXTURE7
  gl_TEXTURE8  = GL.TEXTURE8
  gl_TEXTURE9  = GL.TEXTURE9
  gl_TEXTURE10  = GL.TEXTURE10
  gl_TEXTURE11  = GL.TEXTURE11
  gl_TEXTURE12  = GL.TEXTURE12
  gl_TEXTURE13  = GL.TEXTURE13
  gl_TEXTURE14  = GL.TEXTURE14
  gl_TEXTURE15  = GL.TEXTURE15
  gl_TEXTURE16  = GL.TEXTURE16
  gl_TEXTURE17  = GL.TEXTURE17
  gl_TEXTURE18  = GL.TEXTURE18
  gl_TEXTURE19  = GL.TEXTURE19
  gl_TEXTURE20  = GL.TEXTURE20
  gl_TEXTURE21  = GL.TEXTURE21
  gl_TEXTURE22  = GL.TEXTURE22
  gl_TEXTURE23  = GL.TEXTURE23
  gl_TEXTURE24  = GL.TEXTURE24
  gl_TEXTURE25  = GL.TEXTURE25
  gl_TEXTURE26  = GL.TEXTURE26
  gl_TEXTURE27  = GL.TEXTURE27
  gl_TEXTURE28  = GL.TEXTURE28
  gl_TEXTURE29  = GL.TEXTURE29
  gl_TEXTURE30  = GL.TEXTURE30
  gl_TEXTURE31  = GL.TEXTURE31
  gl_ACTIVE_TEXTURE  = GL.ACTIVE_TEXTURE
  gl_REPEAT  = GL.REPEAT
  gl_CLAMP_TO_EDGE  = GL.CLAMP_TO_EDGE
  gl_MIRRORED_REPEAT  = GL.MIRRORED_REPEAT
  gl_FLOAT_VEC2  = GL.FLOAT_VEC2
  gl_FLOAT_VEC3  = GL.FLOAT_VEC3
  gl_FLOAT_VEC4  = GL.FLOAT_VEC4
  gl_INT_VEC2  = GL.INT_VEC2
  gl_INT_VEC3  = GL.INT_VEC3
  gl_INT_VEC4  = GL.INT_VEC4
  gl_BOOL  = GL.BOOL
  gl_BOOL_VEC2  = GL.BOOL_VEC2
  gl_BOOL_VEC3  = GL.BOOL_VEC3
  gl_BOOL_VEC4  = GL.BOOL_VEC4
  gl_FLOAT_MAT2  = GL.FLOAT_MAT2
  gl_FLOAT_MAT3  = GL.FLOAT_MAT3
  gl_FLOAT_MAT4  = GL.FLOAT_MAT4
  gl_SAMPLER_2D  = GL.SAMPLER_2D
  gl_SAMPLER_CUBE  = GL.SAMPLER_CUBE
  gl_VERTEX_ATTRIB_ARRAY_ENABLED  = GL.VERTEX_ATTRIB_ARRAY_ENABLED
  gl_VERTEX_ATTRIB_ARRAY_SIZE  = GL.VERTEX_ATTRIB_ARRAY_SIZE
  gl_VERTEX_ATTRIB_ARRAY_STRIDE  = GL.VERTEX_ATTRIB_ARRAY_STRIDE
  gl_VERTEX_ATTRIB_ARRAY_TYPE  = GL.VERTEX_ATTRIB_ARRAY_TYPE
  gl_VERTEX_ATTRIB_ARRAY_NORMALIZED  = GL.VERTEX_ATTRIB_ARRAY_NORMALIZED
  gl_VERTEX_ATTRIB_ARRAY_POINTER  = GL.VERTEX_ATTRIB_ARRAY_POINTER
  gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING  = GL.VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
  gl_IMPLEMENTATION_COLOR_READ_TYPE  = GL.IMPLEMENTATION_COLOR_READ_TYPE
  gl_IMPLEMENTATION_COLOR_READ_FORMAT  = GL.IMPLEMENTATION_COLOR_READ_FORMAT
  gl_COMPILE_STATUS  = GL.COMPILE_STATUS
  gl_LOW_FLOAT  = GL.LOW_FLOAT
  gl_MEDIUM_FLOAT  = GL.MEDIUM_FLOAT
  gl_HIGH_FLOAT  = GL.HIGH_FLOAT
  gl_LOW_INT  = GL.LOW_INT
  gl_MEDIUM_INT  = GL.MEDIUM_INT
  gl_HIGH_INT  = GL.HIGH_INT
  gl_FRAMEBUFFER  = GL.FRAMEBUFFER
  gl_RENDERBUFFER  = GL.RENDERBUFFER
  gl_RGBA4  = GL.RGBA4
  gl_RGB5_A1  = GL.RGB5_A1
  gl_RGB565  = GL.RGB565
  gl_DEPTH_COMPONENT16  = GL.DEPTH_COMPONENT16
  gl_STENCIL_INDEX  = GL.STENCIL_INDEX
  gl_STENCIL_INDEX8  = GL.STENCIL_INDEX8
  gl_DEPTH_STENCIL  = GL.DEPTH_STENCIL
  gl_RENDERBUFFER_WIDTH  = GL.RENDERBUFFER_WIDTH
  gl_RENDERBUFFER_HEIGHT  = GL.RENDERBUFFER_HEIGHT
  gl_RENDERBUFFER_INTERNAL_FORMAT  = GL.RENDERBUFFER_INTERNAL_FORMAT
  gl_RENDERBUFFER_RED_SIZE  = GL.RENDERBUFFER_RED_SIZE
  gl_RENDERBUFFER_GREEN_SIZE  = GL.RENDERBUFFER_GREEN_SIZE
  gl_RENDERBUFFER_BLUE_SIZE  = GL.RENDERBUFFER_BLUE_SIZE
  gl_RENDERBUFFER_ALPHA_SIZE  = GL.RENDERBUFFER_ALPHA_SIZE
  gl_RENDERBUFFER_DEPTH_SIZE  = GL.RENDERBUFFER_DEPTH_SIZE
  gl_RENDERBUFFER_STENCIL_SIZE  = GL.RENDERBUFFER_STENCIL_SIZE
  gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE  = GL.FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
  gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME  = GL.FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
  gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL  = GL.FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
  gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  = GL.FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
  gl_COLOR_ATTACHMENT0  = GL.COLOR_ATTACHMENT0
  gl_DEPTH_ATTACHMENT  = GL.DEPTH_ATTACHMENT
  gl_STENCIL_ATTACHMENT  = GL.STENCIL_ATTACHMENT
  gl_DEPTH_STENCIL_ATTACHMENT  = GL.DEPTH_STENCIL_ATTACHMENT
  gl_NONE  = GL.NONE
  gl_FRAMEBUFFER_COMPLETE  = GL.FRAMEBUFFER_COMPLETE
  gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT  = GL.FRAMEBUFFER_INCOMPLETE_ATTACHMENT
  gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT  = GL.FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
  gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS  = GL.FRAMEBUFFER_INCOMPLETE_DIMENSIONS
  gl_FRAMEBUFFER_UNSUPPORTED  = GL.FRAMEBUFFER_UNSUPPORTED
  gl_FRAMEBUFFER_BINDING  = GL.FRAMEBUFFER_BINDING
  gl_RENDERBUFFER_BINDING  = GL.RENDERBUFFER_BINDING
  gl_MAX_RENDERBUFFER_SIZE  = GL.MAX_RENDERBUFFER_SIZE
  gl_INVALID_FRAMEBUFFER_OPERATION  = GL.INVALID_FRAMEBUFFER_OPERATION
  --gl_UNPACK_FLIP_Y_WEBGL  = GL.UNPACK_FLIP_Y_WEBGL
  --gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL  = GL.UNPACK_PREMULTIPLY_ALPHA_WEBGL
  --gl_CONTEXT_LOST_WEBGL  = GL.CONTEXT_LOST_WEBGL
  --gl_UNPACK_COLORSPACE_CONVERSION_WEBGL  = GL.UNPACK_COLORSPACE_CONVERSION_WEBGL
  --gl_BROWSER_DEFAULT_WEBGL  = GL.BROWSER_DEFAULT_WEBGL
