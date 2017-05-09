{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Glucose.WebGL where

import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.Foldable                     (toList)
import           Data.Maybe                        (fromJust, fromMaybe)
import qualified GHCJS.Buffer                      as B
import           GHCJS.Marshal                     (fromJSValUnchecked)
import           JSDOM.ImageData
import           JSDOM.Types                       as GL
import           JSDOM.WebGLActiveInfo
import           JSDOM.WebGLRenderingContextBase   as GL
import           JSDOM.WebGLShaderPrecisionFormat
import           Language.Javascript.JSaddle.Types (MonadJSM, liftJSM)

import           Graphics.Glucose

data WebGL = WebGL

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "new Float32Array($1)"
  allocFloat32ArrayJS :: JSVal -> IO JSVal

allocFloat32Array :: (MonadIO m, ToJSVal a, Real a) => [a] -> m Float32Array
allocFloat32Array xs = (Float32Array <$>) . liftIO $ do
  jsval <- toJSValListOf (map realToFrac xs :: [Float])
  allocFloat32ArrayJS jsval
#else
allocFloat32Array :: (MonadIO m, ToJSVal a, Real a) => [a] -> m Float32Array
allocFloat32Array = error "only available on ghcjs"
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "new Int32Array($1)"
  allocInt32ArrayJS :: JSVal -> IO JSVal

allocInt32Array :: (MonadIO m, ToJSVal a, Integral a) => [a] -> m Int32Array
allocInt32Array xs = (Int32Array <$>) . liftIO $ do
  jsval <- toJSValListOf (map fromIntegral xs :: [Int])
  allocInt32ArrayJS jsval
#else
allocInt32Array :: (MonadIO m, ToJSVal a, Num a) => [a] -> m Int32Array
allocInt32Array = error "only available on ghcjs"
#endif

class MonadJSM m => WebGLConstraint m where
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

  type Buffer          WebGL = GL.WebGLBuffer
  type Framebuffer     WebGL = GL.WebGLFramebuffer
  type Renderbuffer    WebGL = GL.WebGLRenderbuffer
  type FloatArray      WebGL = GL.Float32Array
  type IntArray        WebGL = GL.Int32Array
  type UintArray       WebGL = GL.Uint32Array
  type Extension       WebGL = JSVal

  true  = True
  false = False

  allocFloatArray = allocFloat32Array . toList
  allocIntArray   = allocInt32Array . toList

  glActiveTexture texture = readCtx >>= \ctx ->
    GL.activeTexture ctx texture
  glAttachShader program shader = readCtx >>= \ctx ->
    GL.attachShader ctx (Just program) (Just shader)
  glBindAttribLocation program loc name = readCtx >>= \ctx ->
    GL.bindAttribLocation ctx (Just program) loc name
  glBindBuffer enum buffer = readCtx >>= \ctx ->
    GL.bindBuffer ctx enum (Just buffer)
  glBindFramebuffer enum framebuffer = readCtx >>= \ctx ->
    GL.bindFramebuffer ctx enum (Just framebuffer)
  glBindRenderbuffer enum renderbuffer = readCtx >>= \ctx ->
    GL.bindRenderbuffer ctx enum (Just renderbuffer)
  glBindTexture enum texture = readCtx >>= \ctx ->
    GL.bindTexture ctx enum (Just texture)
  glBlendColor a b c d = readCtx >>= \ctx ->
    GL.blendColor ctx a b c d
  glBlendEquation enum = readCtx >>= \ctx ->
    GL.blendEquation ctx enum
  glBlendEquationSeparate a b = readCtx >>= \ctx ->
    GL.blendEquationSeparate ctx a b
  glBlendFunc a b = readCtx >>= \ctx ->
    GL.blendFunc ctx a b
  glBlendFuncSeparate a b c d = readCtx >>= \ctx ->
    GL.blendFuncSeparate ctx a b c d
  glBufferData target bufferdata usage = readCtx >>= \ctx ->
    GL.bufferData ctx target (Just bufferdata) usage
  glBufferSubData target offset bufferdata = readCtx >>= \ctx ->
    GL.bufferSubData ctx target offset (Just bufferdata)
  glCheckFramebufferStatus target = readCtx >>= \ctx ->
    GL.checkFramebufferStatus ctx target
  glClear bitfield = readCtx >>= \ctx ->
    GL.clear ctx bitfield
  glClearColor r g b a = readCtx >>= \ctx ->
    GL.clearColor ctx r g b a
  glClearDepth val = readCtx >>= \ctx ->
    GL.clearDepth ctx val
  glClearStencil val = readCtx >>= \ctx ->
    GL.clearStencil ctx val
  glColorMask a b c d = readCtx >>= \ctx ->
    GL.colorMask ctx a b c d
  glCompileShader shader = readCtx >>= \ctx ->
    GL.compileShader ctx (Just shader)
  glCopyTexImage2D target level internalformat x y w h _ = readCtx >>= \ctx ->
    GL.copyTexImage2D ctx target level internalformat x y w h 0
  glCopyTexSubImage2D target level xoffset yoffset x y w h = readCtx >>= \ctx ->
    GL.copyTexSubImage2D ctx target level xoffset yoffset x y w h
  glCreateBuffer =
    GL.createBuffer =<< readCtx
  glCreateFramebuffer =
    GL.createFramebuffer =<< readCtx
  glCreateProgram =
    GL.createProgram =<< readCtx
  glCreateRenderbuffer =
    GL.createRenderbuffer =<< readCtx
  glCreateShader shadertype = readCtx >>= \ctx ->
    GL.createShader ctx shadertype
  glCreateTexture =
    GL.createTexture =<< readCtx
  glCullFace mode = readCtx >>= \ctx ->
    GL.cullFace ctx mode
  glDeleteBuffer buffer = readCtx >>= \ctx ->
    GL.deleteBuffer ctx $ Just buffer
  glDeleteFramebuffer framebuffer = readCtx >>= \ctx ->
    GL.deleteFramebuffer ctx $ Just framebuffer
  glDeleteProgram program = readCtx >>= \ctx ->
    GL.deleteProgram ctx $ Just program
  glDeleteRenderbuffer renderbuffer = readCtx >>= \ctx ->
    GL.deleteRenderbuffer ctx $ Just renderbuffer
  glDeleteShader shader = readCtx >>= \ctx ->
    GL.deleteShader ctx $ Just shader
  glDeleteTexture tex = readCtx >>= \ctx ->
    GL.deleteTexture ctx $ Just tex
  glDepthFunc func = readCtx >>= \ctx ->
    GL.depthFunc ctx func
  glDepthMask flag = readCtx >>= \ctx ->
    GL.depthMask ctx flag
  glDepthRange znear zfar = readCtx >>= \ctx ->
    GL.depthRange ctx (realToFrac znear) (realToFrac zfar)
  glDetachShader program shader = readCtx >>= \ctx ->
    GL.detachShader ctx (Just program) (Just shader)
  glDisable val = readCtx >>= \ctx ->
    GL.disable ctx val
  glDisableVertexAttribArray attrib = readCtx >>= \ctx ->
    GL.disableVertexAttribArray ctx attrib
  glDrawArrays enum int size = readCtx >>= \ctx ->
    GL.drawArrays ctx enum int size
  glDrawElements enum size enum2 ptr = readCtx >>= \ctx ->
    GL.drawElements ctx enum size enum2 ptr
  glEnable enum = readCtx >>= \ctx -> GL.enable ctx enum
  glEnableVertexAttribArray uint = readCtx >>= \ctx ->
    GL.enableVertexAttribArray ctx uint
  glFinish = readCtx >>= GL.finish
  glFlush = readCtx >>= GL.flush
  glFramebufferRenderbuffer a b c renderbuffer = readCtx >>= \ctx ->
    GL.framebufferRenderbuffer ctx a b c (Just renderbuffer)
  glFramebufferTexture2D a b c texture int = readCtx >>= \ctx ->
    GL.framebufferTexture2D ctx a b c (Just texture) int
  glFrontFace enum = readCtx >>= \ctx ->
    GL.frontFace ctx enum
  glGenerateMipmap enum = readCtx >>= \ctx ->
    GL.generateMipmap ctx enum
  glGetActiveAttrib program loc = readCtx >>= \ctx -> do
    info <- GL.getActiveAttrib ctx (Just program) loc
    ActiveInfo <$> (fromIntegral <$> getSize info)
               <*> (fromIntegral <$> getType info)
               <*> getName info
  glGetActiveUniform program loc = readCtx >>= \ctx -> do
    info <- GL.getActiveUniform ctx (Just program) loc
    ActiveInfo <$> (fromIntegral <$> getSize info)
               <*> (fromIntegral <$> getType info)
               <*> getName info
  glGetAttachedShaders program = readCtx >>= \ctx ->
    fromMaybe [] <$> GL.getAttachedShaders ctx (Just program)
  glGetAttribLocation program name = readCtx >>= \ctx ->
    GL.getAttribLocation ctx (Just program) name
  glGetBufferParameter target pname = readCtx >>= \ctx -> do
    jsval <- GL.getBufferParameter ctx target pname
    i     <- liftJSM $ fromJSValUnchecked jsval
    return $ if target == GL.BUFFER_SIZE
      then Left i
      else Right $ fromIntegral i
  glGetError = readCtx >>= GL.getError
  glGetExtension name = readCtx >>= \ctx ->
    liftJSM . maybeNullOrUndefined =<< GL.getExtension ctx name
  glGetFramebufferAttachmentParameter target attachment pname =
    readCtx >>= \ctx -> do
      jsval <- GL.getFramebufferAttachmentParameter ctx target attachment pname
      liftJSM $ case () of
        _
          | pname `elem` [ GL.FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
                        , GL.FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
                        ] -> GLESGLenum <$> fromJSValUnchecked jsval
          | pname == GL.FRAMEBUFFER_ATTACHMENT_OBJECT_NAME ->
            GLESTexture <$> fromJSValUnchecked jsval
          | pname == GL.FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL ->
            GLESGLint <$> fromJSValUnchecked jsval
          | otherwise -> return GLESNone
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
  glGetProgramParameter program pname = readCtx >>= \ctx -> do
    jsval <- GL.getProgramParameter ctx (Just program) pname
    let intValues =
            -- Returns a GLint indicating the number of attached shaders to a program.
          [ GL.ATTACHED_SHADERS
            -- Returns a GLint indicating the number of active attribute variables to a program.
          , GL.ACTIVE_ATTRIBUTES
            -- Returns a GLint indicating the number of active uniform variables to a program.
          , GL.ACTIVE_UNIFORMS
          ]
    liftJSM $ if pname `elem` intValues
               then Right <$> fromJSValUnchecked jsval
               else Left <$> fromJSValUnchecked jsval

  glGetProgramInfoLog program = readCtx >>= \ctx ->
    fromMaybe "" <$> GL.getProgramInfoLog ctx (Just program)
  glGetRenderbufferParameter target pname = readCtx >>= \ctx -> do
    jsval <- GL.getRenderbufferParameter ctx target pname
    liftJSM $
      if pname == GL.RENDERBUFFER_INTERNAL_FORMAT
      then Right <$> fromJSValUnchecked jsval
      else Left <$> fromJSValUnchecked jsval
  glGetShaderParameter shader pname = readCtx >>= \ctx -> do
    jsval <- GL.getShaderParameter ctx (Just shader) pname
    liftJSM $ if pname `elem` [GL.DELETE_STATUS, GL.COMPILE_STATUS]
      then Left <$> fromJSValUnchecked jsval
      else Right <$> fromJSValUnchecked jsval
  glGetShaderInfoLog shader = readCtx >>= \ctx -> do
    mlog <- GL.getShaderInfoLog ctx (Just shader)
    return $ fromMaybe "" mlog
  glGetShaderPrecisionFormat shaderType precisionType = readCtx >>= \ctx -> do
    fmt <- GL.getShaderPrecisionFormat ctx shaderType precisionType
    precision <- getPrecision fmt
    rangeMin  <- getRangeMin fmt
    rangeMax  <- getRangeMax fmt
    return ShaderPrecisionFormat { spfRangeMin  = fromIntegral rangeMin
                                 , spfRangeMax  = fromIntegral rangeMax
                                 , spfPrecision = fromIntegral precision
                                 }
  glGetShaderSource shader = readCtx >>= \ctx ->
    fromMaybe "" <$> GL.getShaderSource ctx (Just shader)
  glGetSupportedExtensions = readCtx >>= \ctx ->
    fromMaybe [] <$> GL.getSupportedExtensions ctx
  glGetTexParameter target pname = readCtx >>= \ctx -> do
    jsval <- GL.getTexParameter ctx target pname
    liftJSM $ case () of
      () | pname `elem` [ GL.TEXTURE_MAG_FILTER
                        , GL.TEXTURE_MIN_FILTER
                        , GL.TEXTURE_WRAP_S
                        , GL.TEXTURE_WRAP_T
                        ] -> GLESGLenum <$> fromJSValUnchecked jsval
         -- WebGL2 stuff
         -- | pname == GL.TEXTURE_IMMUTABLE_FORMAT ->
         --   GLESGLboolean <$> fromJSValUnchecked bool
         --
         -- | pname == GL.TEXTURE_IMMUTABLE_LEVELS ->
         --     GLESGLuint <$> fromJSValUnchecked jsval

         -- | pname `elem` [ GL.TEXTURE_BASE_LEVEL
         --                , GL.TEXTURE_MAX_LEVEL
         --                ] -> -- GLint
         --   GLESGLint <$> fromJSValUnchecked jsval

         -- | pname `elem` [ GL.TEXTURE_MAX_LOD
         --                , GL.TEXTURE_MIN_LOD
         --                ] -> -- GLfloat
         --   GLESGLfloat <$> fromJSValUnchecked jsval
         | otherwise -> return GLESNone
  glGetUniformfv program location _ = readCtx >>= \ctx ->
    liftJSM . fromJSVal =<< GL.getUniform ctx (Just program) (Just location)
  glGetUniformiv program location _ = readCtx >>= \ctx ->
    liftJSM . fromJSVal =<< GL.getUniform ctx (Just program) (Just location)
  glGetUniformLocation program str = readCtx >>= \ctx ->
    GL.getUniformLocation ctx (Just program) str
  glGetVertexAttribfv index pname _
    | pname == GL.CURRENT_VERTEX_ATTRIB = readCtx >>= \ctx ->
      liftJSM . fromJSVal =<< GL.getVertexAttrib ctx index pname
    | otherwise = return Nothing
  glGetVertexAttribiv index pname _
    | pname /= GL.CURRENT_VERTEX_ATTRIB = readCtx >>= \ctx -> do
      jsval <- GL.getVertexAttrib ctx index pname
      liftJSM $ fromJSVal jsval >>= \case
        Nothing    -> return Nothing
        Just glint -> Just <$> allocInt32Array [glint :: GL.GLint]
    | otherwise = return Nothing
  glHint a b = readCtx >>= \ctx ->
    GL.hint ctx a b
  glIsBuffer buffer = readCtx >>= \ctx ->
    GL.isBuffer ctx $ Just buffer
  glIsContextLost = readCtx >>= GL.isContextLost
  glIsEnabled enum = readCtx >>= \ctx ->
    GL.isEnabled ctx enum
  glIsFramebuffer fb = readCtx >>= \ctx ->
    GL.isFramebuffer ctx $ Just fb
  glIsProgram p = readCtx >>= \ctx ->
    GL.isProgram ctx $ Just p
  glIsRenderbuffer rb = readCtx >>= \ctx ->
    GL.isRenderbuffer ctx $ Just rb
  glIsShader sh = readCtx >>= \ctx ->
    GL.isShader ctx $ Just sh
  glIsTexture tx = readCtx >>= \ctx ->
    GL.isTexture ctx $ Just tx
  glLineWidth float = readCtx >>= \ctx ->
    GL.lineWidth ctx float
  glLinkProgram p = readCtx >>= \ctx ->
    GL.linkProgram ctx $ Just p
  glPixelStorei enum int = readCtx >>= \ctx ->
    GL.pixelStorei ctx enum int
  glPolygonOffset a b = readCtx >>= \ctx ->
    GL.polygonOffset ctx a b
  glReleaseShaderCompiler = readCtx >>= GL.releaseShaderCompiler
  glRenderbufferStorage a b c d = readCtx >>= \ctx ->
    GL.renderbufferStorage ctx a b c d
  glSampleCoverage clampf bool = readCtx >>= \ctx ->
    GL.sampleCoverage ctx clampf bool
  glScissor a b c d = readCtx >>= \ctx ->
    GL.scissor ctx a b c d
  glShaderSource shader src = readCtx >>= \ctx ->
    GL.shaderSource ctx (Just shader) src
  glStencilFunc a b c = readCtx >>= \ctx ->
    GL.stencilFunc ctx a b c
  glStencilFuncSeparate a b c d = readCtx >>= \ctx ->
    GL.stencilFuncSeparate ctx a b c d
  glStencilMask uint = readCtx >>= \ctx ->
    GL.stencilMask ctx uint
  glStencilMaskSeparate a b = readCtx >>= \ctx ->
    GL.stencilMaskSeparate ctx a b
  glStencilOp a b c = readCtx >>= \ctx ->
    GL.stencilOp ctx a b c
  glStencilOpSeparate a b c d = readCtx >>= \ctx ->
    GL.stencilOpSeparate ctx a b c d
  glTexParameterf a b f = readCtx >>= \ctx ->
    GL.texParameterf ctx a b f
  glTexParameteri a b i = readCtx >>= \ctx ->
    GL.texParameteri ctx a b i
  glTexImage2D target level internalFormat format typ dat = readCtx >>= \ctx ->
    GL.texImage2D ctx target level (fromIntegral internalFormat) format typ $ Just dat
  glTexSubImage2D target level x y format typ dat = readCtx >>= \ctx -> do
    GL.texSubImage2D ctx target level x y format typ $ Just dat
  glUniform1f loc a = readCtx >>= \ctx ->
    GL.uniform1f ctx (Just loc) a
  glUniform1fv loc vec = readCtx >>= \ctx ->
    GL.uniform1fv ctx (Just loc) vec
  glUniform1i loc i = readCtx >>= \ctx ->
    GL.uniform1i ctx (Just loc) i
  glUniform1iv loc vec = readCtx >>= \ctx ->
    GL.uniform1iv ctx (Just loc) vec
  glUniform2f loc a b = readCtx >>= \ctx ->
    GL.uniform2f ctx (Just loc) a b
  glUniform2fv loc vec = readCtx >>= \ctx ->
    GL.uniform2fv ctx (Just loc) vec
  glUniform2i loc a b = readCtx >>= \ctx ->
    GL.uniform2i ctx (Just loc) a b
  glUniform2iv loc vec = readCtx >>= \ctx ->
    GL.uniform2iv ctx (Just loc) vec
  glUniform3f loc a b c = readCtx >>= \ctx ->
    GL.uniform3f ctx (Just loc) a b c
  glUniform3fv loc vec = readCtx >>= \ctx ->
    GL.uniform3fv ctx (Just loc) vec
  glUniform3i loc a b c = readCtx >>= \ctx ->
    GL.uniform3i ctx (Just loc) a b c
  glUniform3iv loc vec = readCtx >>= \ctx ->
    GL.uniform3iv ctx (Just loc) vec
  glUniform4f loc a b c d = readCtx >>= \ctx ->
    GL.uniform4f ctx (Just loc) a b c d
  glUniform4fv loc vec = readCtx >>= \ctx ->
    GL.uniform4fv ctx (Just loc) vec
  glUniform4i loc a b c d = readCtx >>= \ctx ->
    GL.uniform4i ctx (Just loc) a b c d
  glUniform4iv loc vec = readCtx >>= \ctx ->
    GL.uniform4iv ctx (Just loc) vec
  glUniformMatrix2fv loc trans vec = readCtx >>= \ctx ->
    GL.uniformMatrix2fv ctx (Just loc) trans vec
  glUniformMatrix3fv loc trans vec = readCtx >>= \ctx ->
    GL.uniformMatrix3fv ctx (Just loc) trans vec
  glUniformMatrix4fv loc trans vec = readCtx >>= \ctx ->
    GL.uniformMatrix4fv ctx (Just loc) trans vec
  glUseProgram program = readCtx >>= \ctx ->
    GL.useProgram ctx (Just program)
  glValidateProgram program = readCtx >>= \ctx ->
    GL.validateProgram ctx $ Just program
  glVertexAttrib1f loc a = readCtx >>= \ctx ->
    GL.vertexAttrib1f ctx loc a
  glVertexAttrib1fv loc vec = readCtx >>= \ctx ->
    GL.vertexAttrib1fv ctx loc vec
  glVertexAttrib2f loc a b = readCtx >>= \ctx ->
    GL.vertexAttrib2f ctx loc a b
  glVertexAttrib2fv loc vec = readCtx >>= \ctx ->
    GL.vertexAttrib2fv ctx loc vec
  glVertexAttrib3f loc a b c = readCtx >>= \ctx ->
    GL.vertexAttrib3f ctx loc a b c
  glVertexAttrib3fv loc vec = readCtx >>= \ctx ->
    GL.vertexAttrib3fv ctx loc vec
  glVertexAttrib4f loc a b c d = readCtx >>= \ctx ->
    GL.vertexAttrib4f ctx loc a b c d
  glVertexAttrib4fv loc vec = readCtx >>= \ctx ->
    GL.vertexAttrib4fv ctx loc vec
  glVertexAttribPointer index sz typ normed stride n = readCtx >>= \ctx ->
    GL.vertexAttribPointer ctx index sz typ normed stride n
  glViewport x y width height = readCtx >>= \ctx ->
    GL.viewport ctx x y width height

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
