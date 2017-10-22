{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Glucose.WebGL where

import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.Foldable                     (toList)
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Vector.Storable              (Storable, Vector,
                                                    unsafeWith)
import qualified Data.Vector.Storable              as V
import           Data.Word                         (Word32)
import           Foreign.Ptr
import qualified GHCJS.Buffer                      as B
import           GHCJS.Marshal
import           GHCJS.Types                       (JSVal)
--import           JavaScript.TypedArray             as TA
import           JSDOM.ImageData
import           JSDOM.OESVertexArrayObject        as GL
import           JSDOM.Types                       as GL
import           JSDOM.WebGLActiveInfo
import           JSDOM.WebGLRenderingContextBase   as GL
import           JSDOM.WebGLShaderPrecisionFormat
import           Language.Javascript.JSaddle.Types (MonadJSM, liftJSM)
import           Language.Javascript.JSaddle.Value (JSNull (..))
import           Linear                            (M22, M33, M44, V2, V3, V4)

import           Graphics.Glucose

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "console.log($1)" printPtrJS :: Ptr a -> IO ()
foreign import javascript unsafe "console.log($1)" printF32JS :: Float32Array -> IO ()
foreign import javascript unsafe
  "(function(o,n){return o.f3.subarray(0, n);})($1, $2)"
  toFloatArrayJS :: Ptr Float -> Int -> IO Float32Array
foreign import javascript unsafe
  "(function(o,n){return o.i3.subarray(0, n);})($1, $2)"
  toIntArrayJS :: Ptr Int -> Int -> IO Int32Array
foreign import javascript unsafe
  "(function(o,n){return Uint32Array.from(o.i3.subarray(0, n));})($1, $2)"
  toUintArrayJS :: Ptr Word32 -> Int -> IO Uint32Array
#else
toFloatArrayJS :: Ptr Float  -> Int -> IO Float32Array
toFloatArrayJS = error "only available on ghcjs"
toIntArrayJS :: Ptr Int    -> Int -> IO Int32Array
toIntArrayJS = error "only available on ghcjs"
toUintArrayJS :: Ptr Word32 -> Int -> IO Uint32Array
toUintArrayJS = error "only available on ghcjs"
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$1[$2]" indexf32 :: Float32Array -> Int -> IO Float
foreign import javascript unsafe "$1.length" lengthf32 :: Float32Array -> IO Int

foreign import javascript unsafe "$1[$2]" indexi32 :: Int32Array -> Int -> IO Int
foreign import javascript unsafe "$1.length" lengthi32 :: Int32Array -> IO Int

foreign import javascript unsafe "$1[$2]" indexu32 :: Uint32Array -> Int -> IO Word32
foreign import javascript unsafe "$1.length" lengthu32 :: Uint32Array -> IO Int

#else

indexf32 :: Float32Array -> Int -> IO Float
indexf32 = error "ghcjs only"
indexi32 :: Int32Array -> Int -> IO Int
indexi32 = error "ghcjs only"
indexu32 :: Uint32Array -> Int -> IO Word32
indexu32 = error "ghcjs only"
lengthf32 :: Float32Array -> IO Int
lengthf32 = error "ghcjs only"
lengthi32 :: Int32Array -> IO Int
lengthi32 = error "ghcjs only"
lengthu32 :: Uint32Array -> IO Int
lengthu32 = error "ghcjs only"

#endif

fromTypedArray
  :: forall c a t b. (Storable a, Storable b, c a)
  => (t -> IO Int)
  -> (b -> a)
  -> (t -> Int -> IO b)
  -> t
  -> IO (Vector a)
fromTypedArray len mp ndx array = do
  n <- len array
  V.map mp <$> V.generateM n (ndx array)

fromFloatArray :: MonadIO m => Float32Array -> m (Vector Float)
fromFloatArray = liftIO . fromTypedArray @Fractional lengthf32 realToFrac indexf32

fromIntArray :: MonadIO m => Int32Array -> m (Vector Int)
fromIntArray = liftIO . fromTypedArray @Integral lengthi32 fromIntegral indexi32

fromUintArray :: MonadIO m => Uint32Array -> m (Vector Word32)
fromUintArray = liftIO . fromTypedArray @Integral lengthu32 fromIntegral indexu32

type instance GLProgram           GLBackendWebGL = GL.WebGLProgram
type instance GLShader            GLBackendWebGL = GL.WebGLShader
type instance GLTexture           GLBackendWebGL = GL.WebGLTexture
type instance GLUniformLocation   GLBackendWebGL = GL.WebGLUniformLocation
type instance GLClampf            GLBackendWebGL = GL.GLclampf
type instance GLFloat             GLBackendWebGL = GL.GLfloat
type instance GLEnum              GLBackendWebGL = GL.GLenum
type instance GLUint              GLBackendWebGL = GL.GLuint
type instance GLInt               GLBackendWebGL = GL.GLint
type instance GLIntptr            GLBackendWebGL = GL.GLintptr
type instance GLBoolean           GLBackendWebGL = GL.GLboolean
type instance GLSizei             GLBackendWebGL = GL.GLsizei
type instance GLPtr               GLBackendWebGL = GL.GLintptr
type instance GLImageData         GLBackendWebGL = GL.ImageData
type instance GLBuffer            GLBackendWebGL = GL.WebGLBuffer
type instance GLFramebuffer       GLBackendWebGL = GL.WebGLFramebuffer
type instance GLRenderbuffer      GLBackendWebGL = GL.WebGLRenderbuffer
type instance GLVertexArrayObject GLBackendWebGL = GL.WebGLVertexArrayObjectOES
type instance GLExtension         GLBackendWebGL = JSVal

initWebGL :: WebGLRenderingContextBase -> JSM (Either String (GLES JSM 'GLBackendWebGL))
initWebGL ctx = getExtension ctx "OES_vertex_array_object" >>= \case
  Nothing   -> fail "Could not get OES_vertex_array_object"
  Just gobj -> toJSVal gobj >>= fromJSVal >>= \case
    Nothing     -> fail "Could not cast OES_vertex_array_object"
    Just oesvao ->
      Right . webgl ctx oesvao . WebGLVertexArrayObjectOES <$> toJSVal JSNull

webgl
  :: WebGLRenderingContextBase
  -> OESVertexArrayObject
  -> WebGLVertexArrayObjectOES
  -> GLES JSM 'GLBackendWebGL
webgl ctx oesvao nullVAO = GLES{..}
  where
    true  = True
    false = False

    withFloatArray :: (MonadIO m, Storable a, Real a)
                    => Vector a -> (Float32Array -> m b) -> m b
    withFloatArray vec f = let vec' = V.map realToFrac vec in
      liftIO (V.unsafeWith vec' $ \ptr -> toFloatArrayJS ptr (V.length vec')) >>= f

    withIntArray :: (MonadIO m, Storable a, Integral a)
      => Vector a -> (Int32Array -> m b) -> m b
    withIntArray vec f = let vec' = V.map fromIntegral vec in
      liftIO (V.unsafeWith vec' $ \ptr -> toIntArrayJS ptr (V.length vec')) >>= f

    withUintArray :: (MonadIO m, Storable a, Integral a)
                   => Vector a -> (Uint32Array -> m b) -> m b
    withUintArray vec f = let vec' = V.map fromIntegral vec in
      liftIO (V.unsafeWith vec' $ \ptr -> toUintArrayJS ptr (V.length vec')) >>= f

    glCreateVertexArray = GL.createVertexArrayOES oesvao
    glBindVertexArray   = GL.bindVertexArrayOES   oesvao . Just
    glIsVertexArray     = GL.isVertexArrayOES     oesvao . Just
    glDeleteVertexArray = GL.deleteVertexArrayOES oesvao . Just
    noVertexArray       = nullVAO

    glActiveTexture =
      GL.activeTexture ctx
    glAttachShader program shader =
      GL.attachShader ctx (Just program) (Just shader)
    glBindAttribLocation program =
      GL.bindAttribLocation ctx (Just program)
    glBindBuffer enum buffer =
      GL.bindBuffer ctx enum (Just buffer)
    glBindFramebuffer enum framebuffer =
      GL.bindFramebuffer ctx enum (Just framebuffer)
    glBindRenderbuffer enum renderbuffer =
      GL.bindRenderbuffer ctx enum (Just renderbuffer)
    glBindTexture enum texture =
      GL.bindTexture ctx enum (Just texture)
    glBlendColor =
      GL.blendColor ctx
    glBlendEquation enum =
      GL.blendEquation ctx enum
    glBlendEquationSeparate a b =
      GL.blendEquationSeparate ctx a b
    glBlendFunc a b =
      GL.blendFunc ctx a b
    glBlendFuncSeparate a b c d =
      GL.blendFuncSeparate ctx a b c d

    glBufferData :: forall a. (Storable a, ToTypedVector a)
                 => GL.GLenum -> Vector a -> GL.GLenum -> JSM ()
    glBufferData target vec usage = case toTypedVector vec of
      FloatVector dat -> withFloatArray dat $ \(Float32Array val) ->
        GL.bufferData ctx target (Just $ ArrayBufferView val) usage
      IntVector dat -> withIntArray dat $ \(Int32Array val) ->
        GL.bufferData ctx target (Just $ ArrayBufferView val) usage
      UintVector dat -> withUintArray dat $ \(Uint32Array val) ->
        GL.bufferData ctx target (Just $ ArrayBufferView val) usage

    glBufferSubData :: forall a. (Storable a, ToTypedVector a)
                    => GL.GLenum -> GL.GLintptr -> Vector a -> JSM ()
    glBufferSubData target offset vec = case toTypedVector vec of
      FloatVector dat -> withFloatArray dat $ \(Float32Array val) ->
        GL.bufferSubData ctx target offset $ Just $ ArrayBufferView val
      IntVector dat -> withIntArray dat $ \(Int32Array val) ->
        GL.bufferSubData ctx target offset $ Just $ ArrayBufferView val
      UintVector dat -> withUintArray dat $ \(Uint32Array val) ->
        GL.bufferSubData ctx target offset $ Just $ ArrayBufferView val

    glCheckFramebufferStatus target =
      GL.checkFramebufferStatus ctx target
    glClear bitfield =
      GL.clear ctx bitfield
    glClearColor r g b a =
      GL.clearColor ctx r g b a
    glClearDepth val =
      GL.clearDepth ctx val
    glClearStencil val =
      GL.clearStencil ctx val
    glColorMask a b c d =
      GL.colorMask ctx a b c d
    glCompileShader shader =
      GL.compileShader ctx (Just shader)
    glCopyTexImage2D target level internalformat x y w h _ =
      GL.copyTexImage2D ctx target level internalformat x y w h 0
    glCopyTexSubImage2D target level xoffset yoffset x y w h =
      GL.copyTexSubImage2D ctx target level xoffset yoffset x y w h
    glCreateBuffer =
      GL.createBuffer ctx
    glCreateFramebuffer =
      GL.createFramebuffer ctx
    glCreateProgram =
      GL.createProgram ctx
    glCreateRenderbuffer =
      GL.createRenderbuffer ctx
    glCreateShader shadertype =
      GL.createShader ctx shadertype
    glCreateTexture =
      GL.createTexture ctx
    glCullFace mode =
      GL.cullFace ctx mode
    glDeleteBuffer buffer =
      GL.deleteBuffer ctx $ Just buffer
    glDeleteFramebuffer framebuffer =
      GL.deleteFramebuffer ctx $ Just framebuffer
    glDeleteProgram program =
      GL.deleteProgram ctx $ Just program
    glDeleteRenderbuffer renderbuffer =
      GL.deleteRenderbuffer ctx $ Just renderbuffer
    glDeleteShader shader =
      GL.deleteShader ctx $ Just shader
    glDeleteTexture tex =
      GL.deleteTexture ctx $ Just tex
    glDepthFunc func =
      GL.depthFunc ctx func
    glDepthMask flag =
      GL.depthMask ctx flag
    glDepthRange znear zfar =
      GL.depthRange ctx (realToFrac znear) (realToFrac zfar)
    glDetachShader program shader =
      GL.detachShader ctx (Just program) (Just shader)
    glDisable val =
      GL.disable ctx val
    glDisableVertexAttribArray attrib =
      GL.disableVertexAttribArray ctx attrib
    glDrawArrays enum int size =
      GL.drawArrays ctx enum int size
    glDrawElements enum size enum2 ptr =
      GL.drawElements ctx enum size enum2 ptr
    glEnable enum =  GL.enable ctx enum
    glEnableVertexAttribArray uint =
      GL.enableVertexAttribArray ctx uint
    glFinish =
      GL.finish ctx
    glFlush =
      GL.flush ctx
    glFramebufferRenderbuffer a b c renderbuffer =
      GL.framebufferRenderbuffer ctx a b c (Just renderbuffer)
    glFramebufferTexture2D a b c texture int =
      GL.framebufferTexture2D ctx a b c (Just texture) int
    glFrontFace enum =
      GL.frontFace ctx enum
    glGenerateMipmap enum =
      GL.generateMipmap ctx enum
    glGetActiveAttrib program loc =  do
      info <- GL.getActiveAttrib ctx (Just program) loc
      ActiveInfo <$> (fromIntegral <$> getSize info)
                 <*> (fromIntegral <$> getType info)
                 <*> getName info
    glGetActiveUniform program loc =  do
      info <- GL.getActiveUniform ctx (Just program) loc
      ActiveInfo <$> (fromIntegral <$> getSize info)
                 <*> (fromIntegral <$> getType info)
                 <*> getName info
    glGetAttachedShaders program =
      fromMaybe [] <$> GL.getAttachedShaders ctx (Just program)
    glGetAttribLocation program name =
      GL.getAttribLocation ctx (Just program) name
    glGetBufferParameter target pname =  do
      jsval <- GL.getBufferParameter ctx target pname
      i     <- liftJSM $ fromJSValUnchecked jsval
      return $ if target == GL.BUFFER_SIZE
        then Left i
        else Right $ fromIntegral i
    glGetError =
      GL.getError ctx
    glGetExtension name =
      liftJSM . maybeNullOrUndefined =<< GL.getExtension ctx name
    glGetFramebufferAttachmentParameter target attachment pname =
       do
        jsval <- GL.getFramebufferAttachmentParameter ctx target attachment pname
        liftJSM $ case () of
          _
            | pname `elem` [ GL.FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
                          , GL.FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
                          ] -> (, Nothing, Nothing) <$> fromJSVal jsval
            | pname == GL.FRAMEBUFFER_ATTACHMENT_OBJECT_NAME ->
              (Nothing, , Nothing) <$> fromJSVal jsval
            | pname == GL.FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL ->
              (Nothing, Nothing, ) <$> fromJSVal jsval
            | otherwise -> return (Nothing, Nothing, Nothing)
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
    glGetProgramParameter program pname =  do
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

    glGetProgramInfoLog program =
      fromMaybe "" <$> GL.getProgramInfoLog ctx (Just program)
    glGetRenderbufferParameter target pname =  do
      jsval <- GL.getRenderbufferParameter ctx target pname
      liftJSM $
        if pname == GL.RENDERBUFFER_INTERNAL_FORMAT
        then Right <$> fromJSValUnchecked jsval
        else Left <$> fromJSValUnchecked jsval
    glGetShaderParameter shader pname =  do
      jsval <- GL.getShaderParameter ctx (Just shader) pname
      liftJSM $ if pname `elem` [GL.DELETE_STATUS, GL.COMPILE_STATUS]
        then Left <$> fromJSValUnchecked jsval
        else Right <$> fromJSValUnchecked jsval
    glGetShaderInfoLog shader =  do
      mlog <- GL.getShaderInfoLog ctx (Just shader)
      return $ fromMaybe "" mlog
    glGetShaderPrecisionFormat shaderType precisionType =  do
      fmt <- GL.getShaderPrecisionFormat ctx shaderType precisionType
      precision <- getPrecision fmt
      rangeMin  <- getRangeMin fmt
      rangeMax  <- getRangeMax fmt
      return ShaderPrecisionFormat { spfRangeMin  = fromIntegral rangeMin
                                   , spfRangeMax  = fromIntegral rangeMax
                                   , spfPrecision = fromIntegral precision
                                   }
    glGetShaderSource shader =
      fromMaybe "" <$> GL.getShaderSource ctx (Just shader)
    glGetSupportedExtensions =
      fromMaybe [] <$> GL.getSupportedExtensions ctx
    glGetTexParameter target pname =  do
      jsval <- GL.getTexParameter ctx target pname
      liftJSM $ case () of
        () | pname `elem` [ GL.TEXTURE_MAG_FILTER
                          , GL.TEXTURE_MIN_FILTER
                          , GL.TEXTURE_WRAP_S
                          , GL.TEXTURE_WRAP_T
                          ] -> (, Nothing, Nothing, Nothing, Nothing) <$> fromJSVal jsval
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
           | otherwise -> return (Nothing, Nothing, Nothing, Nothing, Nothing)

    glGetUniformfv
      :: forall a. (Storable a, Fractional a)
      => WebGLProgram -> WebGLUniformLocation -> Int -> JSM (Vector a)
    glGetUniformfv program location n = do
      val <- GL.getUniform ctx (Just program) (Just location)
      vec <- liftJSM (fromFloatArray $ Float32Array val)
      return $ V.map (realToFrac @Float @a) vec
    glGetUniformiv
      :: forall a. (Storable a, Integral a)
      => WebGLProgram -> WebGLUniformLocation -> Int -> JSM (Vector a)
    glGetUniformiv program location n = do
      val <- GL.getUniform ctx (Just program) (Just location)
      vec <- liftJSM (fromIntArray $ Int32Array val)
      return $ V.map (fromIntegral @Int @a) vec
    glGetUniformLocation program =
      GL.getUniformLocation ctx (Just program)

    glGetVertexAttribfv
      :: forall a. (Storable a , Fractional a)
      => GL.GLuint -> GL.GLenum -> Int -> JSM (Vector a)
    glGetVertexAttribfv index pname n
      | pname == GL.CURRENT_VERTEX_ATTRIB = do
          val <- GL.getVertexAttrib ctx index pname
          vec <- liftJSM (fromFloatArray $ Float32Array val)
          return $ V.map (realToFrac @Float @a) vec
      | otherwise = return V.empty

    glGetVertexAttribiv
      :: forall a. (Storable a, Integral a)
      => GL.GLuint -> GL.GLenum -> Int -> JSM (Vector a)
    glGetVertexAttribiv index pname n
      | pname /= GL.CURRENT_VERTEX_ATTRIB =  do
          val <- GL.getVertexAttrib ctx index pname
          vec <- liftJSM (fromIntArray $ Int32Array val)
          return $ V.map (fromIntegral @Int @a) vec
      | otherwise = return V.empty
    glHint = GL.hint ctx
    glIsBuffer = GL.isBuffer ctx . Just
    glIsContextLost = GL.isContextLost ctx
    glIsEnabled = GL.isEnabled ctx
    glIsFramebuffer fb =
      GL.isFramebuffer ctx $ Just fb
    glIsProgram p =
      GL.isProgram ctx $ Just p
    glIsRenderbuffer rb =
      GL.isRenderbuffer ctx $ Just rb
    glIsShader sh =
      GL.isShader ctx $ Just sh
    glIsTexture tx =
      GL.isTexture ctx $ Just tx
    glLineWidth float =
      GL.lineWidth ctx float
    glLinkProgram p =
      GL.linkProgram ctx $ Just p
    glPixelStorei enum int =
      GL.pixelStorei ctx enum int
    glPolygonOffset a b =
      GL.polygonOffset ctx a b
    glReleaseShaderCompiler =
      GL.releaseShaderCompiler ctx
    glRenderbufferStorage a b c d =
      GL.renderbufferStorage ctx a b c d
    glSampleCoverage clampf bool =
      GL.sampleCoverage ctx clampf bool
    glScissor a b c d =
      GL.scissor ctx a b c d
    glShaderSource shader src =
      GL.shaderSource ctx (Just shader) src
    glStencilFunc a b c =
      GL.stencilFunc ctx a b c
    glStencilFuncSeparate a b c d =
      GL.stencilFuncSeparate ctx a b c d
    glStencilMask uint =
      GL.stencilMask ctx uint
    glStencilMaskSeparate a b =
      GL.stencilMaskSeparate ctx a b
    glStencilOp a b c =
      GL.stencilOp ctx a b c
    glStencilOpSeparate a b c d =
      GL.stencilOpSeparate ctx a b c d
    glTexParameterf a b f =
      GL.texParameterf ctx a b f
    glTexParameteri a b i =
      GL.texParameteri ctx a b i
    glTexImage2D target level internalFormat format typ dat =
      GL.texImage2D ctx target level (fromIntegral internalFormat) format typ $ Just dat
    glTexSubImage2D target level x y format typ dat =
      GL.texSubImage2D ctx target level x y format typ $ Just dat
    glUniform1f loc a =
      GL.uniform1f ctx (Just loc) a

    glUniform1fv
      :: forall a. (Real a, Storable a)
      => WebGLUniformLocation -> Vector a -> JSM ()
    glUniform1fv loc vec = withFloatArray vec $ GL.uniform1fv ctx (Just loc)

    glUniform1i loc i =
      GL.uniform1i ctx (Just loc) i
    glUniform1iv
      :: forall a. (Integral a, Storable a)
      => WebGLUniformLocation -> Vector a -> JSM ()
    glUniform1iv loc vec = withIntArray vec $ GL.uniform1iv ctx (Just loc)
    glUniform2f loc a b =
      GL.uniform2f ctx (Just loc) a b

    flatten :: (Storable a, Storable (f a), Foldable f) => Vector (f a) -> Vector a
    flatten = V.concatMap (V.fromList . toList)

    glUniform2fv
      :: forall a. (Real a, Storable a)
      => WebGLUniformLocation -> Vector (V2 a) -> JSM ()
    glUniform2fv loc vec = withFloatArray (flatten vec) $
      GL.uniform2fv ctx (Just loc)
    glUniform2i loc a b =
      GL.uniform2i ctx (Just loc) a b
    glUniform2iv
      :: forall a. (Integral a, Storable a)
      => WebGLUniformLocation -> Vector (V2 a) -> JSM ()
    glUniform2iv loc vec = withIntArray (flatten vec) $
      GL.uniform2iv ctx (Just loc)
    glUniform3f loc a b c =
     GL.uniform3f ctx (Just loc) a b c
    glUniform3fv
      :: forall a. (Real a, Storable a)
      => WebGLUniformLocation -> Vector (V3 a) -> JSM ()
    glUniform3fv loc vec = withFloatArray (flatten vec) $
      GL.uniform3fv ctx (Just loc)

    glUniform3i loc a b c =
      GL.uniform3i ctx (Just loc) a b c

    glUniform3iv
      :: forall a. (Integral a, Storable a)
      => WebGLUniformLocation -> Vector (V3 a) -> JSM ()
    glUniform3iv loc vec = withIntArray (flatten vec) $
      GL.uniform3iv ctx (Just loc)

    glUniform4f loc a b c d =
      GL.uniform4f ctx (Just loc) a b c d

    glUniform4fv
      :: forall a. (Real a, Storable a)
      => WebGLUniformLocation -> Vector (V4 a) -> JSM ()
    glUniform4fv loc vec = withFloatArray (flatten vec) $
      GL.uniform4fv ctx (Just loc)

    glUniform4i loc a b c d =
      GL.uniform4i ctx (Just loc) a b c d

    glUniform4iv
      :: forall a. (Integral a, Storable a)
      => WebGLUniformLocation -> Vector (V4 a) -> JSM ()
    glUniform4iv loc vec = withIntArray (flatten vec) $
      GL.uniform4iv ctx (Just loc)

    glUniformMatrix2fv
      :: forall a. (Real a, Storable a)
      => WebGLUniformLocation -> GL.GLboolean -> Vector (M22 a) -> JSM ()
    glUniformMatrix2fv loc trans vec = withFloatArray (flatten $ flatten vec) $
      GL.uniformMatrix2fv ctx (Just loc) trans

    glUniformMatrix3fv
      :: forall a. (Real a, Storable a)
      => WebGLUniformLocation -> GL.GLboolean -> Vector (M33 a) -> JSM ()
    glUniformMatrix3fv loc trans vec = withFloatArray (flatten $ flatten vec) $
      GL.uniformMatrix3fv ctx (Just loc) trans

    glUniformMatrix4fv
      :: forall a. (Real a, Storable a)
      => WebGLUniformLocation -> GL.GLboolean -> Vector (M44 a) -> JSM ()
    glUniformMatrix4fv loc trans vec = withFloatArray (flatten $ flatten vec) $
      GL.uniformMatrix4fv ctx (Just loc) trans

    glUseProgram program =
      GL.useProgram ctx (Just program)
    glValidateProgram program =
      GL.validateProgram ctx $ Just program
    glVertexAttrib1f loc a =
      GL.vertexAttrib1f ctx loc a
    --glVertexAttrib1fv :: forall a. (Real a, Storable a) => uint -> Vector a -> m ()
    --glVertexAttrib1fv loc vec =
    --  GL.vertexAttrib1fv ctx loc vec
    glVertexAttrib2f loc a b =
      GL.vertexAttrib2f ctx loc a b
    --glVertexAttrib2fv loc vec =
    --  GL.vertexAttrib2fv ctx loc vec
    glVertexAttrib3f loc a b c =
      GL.vertexAttrib3f ctx loc a b c
    --glVertexAttrib3fv loc vec =
    --  GL.vertexAttrib3fv ctx loc vec
    glVertexAttrib4f loc a b c d =
      GL.vertexAttrib4f ctx loc a b c d
    --glVertexAttrib4fv loc vec =
    --  GL.vertexAttrib4fv ctx loc vec
    glVertexAttribPointer index sz typ normed stride n =
      GL.vertexAttribPointer ctx index sz typ normed stride n
    glViewport x y width height =
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
