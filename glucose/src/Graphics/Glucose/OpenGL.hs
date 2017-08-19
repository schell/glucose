{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Glucose.OpenGL where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Foldable               (toList)
import           Data.Vector.Storable        (Vector)
import qualified Data.Vector.Storable        as V
import           Foreign.C.String
import           Foreign.ForeignPtr          (newForeignPtr)
import           Foreign.Marshal.Array       (allocaArray, newArray, peekArray,
                                              withArray)
import           Foreign.Marshal.Utils       (with)
import           Foreign.Ptr                 (Ptr, castPtr, intPtrToPtr,
                                              nullPtr)
import           Foreign.Storable            (Storable, peekElemOff, sizeOf)
import qualified Graphics.GL.Core33          as GL
import qualified Graphics.GL.Embedded20      as GL (glGetShaderPrecisionFormat,
                                                    glReleaseShaderCompiler)
import qualified Graphics.GL.Internal.Shared as GL
import qualified Graphics.GL.Types           as GL

import           Graphics.Glucose

createWith :: (MonadIO m, Storable b, Num a) => (a -> Ptr b -> IO ()) -> m b
createWith f = liftIO $ do
  [x] <- allocaArray 1 $ \ptr -> do
    f 1 ptr
    peekArray 1 ptr
  return x

deleteWith :: (MonadIO m, Storable b, Num a) => (a -> Ptr b -> IO ()) -> b -> m ()
deleteWith f b = liftIO $ withArray [b] $ f 1

getActiveThing
  :: (MonadIO m, Num i)
  => (GL.GLuint
      -> GL.GLuint
      -> GL.GLsizei
      -> Ptr GL.GLsizei
      -> Ptr GL.GLint
      -> Ptr GL.GLenum
      -> Ptr GL.GLchar
      -> IO ())
  -> GL.GLuint
  -> GL.GLuint
  -> m (ActiveInfo i)
getActiveThing f program index = liftIO $
  allocaArray 1 $ \lenPtr ->
    allocaArray 1 $ \szPtr ->
      allocaArray 1 $ \typPtr ->
        withCString (replicate 64 ' ') $ \cStr -> do
          f program index 64 lenPtr szPtr typPtr cStr
          [sz]  <- peekArray 1 szPtr
          [typ] <- peekArray 1 typPtr
          str   <- peekCString cStr
          return ActiveInfo { aiSize = fromIntegral sz
                            , aiType = fromIntegral typ
                            , aiName = str
                            }

mayTrue :: Bool -> a -> Maybe a
mayTrue True  a = Just a
mayTrue False _ = Nothing


data PtrInfo len a = PtrInfo { ptrElementSize      :: Int
                             -- ^ The size in bytes of one specific element in this pointer/array.
                             , ptrNumberOfElements :: len
                             -- ^ The number of elements in the pointer/array.
                             , ptrPtr              :: Ptr a
                             -- ^ The pointer itself.
                             }

newtype OpenGL (m :: * -> *) =
  OpenGL { unOpenGL :: GLES m                              -- m
                            GL.GLuint                      -- program
                            GL.GLuint                      -- shader
                            GL.GLuint                      -- texture
                            GL.GLint                       -- uniformlocation
                            GL.GLfloat                     -- clampf
                            GL.GLfloat                     -- float
                            GL.GLenum                      -- enum
                            GL.GLuint                      -- uint
                            GL.GLint                       -- int
                            GL.GLintptr                    -- intptr
                            GL.GLboolean                   -- boolean
                            GL.GLsizei                     -- sizei
                            (Ptr ())                       -- ptr
                            (PtrInfo (Int, Int) ())        -- imagedata
                            GL.GLuint                      -- buffer
                            GL.GLuint                      -- framebuffer
                            GL.GLuint                      -- renderbuffer
                            GL.GLuint                      -- vertexarrayobject
                            ()                             -- extension
         }

instance MonadIO m => IsGLES m (OpenGL m) where
  type GLProgram           (OpenGL m) = GL.GLuint
  type GLShader            (OpenGL m) = GL.GLuint
  type GLTexture           (OpenGL m) = GL.GLuint
  type GLUniformlocation   (OpenGL m) = GL.GLint
  type GLClampf            (OpenGL m) = GL.GLfloat
  type GLFloat             (OpenGL m) = GL.GLfloat
  type GLEnum              (OpenGL m) = GL.GLenum
  type GLUint              (OpenGL m) = GL.GLuint
  type GLInt               (OpenGL m) = GL.GLint
  type GLIntptr            (OpenGL m) = GL.GLintptr
  type GLBoolean           (OpenGL m) = GL.GLboolean
  type GLSizei             (OpenGL m) = GL.GLsizei
  type GLPtr               (OpenGL m) = Ptr ()
  type GLImagedata         (OpenGL m) = PtrInfo (Int, Int) ()
  type GLBuffer            (OpenGL m) = GL.GLuint
  type GLFramebuffer       (OpenGL m) = GL.GLuint
  type GLRenderbuffer      (OpenGL m) = GL.GLuint
  type GLVertexArrayObject (OpenGL m) = GL.GLuint
  type GLExtension         (OpenGL m) = ()
  gles = unOpenGL

opengl :: MonadIO m => OpenGL m
opengl = OpenGL GLES {..}
  where
    true = GL.GL_TRUE
    false = GL.GL_FALSE

    --withFloatArray :: Vector Float -> () -> m a
    --withFloatArray vec f =
    --  let vecf = V.map realToFrac vec
    --      sz   = sizeOf (undefined :: GL.GLfloat)
    --  in liftIO $ V.unsafeWith vecf $ f . PtrInfo sz (fromIntegral $ V.length vecf)
    --withIntArray   vec f =
    --  let veci = V.map fromIntegral vec
    --      sz   = sizeOf (undefined :: GL.GLint)
    --  in liftIO $ V.unsafeWith veci $ f . PtrInfo sz (fromIntegral $ V.length veci)
    --withUintArray  vec f =
    --  let veci = V.map fromIntegral vec
    --      sz   = sizeOf (undefined :: GL.GLuint)
    --  in liftIO $ V.unsafeWith veci $ f . PtrInfo sz (fromIntegral $ V.length veci)

    --fromFloatArray (PtrInfo _ n ptr) = liftIO $ 
    --  V.map realToFrac   <$> V.generateM (fromIntegral n) (peekElemOff ptr)
    --fromIntArray   (PtrInfo _ n ptr) = liftIO $ 
    --  V.map fromIntegral <$> V.generateM (fromIntegral n) (peekElemOff ptr)
    --fromUintArray  (PtrInfo _ n ptr) = liftIO $
    --  V.map fromIntegral <$> V.generateM (fromIntegral n) (peekElemOff ptr)

    glCreateVertexArray = createWith GL.glGenVertexArrays
    glBindVertexArray   = GL.glBindVertexArray
    glIsVertexArray     = GL.glIsVertexArray
    glDeleteVertexArray = deleteWith GL.glDeleteVertexArrays
    noVertexArray       = 0

    glActiveTexture = GL.glActiveTexture
    glAttachShader  = GL.glAttachShader
    glBindAttribLocation p n s =
      liftIO $ withCString s $ GL.glBindAttribLocation p n
    glBindBuffer = GL.glBindBuffer
    glBindFramebuffer = GL.glBindFramebuffer
    glBindRenderbuffer = GL.glBindRenderbuffer
    glBindTexture = GL.glBindTexture
    glBlendColor = GL.glBlendColor
    glBlendEquation = GL.glBlendEquation
    glBlendEquationSeparate = GL.glBlendEquationSeparate
    glBlendFunc = GL.glBlendFunc
    glBlendFuncSeparate = GL.glBlendFuncSeparate

    glBufferData :: forall m a. (MonadIO m, Storable a, ToTypedVector a)
                 => GL.GLenum -> Vector a -> GL.GLenum -> m ()
    glBufferData target vec drawType =
      let n  = fromIntegral $ V.length vec
          sz = sizeOf (undefined :: a)
      in liftIO $ V.unsafeWith vec $ \ptr ->
           GL.glBufferData target (fromIntegral sz * n) (castPtr ptr) drawType

    glBufferSubData :: forall m a. (MonadIO m, Storable a, ToTypedVector a)
                    => GL.GLenum -> GL.GLintptr -> Vector a -> m ()
    glBufferSubData target offset vec =
      let n  = fromIntegral $ V.length vec
          sz = sizeOf (undefined :: a)
      in liftIO $ V.unsafeWith vec $ \ptr ->
           GL.glBufferSubData target offset (fromIntegral sz * n) $ castPtr ptr

    glCheckFramebufferStatus = GL.glCheckFramebufferStatus
    glClear = GL.glClear
    glClearColor = GL.glClearColor
    glClearDepth = GL.glClearDepth . realToFrac
    glClearStencil = GL.glClearStencil
    glColorMask = GL.glColorMask
    glCompileShader = GL.glCompileShader
    glCopyTexImage2D = GL.glCopyTexImage2D
    glCopyTexSubImage2D = GL.glCopyTexSubImage2D
    glCreateBuffer = createWith GL.glGenBuffers
    glCreateFramebuffer = createWith GL.glGenFramebuffers
    glCreateProgram = GL.glCreateProgram
    glCreateRenderbuffer = createWith GL.glGenRenderbuffers
    glCreateShader = GL.glCreateShader
    glCreateTexture = createWith GL.glGenTextures
    glCullFace = GL.glCullFace
    glDeleteBuffer = deleteWith GL.glDeleteBuffers
    glDeleteFramebuffer = deleteWith GL.glDeleteFramebuffers
    glDeleteProgram = GL.glDeleteProgram
    glDeleteRenderbuffer = deleteWith GL.glDeleteRenderbuffers
    glDeleteShader = GL.glDeleteShader
    glDeleteTexture = deleteWith GL.glDeleteTextures
    glDepthFunc = GL.glDepthFunc
    glDepthMask = GL.glDepthMask
    glDepthRange x y = GL.glDepthRange (realToFrac x) (realToFrac y)
    glDetachShader = GL.glDetachShader
    glDisable = GL.glDisable
    glDisableVertexAttribArray = GL.glDisableVertexAttribArray
    glDrawArrays = GL.glDrawArrays
    glDrawElements = GL.glDrawElements
    glEnable = GL.glEnable
    glEnableVertexAttribArray = GL.glEnableVertexAttribArray
    glFinish = GL.glFinish
    glFlush = GL.glFlush
    glFramebufferRenderbuffer = GL.glFramebufferRenderbuffer
    glFramebufferTexture2D = GL.glFramebufferTexture2D
    glFrontFace = GL.glFrontFace
    glGenerateMipmap = GL.glGenerateMipmap
    glGetActiveAttrib = getActiveThing GL.glGetActiveAttrib
    glGetActiveUniform = getActiveThing GL.glGetActiveUniform
    glGetAttachedShaders program = liftIO $
      allocaArray 1 $ \countPtr ->
        allocaArray 256 $ \shadersPtr -> do
          GL.glGetAttachedShaders program 256 countPtr shadersPtr
          [count] <- peekArray 1 countPtr
          peekArray (fromIntegral count) shadersPtr
    glGetAttribLocation program name = liftIO $
      withCString name $ GL.glGetAttribLocation program
    glGetBufferParameter target pname = liftIO $ do
      [i] <- allocaArray 1 $ \ptr -> do
        GL.glGetBufferParameteriv target pname ptr
        peekArray 1 ptr
      return $ if target == GL.GL_BUFFER_SIZE
        then Left  $ fromIntegral i
        else Right $ fromIntegral i
    glGetError = GL.glGetError
    glGetExtension = const $ return Nothing
    glGetFramebufferAttachmentParameter target attachment pname = do
      val <- getVal
      return ( fromIntegral <$> mayEnum val
             , fromIntegral <$> mayTex val
             , fromIntegral <$> mayInt val
             )
      where
        mayEnum = mayTrue (pname `elem` [GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
                                        , GL.GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
                                        ])
        mayTex = mayTrue (pname == GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME)
        mayInt = mayTrue (pname == GL.GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL)
        getVal = liftIO $ allocaArray 1 $ \ptr -> do
          GL.glGetFramebufferAttachmentParameteriv target attachment pname ptr
          [val] <- peekArray 1 ptr
          return val
    -- | TODO: Flesh this out
    --glGetParameter pname
    --  --https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGet.xml
    --  -- Float32Array (with 2 elements)
    --  |    pname == GL.GL_ALIASED_LINE_WIDTH_RANGE
    --    || pname == GL.GL_ALIASED_POINT_SIZE_RANGE
    --    || pname == GL.GL_DEPTH_RANGE = allocaArray 2 $ \ptr -> do
    --    GL.glGetFloatv pname ptr
    --    return $ GLESFloatArray (2, ptr)
    --  -- Float32Array (with 4 values)
    --  |    pname == GL.GL_BLEND_COLOR
    --    || pname == GL.GL_COLOR_CLEAR_VALUE = allocaArray 4 $ \ptr -> do
    --    GL.glGetFloatv pname ptr
    --    return $ GLESFloatArray
    --  GLboolean | pname == GL.GL_BLEND
    --  GLboolean || pname == GL.GL_CULL_FACE
    --  GLboolean || pname == GL.GL_DEPTH_TEST
    --  GLboolean || pname == GL.GL_DEPTH_WRITEMASK
    --  GLboolean || pname == GL.GL_DITHER
    --  GLboolean || pname == GL.GL_POLYGON_OFFSET_FILL
    --  GLboolean || pname == GL.GL_SAMPLE_COVERAGE_INVERT
    --  GLboolean || pname == GL.GL_SCISSOR_TEST
    --  GLboolean || pname == GL.GL_STENCIL_TEST
    --  GLboolean || pname == GL.GL_UNPACK_FLIP_Y_WEBGL
    --  GLboolean || pname == GL.GL_UNPACK_PREMULTIPLY_ALPHA_WEBGL
    --  GLenum | pname == GL.GL_ACTIVE_TEXTURE
    --  GLenum || pname == GL.GL_BLEND_DST_ALPHA
    --  GLenum || pname == GL.GL_BLEND_DST_RGB
    --  GLenum || pname == GL.GL_BLEND_EQUATION
    --  GLenum || pname == GL.GL_BLEND_EQUATION_ALPHA
    --  GLenum || pname == GL.GL_BLEND_EQUATION_RGB
    --  GLenum || pname == GL.GL_BLEND_SRC_ALPHA
    --  GLenum || pname == GL.GL_BLEND_SRC_RGB
    --  GLenum || pname == GL.GL_CULL_FACE_MODE
    --  GLenum || pname == GL.GL_DEPTH_FUNC
    --  GLenum || pname == GL.GL_FRONT_FACE
    --  GLenum || pname == GL.GL_GENERATE_MIPMAP_HINT
    --  GLenum || pname == GL.GL_IMPLEMENTATION_COLOR_READ_FORMAT
    --  GLenum || pname == GL.GL_IMPLEMENTATION_COLOR_READ_TYPE
    --  GLenum || pname == GL.GL_STENCIL_BACK_FAIL
    --  GLenum || pname == GL.GL_STENCIL_BACK_FUNC
    --  GLenum || pname == GL.GL_STENCIL_BACK_PASS_DEPTH_FAIL
    --  GLenum || pname == GL.GL_STENCIL_BACK_PASS_DEPTH_PASS
    --  GLenum || pname == GL.GL_STENCIL_FAIL
    --  GLenum || pname == GL.GL_STENCIL_FUNC
    --  GLenum || pname == GL.GL_STENCIL_PASS_DEPTH_FAIL
    --  GLenum || pname == GL.GL_STENCIL_PASS_DEPTH_PASS
    --  GLenum || pname == GL.GL_UNPACK_COLORSPACE_CONVERSION_WEBGL
    --  GLfloat | pname == GL.GL_DEPTH_CLEAR_VALUE
    --  GLfloat || pname == GL.GL_LINE_WIDTH
    --  GLfloat || pname == GL.GL_POLYGON_OFFSET_FACTOR
    --  GLfloat || pname == GL.GL_POLYGON_OFFSET_UNITS
    --  GLfloat || pname == GL.GL_SAMPLE_COVERAGE_VALUE
    --  GLint | pname == GL.GL_ALPHA_BITS
    --  GLint || pname == GL.GL_BLUE_BITS
    --  GLint || pname == GL.GL_DEPTH_BITS
    --  GLint || pname == GL.GL_GREEN_BITS
    --  GLint || pname == GL.GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
    --  GLint || pname == GL.GL_MAX_CUBE_MAP_TEXTURE_SIZE
    --  GLint || pname == GL.GL_MAX_FRAGMENT_UNIFORM_VECTORS
    --  GLint || pname == GL.GL_MAX_RENDERBUFFER_SIZE
    --  GLint || pname == GL.GL_MAX_TEXTURE_IMAGE_UNITS
    --  GLint || pname == GL.GL_MAX_TEXTURE_SIZE
    --  GLint || pname == GL.GL_MAX_VARYING_VECTORS
    --  GLint || pname == GL.GL_MAX_VERTEX_ATTRIBS
    --  GLint || pname == GL.GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
    --  GLint || pname == GL.GL_MAX_VERTEX_UNIFORM_VECTORS
    --  GLint || pname == GL.GL_PACK_ALIGNMENT
    --  GLint || pname == GL.GL_RED_BITS
    --  GLint || pname == GL.GL_SAMPLES
    --  GLint || pname == GL.GL_SAMPLE_BUFFERS
    --  GLint || pname == GL.GL_STENCIL_BACK_REF
    --  GLint || pname == GL.GL_STENCIL_BITS
    --  GLint || pname == GL.GL_STENCIL_CLEAR_VALUE
    --  GLint || pname == GL.GL_STENCIL_REF
    --  GLint || pname == GL.GL_SUBPIXEL_BITS
    --  GLint || pname == GL.GL_UNPACK_ALIGNMENT
    --  GLuint | pname == GL.GL_STENCIL_BACK_VALUE_MASK
    --  GLuint || pname == GL.GL_STENCIL_BACK_WRITEMASK
    --  GLuint || pname == GL.GL_STENCIL_VALUE_MASK
    --  GLuint || pname == GL.GL_STENCIL_WRITEMASK
    --  Int32Array (with 2 elements) | pname == GL.GL_MAX_VIEWPORT_DIMS
    --  Int32Array (with 4 elements) || pname == GL.GL_SCISSOR_BOX
    --  Int32Array (with 4 elements)| pname == GL.GL_VIEWPORT
    --  Uint32Array | pname == GL.GL_COMPRESSED_TEXTURE_FORMATS
    --  WebGLBuffer | pname == GL.GL_ARRAY_BUFFER_BINDING
    --  WebGLBuffer || pname == GL.GL_ELEMENT_ARRAY_BUFFER_BINDING
    --  WebGLFramebuffer | pname == GL.GL_FRAMEBUFFER_BINDING
    --  WebGLProgram | pname == GL.GL_CURRENT_PROGRAM
    --  WebGLRenderbuffer | pname == GL.GL_RENDERBUFFER_BINDING
    --  WebGLTexture | pname == GL.GL_TEXTURE_BINDING_2D
    --  WebGLTexture || pname == GL.GL_TEXTURE_BINDING_CUBE_MAP
    --  sequence<GLboolean> (with 4 values) | pname == GL.GL_COLOR_WRITEMASK
    --  | otherwise = return GLESNone
    --glGetProgramParameter program pname = liftIO $ allocaArray 1 $ \ptr -> do
    --  GL.glGetProgramiv program pname ptr
    --  [glint] <- peekArray 1 ptr
    --  let intValues =
    --          -- Returns a GLint indicating the number of attached shaders to a program.
    --        [ GL.GL_ATTACHED_SHADERS
    --          -- Returns a GLint indicating the number of active attribute variables to a program.
    --        , GL.GL_ACTIVE_ATTRIBUTES
    --          -- Returns a GLint indicating the number of active uniform variables to a program.
    --        , GL.GL_ACTIVE_UNIFORMS
    --        ]
    --  return $ if pname `elem` intValues
    --             then Right glint
    --             else Left $ fromIntegral glint

    glGetProgramInfoLog program = liftIO $
      withCString (replicate 1024 ' ') $ \ptr -> do
        GL.glGetProgramInfoLog program 1024 nullPtr ptr
        peekCString ptr
    glGetRenderbufferParameter target pname = liftIO $ allocaArray 1 $ \ptr -> do
      GL.glGetRenderbufferParameteriv target pname ptr
      [params] <- peekArray 1 ptr
      return $
        if pname == GL.GL_RENDERBUFFER_INTERNAL_FORMAT
        then Right $ fromIntegral params
        else Left params
    glGetShaderParameter shader pname = liftIO $ allocaArray 1 $ \ptr -> do
      GL.glGetShaderiv shader pname ptr
      [params] <- peekArray 1 ptr
      return $
        if pname == GL.GL_SHADER_TYPE
          then Right $ fromIntegral params
          else Left $ fromIntegral params
    glGetShaderInfoLog shader = liftIO $ do
      len <- allocaArray 1 $ \ptr -> do
        GL.glGetShaderiv shader GL.GL_INFO_LOG_LENGTH ptr
        [len] <- peekArray 1 ptr
        return len
      withCString (replicate (fromIntegral len) ' ') $ \ptr -> do
        GL.glGetShaderInfoLog shader len nullPtr ptr
        peekCString ptr
    glGetShaderPrecisionFormat shaderType precisionType = liftIO $
      allocaArray 1 $ \precisionPtr -> allocaArray 2 $ \rangePtr -> do
        GL.glGetShaderPrecisionFormat shaderType precisionType rangePtr precisionPtr
        [precision]          <- peekArray 1 precisionPtr
        [rangeMin, rangeMax] <- peekArray 2 rangePtr
        return ShaderPrecisionFormat { spfRangeMin  = rangeMin
                                     , spfRangeMax  = rangeMax
                                     , spfPrecision = precision
                                     }
    glGetShaderSource shader = liftIO $ do
      len <- allocaArray 1 $ \ptr -> do
        GL.glGetShaderiv shader GL.GL_SHADER_SOURCE_LENGTH ptr
        [len] <- peekArray 1 ptr
        return len
      withCString (replicate (fromIntegral len) ' ') $ \ptr -> do
        GL.glGetShaderSource shader (fromIntegral len) nullPtr ptr
        peekCString ptr
    glGetSupportedExtensions = return []
    glGetTexParameter target pname
      | pname `elem` [ GL.GL_TEXTURE_MAG_FILTER
                     , GL.GL_TEXTURE_MIN_FILTER
                     , GL.GL_TEXTURE_WRAP_S
                     , GL.GL_TEXTURE_WRAP_T
                     , GL.GL_TEXTURE_COMPARE_FUNC
                     , GL.GL_TEXTURE_COMPARE_MODE
                     , GL.GL_TEXTURE_WRAP_R
                     ] = liftIO $
          allocaArray 1 $ \ptr -> do
             GL.glGetTextureParameteriv target pname ptr
             [enum] <- peekArray 1 ptr
             return (Just $ fromIntegral enum, Nothing, Nothing, Nothing, Nothing)

      | pname == GL.GL_TEXTURE_IMMUTABLE_FORMAT = liftIO $
          allocaArray 1 $ \ptr -> do
            GL.glGetTextureParameteriv target pname ptr
            [bool] <- peekArray 1 ptr
            return (Nothing, Just $ fromIntegral bool, Nothing, Nothing, Nothing)

      | pname == GL.GL_TEXTURE_IMMUTABLE_LEVELS = liftIO $
        allocaArray 1 $ \ptr -> do
          GL.glGetTextureParameterIuiv target pname ptr
          [gluint] <- peekArray 1 ptr
          return (Nothing, Nothing, Just gluint, Nothing, Nothing)

      | pname `elem` [ GL.GL_TEXTURE_BASE_LEVEL
                     , GL.GL_TEXTURE_MAX_LEVEL
                     ] = liftIO $ -- GLint
        allocaArray 1 $ \ptr -> do
          GL.glGetTextureParameteriv target pname ptr
          [glint] <- peekArray 1 ptr
          return (Nothing, Nothing, Nothing, Just glint, Nothing)

      | pname `elem` [ GL.GL_TEXTURE_MAX_LOD
                     , GL.GL_TEXTURE_MIN_LOD
                     ] = liftIO $ -- GLfloat
        allocaArray 1 $ \ptr -> do
          GL.glGetTextureParameterfv target pname ptr
          [glfloat] <- peekArray 1 ptr
          return (Nothing, Nothing, Nothing, Nothing, Just glfloat)
      | otherwise = return (Nothing, Nothing, Nothing, Nothing, Nothing)
    glGetUniformfv program location n = liftIO $ withArray (replicate n 0) $ \ptr -> do
        GL.glGetUniformfv program location ptr
        V.fromList . map realToFrac <$> peekArray n ptr
    glGetUniformiv program location n = liftIO $ withArray (replicate n 0) $ \ptr -> do
      GL.glGetUniformiv program location ptr
      V.fromList . map fromIntegral <$> peekArray n ptr
    glGetUniformLocation program str = liftIO $
      withCString str $ GL.glGetUniformLocation program
    glGetVertexAttribfv index pname n = liftIO $ withArray (replicate n 0) $ \ptr -> do
      GL.glGetVertexAttribfv index pname ptr
      V.fromList . map realToFrac <$> liftIO (peekArray n ptr)
    glGetVertexAttribiv index pname n = liftIO $ withArray (replicate n 0) $ \ptr -> do
      GL.glGetVertexAttribiv index pname ptr
      V.fromList . map fromIntegral <$> liftIO (peekArray n ptr)
    glHint = GL.glHint
    glIsBuffer = GL.glIsBuffer
    glIsContextLost = return 0
    glIsEnabled = GL.glIsEnabled
    glIsFramebuffer = GL.glIsFramebuffer
    glIsProgram = GL.glIsProgram
    glIsRenderbuffer = GL.glIsRenderbuffer
    glIsShader = GL.glIsShader
    glIsTexture = GL.glIsTexture
    glLineWidth = GL.glLineWidth
    glLinkProgram = GL.glLinkProgram
    glPixelStorei = GL.glPixelStorei
    glPolygonOffset = GL.glPolygonOffset
    glReleaseShaderCompiler = GL.glReleaseShaderCompiler
    glRenderbufferStorage = GL.glRenderbufferStorage
    glSampleCoverage = GL.glSampleCoverage
    glScissor = GL.glScissor
    glShaderSource shader src = liftIO $ withCString src $ \ptr ->
      with ptr $ \ptrptr -> GL.glShaderSource shader 1 ptrptr nullPtr
    glStencilFunc = GL.glStencilFunc
    glStencilFuncSeparate = GL.glStencilFuncSeparate
    glStencilMask = GL.glStencilMask
    glStencilMaskSeparate = GL.glStencilMaskSeparate
    glStencilOp = GL.glStencilOp
    glStencilOpSeparate = GL.glStencilOpSeparate
    glTexParameterf = GL.glTexParameterf
    glTexParameteri = GL.glTexParameteri
    glTexImage2D target level internalFormat format typ (PtrInfo _ (w, h) dat) =
      GL.glTexImage2D target level internalFormat (fromIntegral w) (fromIntegral h) 0 format typ dat
    glTexSubImage2D target level x y format typ (PtrInfo _ (w, h) dat) =
      GL.glTexSubImage2D target level x y (fromIntegral w) (fromIntegral h) format typ dat
    glUniform1f  = GL.glUniform1f
    glUniform1fv loc vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniform1fv loc (fromIntegral $ V.length vec) $ castPtr ptr
    glUniform1i  = GL.glUniform1i
    glUniform1iv loc vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniform1iv loc (fromIntegral $ V.length vec) $ castPtr ptr
    glUniform2f  = GL.glUniform2f
    glUniform2fv loc vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniform2fv loc (fromIntegral $ V.length vec) $ castPtr ptr
    glUniform2i  = GL.glUniform2i
    glUniform2iv loc vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniform2iv loc (fromIntegral $ V.length vec) $ castPtr ptr
    glUniform3f  = GL.glUniform3f
    glUniform3fv loc vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniform3fv loc (fromIntegral $ V.length vec) $ castPtr ptr
    glUniform3i  = GL.glUniform3i
    glUniform3iv loc vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniform3iv loc (fromIntegral $ V.length vec) $ castPtr ptr
    glUniform4f  = GL.glUniform4f
    glUniform4fv loc vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniform4fv loc (fromIntegral $ V.length vec) $ castPtr ptr
    glUniform4i  = GL.glUniform4i
    glUniform4iv loc vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniform4iv loc (fromIntegral $ V.length vec) $ castPtr ptr
    glUniformMatrix2fv loc trans vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniformMatrix2fv loc (fromIntegral $ V.length vec) trans $ castPtr ptr
    glUniformMatrix3fv loc trans vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniformMatrix3fv loc (fromIntegral $ V.length vec) trans $ castPtr ptr
    glUniformMatrix4fv loc trans vec = liftIO $ V.unsafeWith vec $ \ptr ->
      GL.glUniformMatrix4fv loc (fromIntegral $ V.length vec) trans $ castPtr ptr
    glUseProgram = GL.glUseProgram
    glValidateProgram = GL.glValidateProgram
    glVertexAttrib1f = GL.glVertexAttrib1f
    --glVertexAttrib1fv loc vec = V.unsafeWith (V.map realToFrac vec) $
    --  GL.glVertexAttrib1fv loc
    glVertexAttrib2f = GL.glVertexAttrib2f
    --glVertexAttrib2fv loc vec = V.unsafeWith (V.map realToFrac vec) $
    --  GL.glVertexAttrib2fv loc
    glVertexAttrib3f = GL.glVertexAttrib3f
    --glVertexAttrib3fv loc vec = V.unsafeWith (V.map realToFrac vec) $
    --  GL.glVertexAttrib3fv loc
    glVertexAttrib4f = GL.glVertexAttrib4f
    --glVertexAttrib4fv loc vec = V.unsafeWith (V.map realToFrac vec) $
    --  GL.glVertexAttrib4fv loc
    glVertexAttribPointer index sz typ normed stride n =
      GL.glVertexAttribPointer index sz typ normed stride (intPtrToPtr $ fromIntegral n)
    glViewport = GL.glViewport

    gl_DEPTH_BUFFER_BIT  = GL.GL_DEPTH_BUFFER_BIT
    gl_STENCIL_BUFFER_BIT  = GL.GL_STENCIL_BUFFER_BIT
    gl_COLOR_BUFFER_BIT  = GL.GL_COLOR_BUFFER_BIT
    gl_POINTS  = GL.GL_POINTS
    gl_LINES  = GL.GL_LINES
    gl_LINE_LOOP  = GL.GL_LINE_LOOP
    gl_LINE_STRIP  = GL.GL_LINE_STRIP
    gl_TRIANGLES  = GL.GL_TRIANGLES
    gl_TRIANGLE_STRIP  = GL.GL_TRIANGLE_STRIP
    gl_TRIANGLE_FAN  = GL.GL_TRIANGLE_FAN
    gl_ZERO  = GL.GL_ZERO
    gl_ONE  = GL.GL_ONE
    gl_SRC_COLOR  = GL.GL_SRC_COLOR
    gl_ONE_MINUS_SRC_COLOR  = GL.GL_ONE_MINUS_SRC_COLOR
    gl_SRC_ALPHA  = GL.GL_SRC_ALPHA
    gl_ONE_MINUS_SRC_ALPHA  = GL.GL_ONE_MINUS_SRC_ALPHA
    gl_DST_ALPHA  = GL.GL_DST_ALPHA
    gl_ONE_MINUS_DST_ALPHA  = GL.GL_ONE_MINUS_DST_ALPHA
    gl_DST_COLOR  = GL.GL_DST_COLOR
    gl_ONE_MINUS_DST_COLOR  = GL.GL_ONE_MINUS_DST_COLOR
    gl_SRC_ALPHA_SATURATE  = GL.GL_SRC_ALPHA_SATURATE
    gl_FUNC_ADD  = GL.GL_FUNC_ADD
    --gl_BLEND_EQUATION  = GL.GL_BLEND_EQUATION
    gl_BLEND_EQUATION_RGB  = GL.GL_BLEND_EQUATION_RGB
    gl_BLEND_EQUATION_ALPHA  = GL.GL_BLEND_EQUATION_ALPHA
    gl_FUNC_SUBTRACT  = GL.GL_FUNC_SUBTRACT
    gl_FUNC_REVERSE_SUBTRACT  = GL.GL_FUNC_REVERSE_SUBTRACT
    gl_BLEND_DST_RGB  = GL.GL_BLEND_DST_RGB
    gl_BLEND_SRC_RGB  = GL.GL_BLEND_SRC_RGB
    gl_BLEND_DST_ALPHA  = GL.GL_BLEND_DST_ALPHA
    gl_BLEND_SRC_ALPHA  = GL.GL_BLEND_SRC_ALPHA
    gl_CONSTANT_COLOR  = GL.GL_CONSTANT_COLOR
    gl_ONE_MINUS_CONSTANT_COLOR  = GL.GL_ONE_MINUS_CONSTANT_COLOR
    gl_CONSTANT_ALPHA  = GL.GL_CONSTANT_ALPHA
    gl_ONE_MINUS_CONSTANT_ALPHA  = GL.GL_ONE_MINUS_CONSTANT_ALPHA
    gl_BLEND_COLOR  = GL.GL_BLEND_COLOR
    gl_ARRAY_BUFFER  = GL.GL_ARRAY_BUFFER
    gl_ELEMENT_ARRAY_BUFFER  = GL.GL_ELEMENT_ARRAY_BUFFER
    gl_ARRAY_BUFFER_BINDING  = GL.GL_ARRAY_BUFFER_BINDING
    gl_ELEMENT_ARRAY_BUFFER_BINDING  = GL.GL_ELEMENT_ARRAY_BUFFER_BINDING
    gl_STREAM_DRAW  = GL.GL_STREAM_DRAW
    gl_STATIC_DRAW  = GL.GL_STATIC_DRAW
    gl_DYNAMIC_DRAW  = GL.GL_DYNAMIC_DRAW
    gl_BUFFER_SIZE  = GL.GL_BUFFER_SIZE
    gl_BUFFER_USAGE  = GL.GL_BUFFER_USAGE
    gl_CURRENT_VERTEX_ATTRIB  = GL.GL_CURRENT_VERTEX_ATTRIB
    gl_FRONT  = GL.GL_FRONT
    gl_BACK  = GL.GL_BACK
    gl_FRONT_AND_BACK  = GL.GL_FRONT_AND_BACK
    gl_TEXTURE_2D  = GL.GL_TEXTURE_2D
    gl_CULL_FACE  = GL.GL_CULL_FACE
    gl_BLEND  = GL.GL_BLEND
    gl_DITHER  = GL.GL_DITHER
    gl_STENCIL_TEST  = GL.GL_STENCIL_TEST
    gl_DEPTH_TEST  = GL.GL_DEPTH_TEST
    gl_SCISSOR_TEST  = GL.GL_SCISSOR_TEST
    gl_POLYGON_OFFSET_FILL  = GL.GL_POLYGON_OFFSET_FILL
    gl_SAMPLE_ALPHA_TO_COVERAGE  = GL.GL_SAMPLE_ALPHA_TO_COVERAGE
    gl_SAMPLE_COVERAGE  = GL.GL_SAMPLE_COVERAGE
    gl_NO_ERROR  = GL.GL_NO_ERROR
    gl_INVALID_ENUM  = GL.GL_INVALID_ENUM
    gl_INVALID_VALUE  = GL.GL_INVALID_VALUE
    gl_INVALID_OPERATION  = GL.GL_INVALID_OPERATION
    gl_OUT_OF_MEMORY  = GL.GL_OUT_OF_MEMORY
    gl_CW  = GL.GL_CW
    gl_CCW  = GL.GL_CCW
    gl_LINE_WIDTH  = GL.GL_LINE_WIDTH
    -- | NOTE: I'm not sure if this is correct
    gl_ALIASED_POINT_SIZE_RANGE  = GL.GL_POINT_SIZE_RANGE
    gl_ALIASED_LINE_WIDTH_RANGE  = GL.GL_ALIASED_LINE_WIDTH_RANGE
    gl_CULL_FACE_MODE  = GL.GL_CULL_FACE_MODE
    gl_FRONT_FACE  = GL.GL_FRONT_FACE
    gl_DEPTH_RANGE  = GL.GL_DEPTH_RANGE
    gl_DEPTH_WRITEMASK  = GL.GL_DEPTH_WRITEMASK
    gl_DEPTH_CLEAR_VALUE  = GL.GL_DEPTH_CLEAR_VALUE
    gl_DEPTH_FUNC  = GL.GL_DEPTH_FUNC
    gl_STENCIL_CLEAR_VALUE  = GL.GL_STENCIL_CLEAR_VALUE
    gl_STENCIL_FUNC  = GL.GL_STENCIL_FUNC
    gl_STENCIL_FAIL  = GL.GL_STENCIL_FAIL
    gl_STENCIL_PASS_DEPTH_FAIL  = GL.GL_STENCIL_PASS_DEPTH_FAIL
    gl_STENCIL_PASS_DEPTH_PASS  = GL.GL_STENCIL_PASS_DEPTH_PASS
    gl_STENCIL_REF  = GL.GL_STENCIL_REF
    gl_STENCIL_VALUE_MASK  = GL.GL_STENCIL_VALUE_MASK
    gl_STENCIL_WRITEMASK  = GL.GL_STENCIL_WRITEMASK
    gl_STENCIL_BACK_FUNC  = GL.GL_STENCIL_BACK_FUNC
    gl_STENCIL_BACK_FAIL  = GL.GL_STENCIL_BACK_FAIL
    gl_STENCIL_BACK_PASS_DEPTH_FAIL  = GL.GL_STENCIL_BACK_PASS_DEPTH_FAIL
    gl_STENCIL_BACK_PASS_DEPTH_PASS  = GL.GL_STENCIL_BACK_PASS_DEPTH_PASS
    gl_STENCIL_BACK_REF  = GL.GL_STENCIL_BACK_REF
    gl_STENCIL_BACK_VALUE_MASK  = GL.GL_STENCIL_BACK_VALUE_MASK
    gl_STENCIL_BACK_WRITEMASK  = GL.GL_STENCIL_BACK_WRITEMASK
    gl_VIEWPORT  = GL.GL_VIEWPORT
    gl_SCISSOR_BOX  = GL.GL_SCISSOR_BOX
    gl_COLOR_CLEAR_VALUE  = GL.GL_COLOR_CLEAR_VALUE
    gl_COLOR_WRITEMASK  = GL.GL_COLOR_WRITEMASK
    gl_UNPACK_ALIGNMENT  = GL.GL_UNPACK_ALIGNMENT
    gl_PACK_ALIGNMENT  = GL.GL_PACK_ALIGNMENT
    gl_MAX_TEXTURE_SIZE  = GL.GL_MAX_TEXTURE_SIZE
    gl_MAX_VIEWPORT_DIMS  = GL.GL_MAX_VIEWPORT_DIMS
    gl_SUBPIXEL_BITS  = GL.GL_SUBPIXEL_BITS
    gl_RED_BITS  = GL.GL_RED_BITS
    gl_GREEN_BITS  = GL.GL_GREEN_BITS
    gl_BLUE_BITS  = GL.GL_BLUE_BITS
    gl_ALPHA_BITS  = GL.GL_ALPHA_BITS
    gl_DEPTH_BITS  = GL.GL_DEPTH_BITS
    gl_STENCIL_BITS  = GL.GL_STENCIL_BITS
    gl_POLYGON_OFFSET_UNITS  = GL.GL_POLYGON_OFFSET_UNITS
    gl_POLYGON_OFFSET_FACTOR  = GL.GL_POLYGON_OFFSET_FACTOR
    gl_TEXTURE_BINDING_2D  = GL.GL_TEXTURE_BINDING_2D
    gl_SAMPLE_BUFFERS  = GL.GL_SAMPLE_BUFFERS
    gl_SAMPLES  = GL.GL_SAMPLES
    gl_SAMPLE_COVERAGE_VALUE  = GL.GL_SAMPLE_COVERAGE_VALUE
    gl_SAMPLE_COVERAGE_INVERT  = GL.GL_SAMPLE_COVERAGE_INVERT
    gl_COMPRESSED_TEXTURE_FORMATS  = GL.GL_COMPRESSED_TEXTURE_FORMATS
    gl_DONT_CARE  = GL.GL_DONT_CARE
    gl_FASTEST  = GL.GL_FASTEST
    gl_NICEST  = GL.GL_NICEST
    gl_GENERATE_MIPMAP_HINT  = GL.GL_GENERATE_MIPMAP_HINT
    gl_BYTE  = GL.GL_BYTE
    gl_UNSIGNED_BYTE  = GL.GL_UNSIGNED_BYTE
    gl_SHORT  = GL.GL_SHORT
    gl_UNSIGNED_SHORT  = GL.GL_UNSIGNED_SHORT
    gl_INT  = GL.GL_INT
    gl_UNSIGNED_INT  = GL.GL_UNSIGNED_INT
    gl_FLOAT  = GL.GL_FLOAT
    gl_DEPTH_COMPONENT  = GL.GL_DEPTH_COMPONENT
    gl_ALPHA  = GL.GL_ALPHA
    gl_RGB  = GL.GL_RGB
    gl_RGBA  = GL.GL_RGBA
    gl_LUMINANCE  = GL.GL_LUMINANCE
    gl_LUMINANCE_ALPHA  = GL.GL_LUMINANCE_ALPHA
    gl_UNSIGNED_SHORT_4_4_4_4  = GL.GL_UNSIGNED_SHORT_4_4_4_4
    gl_UNSIGNED_SHORT_5_5_5_1  = GL.GL_UNSIGNED_SHORT_5_5_5_1
    gl_UNSIGNED_SHORT_5_6_5  = GL.GL_UNSIGNED_SHORT_5_6_5
    gl_FRAGMENT_SHADER  = GL.GL_FRAGMENT_SHADER
    gl_VERTEX_SHADER  = GL.GL_VERTEX_SHADER
    gl_MAX_VERTEX_ATTRIBS  = GL.GL_MAX_VERTEX_ATTRIBS
    gl_MAX_VERTEX_UNIFORM_VECTORS  = GL.GL_MAX_VERTEX_UNIFORM_VECTORS
    gl_MAX_VARYING_VECTORS  = GL.GL_MAX_VARYING_VECTORS
    gl_MAX_COMBINED_TEXTURE_IMAGE_UNITS  = GL.GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
    gl_MAX_VERTEX_TEXTURE_IMAGE_UNITS  = GL.GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
    gl_MAX_TEXTURE_IMAGE_UNITS  = GL.GL_MAX_TEXTURE_IMAGE_UNITS
    gl_MAX_FRAGMENT_UNIFORM_VECTORS  = GL.GL_MAX_FRAGMENT_UNIFORM_VECTORS
    gl_SHADER_TYPE  = GL.GL_SHADER_TYPE
    gl_DELETE_STATUS  = GL.GL_DELETE_STATUS
    gl_LINK_STATUS  = GL.GL_LINK_STATUS
    gl_VALIDATE_STATUS  = GL.GL_VALIDATE_STATUS
    gl_ATTACHED_SHADERS  = GL.GL_ATTACHED_SHADERS
    gl_ACTIVE_UNIFORMS  = GL.GL_ACTIVE_UNIFORMS
    gl_ACTIVE_ATTRIBUTES  = GL.GL_ACTIVE_ATTRIBUTES
    gl_SHADING_LANGUAGE_VERSION  = GL.GL_SHADING_LANGUAGE_VERSION
    gl_CURRENT_PROGRAM  = GL.GL_CURRENT_PROGRAM
    gl_NEVER  = GL.GL_NEVER
    gl_LESS  = GL.GL_LESS
    gl_EQUAL  = GL.GL_EQUAL
    gl_LEQUAL  = GL.GL_LEQUAL
    gl_GREATER  = GL.GL_GREATER
    gl_NOTEQUAL  = GL.GL_NOTEQUAL
    gl_GEQUAL  = GL.GL_GEQUAL
    gl_ALWAYS  = GL.GL_ALWAYS
    gl_KEEP  = GL.GL_KEEP
    gl_REPLACE  = GL.GL_REPLACE
    gl_INCR  = GL.GL_INCR
    gl_DECR  = GL.GL_DECR
    gl_INVERT  = GL.GL_INVERT
    gl_INCR_WRAP  = GL.GL_INCR_WRAP
    gl_DECR_WRAP  = GL.GL_DECR_WRAP
    gl_VENDOR  = GL.GL_VENDOR
    gl_RENDERER  = GL.GL_RENDERER
    gl_VERSION  = GL.GL_VERSION
    gl_NEAREST  = GL.GL_NEAREST
    gl_LINEAR  = GL.GL_LINEAR
    gl_NEAREST_MIPMAP_NEAREST  = GL.GL_NEAREST_MIPMAP_NEAREST
    gl_LINEAR_MIPMAP_NEAREST  = GL.GL_LINEAR_MIPMAP_NEAREST
    gl_NEAREST_MIPMAP_LINEAR  = GL.GL_NEAREST_MIPMAP_LINEAR
    gl_LINEAR_MIPMAP_LINEAR  = GL.GL_LINEAR_MIPMAP_LINEAR
    gl_TEXTURE_MAG_FILTER  = GL.GL_TEXTURE_MAG_FILTER
    gl_TEXTURE_MIN_FILTER  = GL.GL_TEXTURE_MIN_FILTER
    gl_TEXTURE_WRAP_S  = GL.GL_TEXTURE_WRAP_S
    gl_TEXTURE_WRAP_T  = GL.GL_TEXTURE_WRAP_T
    gl_TEXTURE  = GL.GL_TEXTURE
    gl_TEXTURE_CUBE_MAP  = GL.GL_TEXTURE_CUBE_MAP
    gl_TEXTURE_BINDING_CUBE_MAP  = GL.GL_TEXTURE_BINDING_CUBE_MAP
    gl_TEXTURE_CUBE_MAP_POSITIVE_X  = GL.GL_TEXTURE_CUBE_MAP_POSITIVE_X
    gl_TEXTURE_CUBE_MAP_NEGATIVE_X  = GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_X
    gl_TEXTURE_CUBE_MAP_POSITIVE_Y  = GL.GL_TEXTURE_CUBE_MAP_POSITIVE_Y
    gl_TEXTURE_CUBE_MAP_NEGATIVE_Y  = GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
    gl_TEXTURE_CUBE_MAP_POSITIVE_Z  = GL.GL_TEXTURE_CUBE_MAP_POSITIVE_Z
    gl_TEXTURE_CUBE_MAP_NEGATIVE_Z  = GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
    gl_MAX_CUBE_MAP_TEXTURE_SIZE  = GL.GL_MAX_CUBE_MAP_TEXTURE_SIZE
    gl_TEXTURE0  = GL.GL_TEXTURE0
    gl_TEXTURE1  = GL.GL_TEXTURE1
    gl_TEXTURE2  = GL.GL_TEXTURE2
    gl_TEXTURE3  = GL.GL_TEXTURE3
    gl_TEXTURE4  = GL.GL_TEXTURE4
    gl_TEXTURE5  = GL.GL_TEXTURE5
    gl_TEXTURE6  = GL.GL_TEXTURE6
    gl_TEXTURE7  = GL.GL_TEXTURE7
    gl_TEXTURE8  = GL.GL_TEXTURE8
    gl_TEXTURE9  = GL.GL_TEXTURE9
    gl_TEXTURE10  = GL.GL_TEXTURE10
    gl_TEXTURE11  = GL.GL_TEXTURE11
    gl_TEXTURE12  = GL.GL_TEXTURE12
    gl_TEXTURE13  = GL.GL_TEXTURE13
    gl_TEXTURE14  = GL.GL_TEXTURE14
    gl_TEXTURE15  = GL.GL_TEXTURE15
    gl_TEXTURE16  = GL.GL_TEXTURE16
    gl_TEXTURE17  = GL.GL_TEXTURE17
    gl_TEXTURE18  = GL.GL_TEXTURE18
    gl_TEXTURE19  = GL.GL_TEXTURE19
    gl_TEXTURE20  = GL.GL_TEXTURE20
    gl_TEXTURE21  = GL.GL_TEXTURE21
    gl_TEXTURE22  = GL.GL_TEXTURE22
    gl_TEXTURE23  = GL.GL_TEXTURE23
    gl_TEXTURE24  = GL.GL_TEXTURE24
    gl_TEXTURE25  = GL.GL_TEXTURE25
    gl_TEXTURE26  = GL.GL_TEXTURE26
    gl_TEXTURE27  = GL.GL_TEXTURE27
    gl_TEXTURE28  = GL.GL_TEXTURE28
    gl_TEXTURE29  = GL.GL_TEXTURE29
    gl_TEXTURE30  = GL.GL_TEXTURE30
    gl_TEXTURE31  = GL.GL_TEXTURE31
    gl_ACTIVE_TEXTURE  = GL.GL_ACTIVE_TEXTURE
    gl_REPEAT  = GL.GL_REPEAT
    gl_CLAMP_TO_EDGE  = GL.GL_CLAMP_TO_EDGE
    gl_MIRRORED_REPEAT  = GL.GL_MIRRORED_REPEAT
    gl_FLOAT_VEC2  = GL.GL_FLOAT_VEC2
    gl_FLOAT_VEC3  = GL.GL_FLOAT_VEC3
    gl_FLOAT_VEC4  = GL.GL_FLOAT_VEC4
    gl_INT_VEC2  = GL.GL_INT_VEC2
    gl_INT_VEC3  = GL.GL_INT_VEC3
    gl_INT_VEC4  = GL.GL_INT_VEC4
    gl_BOOL  = GL.GL_BOOL
    gl_BOOL_VEC2  = GL.GL_BOOL_VEC2
    gl_BOOL_VEC3  = GL.GL_BOOL_VEC3
    gl_BOOL_VEC4  = GL.GL_BOOL_VEC4
    gl_FLOAT_MAT2  = GL.GL_FLOAT_MAT2
    gl_FLOAT_MAT3  = GL.GL_FLOAT_MAT3
    gl_FLOAT_MAT4  = GL.GL_FLOAT_MAT4
    gl_SAMPLER_2D  = GL.GL_SAMPLER_2D
    gl_SAMPLER_CUBE  = GL.GL_SAMPLER_CUBE
    gl_VERTEX_ATTRIB_ARRAY_ENABLED  = GL.GL_VERTEX_ATTRIB_ARRAY_ENABLED
    gl_VERTEX_ATTRIB_ARRAY_SIZE  = GL.GL_VERTEX_ATTRIB_ARRAY_SIZE
    gl_VERTEX_ATTRIB_ARRAY_STRIDE  = GL.GL_VERTEX_ATTRIB_ARRAY_STRIDE
    gl_VERTEX_ATTRIB_ARRAY_TYPE  = GL.GL_VERTEX_ATTRIB_ARRAY_TYPE
    gl_VERTEX_ATTRIB_ARRAY_NORMALIZED  = GL.GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
    gl_VERTEX_ATTRIB_ARRAY_POINTER  = GL.GL_VERTEX_ATTRIB_ARRAY_POINTER
    gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING  = GL.GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
    gl_IMPLEMENTATION_COLOR_READ_TYPE  = GL.GL_IMPLEMENTATION_COLOR_READ_TYPE
    gl_IMPLEMENTATION_COLOR_READ_FORMAT  = GL.GL_IMPLEMENTATION_COLOR_READ_FORMAT
    gl_COMPILE_STATUS  = GL.GL_COMPILE_STATUS
    gl_LOW_FLOAT  = GL.GL_LOW_FLOAT
    gl_MEDIUM_FLOAT  = GL.GL_MEDIUM_FLOAT
    gl_HIGH_FLOAT  = GL.GL_HIGH_FLOAT
    gl_LOW_INT  = GL.GL_LOW_INT
    gl_MEDIUM_INT  = GL.GL_MEDIUM_INT
    gl_HIGH_INT  = GL.GL_HIGH_INT
    gl_FRAMEBUFFER  = GL.GL_FRAMEBUFFER
    gl_RENDERBUFFER  = GL.GL_RENDERBUFFER
    gl_RGBA4  = GL.GL_RGBA4
    gl_RGB5_A1  = GL.GL_RGB5_A1
    gl_RGB565  = GL.GL_RGB565
    gl_DEPTH_COMPONENT16  = GL.GL_DEPTH_COMPONENT16
    gl_STENCIL_INDEX  = GL.GL_STENCIL_INDEX
    gl_STENCIL_INDEX8  = GL.GL_STENCIL_INDEX8
    gl_DEPTH_STENCIL  = GL.GL_DEPTH_STENCIL
    gl_RENDERBUFFER_WIDTH  = GL.GL_RENDERBUFFER_WIDTH
    gl_RENDERBUFFER_HEIGHT  = GL.GL_RENDERBUFFER_HEIGHT
    gl_RENDERBUFFER_INTERNAL_FORMAT  = GL.GL_RENDERBUFFER_INTERNAL_FORMAT
    gl_RENDERBUFFER_RED_SIZE  = GL.GL_RENDERBUFFER_RED_SIZE
    gl_RENDERBUFFER_GREEN_SIZE  = GL.GL_RENDERBUFFER_GREEN_SIZE
    gl_RENDERBUFFER_BLUE_SIZE  = GL.GL_RENDERBUFFER_BLUE_SIZE
    gl_RENDERBUFFER_ALPHA_SIZE  = GL.GL_RENDERBUFFER_ALPHA_SIZE
    gl_RENDERBUFFER_DEPTH_SIZE  = GL.GL_RENDERBUFFER_DEPTH_SIZE
    gl_RENDERBUFFER_STENCIL_SIZE  = GL.GL_RENDERBUFFER_STENCIL_SIZE
    gl_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE  = GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
    gl_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME  = GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
    gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL  = GL.GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
    gl_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  = GL.GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
    gl_COLOR_ATTACHMENT0  = GL.GL_COLOR_ATTACHMENT0
    gl_DEPTH_ATTACHMENT  = GL.GL_DEPTH_ATTACHMENT
    gl_STENCIL_ATTACHMENT  = GL.GL_STENCIL_ATTACHMENT
    gl_DEPTH_STENCIL_ATTACHMENT  = GL.GL_DEPTH_STENCIL_ATTACHMENT
    gl_NONE  = GL.GL_NONE
    gl_FRAMEBUFFER_COMPLETE  = GL.GL_FRAMEBUFFER_COMPLETE
    gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT  = GL.GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
    gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT  = GL.GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
    gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS  = GL.GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
    gl_FRAMEBUFFER_UNSUPPORTED  = GL.GL_FRAMEBUFFER_UNSUPPORTED
    gl_FRAMEBUFFER_BINDING  = GL.GL_FRAMEBUFFER_BINDING
    gl_RENDERBUFFER_BINDING  = GL.GL_RENDERBUFFER_BINDING
    gl_MAX_RENDERBUFFER_SIZE  = GL.GL_MAX_RENDERBUFFER_SIZE
    gl_INVALID_FRAMEBUFFER_OPERATION  = GL.GL_INVALID_FRAMEBUFFER_OPERATION
    --gl_UNPACK_FLIP_Y_WEBGL  = GL.GL_UNPACK_FLIP_Y_WEBGL
    --gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL  = GL.GL_UNPACK_PREMULTIPLY_ALPHA_WEBGL
    --gl_CONTEXT_LOST_WEBGL  = GL.GL_CONTEXT_LOST_WEBGL
    --gl_UNPACK_COLORSPACE_CONVERSION_WEBGL  = GL.GL_UNPACK_COLORSPACE_CONVERSION_WEBGL
    --gl_BROWSER_DEFAULT_WEBGL  = GL.GL_BROWSER_DEFAULT_WEBGL
