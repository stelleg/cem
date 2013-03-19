module VM where

import Foreign.C.Types
import Foreign.Ptr 
import Foreign.Storable
import Foreign.Marshal.Array 
import LC

foreign import ccall "cem.h runCEM" c_cem :: Ptr Instr -> IO (Ptr Instr)

instance Storable Instr where
  sizeOf _ = 2 * sizeOf (undefined :: Ptr ())
  alignment _ = alignment (undefined :: CIntPtr) 
  peek ptr = do 
    code <- peekByteOff ptr 0 :: IO CUChar
    case code of
      0 -> (peekByteOff ptr 8 :: IO (Ptr Instr)) >>= (return.PUSH.(`minusPtr` ptr))
      1 -> (peekByteOff ptr 8 :: IO CInt) >>= (return.TAKE . fromEnum)
      2 -> (peekByteOff ptr 8 :: IO CInt) >>= (return.ENTER . fromEnum)
  poke ptr (PUSH m) = do
    (\ptr val -> do {pokeByteOff ptr 0 (val::CUChar)}) ptr 0 
    (\ptr val -> do {pokeByteOff ptr 8 (val::(Ptr Instr))}) ptr $ plusPtr' ptr m
  poke ptr (TAKE fv) = do
    (\ptr val -> do {pokeByteOff ptr 0 (val::CUChar)}) ptr 1
    (\ptr val -> do {pokeByteOff ptr 8 (val::CInt)}) ptr $ toEnum fv  
  poke ptr (ENTER v) = do
    (\ptr val -> do {pokeByteOff ptr 0 (val::CUChar)}) ptr 2
    (\ptr val -> do {pokeByteOff ptr 8 (val::CInt)}) ptr $ toEnum v

plusPtr' = advancePtr

minusPtr' :: Storable a => Ptr a -> Ptr a -> Int
minusPtr' = doMinus undefined 
  where doMinus :: Storable a' => a' -> Ptr a' -> Ptr a' -> Int
        doMinus dummy a b = (a `minusPtr` b) `div` sizeOf dummy
        
