module Syscall where

foreign import ccall "syscall" c_syscall_0 :: Int -> IO Int
foreign import ccall "syscall" c_syscall_1 :: Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_2 :: Int -> Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_3 :: Int -> Int -> Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_4 :: Int -> Int -> Int -> Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_5 :: Int -> Int -> Int -> Int -> Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_6 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int

syscall :: Int -> Int -> [Int] -> IO Int
syscall i t args' = let args = reverse args' in case i of
  0 -> c_syscall_0 t
  1 -> c_syscall_1 t (args!!0)
  2 -> c_syscall_2 t (args!!0) (args!!1)
  3 -> c_syscall_3 t (args!!0) (args!!1) (args!!2)
  4 -> c_syscall_4 t (args!!0) (args!!1) (args!!2) (args!!3)
  5 -> c_syscall_5 t (args!!0) (args!!1) (args!!2) (args!!3) (args!!4)
  6 -> c_syscall_6 t (args!!0) (args!!1) (args!!2) (args!!3) (args!!4) (args!!5)
