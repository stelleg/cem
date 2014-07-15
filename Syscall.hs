module Syscall where

foreign import ccall "syscall" c_syscall_0 :: Int -> IO Int
foreign import ccall "syscall" c_syscall_1 :: Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_2 :: Int -> Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_3 :: Int -> Int -> Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_4 :: Int -> Int -> Int -> Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_5 :: Int -> Int -> Int -> Int -> Int -> Int -> IO Int
foreign import ccall "syscall" c_syscall_6 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int

syscall :: Int -> [Int] -> IO Int
syscall i args = case i of
  0 -> c_syscall_0 a where [a] = args
  1 -> c_syscall_1 a b where [a,b] = args 
  2 -> c_syscall_2 a b c where [a,c,b] = args
  3 -> c_syscall_3 a b c d where [a,d,c,b] = args
  4 -> c_syscall_4 a b c d e where [a,e,d,c,b] = args
  5 -> c_syscall_5 a b c d e f where [a,f,e,d,c,b] = args
  6 -> c_syscall_6 a b c d e f g where [a,g,f,e,d,c,b] = args
