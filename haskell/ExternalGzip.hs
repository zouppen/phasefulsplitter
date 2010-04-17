-- |External gzip. Uses runProcess to execute gzip. The benefit is
-- |that we get a handle. In 'Codec.Compression.GZip' it is impossible
-- |to write to a compressed file in multiple passes. This may work
-- |only in POSIX platforms like Linux and Mac OS X.

module ExternalGzip (openGzipOutFile,ExternalHandle,getWriteH,
                     closeExternalHandle) where

import System.IO
import System.Process
import System.Exit

type ExternalHandle = (Handle, Handle, Handle, ProcessHandle)

-- |Opens Gzip output file and returns handles.
openGzipOutFile :: FilePath -> IO ExternalHandle
openGzipOutFile fileName = runInteractiveCommand $
                           "gzip >" ++ escapeForShell fileName

-- |Returns Handle for writing to Gzip file.
getWriteH :: ExternalHandle -> Handle
getWriteH (inH,_,_,_) = inH

-- |Closes external handle.
closeExternalHandle :: ExternalHandle -> IO ExitCode
closeExternalHandle (inH,outH,errH,pH) = do
  hClose inH
  hClose outH
  hClose errH
  waitForProcess pH

-- |Converts FilePath to escaped form. For using inside shell context
-- |where mysterious things may happen if strings are not escaped
-- |properly.
escapeForShell :: String -> String
escapeForShell str = '\'':escapeForShell' str

-- The man page of GNU Bash states: Enclosing characters in single quotes
-- preserves the literal value of each character within the quotes. A
-- single quote may not occur between single quotes, even when
-- preceded by a backslash.

escapeForShell' ('\'':xs) = error "Single quote in file name is unsupported at the moment."
escapeForShell' [] = "'"
escapeForShell' (x:xs) = x:escapeForShell' xs
