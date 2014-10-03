{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell io.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs
  >>= (getFile . head)
  >>= \(_, paths) -> run paths

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run =
  (printFiles <=< getFiles) . lines

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
{-
getFiles Nil = pure Nil
getFiles (x :. xs) = do
  f <- getFile x
  fs <- getFiles xs
  return $ f :. fs
-}
getFiles =
  foldRight
    (\fp acc ->
      --acc >>= (\a -> (do gfp <- getFile fp; return (gfp :. a))))
      (:.) <$> (getFile fp) <*> acc)
    (pure Nil)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fp = do
  contents <- readFile fp
  return (fp, contents)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  foldLeft
    (\m (fp, contents) ->
      m >> printFile fp contents)
    (return ())

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fp contents = do
  putStrLn $ "============ " ++ fp
  putStrLn contents

