module Output(pprintList) where

import qualified Util as U

pprintIterate :: [Int] -> Int -> Int -> String
pprintIterate (f:rest) lineIndex size =
  show f ++ end ++ pprintIterate rest newLineIndex size
  where
    isEnd = lineIndex == size
    end = if isEnd then "\n" else " "
    newLineIndex = if isEnd then 1 else lineIndex + 1
pprintIterate [] _ _ = ""

pprintList :: [Int] -> IO ()
pprintList list = putStrLn $ pprintIterate list 1 size where
    size = U.getSize list
