import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Puzzle
import Table

puzzleFromImage :: String -> IO(Diagram B)
puzzleFromImage path =
  do (rows, cols) <- imageToRowsAndColums path
     makeNoisyPuzzle rows cols 

main :: IO ()
main = mainWith puzzleFromImage
