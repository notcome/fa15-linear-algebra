\documentclass{article}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\begin{document}
\begin{code}
import Data.Matrix
import Data.Ratio

echelonForm :: forall a. (Num a, Fractional a, Eq a) => Matrix a -> Matrix a
echelonForm m = foldl reduceColumn m [1..width]
  where
    height  =  nrows m
    width   =  ncols m

    findPivot :: Int -> Matrix a -> Maybe Int
    findPivot col mat = let
      list = [row | row <- [col..height], getElem row col mat /= 0]
      in case list of
        []    ->  Nothing
        c:cs  ->  Just c

    reduceColumn :: Matrix a -> Int -> Matrix a
    reduceColumn mat col = case findPivot col mat of
      Nothing  ->  mat
      Just p   ->  let
        row    =  col
        mat'   =  switchRows p row mat
        scale  =  1 / getElem row col mat'
        mat''  =  scaleRow scale row mat'
        rows   =  [r | r <- [1..height], r /= row]
        in foldl (zeroify col) mat'' rows

    zeroify :: Int -> (Matrix a -> Int -> Matrix a)
    zeroify col mat row = combineRows row n col mat
      where n = -getElem row col mat

buildMatrix :: Int -> Matrix Rational
buildMatrix n = matrix (n + 1) (n + 2) generator
  where
    generator (row, col) = let
      pow  =  toInteger (row - 1)
      x    =  toInteger (col - 1) % toInteger n
      in  if col == n + 2
          then 1 % (pow + 1)
          else x ^ pow

main :: IO ()
main = do
  let mat   =  buildMatrix 8
  putStrLn  $  prettyMatrix mat
  let mat'  =  echelonForm mat
  let sols  =  getCol 10 mat'
  print sols
\end{code}
\end{document}
