module Models where

shipModel :: HaskeroidsGame -> Picture
shipModel game
  | acc game = pictures [
    (line [(0, (shL / 2)), (-(shW / 2), -(shL / 2))])
    ,(line [(0, (shL / 2)),((shW / 2), -(shL / 2))])
    ,(line [(-joint,-(shL / 3)),(joint,-(shL / 3))])
    ,(color gameColor $ polygon [(-(shW / 5), -(shL / 2)), ((shW / 5), -(shL / 2)), (0, -(shL / 1.3))])]
  | otherwise = pictures [
    (line [(0, (shL / 2)), (-(shW / 2), -(shL / 2))])
    ,(line [(0, (shL / 2)),((shW / 2), -(shL / 2))])
    ,(line [(-joint,-(shL / 3)),(joint,-(shL / 3))])]
  where
    shW :: Float
    shW = 16
    shL :: Float
    shL = 24
    joint :: Float
    joint = shW/3
