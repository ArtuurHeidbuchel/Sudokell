module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (transpose)

-- ─── Types ───────────────────────────────────────────────────────────────────

type Board = [[Int]]  -- 0 means empty

data GameState = GameState
  { board       :: Board   -- current board (with user input)
  , puzzle      :: Board   -- original puzzle (locked cells)
  , selected    :: Maybe (Int, Int)  -- (row, col)
  , message     :: String
  , solved      :: Bool
  }

-- ─── Puzzle ──────────────────────────────────────────────────────────────────

examplePuzzle :: Board
examplePuzzle =
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0]
  , [6, 0, 0, 1, 9, 5, 0, 0, 0]
  , [0, 9, 8, 0, 0, 0, 0, 6, 0]
  , [8, 0, 0, 0, 6, 0, 0, 0, 3]
  , [4, 0, 0, 8, 0, 3, 0, 0, 1]
  , [7, 0, 0, 0, 2, 0, 0, 0, 6]
  , [0, 6, 0, 0, 0, 0, 2, 8, 0]
  , [0, 0, 0, 4, 1, 9, 0, 0, 5]
  , [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

initialState :: GameState
initialState = GameState
  { board    = examplePuzzle
  , puzzle   = examplePuzzle
  , selected = Nothing
  , message  = ""
  , solved   = False
  }

-- ─── Layout constants ────────────────────────────────────────────────────────

windowSize :: Int
windowSize = 600

cellSize :: Float
cellSize = fromIntegral windowSize / 9

boardOffset :: Float
boardOffset = fromIntegral windowSize / 2  -- we center at origin

-- Convert board (row,col) to screen (x,y) center of cell
cellCenter :: Int -> Int -> (Float, Float)
cellCenter row col =
  ( fromIntegral col * cellSize + cellSize / 2 - boardOffset
  , boardOffset - fromIntegral row * cellSize - cellSize / 2
  )

-- Convert screen position to board (row, col)
screenToCell :: Float -> Float -> Maybe (Int, Int)
screenToCell x y =
  let col = floor ((x + boardOffset) / cellSize)
      row = floor ((boardOffset - y) / cellSize)
  in if row >= 0 && row < 9 && col >= 0 && col < 9
     then Just (row, col)
     else Nothing

-- ─── Validation ──────────────────────────────────────────────────────────────

getRow :: Board -> Int -> [Int]
getRow b r = b !! r

getCol :: Board -> Int -> [Int]
getCol b c = map (!! c) b

getBox :: Board -> Int -> Int -> [Int]
getBox b r c =
  let br = (r `div` 3) * 3
      bc = (c `div` 3) * 3
  in [ b !! rr !! cc | rr <- [br..br+2], cc <- [bc..bc+2] ]

-- Is the value at (r,c) in conflict with any other cell?
isConflict :: Board -> Int -> Int -> Bool
isConflict b r c =
  let v = b !! r !! c
  in v /= 0 && ( countIn (getRow b r) v > 1
              || countIn (getCol b c) v > 1
              || countIn (getBox b r c) v > 1 )
  where
    countIn xs x = length (filter (== x) xs)

isBoardFull :: Board -> Bool
isBoardFull = all (all (/= 0))

noConflictsAnywhere :: Board -> Bool
noConflictsAnywhere b =
  not $ any (\(r,c) -> isConflict b r c)
            [(r,c) | r <- [0..8], c <- [0..8]]

isSolved :: Board -> Bool
isSolved b = isBoardFull b && noConflictsAnywhere b

-- ─── Rendering ───────────────────────────────────────────────────────────────

render :: GameState -> Picture
render gs = Pictures
  [ drawHighlight gs
  , drawNumbers gs
  , drawGrid
  , drawMessage (message gs)
  , drawCheckButton
  ]

-- Highlight selected cell (blue) and conflicting cells (red)
drawHighlight :: GameState -> Picture
drawHighlight gs = Pictures
  [ highlightCell r c col
  | r <- [0..8], c <- [0..8]
  , let maybeCol = cellColor gs r c
  , maybeCol /= Nothing
  , let Just col = maybeCol
  ]
  where
    cellColor state r c
      | Just (r,c) == selected state = Just selectedColor
      | isConflict (board state) r c  = Just conflictColor
      | otherwise                     = Nothing
    selectedColor = makeColorI 173 216 230 255  -- light blue
    conflictColor = makeColorI 255 182 193 255  -- light red

highlightCell :: Int -> Int -> Color -> Picture
highlightCell row col col' =
  let (x, y) = cellCenter row col
  in Translate x y $ Color col' $ rectangleSolid cellSize cellSize

drawNumbers :: GameState -> Picture
drawNumbers gs = Pictures
  [ drawNumber r c | r <- [0..8], c <- [0..8] ]
  where
    drawNumber r c =
      let v = board gs !! r !! c
          (x, y) = cellCenter r c
          isLocked = puzzle gs !! r !! c /= 0
          col = if isLocked then black else makeColorI 30 100 200 255
      in if v == 0 then Blank
         else Translate (x - 7) (y - 10) $
              Scale 0.18 0.18 $
              Color col $
              Text (show v)

drawGrid :: Picture
drawGrid = Pictures $ thinLines ++ thickLines
  where
    thinLines =
      [ Color (greyN 0.6) $ Line [(x, -boardOffset), (x, boardOffset)]
      | i <- [0..9]
      , let x = fromIntegral i * cellSize - boardOffset
      ] ++
      [ Color (greyN 0.6) $ Line [(-boardOffset, y), (boardOffset, y)]
      | i <- [0..9]
      , let y = boardOffset - fromIntegral i * cellSize
      ]
    thickLines =
      [ Color black $ Pictures
          [ Line [(x-1, -boardOffset), (x-1, boardOffset)]
          , Line [(x,   -boardOffset), (x,   boardOffset)]
          , Line [(x+1, -boardOffset), (x+1, boardOffset)]
          ]
      | i <- [0,3,6,9]
      , let x = fromIntegral i * cellSize - boardOffset
      ] ++
      [ Color black $ Pictures
          [ Line [(-boardOffset, y-1), (boardOffset, y-1)]
          , Line [(-boardOffset, y  ), (boardOffset, y  )]
          , Line [(-boardOffset, y+1), (boardOffset, y+1)]
          ]
      | i <- [0,3,6,9]
      , let y = boardOffset - fromIntegral i * cellSize
      ]

drawCheckButton :: Picture
drawCheckButton =
  Translate 0 (-boardOffset - 35) $ Pictures
    [ Color (makeColorI 70 130 180 255) $ rectangleSolid 160 36
    , Translate (-52) (-10) $ Scale 0.13 0.13 $ Color white $ Text "Check Solution"
    ]

drawMessage :: String -> Picture
drawMessage "" = Blank
drawMessage msg =
  Translate (-150) (-boardOffset - 75) $
  Scale 0.15 0.15 $
  Color (makeColorI 0 128 0 255) $
  Text msg

-- ─── Input handling ───────────────────────────────────────────────────────────

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gs
  -- Check Solution button
  | x >= -80 && x <= 80 && y >= (-boardOffset - 53) && y <= (-boardOffset - 17) =
      let ok = isSolved (board gs)
      in gs { message = if ok then "Solved! Congratulations!" else "Not solved yet — keep going!"
            , solved  = ok }
  -- Cell selection
  | otherwise =
      gs { selected = screenToCell x y, message = "" }

handleEvent (EventKey (Char c) Down _ _) gs
  | Just (r, col) <- selected gs
  , puzzle gs !! r !! col == 0   -- only edit non-locked cells
  , c >= '1' && c <= '9' =
      let v  = fromEnum c - fromEnum '0'
          b' = setCell (board gs) r col v
      in gs { board = b' }

handleEvent (EventKey (SpecialKey KeyDelete) Down _ _) gs =
    clearSelected gs
handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) gs =
    clearSelected gs
handleEvent (EventKey (Char '0') Down _ _) gs =
    clearSelected gs

handleEvent _ gs = gs

clearSelected :: GameState -> GameState
clearSelected gs = case selected gs of
  Just (r, c)
    | puzzle gs !! r !! c == 0 ->
        gs { board = setCell (board gs) r c 0 }
  _ -> gs

setCell :: Board -> Int -> Int -> Int -> Board
setCell b r c v =
  [ [ if rr == r && cc == c then v else b !! rr !! cc
    | cc <- [0..8] ]
  | rr <- [0..8] ]

-- ─── Main ─────────────────────────────────────────────────────────────────────

main :: IO ()
main = play
  (InWindow "Sudoku" (windowSize, windowSize + 100) (100, 100))
  white
  30
  initialState
  render
  handleEvent
  (\_ gs -> gs)  -- no time-based updates needed