{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Safe                      ( atMay )
import Control.Lens              ( (.=), ix )
import Control.Monad             ( when )
import Control.Monad.Trans.State ( State, get, execState )
import Data.Maybe                ( catMaybes )
import Data.List                 ( intercalate )
import Miso
import Miso.String               ( ms )

data Action
    = NoOp
    | Knockout Position

type Color = Int
type Field = [[Color]]
type MarkedField = [[Maybe Color]]
type Position = (Int, Int)

main :: IO ()
main = startApp App {..}
    where
        initialAction = NoOp
        model         = undefined
        update        = updateField
        view          = viewField
        subs          = []
        events        = defaultEvents
        mountPoint    = Nothing

updateField :: Action -> Field -> Effect Action Field
updateField (Knockout pos) = noEff . sweep . (mark pos)
updateField NoOp           = noEff

mark :: Position -> Field -> MarkedField
mark pos field =
    let mField = map (map Just) field
    in case colorOf pos mField of
        Just color -> execState (markWithColor color pos) mField
        Nothing    -> mField

-- Todo: Use Lenses
colorOf ::  Position -> MarkedField -> Maybe Color
colorOf (i, j) mField = do
    mColumn <- mField `atMay` i
    mColor  <- mColumn `atMay` j
    mColor

markWithColor :: Color -> Position -> State MarkedField ()
markWithColor color (i, j) = do
    mColor <- colorOf (i, j) <$> get
    when (mColor == Just color) $ do
        ix i . ix j .= Nothing
        mapM_ (markWithColor color)
            [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

sweep :: MarkedField -> Field
sweep = filter (not . null) . map catMaybes

viewField :: Field -> View Action
viewField field = div_ [ id_ "field" ] blocks
    where
      blocks :: [View Action]
      blocks = map (uncurry viewBlock) $ index 0 field

      index :: Int -> Field -> [(Position, Color)]
      index _ []         = []
      index j (co : cos) = zipWith (index' j) [0..] co ++ index (j + 1) cos

      index' :: Int -> Int -> Color -> (Position, Color)
      index' j i color = ((i, j), color) ]

viewBlock :: Position -> Color -> View Action
viewBlock (i, j) color = div_ [ id_ (ms selector) ] []
    where
        selector = intercalate "-" ["block", show i, show j, show color]
