{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Safe                      ( atMay )
import Control.Lens              ( (.=), ix )
import Control.Monad             ( when )
import Control.Monad.Trans.State ( State, get, execState )
import Data.Maybe                ( catMaybes )
import Data.List                 ( intercalate )
import Data.List.Split           ( chunksOf )
import Miso
import Miso.String               ( ms )
import System.Random.MWC         ( createSystemRandom, uniformR )

data Action
    = NoOp
    | Knockout Position

type Color = Int
type Field = [[Color]]
type MarkedField = [[Maybe Color]]
type Position = (Int, Int)

main :: IO ()
main = do
    initialField <- generateField
    startApp App { model = initialField, ..}
    where
        initialAction = NoOp
        update        = updateField
        view          = viewField
        subs          = []
        events        = defaultEvents
        mountPoint    = Nothing

-- Todo: Make the configus changeable
fieldWidth :: Int
fieldWidth  = 10

fieldHeight :: Int
fieldHeight = 7

numOfColors :: Int
numOfColors = 5

generateField :: IO Field
generateField = do
    gen    <- createSystemRandom
    colors <- sequence $ replicate (fieldWidth * fieldHeight) $
        uniformR (0, numOfColors - 1) gen
    return $ chunksOf fieldHeight colors

updateField :: Action -> Field -> Effect Action Field
updateField (Knockout pos) = noEff . sweep . (mark pos)
updateField NoOp           = noEff

mark :: Position -> Field -> MarkedField
mark pos field =
    let mField = map (map Just) field
    in case colorOf pos mField of
        Just color -> if isClustered pos mField
            then execState (markWithColor color pos) mField
            else mField
        Nothing -> mField

-- Todo: Use Lenses
colorOf ::  Position -> MarkedField -> Maybe Color
colorOf (i, j) mField = do
    mColumn <- mField `atMay` i
    mColor  <- mColumn `atMay` j
    mColor

-- Todo: integrate it in the marking logic
isClustered :: Position -> MarkedField -> Bool
isClustered (i, j) mField =
    or $ map (\p -> colorOf p mField == colorOf (i, j) mField) $
        [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

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
      index i (co : cos) = zipWith (index' i) [0..] co ++ index (i + 1) cos

      index' :: Int -> Int -> Color -> (Position, Color)
      index' i j color = ((i, j), color)

viewBlock :: Position -> Color -> View Action
viewBlock (i, j) color =
    div_
        [ id_    . ms $ intercalate "-" ["position", show i, show j]
        , class_ . ms $ "block color-" ++ show color
        ]
        [ text . ms $ show color
        ]
