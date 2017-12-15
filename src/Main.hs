{-# LANGUAGE RecordWildCards #-}
module Main where

import Safe                      ( atMay )
import Control.Lens              ( (.=), ix )
import Control.Monad             ( when )
import Control.Monad.Trans.State ( State, get, execState )
import Data.Maybe                ( isJust )
import Miso

data Action
    = NoOp
    | Knockout Position

type Color = Int
type Field = [[Maybe Color]]
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

mark :: Position -> Field -> Field
mark pos field =
    case colorOf pos field of
        Just color -> execState (markWithColor color pos) field
        Nothing    -> field

-- Todo: Use Lenses
colorOf ::  Position -> Field -> Maybe Color
colorOf (i, j) field = do
    mColumn <- field `atMay` i
    mColor  <- mColumn `atMay` j
    mColor

markWithColor :: Color -> Position -> State Field ()
markWithColor color (i, j) = do
    mColor <- colorOf (i, j) <$> get
    when (mColor == Just color) $ do
        ix i . ix j .= Nothing
        mapM_ (markWithColor color)
            [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

sweep :: Field -> Field
sweep = filter (not . null) . map (filter isJust)

viewField :: Field -> View Action
viewField = undefined
