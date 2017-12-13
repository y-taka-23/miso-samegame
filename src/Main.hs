{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Maybe ( isJust )
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
updateField (Knowkout pos) = noEff . sweep . (mark pos)
updateField NoOp           = noEff

mark :: Position -> Field -> Field
mark = undefined

sweep :: Field -> Field
sweep = filter (not . null) . map (filter isJust)

viewField :: Field -> View Action
viewField = undefined
