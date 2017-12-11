{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso

data Action
    = NoOp

type Color = Int
type Field = [[Maybe Color]]

main :: IO ()
main = startApp App {..}
    where
        initialAction = NoOp
        model         = undefined
        update        = updateField
        view          = viewField
        subs          = []
        events        = defaultEvents

updateField :: Action -> Field -> Effect Action Field
updateField = undefined

viewField :: Field -> View Action
viewField = undefined
