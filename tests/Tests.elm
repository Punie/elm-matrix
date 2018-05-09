module Tests exposing (all)

import Matrix.Tests as MT
import Test exposing (Test, describe, test)


all : Test
all =
    describe "All Tests"
        [ MT.repeat
        , MT.initialize
        , MT.identity
        , MT.map
        , MT.map2
        , MT.transpose
        , MT.dot
        , MT.toLists
        ]
