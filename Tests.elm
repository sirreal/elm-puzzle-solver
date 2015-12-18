module Tests where

import ElmTest exposing (..)

import Solver exposing (..)


all : Test
all =
    suite "All"
        [ test "Row groups" (assertEqual [1, 2, 3] (rowGroups [True, False, True, True, False, True, True, True]))
        , test "Row groups empty" (assertEqual [] (rowGroups []))
        , test "Row groups all false" (assertEqual [] (rowGroups [False, False, False]))
        , test "Row groups true end" (assertEqual [1] (rowGroups [False, False, False, True]))

        , test "Take while none" (assertEqual [] (takeWhile (identity) [False, False, False, True]))
        , test "Take while" (assertEqual [True, True] (takeWhile (identity) [True, True, False, True]))

        , test "Drop while no change" (assertEqual [True, False, False, False, True] (dropWhile (not) [True, False, False, False, True]))
        , test "Drop while" (assertEqual [True] (dropWhile (not) [False, False, False, True]))

        , test "Model by rows" (assertEqual
            [ [1, 4, 7]
            , [2, 5, 8]
            , [3, 6, 9]
            ]
            (modelByCols [ [1, 2, 3]
                         , [4, 5, 6]
                         , [7, 8, 9]
                         ])
          )
        ]
