module Tests where

import ElmTest exposing (..)

import Solver exposing (..)


all : Test
all =
    suite "Row groups"
        [ test "Row groups" (assertEqual [1, 2, 3] (rowGroups [True, False, True, True, False, True, True, True]))
        , test "Row groups empty" (assertEqual [] (rowGroups []))
        , test "Row groups all false" (assertEqual [] (rowGroups [False, False, False]))
        , test "Row groups true end" (assertEqual [] (rowGroups [False, False, False, True]))
        ]
