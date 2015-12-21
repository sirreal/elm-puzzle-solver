module Solver where

import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Set as Set exposing ( Set, fromList, foldr, member )

type alias Model = List (List Bool)
type alias Coordinate = (Int, Int)
type alias Hint = List Int
type Action = Toggle Coordinate | Noop

main = StartApp.Simple.start { model = initialModel, update = update, view = view }

-- MODEL

initialModel : Model
initialModel = let empty = List.repeat 25 (List.repeat 25 False)
               in  Set.foldr toggleCoordinate empty initialCheckedBoxes

-- Model is [ Rows... ]
-- change to [ Cols... ]
modelByCols : List (List a) -> List (List a)
modelByCols m = case List.head (Maybe.withDefault [] (List.head m)) of
  Nothing -> []
  _       -> List.foldr (++) [] (List.map (List.take 1) m) :: modelByCols (List.map (List.drop 1) m)

initialCheckedBoxes : Set.Set Coordinate
initialCheckedBoxes = Set.fromList
  [ (3, 3)
  , (4, 3)
  , (12, 3)
  , (13, 3)
  , (21, 3)

  , (6, 8)
  , (7, 8)
  , (10, 8)
  , (14, 8)
  , (15, 8)
  , (18, 8)

  , (6, 16)
  , (11, 16)
  , (16, 16)
  , (20, 16)

  , (3, 21)
  , (4, 21)
  , (9, 21)
  , (10, 21)
  , (15, 21)
  , (20, 21)
  , (21, 21)
  ]

hintsY : List Hint
hintsY =
  [ [7, 2, 1, 1, 7]
  , [1, 1, 2, 2, 1, 1]
  , [1, 3, 1, 3, 1, 3, 1, 3, 1]
  , [1, 3, 1, 1, 5, 1, 3, 1]
  , [1, 3, 1, 1, 4, 1, 3, 1]
  , [1, 1, 1, 2, 1, 1]
  , [7, 1, 1, 1, 1, 1, 7]
  , [1, 1, 3]
  , [2, 1, 2, 1, 8, 2, 1]
  , [2, 2, 1, 2, 1, 1, 1, 2]
  , [1, 7, 3, 2, 1]
  , [1, 2, 3, 1, 1, 1, 1, 1]
  , [4, 1, 1, 2, 6]
  , [3, 3, 1, 1, 1, 3, 1]
  , [1, 2, 5, 2, 2]
  , [2, 2, 1, 1, 1, 1, 1, 2, 1]
  , [1, 3, 3, 2, 1, 8, 1]
  , [6, 2, 1]
  , [7, 1, 4, 1, 1, 3]
  , [1, 1, 1, 1, 4]
  , [1, 3, 1, 3, 7, 1]
  , [1, 3, 1, 1, 1, 2, 1, 1, 4]
  , [1, 3, 1, 4, 3, 3]
  , [1, 1, 2, 2, 2, 6, 1]
  , [7, 1, 3, 2, 1, 1]
  ]

hintsX : List Hint
hintsX =
  [ [7, 3, 1, 1, 7]
  , [1, 1, 2, 2, 1, 1]
  , [1, 3, 1, 3, 1, 1, 3, 1]
  , [1, 3, 1, 1, 6, 1, 3, 1]
  , [1, 3, 1, 5, 2, 1, 3, 1]
  , [1, 1, 2, 1, 1]
  , [7, 1, 1, 1, 1, 1, 7]
  , [3, 3]
  , [1, 2, 3, 1, 1, 3, 1, 1, 2]
  , [1, 1, 3, 2, 1, 1]
  , [4, 1, 4, 2, 1, 2]
  , [1, 1, 1, 1, 1, 4, 1, 3]
  , [2, 1, 1, 1, 2, 5]
  , [3, 2, 2, 6, 3, 1]
  , [1, 9, 1, 1, 2, 1]
  , [2, 1, 2, 2, 3, 1]
  , [3, 1, 1, 1, 1, 5, 1]
  , [1, 2, 2, 5]
  , [7, 1, 2, 1, 1, 1, 3]
  , [1, 1, 2, 1, 2, 2, 1]
  , [1, 3, 1, 4, 5, 1]
  , [1, 3, 1, 3, 10, 2]
  , [1, 3, 1, 1, 6, 6]
  , [1, 1, 2, 1, 1, 2]
  , [7, 2, 1, 2, 5]
  ]
-- UPDATE

update : Action -> Model -> Model
update action model = case action of
    Toggle c -> toggleCoordinate c model
    Noop     -> model

toggleCoordinate : Coordinate -> Model -> Model
toggleCoordinate (x, y) model = List.take y model ++ toggleCol x (List.head (List.drop y model)) :: List.drop (y + 1) model

toggleCol : Int -> Maybe (List Bool) -> List Bool
toggleCol col row = case row of
  Just r -> List.map (\(i, bool) -> if i == col then not bool else bool) (zip [0..24] r)
  _ -> []


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = let hintRow = tr [] (td [] [] :: List.map (drawHints (div []) [("verticalAlign", "bottom"), ("textAlign", "center")]) (zip hintsY (modelByCols model)))
                     in div [] (hintRow :: (drawRow address (zip3 [0..(List.length model)] hintsX model)))


drawRow : Signal.Address Action -> List (Int, Hint, List Bool) -> List Html
drawRow address l = case l of
    []                 -> []
    (i, h, cs) :: rest -> tr [] (drawHints (span [style [("display", "inline-block"), ("padding", ".3em")]]) [("white-space", "nowrap"), ("textAlign", "right")] (h, cs) :: List.map (drawCb address i) (zip [0..(List.length cs)] cs)) :: drawRow address rest


drawHints : (List Html -> Html) -> List (String, String) -> (Hint, List Bool) -> Html
drawHints el tdStyle (hs, checks) = td [style tdStyle] (List.map (drawHint el) (zipLongest hs (rowGroups checks)))


drawHint : (List Html -> Html) -> (Int, Maybe Int) -> Html
drawHint el (h, h2) = case h2 of
  Nothing -> el [span [style [("color", "red")]] [text (toString h)]]
  Just x  -> if x == h
                then el [text (toString h)]
                else el [span [style [("color", "red")]] [text (toString h)]]

rowGroups : List Bool -> List Int
rowGroups row = case row of
  [] -> []
  bools -> if (List.head bools == Just True)
              then List.length (takeWhile identity bools) :: rowGroups (dropWhile identity bools)
              else rowGroups (dropWhile not bools)


drawCb : Signal.Address Action -> Int -> (Int, Bool) -> Html
drawCb address y (x, check) =
    let isMember = Set.member (x, y) initialCheckedBoxes
        action = if isMember
                    then Noop
                    else Toggle (x, y)
        borderColor = if isMember
                         then "darkgray"
                         else "black"
        bgColor = if check
                     then "black"
                     else "white"
    in td [ onClick address action
          , style [ ("width", "30px")
                  , ("height", "30px")
                  , ("border", "1px solid " ++ borderColor)
                  , ("background-color", bgColor)
                  ]
          ]
          []


zip : List a -> List b -> List (a, b)
zip xs ys = case (xs, ys) of
    (x :: xs', y :: ys') -> (x, y) :: zip xs' ys'
    _                    -> []

zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 xs ys zs = case (xs, ys, zs) of
    (x :: xs', y :: ys', z :: zs') -> (x, y, z) :: zip3 xs' ys' zs'
    _                              -> []

zipLongest : List a -> List b -> List (a, Maybe b)
zipLongest xs ys = case (xs, ys) of
    (x :: xs', y :: ys') -> (x, Just y) :: zipLongest xs' ys'
    (x :: xs', [])       -> (x, Nothing) :: zipLongest xs' []
    _                    -> []


takeWhile : (a -> Bool) -> (List a) -> (List a)
takeWhile predicate list =
  case list of
    []      -> []
    x :: xs -> if predicate x
                  then x :: takeWhile predicate xs
                  else []

dropWhile : (a -> Bool) -> (List a) -> (List a)
dropWhile predicate list =
  case list of
    []      -> []
    x :: xs -> if predicate x
                  then dropWhile predicate xs
                  else list

-- vi: set et shiftwidth=4 tabstop=4 softtabstop=4
