import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug

type alias Model = List (List Bool)
type alias Coordinate = (Int, Int)
type alias Hint = List Int
type Action = Toggle Coordinate | Noop

main = StartApp.Simple.start { model = initialModel, update = update, view = view }

-- MODEL

initialModel : Model
initialModel = let empty = List.repeat 25 (List.repeat 25 False)
               in  List.foldr toggleCoordinate empty initialCheckedBoxes

-- Model is [ Rows... ]
-- change to [ Cols... ]
modelByCols : Model -> Model
modelByCols m = case m of
  [] -> []
  _ -> if List.isEmpty (Maybe.withDefault [] ((List.head) m))
          then []
          else List.map (List.take 1) m ++ modelByCols (List.map (List.drop 1) m)

initialCheckedBoxes : List Coordinate
initialCheckedBoxes = [ (3, 3)
                      , (4, 3)
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

-- UPDATE

update : Action -> Model -> Model
update action model = case action of
    Toggle c -> Debug.log "" (toggleCoordinate (Debug.watch "Coordinate change" c) model)
    Noop     -> model

toggleCoordinate : Coordinate -> Model -> Model
toggleCoordinate (x, y) model = List.take y model ++ toggleCol x (List.head (List.drop y model)) :: List.drop (y + 1) model

toggleCol : Int -> Maybe (List Bool) -> List Bool
toggleCol col row = case row of
  Just r -> List.map (\(i, bool) -> if i == col then not bool else bool) (zip [0..24] r)
  _ -> []


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = let hintRow = tr [] (td [] [] :: List.map drawHintY (zip hintsY (modelByCols model)))
                     in div [] (hintRow :: (drawRow address (zip3 [0..(List.length model)] hintsY model)))


drawRow : Signal.Address Action -> List (Int, Hint, List Bool) -> List Html
drawRow address l = case l of
    []                 -> []
    (i, h, cs) :: rest -> tr [] (drawHintX (h, cs) :: List.map (drawCb address i) (zip [0..(List.length cs)] cs)) :: drawRow address rest


drawHintY : (Hint, List Bool) -> Html
drawHintY (hs, checks) = td [style [("verticalAlign", "bottom"), ("textAlign", "center")]] (List.map (\h -> div [] [text (toString h)]) hs)

drawHintX : (Hint, List Bool) -> Html
drawHintX (hs, checks) = td [style [("textAlign", "right")]] (List.map (\h -> span [] [text (toString h)]) hs)


drawCb : Signal.Address Action -> Int -> (Int, Bool) -> Html
drawCb address y (x, check) =
    let cb = if List.member (x, y) initialCheckedBoxes
                then input [ type' "checkbox"
                           , checked check
                           , disabled True
                           , onClick address Noop
                           ]
                           []

                else input [ type' "checkbox"
                           , checked check
                           , onClick address (Toggle (x, y))
                           ]
                           []
    in td [] [cb]


zip : List a -> List b -> List (a, b)
zip xs ys = case (xs, ys) of
  (x :: xs', y :: ys') -> (x, y) :: zip xs' ys'
  _ -> []

zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 xs ys zs = case (xs, ys, zs) of
  (x :: xs', y :: ys', z :: zs') -> (x, y, z) :: zip3 xs' ys' zs'
  _ -> []


-- vi: set et shiftwidth=4 tabstop=4 softtabstop=4
