import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug

main = StartApp.Simple.start { model = initialModel, update = update, view = view }

-- MODEL
type alias Model = List (List Bool)

initialModel : Model
initialModel = List.repeat 25 (List.repeat 25 False)


-- UPDATE
type alias Coordinate = (Int, Int)

type Action = Toggle Coordinate

update : Action -> Model -> Model
update action model = case action of
    Toggle c -> Debug.log "" (toggleCoordinate c model)

toggleCoordinate : Coordinate -> Model -> Model
toggleCoordinate (x, y) model = List.take y model ++ toggleCol x (List.head (List.drop y model)) :: List.drop (y + 1) model

toggleCol : Int -> Maybe (List Bool) -> List Bool
toggleCol col row = case row of
  Just r -> List.map (\(i, bool) -> if i == col then not bool else bool) (zip [0..25] r)
  _ -> []


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model = div [] (drawRows address (zip [0..25] model))

drawRows : Signal.Address Action -> List (Int, List Bool) -> List Html
drawRows address l = case l of
    [] -> []
    (i, cs) :: rest -> div [] (List.map (drawCb address i) (zip [0..25] cs)) :: drawRows address rest

drawCb : Signal.Address Action -> Int -> (Int, Bool) -> Html
drawCb address y (x, check) = input [ type' "checkbox"
                       , checked check
                       , onClick address (Toggle (x, y))
                       ]
                       []

zip : List a -> List b -> List (a, b)
zip xs ys = case (xs, ys) of
  (x :: xs', y :: ys') -> (x, y) :: zip xs' ys'
  (_, _)               -> []


-- vi: set et shiftwidth=4 tabstop=4 softtabstop=4
