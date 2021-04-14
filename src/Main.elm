module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, sub, text)
import Html.Attributes exposing (height, src)
import Mine


type Case
    = Hint ( Int, Int ) Int
    | Mine ( Int, Int )
    | Empty ( Int, Int )


type alias Model =
    { grid : List (List Case), width : Int, height : Int }


exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = 100
        , height = 100
        , minMines = 10
        , maxMines = 30
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

indexCol: List Case -> Int -> Int -> List Case
indexCol res i m =
    if m > 0 then
        indexCol (Empty ( i, (m-1) ) :: res) i (m - 1)

    else
        res

indexGrid: List (List Case) -> Int -> Int -> List (List Case)
indexGrid res n m =
    if n > 0 then
        indexGrid (indexCol [] (n-1) 100 :: res) (n-1) m

    else
        res


init : ( Model, Cmd Msg )
init =
    let
        height =
            100
    in
    let
        width =
            100
    in
    let
        grid =
            indexGrid [] height width
    in
    ( { grid = grid, height = height, width = width }, exampleGenerateRandomMines )


type Msg
    = MinesGenerated (List ( Int, Int ))

-- checkRow row coord =
--     case List.head row of
--         Nothing -> False
--         Just a -> case a of
--             Empty (x,_) -> 
--                case coord of
--                 (x1,_) -> if x == x1 then True else False
--             _ -> False

-- updateRow row coord =
--     case row of
--        Empty coord1 -> if coord == coord1 then (Mine coord)::


-- updateGrid grid coord =
--     case grid of
--         [] -> grid
--         h::t -> if (checkRow h coord) then grid else updateGrid t coord
                    
-- initializeGrid: List ( Int, Int ) -> Model -> ( Model, Cmd Msg )
-- initializeGrid mines model =
--     case mines of
--         [] -> (model , exampleGenerateRandomMines)
--         h::t -> ( { model | grid = model.grid } , exampleGenerateRandomMines)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- case msg of
    --     MinesGenerated mines ->
    --         initializeGrid(mines, model)
    --         _ ->
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , text "Implémentez le démineur !"
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
