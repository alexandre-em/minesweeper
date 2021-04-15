module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, sub, text)
import Html.Attributes exposing (height, src)
import Mine


type Case
    = Hint ( Int, Int ) Int Bool
    | Mine ( Int, Int ) Bool
    | Empty ( Int, Int ) Bool
    | Flag ( Int, Int ) Bool


type alias Model =
    { grid : List Case, width : Int, height : Int }


exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = 7
        , height = 7
        , minMines = 1
        , maxMines = 3
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated


indexCol : List Case -> Int -> Int -> List Case
indexCol res i m =
    if m > 0 then
        indexCol (Empty ( i, m - 1 ) False :: res) i (m - 1)

    else
        res


indexGrid : List Case -> Int -> Int -> Int -> Int -> List Case
indexGrid res n m i j =
    if i < 0 then
        res

    else if j < 0 then
        indexGrid res n m (i - 1) (m-1)

    else
        indexGrid ((Empty (i,j) False)::res) n m i (j - 1)


init : ( Model, Cmd Msg )
init =
    let
        height =
            8
    in
    let
        width =
            8
    in
    let
        grid =
            indexGrid [] height width (width-1) (height-1) 
    in
    ( { grid = grid, height = height, width = width }, exampleGenerateRandomMines )


type Msg
    = MinesGenerated (List ( Int, Int ))
    | Begin


initCase : ( Int, Int ) -> ( Int, Int ) -> Int -> Case -> Bool -> Case
initCase ( i, j ) ( x, y ) val defaut show =
    if (i == x) && (j == y) then
        Mine ( x, y ) show

    else if ((x + 1) == i || (x - 1) == i || x == i) && ((y - 1) == j || (y + 1) == j || y == j) then
        Hint ( i, j ) val show

    else
        defaut


initGrid : List Case -> ( Int, Int ) -> List Case
initGrid grid ( x, y ) =
    List.map
        (\casex ->
            case casex of
                Empty ( i, j ) show ->
                    initCase ( i, j ) ( x, y ) 1 casex show

                Hint ( i, j ) val show ->
                    initCase ( i, j ) ( x, y ) (val + 1) casex show

                _ ->
                    casex
        )
        grid


initializeGrid : List ( Int, Int ) -> Model -> ( Model, Cmd Msg )
initializeGrid mines model =
    case mines of
        [] ->
            ( model, Cmd.none )

        h :: t ->
            initializeGrid t { model | grid = initGrid model.grid h }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MinesGenerated mines ->
            initializeGrid mines model

        _ ->
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
