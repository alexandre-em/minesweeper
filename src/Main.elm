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
        indexCol (Empty ( i, m - 1 ) :: res) i (m - 1)

    else
        res


indexGrid : List (List Case) -> Int -> Int -> List (List Case)
indexGrid res n m =
    if n > 0 then
        indexGrid (indexCol [] (n - 1) m :: res) (n - 1) m

    else
        res


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
            indexGrid [] height width
    in
    ( { grid = grid, height = height, width = width }, exampleGenerateRandomMines )


type Msg
    = MinesGenerated (List ( Int, Int ))
    | Begin

initCase (i, j) (x, y) val defaut =
    if (i == x) && (j == y) then
                                Mine ( x, y )

                            else if ((x + 1) == i || (x - 1) == i || x == i) && ((y - 1) == j || (y + 1) == j || y == j) then
                                Hint ( i, j ) val
                                else
                                    defaut



updateGrid : List (List Case) -> ( Int, Int ) -> List (List Case)
updateGrid grid ( x, y ) =
    List.map
        (\row ->
            List.map
                (\casex ->
                    case casex of
                        Empty ( i, j ) ->
                            initCase (i,j) (x,y) 1 casex

                        Hint ( i, j ) val ->
                            initCase (i,j) (x,y) (val+1) casex

                        _ ->
                            casex
                )
                row
        )
        grid


initializeGrid : List ( Int, Int ) -> Model -> ( Model, Cmd Msg )
initializeGrid mines model =
    case mines of
        [] ->
            ( model, Cmd.none )

        h :: t ->
            initializeGrid t { model | grid = updateGrid model.grid h }


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
