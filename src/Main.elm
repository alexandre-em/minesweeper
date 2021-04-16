module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, sub, text)
import Html.Attributes exposing (class, height, src)
import Html.Events exposing (onClick)
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
        indexGrid res n m (i - 1) (m - 1)

    else
        indexGrid (Empty ( i, j ) False :: res) n m i (j - 1)


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
            indexGrid [] height width (width - 1) (height - 1)
    in
    ( { grid = grid, height = height, width = width }, exampleGenerateRandomMines )


type Msg
    = MinesGenerated (List ( Int, Int ))
    | Click (Int, Int)


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

updateCase: Case -> (Int, Int)-> Case
updateCase c (x1, y1) =
    case c of
        Hint ( x, y ) hint _ ->
            if x==x1 && y==y1 then Hint ( x, y ) hint True else c
        Mine ( x, y ) _ -> if x==x1 && y==y1 then Mine ( x, y ) True else c
        Empty ( x, y ) _ -> if x==x1 && y==y1 then Empty ( x, y ) True else c
        Flag ( x, y ) _ -> if x==x1 && y==y1 then Flag ( x, y ) True else c


updateGrid: Model -> (Int, Int) -> ( Model, Cmd Msg )
updateGrid model (x, y) =
    let updated = { model | grid = List.map (\c -> updateCase c (x, y)) model.grid} in
    ( updated , Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MinesGenerated mines ->
            initializeGrid mines model

        Click (x, y) ->
            updateGrid model (x, y)


rowItem : Case -> Html Msg
rowItem e =
    case e of
        Empty (x, y) show ->
            if show == False then
                div [ class "item hide", onClick (Click (x, y)) ] []

            else
                div [ class "item" ]
                    []

        Mine (x, y) show ->
            if show == False then
                div [ class "item hide", onClick (Click (x, y)) ] []

            else
                div [ class "item" ]
                    [ text "x" ]

        Flag (x, y) show ->
            if show == False then
                div [ class "item hide", onClick (Click (x, y)) ] []

            else
                div [ class "item" ]
                    [ text "flag" ]

        Hint (x, y) value show ->
            if show == False then
                div [ class "item hide", onClick (Click (x, y)) ] []

            else
                div [ class "item" ]
                    [ text (String.fromInt value) ]


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ Html.h1 [] [ text "Demineur" ]
        , div [ class "grid-container" ]
            (List.map rowItem model.grid)
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
