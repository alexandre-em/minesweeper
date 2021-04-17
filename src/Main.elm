module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, sub, text)
import Html.Attributes exposing (class, height, style)
import Html.Events exposing (onClick)
import Mine
import Bootstrap.Alert as Alert
import Html.Events.Extra.Mouse exposing (onContextMenu)

width: Int
width = 8
height: Int
height = 8
type Case
    = Hint ( Int, Int ) Int Bool Bool
    | Mine ( Int, Int ) Bool Bool
    | Empty ( Int, Int ) Bool Bool


type alias Model =
    { grid : List Case, width : Int, height : Int, onGoing: Bool }


exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = (width-1)
        , height = (height-1)
        , minMines = 5
        , maxMines = 10
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated
indexGrid : List Case -> Int -> Int -> Int -> Int -> List Case
indexGrid res n m i j =
    if i < 0 then
        res

    else if j < 0 then
        indexGrid res n m (i - 1) (m - 1)

    else
        indexGrid (Empty ( i, j ) False False :: res) n m i (j - 1)


init : ( Model, Cmd Msg )
init =
    let
        grid =
            indexGrid [] height width (width - 1) (height - 1)
    in
    ( { grid = grid, height = height, width = width, onGoing = True }, exampleGenerateRandomMines )


type Msg
    = MinesGenerated (List ( Int, Int ))
    | Click (Int, Int)
    | Flag (Int, Int)
    | End


initCase : ( Int, Int ) -> ( Int, Int ) -> Int -> Case -> Bool -> Bool -> Case
initCase ( i, j ) ( x, y ) val defaut show flag =
    if (i == x) && (j == y) then
        Mine ( x, y ) show flag

    else if ((x + 1) == i || (x - 1) == i || x == i) && ((y - 1) == j || (y + 1) == j || y == j) then
        Hint ( i, j ) val show flag

    else
        defaut


initGrid : List Case -> ( Int, Int ) -> List Case
initGrid grid ( x, y ) =
    List.map
        (\casex ->
            case casex of
                Empty ( i, j ) show flag ->
                    initCase ( i, j ) ( x, y ) 1 casex show flag

                Hint ( i, j ) val show flag ->
                    initCase ( i, j ) ( x, y ) (val + 1) casex show flag

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

getElt grid (x, y) =
    let elt = List.filter (\val -> case val of
            Empty ( x1, y1 ) _ _ -> x1==x && y1==y
            Mine ( x1, y1 ) _ _ -> x1==x && y1==y
            Hint (x1, y1) _ _ _ -> x1==x && y1==y) grid in
    case List.head elt of
        Just a -> a
        Nothing -> Empty (x, y) False False --cas ou la case est hors grille donc vide

checkFill grid (x, y) =
    let elt = getElt grid (x, y) in
    case elt of
        Empty _ show _ -> show
        Mine _ show _ -> show
        Hint _ _ show _ -> show


checkCase case1 case2 =
    case (case1, case2) of
    (Empty _ _ _, Empty _ show2 _) -> False || show2
    (Hint _ _ _ _, Empty _ show _) -> False || show
    _ -> True

clickMineHint grid origin =
    case origin of
        Mine (x, y) _ _ -> List.map(\val -> case val of
            Mine (x1, y1) _ flag -> if x1==x && y1==y && (not flag) then Mine (x, y) True flag else val
            _ -> val) grid
        Hint (x, y) _ _ _ -> List.map(\val -> case val of
            Hint (x1, y1) hint _ flag -> if x1==x && y1==y && (not flag) then Hint (x, y) hint True flag else val
            _ -> val) grid
        _ -> grid

floodfillRec: List Case -> (Int, Int) -> Case -> Case -> List Case
floodfillRec grid (x, y) prev origin=
    if (x < 0) || (x >= height) || (y < 0) || (y >= width) then grid
    else if checkCase (getElt grid (x, y)) prev then clickMineHint grid origin
    else if checkFill grid (x, y) then grid
    else
        let grid1 =List.map (\val -> case val of
                Empty ( x1, y1 ) _ flag -> if x1==x && y1==y && (not flag) then Empty (x1, y1) True flag else val
                Mine ( x1, y1 ) _ flag -> if x1==x && y1==y && (not flag) then Mine (x1, y1) True flag else val
                Hint (x1, y1) ht _ flag -> if x1==x && y1==y && (not flag) then Hint (x1, y1) ht True flag else val) grid in
        let north = floodfillRec grid1 ((x+1), y) (getElt grid (x, y)) origin in
        let east = floodfillRec north ((x-1), y) (getElt grid (x, y)) origin in
        let south = floodfillRec east (x, (y+1)) (getElt grid (x, y)) origin in
        floodfillRec south (x, (y-1)) (getElt grid (x, y)) origin

floodfill: List Case -> (Int, Int) -> List Case
floodfill grid (x, y)=
    let prev = getElt grid (x, y) in
    floodfillRec grid (x,y) prev prev

updateGrid: Model -> (Int, Int) -> ( Model, Cmd Msg )
updateGrid model (x, y) =
    let updated = { model | grid = floodfill model.grid (x, y) } in
    ( updated , Cmd.none )

endGame: Model -> ( Model, Cmd Msg )
endGame model = 
    let newm = { model | grid = List.map(\val -> case val of
            Mine coord _ flag -> Mine coord True flag
            _ -> val) model.grid , onGoing = False } in
    ( newm, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MinesGenerated mines ->
            initializeGrid mines model

        Click (x, y) ->
            if model.onGoing then
                updateGrid model (x, y)
            else
                ( model, Cmd.none )

        Flag (x, y) ->
            let newm = { model | grid = List.map (\val -> case val of
                    Empty (x1, y1) show flag -> if x1==x && y1==y then Empty (x1, y1) show (not flag) else val
                    Mine (x1, y1) show flag -> if x1==x && y1==y then Mine (x1, y1) show (not flag) else val
                    Hint (x1, y1) hint show flag -> if x1==x && y1==y then Hint (x1, y1) hint show (not flag) else val) model.grid } in
            ( newm, Cmd.none )

        End ->
            endGame model


rowItem : Case -> Html Msg
rowItem e =
    case e of
        Empty (x, y) show flag ->
            if show == False && flag == False then
                div [ class "item hide", onClick (Click (x, y)), onContextMenu (\e -> Flag (x, y)) ] []
            else if flag == True then
                div [ class "item flag" ]
                    [ text "🚩" ]
            else
                div [ class "item" ]
                    []

        Mine (x, y) show flag ->
            if show == False && flag == False then
                div [ class "item hide", onClick (End) ] []
            else if flag == True then
                div [ class "item flag" ]
                    [ text "🚩" ]
            else
                div [ class "item mine" ]
                    [ text "💣" ]

        Hint (x, y) value show flag ->
            if show == False then
                div [ class "item hide", onClick (Click (x, y)) ] []
            else if flag == True then
                div [ class "item flag" ]
                    [ text "🚩" ]
            else
                div [ class "item" ]
                    [ text (String.fromInt value) ]


view : Model -> Html Msg
view model =
    div [ class "main" ]
        
        [if model.onGoing == False then div [][ Alert.simpleDanger [] [ text "Game Over !" ] ] else div [][]
        , Html.h1 [ style "font-weight" "900"] [ text "Demineur" ]
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
