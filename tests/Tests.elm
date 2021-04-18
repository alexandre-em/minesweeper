module Tests exposing (..)

import Expect
import Test exposing (..)
import Main exposing (Case, indexGrid, initializeGrid, getElt, floodfill)
import Main exposing (Case(..))
import Main exposing (checkFill)
import Main exposing (checkCase)


-- example grid :
-----------------
-- || 0 1 x 1 ||
-- || 1 2 2 1 ||
-- || 1 x 1 0 ||
-- || 1 1 1 0 ||
-----------------
suite : Test
suite =
    let mines = [(0, 2), (2, 1)] in
            let grid = indexGrid [] 4 4 (4 - 1) (4 - 1) in
            let initresult = [(Empty (0,0) False False), (Hint (0,1) 1 False False), (Mine (0,2) False False), (Hint (0,3) 1 False False)
                                , (Hint (1,0) 1 False False), (Hint (1,1) 2 False False), (Hint (1,2) 2 False False), (Hint (1,3) 1 False False)
                                , (Hint (2,0) 1 False False), (Mine (2,1) False False), (Hint (2,2) 1 False False), (Empty (2,3) False False)
                                , (Hint (3,0) 1 False False), (Hint (3,1) 1 False False), (Hint (3,2) 1 False False), (Empty (3,3) False False)] in
            let checkgrid = (List.map (\val ->
                    case val of 
                        Mine (x, y) _ _ -> if x==0 && y==2 then Mine (x, y) True False else val 
                        _ -> val) initresult) in
    describe "A Test Suite"
        [ 
          test "Initialisation grid" <|
            \_ ->
                Expect.equal initresult (initializeGrid mines grid)
        , test "get Case from grid" <|
            \_ ->
                Expect.equal (Mine (2, 1) False False) (getElt initresult (2, 1))
        , test "check case visibility" <|
            \_ ->
                Expect.equal True (checkFill checkgrid (0, 2))
        , test "check case visibility2" <|
            \_ ->
                Expect.equal False (checkFill checkgrid (0, 0))
        , test "check case for floodfill" <|
            \_ -> 
                Expect.equal False (checkCase (Hint (3,0) 1 False False) (Empty (3,1) False False))
        , test "check case for floodfill2" <|
            \_ -> 
                Expect.equal True (checkCase (Empty (3,1) False False) (Empty (3,1) True False))
        , test "check case for floodfill3" <|
            \_ -> 
                Expect.equal True (checkCase (Empty (3,1) False False) (Hint (3,1) 1 False False))
        , test "Floodfill algorithm" <|
            \_ ->
                Expect.equal (Hint (1, 0) 1 True False) (getElt (floodfill initresult (0, 0)) (1, 0))
        , test "Floodfill algorithm2" <|
            \_ ->
                Expect.equal (Hint (0, 1) 1 True False) (getElt (floodfill initresult (0, 0)) (0, 1))
        , test "Floodfill algorithm3" <|
            \_ ->
                Expect.equal (Hint (1, 1) 2 True False) (getElt (floodfill initresult (1, 1)) (1, 1))
        ]
