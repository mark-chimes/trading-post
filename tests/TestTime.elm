module TestTime exposing (..)

import Expect exposing (Expectation)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "A Test Suite"
        [ test "calculateTimeOfNextCustomer1" <|
            \_ ->
                Expect.equal ( 540, 1 ) (Main.calculateTimeOfNextCustomer 481 480)
        , test "calculateTimeOfNextCustomer2" <|
            \_ ->
                Expect.equal ( 540, 1 ) (Main.calculateTimeOfNextCustomer 480 480)
        , test "calculateTimeOfNextCustomer3" <|
            \_ ->
                Expect.equal ( 540, 1 ) (Main.calculateTimeOfNextCustomer 539 480)
        , test "calculateTimeOfNextCustomer4" <|
            \_ ->
                Expect.equal ( 600, 2 ) (Main.calculateTimeOfNextCustomer 540 480)
        , test "calculateTimeOfNextCustomer5" <|
            \_ ->
                Expect.equal ( 540, 0 ) (Main.calculateTimeOfNextCustomer 480 540)
        , test "calculateTimeOfNextCustomer6" <|
            \_ ->
                Expect.equal ( 540, 0 ) (Main.calculateTimeOfNextCustomer 481 540)
        ]
