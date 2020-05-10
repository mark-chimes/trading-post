module TestTime exposing (..)

import Expect exposing (Expectation)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "A Test Suite"
        [ test "calculateTimeOfNextCustomer1" <|
            \_ ->
                Expect.equal ( 510, 1 ) (Main.calculateTimeOfNextCustomer 481 480)
        , test "calculateTimeOfNextCustomer2" <|
            \_ ->
                Expect.equal ( 510, 1 ) (Main.calculateTimeOfNextCustomer 480 480)
        , test "calculateTimeOfNextCustomer3" <|
            \_ ->
                Expect.equal ( 540, 2 ) (Main.calculateTimeOfNextCustomer 539 480)
        , test "calculateTimeOfNextCustomer4" <|
            \_ ->
                Expect.equal ( 570, 3 ) (Main.calculateTimeOfNextCustomer 540 480)
        , test "calculateTimeOfNextCustomer5" <|
            \_ ->
                Expect.equal ( 540, 0 ) (Main.calculateTimeOfNextCustomer 510 540)
        , test "calculateTimeOfNextCustomer6" <|
            \_ ->
                Expect.equal ( 540, 0 ) (Main.calculateTimeOfNextCustomer 511 540)
        , test "hourOfDay1" <|
            \_ ->
                Expect.equal 0 <| Main.hourOfDay 0
        , test "hourOfDay2" <|
            \_ ->
                Expect.equal 1 <| Main.hourOfDay 60
        , test "hourOfDay3" <|
            \_ ->
                Expect.equal 9 <| Main.hourOfDay 540
        , test "hourOfDay4" <|
            \_ ->
                Expect.equal 0 <| Main.hourOfDay 1440
        , test "hourOfDay5" <|
            \_ ->
                Expect.equal 9 <| Main.hourOfDay 2000
        , test "dayOfYear1" <|
            \_ ->
                Expect.equal 0 <| Main.dayOfYear 540
        , test "dayOfYear2" <|
            \_ ->
                Expect.equal 1 <| Main.dayOfYear 2000
        ]
