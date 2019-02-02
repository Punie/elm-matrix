module Matrix.Tests exposing (dot, fromList, fromLists, identity, initialize, int, map, map2, matrix, repeat, size, toLists, transpose)

import Expect
import Fuzz exposing (Fuzzer, custom, float, int, intRange, tuple, tuple3)
import Matrix exposing (Matrix)
import Random
import Shrink
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test, todo)


int : Fuzzer Int
int =
    intRange 0 100


size : Fuzzer ( Int, Int )
size =
    tuple ( intRange 0 100, intRange 0 100 )


matrix : Fuzzer (Matrix Int)
matrix =
    let
        generator =
            Random.map3 Matrix.repeat (Random.int 0 100) (Random.int 0 100) (Random.int -100 100)

        shrinker mat =
            Shrink.map Matrix.repeat (Shrink.int (Matrix.height mat))
                |> Shrink.andMap (Shrink.int (Matrix.width mat))
                |> Shrink.andMap (Shrink.int 0)
    in
    custom generator shrinker


repeat : Test
repeat =
    describe "Repeat"
        [ fuzz size "size of matrix" <|
            \( i, j ) ->
                Matrix.repeat i j 0
                    |> Matrix.size
                    |> Expect.equal ( i, j )
        , fuzz2 size int "contains the right elements." <|
            \( i, j ) v ->
                Matrix.repeat i j v
                    |> Matrix.toList
                    |> Expect.equalLists (List.repeat (i * j) v)
        ]


initialize : Test
initialize =
    describe "Matrix"
        [ test "empty matrix" <|
            \() ->
                Matrix.initialize 0 0 (\_ -> ())
                    |> Expect.equal Matrix.empty
        , test "repeat matrix" <|
            \() ->
                Matrix.initialize 3 3 (\_ -> 1)
                    |> Expect.equal (Matrix.repeat 3 3 1)
        ]


identity : Test
identity =
    describe "Identity"
        [ test "identity 0" <|
            \() ->
                Matrix.identity 0
                    |> Expect.equal Matrix.empty
        , fuzz int "identity n" <|
            \n ->
                Matrix.identity n
                    |> Matrix.size
                    |> Expect.equal ( n, n )
        ]


fromList : Test
fromList =
    describe "From List"
        [ test "empty list" <|
            \() ->
                Matrix.fromList 0 0 []
                    |> Expect.equal (Just Matrix.empty)
        , test "wrong dimensions with empty list" <|
            \() ->
                Matrix.fromList 1 2 []
                    |> Expect.equal Nothing
        , test "non-empty list with perfect dimensions" <|
            \() ->
                Matrix.fromList 2 3 [ 1, 1, 1, 1, 1, 1 ]
                    |> Expect.equal (Just <| Matrix.repeat 2 3 1)
        , test "non-empty list larger than needed" <|
            \() ->
                Matrix.fromList 2 2 [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
                    |> Expect.equal (Just <| Matrix.repeat 2 2 1)
        , test "toList >> fromList" <|
            \() ->
                Matrix.identity 3
                    |> Matrix.toList
                    |> Matrix.fromList 3 3
                    |> Expect.equal (Just <| Matrix.identity 3)
        ]


fromLists : Test
fromLists =
    describe "From Lists"
        [ test "empty list" <|
            \() ->
                Matrix.fromLists []
                    |> Expect.equal (Just Matrix.empty)
        , test "list of one empty list" <|
            \() ->
                Matrix.fromLists [ [] ]
                    |> Expect.equal (Just Matrix.empty)
        , test "list of empty lists" <|
            \() ->
                Matrix.fromLists [ [], [], [] ]
                    |> Expect.equal (Just Matrix.empty)
        , test "list of impossibly unmatched lists" <|
            \() ->
                Matrix.fromLists [ [ 1, 2 ], [ 1 ], [ 1, 2 ] ]
                    |> Expect.equal Nothing
        , test "list of possibly unmatched lists" <|
            \() ->
                Matrix.fromLists [ [ 1, 2 ], [ 1, 2, 3 ], [ 1, 2 ] ]
                    |> Expect.equal (Just <| Matrix.initialize 3 2 (\( i, j ) -> j))
        , test "toLists >> fromLists" <|
            \() ->
                let
                    m =
                        Matrix.initialize 7 12 (\( i, j ) -> i * 2 + 3 * j - 1)
                in
                m
                    |> Matrix.toLists
                    |> Matrix.fromLists
                    |> Expect.equal (Just m)
        ]


map : Test
map =
    describe "Map"
        [ fuzz2 size int "preserves the size" <|
            \( i, j ) v ->
                Matrix.repeat i j v
                    |> Matrix.map ((*) 6)
                    |> Matrix.size
                    |> Expect.equal ( i, j )
        , fuzz2 size float "applies the function" <|
            \( i, j ) v ->
                Matrix.repeat i j v
                    |> Matrix.map ((*) 10)
                    |> Expect.equal (Matrix.repeat i j (v * 10))
        ]


map2 : Test
map2 =
    describe "Map2"
        [ fuzz3 size int int "applies the function" <|
            \( i, j ) v1 v2 ->
                Matrix.map2 (+) (Matrix.repeat i j v1) (Matrix.repeat i j v2)
                    |> Expect.equal (Just <| Matrix.repeat i j (v1 + v2))
        , fuzz3 size size int "returns Nothing for incompatible sizes" <|
            \( i1, j1 ) ( i2, j2 ) v ->
                let
                    comparison =
                        if ( i1, j1 ) == ( i2, j2 ) then
                            Just <| Matrix.repeat i1 j1 (v + v)

                        else
                            Nothing
                in
                Matrix.map2 (+) (Matrix.repeat i1 j1 v) (Matrix.repeat i2 j2 v)
                    |> Expect.equal comparison
        ]


transpose : Test
transpose =
    describe "Transpose"
        [ fuzz int "doesn't change identity matrix" <|
            \n ->
                Matrix.identity n
                    |> Matrix.transpose
                    |> Expect.equal (Matrix.identity n)
        , fuzz2 size int "correctly swaps height and width" <|
            \( i, j ) v ->
                Matrix.repeat i j v
                    |> Matrix.transpose
                    |> Matrix.size
                    |> Expect.equal ( j, i )
        , test "correctly transposes the values" <|
            \() ->
                Matrix.initialize 2 3 (\( i, j ) -> 2 * i + 3 * j)
                    |> Matrix.transpose
                    |> Expect.equal (Matrix.initialize 3 2 (\( i, j ) -> 3 * i + 2 * j))
        ]


dot : Test
dot =
    describe "Dot Product"
        [ test "empty matrix" <|
            \() ->
                Matrix.dot Matrix.empty Matrix.empty
                    |> Expect.equal (Just Matrix.empty)
        , test "identity matrix (square)" <|
            \() ->
                let
                    m1 =
                        Matrix.repeat 5 5 42

                    m2 =
                        Matrix.identity 5
                in
                Matrix.dot m1 m2
                    |> Expect.equal (Just m1)
        , test "identity matrix (not square)" <|
            \() ->
                let
                    m1 =
                        Matrix.repeat 12 5 42

                    m2 =
                        Matrix.identity 5
                in
                Matrix.dot m1 m2
                    |> Expect.equal (Just m1)
        , test "vectors" <|
            \() ->
                let
                    v1 =
                        Matrix.repeat 1 12 1

                    v2 =
                        Matrix.repeat 12 1 10

                    result =
                        Matrix.repeat 1 1 120
                in
                Matrix.dot v1 v2
                    |> Expect.equal (Just result)
        , test "non-compatible dimensions" <|
            \() ->
                let
                    m1 =
                        Matrix.repeat 3 5 1

                    m2 =
                        Matrix.repeat 7 2 0
                in
                Matrix.dot m1 m2
                    |> Expect.equal Nothing
        ]


toLists : Test
toLists =
    describe "To Lists"
        [ test "empty matrix" <|
            \() ->
                Matrix.empty
                    |> Matrix.toLists
                    |> Expect.equalLists []
        , test "identity matrix" <|
            \() ->
                Matrix.identity 3
                    |> Matrix.toLists
                    |> Expect.equalLists
                        [ [ 1, 0, 0 ]
                        , [ 0, 1, 0 ]
                        , [ 0, 0, 1 ]
                        ]
        , test "non-square matrix" <|
            \() ->
                Matrix.initialize 3 5 (\( i, j ) -> remainderBy 10 ((i - 1) * 5 + j))
                    |> Matrix.toLists
                    |> Expect.equalLists
                        [ [ 1, 2, 3, 4, 5 ]
                        , [ 6, 7, 8, 9, 0 ]
                        , [ 1, 2, 3, 4, 5 ]
                        ]
        ]
