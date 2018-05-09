module Matrix
    exposing
        ( Matrix
        , empty
        , repeat
        , initialize
        , identity
        , height
        , width
        , size
        , get
        , map
        , map2
        , transpose
        , dot
        , toList
        , toLists
        , pretty
        )

{-| A simple linear algebra library using flat-arrays


# The Matrix type

@docs Matrix


# Creating matrices

@docs empty, repeat, initialize, identity


# Get matrix dimensions

@docs height, width, size


# Working with individual elements

@docs get


# Matrix manipulation

@docs map, map2, transpose, dot


# Matrix representation

@docs toList, toLists, pretty

-}

import Array.Hamt as Array exposing (Array)


{-| Representation of a matrix. You can create matrices of any type
but arithmetic operations in Matrix.Operations requires the matrices
to have numeric types.
-}
type Matrix a
    = Matrix
        { nrows : Int
        , ncols : Int
        , mvect : Array a
        }


{-| Create an empty matrix.

    size empty == (0, 0)

-}
empty : Matrix a
empty =
    Matrix
        { nrows = 0
        , ncols = 0
        , mvect = Array.empty
        }


{-| Create a matrix with a given size, filled with a default value.

    repeat 2 3 0 ~= [ [ 0, 0, 0 ], [ 0, 0, 0 ] ]

-}
repeat : Int -> Int -> a -> Matrix a
repeat nrows ncols value =
    Matrix
        { nrows = nrows
        , ncols = ncols
        , mvect = Array.repeat (nrows * ncols) value
        }


{-| Createsa matrix with a given size, with the elements at index `(i, j)` initialized to the result of `f (i, j)`.

    initialize 3 3 (\(i,j) -> if i == j then 1 else 0) == identity 3

-}
initialize : Int -> Int -> (( Int, Int ) -> a) -> Matrix a
initialize nrows ncols f =
    let
        f_ i =
            f (decode ncols i)

        data =
            Array.initialize (nrows * ncols) f_
    in
        Matrix
            { nrows = nrows
            , ncols = ncols
            , mvect = data
            }


{-| Create the identity matrix of dimension `n`.
-}
identity : Int -> Matrix number
identity n =
    let
        f ( i, j ) =
            if i == j then
                1
            else
                0
    in
        initialize n n f


{-| Return the number of rows in a given matrix.
-}
height : Matrix a -> Int
height (Matrix { nrows }) =
    nrows


{-| Return the number of columns in a given matrix.
-}
width : Matrix a -> Int
width (Matrix { ncols }) =
    ncols


{-| Return the dimensions of a given matrix in the form `(rows, columns)`.
-}
size : Matrix a -> ( Int, Int )
size m =
    ( height m, width m )


{-| Return `Just` the element at the index or `Nothing` if the index is out of bounds.
-}
get : Int -> Int -> Matrix a -> Maybe a
get i j (Matrix { ncols, mvect }) =
    Array.get (encode ncols ( i, j )) mvect


{-| Apply a function on every element of a matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f (Matrix m) =
    Matrix { m | mvect = Array.map f m.mvect }


{-| Apply a function between pairwise elements of two matrices.
If the matrices are of differents sizes, returns `Nothing`.
-}
map2 : (a -> b -> c) -> Matrix a -> Matrix b -> Maybe (Matrix c)
map2 f m1 m2 =
    if size m1 == size m2 then
        Just <|
            initialize (height m1) (width m1) <|
                \x -> f (m1 !! x) (m2 !! x)
    else
        Nothing


{-| Return the transpose of a matrix.
-}
transpose : Matrix a -> Matrix a
transpose m =
    initialize (width m) (height m) <| \( i, j ) -> m !! ( j, i )


{-| Perform the standard matrix multiplication.
If the dimensions of the matrices are incompatible, returns `Nothing`.
-}
dot : Matrix number -> Matrix number -> Maybe (Matrix number)
dot m1 m2 =
    let
        n =
            width m1

        m =
            height m2
    in
        if n == m then
            Just <| multStd m1 m2
        else
            Nothing


{-| Convert the matrix to a flat list.

    toList (identity 3) == [1,0,0,0,1,0,0,0,1]

-}
toList : Matrix a -> List a
toList (Matrix { mvect }) =
    Array.toList mvect


{-| Convert the matrix to a list of lists.

    toLists (identity 3) = [ [1,0,0], [0,1,0], [0,0,1] ]

-}
toLists : Matrix a -> List (List a)
toLists m =
    List.range 1 (height m)
        |> List.concatMap
            (\i ->
                [ List.range 1 (width m)
                    |> List.concatMap (\j -> [ m !! ( i, j ) ])
                ]
            )


{-| Convert a matrix to a formatted string.

    pretty (identity 3) = """
        [ [ 1, 0, 0 ]
        , [ 0, 1, 0 ]
        , [ 0, 0, 1 ] ]
    """

-}
pretty : Matrix a -> String
pretty m =
    let
        list =
            toLists m
    in
        "[ " ++ prettyPrint list ++ " ]"



{- Utilities -}


encode : Int -> ( Int, Int ) -> Int
encode ncols ( i, j ) =
    (i - 1) * ncols + j - 1


decode : Int -> Int -> ( Int, Int )
decode ncols index =
    let
        q =
            index // ncols

        r =
            rem index ncols
    in
        ( q + 1, r + 1 )


unsafeGet : Int -> Int -> Matrix a -> a
unsafeGet i j m =
    case get i j m of
        Just v ->
            v

        Nothing ->
            Debug.crash ""


(!!) : Matrix a -> ( Int, Int ) -> a
(!!) m ( i, j ) =
    unsafeGet i j m


multStd : Matrix number -> Matrix number -> Matrix number
multStd m1 m2 =
    let
        ( n, m ) =
            size m1

        ( _, m_ ) =
            size m2

        f ( i, j ) =
            List.foldr (+) 0 <| List.map (\k -> (m1 !! ( i, k )) * (m2 !! ( k, j ))) (List.range 1 m)
    in
        initialize n m_ f


prettyPrint : List (List a) -> String
prettyPrint list =
    case list of
        [] ->
            ""

        x :: [] ->
            "[ " ++ prettyList x ++ " ]"

        x :: xs ->
            "[ " ++ prettyList x ++ " ]\n, " ++ prettyPrint xs


prettyList : List a -> String
prettyList list =
    case list of
        [] ->
            ""

        x :: [] ->
            toString x

        x :: xs ->
            toString x ++ ", " ++ prettyList xs
