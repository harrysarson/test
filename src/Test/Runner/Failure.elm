module Test.Runner.Failure exposing (Reason(..), InvalidReason(..))

{-| The reason a test failed.

@docs Reason, InvalidReason

-}


{-| The reason a test failed.

Test runners can use this to provide nice output, e.g. by doing diffs on the
two parts of an `Expect.equal` failure.

-}
type Reason
    = Custom
    | Equality String String
    | Comparison String String
      -- Expected, actual, (index of problem, expected element, actual element)
    | ListDiff (List String) (List String)
      {- I don't think we need to show the diff twice with + and - reversed. Just show it after the main vertical bar.
         "Extra" and "missing" are relative to the actual value.
      -}
    | CollectionDiff
        { expected : String
        , actual : String
        , extra : List String
        , missing : List String
        }
    | TODO
    | Invalid InvalidReason


{-| The reason a test run was invalid.

Test runners should report these to the user in whatever format is appropriate.

-}
type InvalidReason
    = EmptyList
    | NonpositiveFuzzCount
    | InvalidFuzzer
    | BadDescription
    | DuplicatedName


verticalBar : String -> String -> String -> String
verticalBar comparison expected actual =
    [ actual
    , "╵"
    , "│ " ++ comparison
    , "╷"
    , expected
    ]
        |> String.join "\n"


toStringLists : List String -> String
toStringLists =
    String.join ", "


listDiffToString :
    Int
    -> String
    -> { expected : List String, actual : List String }
    -> { originalExpected : List String, originalActual : List String }
    -> String
listDiffToString index description { expected, actual } originals =
    case ( expected, actual ) of
        ( [], [] ) ->
            [ "Two lists were unequal previously, yet ended up equal later."
            , "This should never happen!"
            , "Please report this bug to https://github.com/elm-community/elm-test/issues - and include these lists: "
            , "\n"
            , toStringLists originals.originalExpected
            , "\n"
            , toStringLists originals.originalActual
            ]
                |> String.join ""

        ( first :: _, [] ) ->
            verticalBar (description ++ " was shorter than")
                (toStringLists originals.originalExpected)
                (toStringLists originals.originalActual)

        ( [], first :: _ ) ->
            verticalBar (description ++ " was longer than")
                (toStringLists originals.originalExpected)
                (toStringLists originals.originalActual)

        ( firstExpected :: restExpected, firstActual :: restActual ) ->
            if firstExpected == firstActual then
                -- They're still the same so far; keep going.
                listDiffToString (index + 1)
                    description
                    { expected = restExpected
                    , actual = restActual
                    }
                    originals

            else
                -- We found elements that differ; fail!
                String.join ""
                    [ verticalBar description
                        (toStringLists originals.originalExpected)
                        (toStringLists originals.originalActual)
                    , "\n\nThe first diff is at index "
                    , String.fromInt index
                    , ": it was `"
                    , firstActual
                    , "`, but `"
                    , firstExpected
                    , "` was expected."
                    ]
