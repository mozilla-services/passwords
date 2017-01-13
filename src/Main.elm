port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Random


-- Main


main : Program (List String) Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type Msg
    = Draw
    | NewWords (List Int)
    | NewPassphraseLength String


type alias Model =
    { passphrase : String
    , passphraseLength : Int
    , wordlist : List String
    , wordlistLength : Int
    }


init : List String -> ( Model, Cmd Msg )
init wordlist =
    let
        wordlistLength =
            List.length wordlist
    in
        ( Model "" 6 wordlist wordlistLength, drawWord 6 wordlistLength )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Draw ->
            ( model, drawWord model.passphraseLength model.wordlistLength )

        NewWords indices ->
            let
                newWords =
                    indices
                        |> List.map
                            (\index ->
                                model.wordlist
                                    |> List.drop (index - 1)
                                    |> List.head
                                    |> Maybe.withDefault ""
                            )
                        |> String.join " "
            in
                ( { model | passphrase = newWords }, Cmd.none )

        NewPassphraseLength lengthString ->
            let
                length =
                    String.toInt lengthString
                        |> Result.withDefault 6
                        |> clamp 1 30
            in
                ( { model | passphraseLength = length }
                , drawWord length model.wordlistLength
                )


drawWord : Int -> Int -> Cmd Msg
drawWord numWords size =
    Random.generate NewWords (Random.list numWords (Random.int 0 size))



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.label
            []
            [ Html.text "Passphrase length: "
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (toString model.passphraseLength)
                , Html.Attributes.min "1"
                , Html.Attributes.max "30"
                , Html.Events.onInput NewPassphraseLength
                ]
                []
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.value (toString model.passphraseLength)
                , Html.Attributes.min "1"
                , Html.Attributes.max "30"
                , Html.Events.onInput NewPassphraseLength
                ]
                []
            ]
        , Html.br [] []
        , Html.button
            [ Html.Events.onClick Draw ]
            [ Html.text "New passphrase" ]
        , Html.h2
            []
            [ Html.text model.passphrase ]
        ]
