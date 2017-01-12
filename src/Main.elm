port module Main exposing (..)

import Html
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
    | NewWord Int


type alias Model =
    { word : String
    , wordlist : List String
    , wordlistLength : Int
    }


init : List String -> ( Model, Cmd Msg )
init wordlist =
    let
        wordlistLength =
            List.length wordlist
    in
        ( Model "" wordlist wordlistLength, drawWord wordlistLength )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Draw ->
            ( model, drawWord model.wordlistLength )

        NewWord index ->
            let
                newWord =
                    model.wordlist
                        |> List.drop (index - 1)
                        |> List.head
                        |> Maybe.withDefault ""
            in
                ( { model | word = newWord }, Cmd.none )


drawWord : Int -> Cmd Msg
drawWord size =
    Random.generate NewWord (Random.int 0 size)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.button
            [ Html.Events.onClick Draw ]
            [ Html.text "New word" ]
        , Html.text <| "Word: " ++ model.word
        ]
