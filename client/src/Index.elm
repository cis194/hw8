module Index exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url exposing (Url)



-- MODEL


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type alias Model =
    { linkTextInput : String
    , linkId : RemoteData Http.Error String
    , location : Url
    }


init : Url -> Key -> ( Model, Cmd msg )
init url _ =
    ( { linkTextInput = ""
      , linkId = NotAsked
      , location = url
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeLink String
    | SubmitLink
    | ReceivedLinkHash (Result Http.Error String)
    | ClickedLink UrlRequest
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLink newLink ->
            ( { model | linkTextInput = newLink }, Cmd.none )

        SubmitLink ->
            ( { model | linkId = Loading }
            , sendShortenRequest model.linkTextInput
            )

        ReceivedLinkHash (Ok linkId) ->
            ( { model | linkId = Success linkId, linkTextInput = "" }
            , Cmd.none
            )

        ReceivedLinkHash (Err httpError) ->
            ( { model | linkId = Failure httpError }
            , Cmd.none
            )

        ClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.load (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        DoNothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Element Msg
view model =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacing 10
        , Element.paddingEach { top = 0, right = 0, left = 0, bottom = 80 }
        ]
        [ Element.el
            [ Element.centerX
            , Font.size 26
            ]
            (Element.text "Link Shortener")
        , Element.row
            [ Element.spacing 5
            , Font.size 16
            ]
            [ Input.text [ Element.width (Element.px 300) ]
                { onChange = ChangeLink
                , text = model.linkTextInput
                , placeholder = Nothing
                , label = Input.labelHidden "Enter a link to shorten it"
                }
            , Input.button
                [ Background.color (Element.rgb 0.204 0.396 0.643)
                , Border.rounded 3
                , Element.padding 13
                , Font.color (Element.rgb 1 1 1)
                ]
                { onPress = Just SubmitLink
                , label = Element.text "Shorten!"
                }
            ]
        , viewShortenedLink model.linkId model.location
        ]



-- TODO maybe handle the error case better


viewShortenedLink : RemoteData Http.Error String -> Url -> Element msg
viewShortenedLink linkId location =
    case linkId of
        Success id_ ->
            Element.row []
                [ Element.text "Your shortened link is... "
                , Element.link [ Font.bold ]
                    { url = Url.toString location ++ id_
                    , label = Element.text (Url.toString location ++ id_)
                    }
                ]

        NotAsked ->
            Element.text "Your shortened link is... "

        Loading ->
            Element.row []
                [ Element.text "Your shortened link is... Loading"
                ]

        Failure httpError ->
            Element.text (httpErrorToEnglish httpError)


httpErrorToEnglish : Http.Error -> String
httpErrorToEnglish error =
    case error of
        Http.BadUrl url ->
            "I was expecting a valid URL, but I got the url: " ++ url

        Http.Timeout ->
            "It took too long to get a response from the server!"

        Http.NetworkError ->
            "Unable to make a connection. Is your network working?"

        Http.BadStatus code ->
            "The response gave me the error code: " ++ String.fromInt code

        Http.BadBody errorMessage ->
            "I failed because of the following error: " ++ errorMessage



-- HTTP


sendShortenRequest : String -> Cmd Msg
sendShortenRequest linkTextInput =
    Http.post
        { body = Http.stringBody "text/plain" linkTextInput
        , expect = Http.expectString ReceivedLinkHash
        , url = "/shorten"
        }



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = always init
        , view =
            \model ->
                { title = "Link Shortener"
                , body = [ Element.layout [] (view model) ]
                }
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = always DoNothing
        , onUrlRequest = ClickedLink
        }
