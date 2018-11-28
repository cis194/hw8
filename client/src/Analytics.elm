module Analytics exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key)
import Element exposing (Element)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)



-- MODEL


type alias LinkData =
    { longUrl : String
    , clicks : Int
    }


type alias Model =
    Result Decode.Error LinkData


init : Decode.Value -> ( Model, Cmd Never )
init flags =
    ( Decode.decodeValue decodeLinkData flags
    , Cmd.none
    )



-- VIEW


view : Model -> Element Never
view model =
    case model of
        Ok linkData ->
            Element.column
                [ Element.centerX, Element.centerY, Element.spacing 10 ]
                [ Element.row []
                    [ Element.text "Original URL: "
                    , Element.link []
                        { url = linkData.longUrl
                        , label = Element.text linkData.longUrl
                        }
                    ]
                , Element.text ("Clicks: " ++ String.fromInt linkData.clicks)
                ]

        Err error ->
            Element.el
                [ Element.centerX, Element.centerY ]
                (Element.text ("Error: " ++ Decode.errorToString error))



-- JSON


decodeLinkData : Decoder LinkData
decodeLinkData =
    Decode.map2 LinkData
        (Decode.field "longUrl" Decode.string)
        (Decode.field "clicks" Decode.int)



-- MAIN


main : Program Decode.Value Model Never
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "Analytics | Link Shortener"
                , body = [ Element.layout [] (view model) ]
                }
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = always Sub.none
        }
