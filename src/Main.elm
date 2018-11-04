----------------------------------------------------------------------
--
-- example.elm
-- Example of using the Gab API client.
-- Copyright (c) 2017-2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- Search for TODO to see remaining work.
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import CustomElement.FileListener as File exposing (File)
import Dict exposing (Dict)
import Element exposing (Element, column, el, paragraph, row, text, textColumn)
import Gab
import Gab.EncodeDecode as ED
import Gab.Types
    exposing
        ( ActivityLog
        , Post
        , PostForm
        , RequestParts
        , SavedToken
        , User
        )
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import OAuth exposing (Token(..))
import OAuthMiddleware
    exposing
        ( Authorization
        , ResponseToken
        , TokenAuthorization
        , TokenState(..)
        , authorize
        , getAuthorization
        , locationToRedirectBackUri
        , receiveTokenAndState
        , use
        )
import OAuthMiddleware.EncodeDecode
    exposing
        ( authorizationEncoder
        , responseTokenEncoder
        )
import PortFunnel.LocalStorage as LocalStorage
import PortFunnels exposing (FunnelDict, Handler(..))
import String
import String.Extra as SE
import Task
import Time exposing (Posix)
import Url exposing (Url)


type Thing
    = ValueThing Value
    | UserThing Value
    | UserListThing Value
    | ActivityLogListThing Value
    | PostThing Value
    | PostedThing Value
    | ImageUploadThing Value


nullThing : Thing
nullThing =
    ValueThing JE.null


allScopes : List ( String, String )
allScopes =
    [ ( "Read", "read" )
    , ( "Engage User", "engage-user" )
    , ( "Engage Post", "engage-post" )
    , ( "Post", "write-post" )
    , ( "Notifications", "notifications" )
    ]


type alias Model =
    { key : Key
    , funnelState : PortFunnels.State
    , token : Maybe SavedToken
    , state : Maybe String
    , msg : Maybe String
    , loggedInUser : Maybe String
    , replyType : String
    , reply : Maybe Value
    , redirectBackUri : String
    , authorization : Maybe Authorization
    , scopes : List String
    , receivedScopes : List String
    , tokenAuthorization : Maybe TokenAuthorization
    , username : String
    }


type UploadingState
    = NotUploading
    | Uploading
    | FinishedUploading String
    | ErrorUploading String


type Msg
    = HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | ReceiveAuthorization (Result Http.Error Authorization)
    | ReceiveLoggedInUser (Result Http.Error User)
    | PersistResponseToken ResponseToken Posix
    | ProcessLocalStorage Value


{-| GitHub requires the "User-Agent" header.
-}
userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "GabDecker"


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = PortFunnels.subscriptions ProcessLocalStorage
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


localStoragePrefix : String
localStoragePrefix =
    "gab-api-example"


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        tokenAndState =
            receiveTokenAndState url

        nono =
            ( Nothing, Nothing )

        ( ( token, savedToken ), state, msg ) =
            case tokenAndState of
                TokenAndState tok stat ->
                    let
                        st =
                            Gab.savedTokenFromResponseToken
                                (Time.millisToPosix 0)
                                tok
                    in
                    ( ( Just tok, Just st ), stat, Nothing )

                TokenErrorAndState m stat ->
                    ( nono, stat, Just m )

                TokenDecodeError m ->
                    ( nono, Nothing, Just m )

                NoToken ->
                    ( nono, Nothing, Nothing )

        ( reply, scopes ) =
            case token of
                Nothing ->
                    ( Nothing, [ "read" ] )

                Just tok ->
                    ( Just <| responseTokenEncoder tok
                    , tok.scope
                    )

        model =
            { key = key
            , funnelState = PortFunnels.initialState localStoragePrefix
            , token = savedToken
            , state = state
            , msg = msg
            , loggedInUser = Nothing
            , replyType = "Token"
            , reply = reply
            , redirectBackUri = locationToRedirectBackUri url
            , authorization = Nothing
            , scopes = scopes
            , receivedScopes = scopes
            , tokenAuthorization = Nothing
            , username = "xossbow"
            }
    in
    model
        |> withCmds
            [ Http.send ReceiveAuthorization <|
                getAuthorization False "authorization.json"
            , Navigation.replaceUrl key "#"
            , case token of
                Just t ->
                    Task.perform (PersistResponseToken t) Time.now

                Nothing ->
                    if tokenAndState == NoToken then
                        localStorageSend (LocalStorage.get tokenKey) model

                    else
                        Cmd.none
            ]


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    case response of
        LocalStorage.GetResponse { key, value } ->
            if key /= tokenKey then
                model |> withNoCmd

            else
                case value of
                    Nothing ->
                        model |> withNoCmd

                    Just v ->
                        case JD.decodeValue ED.savedTokenDecoder v of
                            Err err ->
                                { model | msg = Just <| JD.errorToString err }
                                    |> withNoCmd

                            Ok savedToken ->
                                ( { model
                                    | token = Just savedToken
                                    , scopes = savedToken.scope
                                    , receivedScopes = savedToken.scope
                                  }
                                , Http.send ReceiveLoggedInUser <|
                                    Gab.me savedToken.token
                                )

        _ ->
            model |> withNoCmd


{-| TODO: add checkboxes to UI to select scopes.
-}
lookupProvider : Model -> Model
lookupProvider model =
    case model.authorization of
        Nothing ->
            model

        Just auth ->
            { model
                | tokenAuthorization =
                    Just
                        { authorization = auth

                        -- This will be overridden by the user checkboxes
                        , scope = List.map Tuple.second <| Dict.toList auth.scopes
                        , state = Nothing
                        , redirectBackUri = model.redirectBackUri
                        }
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleUrlRequest request ->
            ( model
            , case request of
                Internal url ->
                    -- For now
                    Navigation.load <| Url.toString url

                External urlString ->
                    Navigation.load urlString
            )

        HandleUrlChange url ->
            model |> withNoCmd

        ReceiveAuthorization result ->
            case result of
                Err err ->
                    { model | msg = Just <| Debug.toString err }
                        |> withNoCmd

                Ok authorization ->
                    let
                        ( replyType, reply ) =
                            case ( model.reply, model.msg ) of
                                ( Nothing, Nothing ) ->
                                    ( "Authorization"
                                    , Just <|
                                        authorizationEncoder
                                            { authorization
                                                | clientId = "not telling"
                                                , redirectUri = "don't ask"
                                            }
                                    )

                                _ ->
                                    ( model.replyType
                                    , model.reply
                                    )
                    in
                    lookupProvider
                        { model
                            | authorization = Just authorization
                            , scopes =
                                if model.token == Nothing then
                                    List.map Tuple.second <| Dict.toList authorization.scopes

                                else
                                    model.scopes
                            , replyType = replyType
                            , reply = reply
                        }
                        |> withCmd
                            (case model.token of
                                Nothing ->
                                    Cmd.none

                                Just token ->
                                    Http.send ReceiveLoggedInUser <|
                                        Gab.me token.token
                            )

        ReceiveLoggedInUser result ->
            case result of
                Err _ ->
                    { model | msg = Just "Error getting logged-in user name." }
                        |> withNoCmd

                Ok user ->
                    { model | loggedInUser = Just user.username }
                        |> withNoCmd

        PersistResponseToken token time ->
            let
                value =
                    Gab.savedTokenFromResponseToken time token
                        |> ED.savedTokenEncoder
            in
            ( model
            , localStorageSend
                (LocalStorage.put tokenKey <| Just value)
                model
            )

        ProcessLocalStorage value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    { model | msg = Just error } |> withNoCmd

                Ok res ->
                    res


tokenKey : String
tokenKey =
    "token"


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict [ LocalStorageHandler storageHandler ] getCmdPort


getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort ProcessLocalStorage moduleName False


localStorageSend : LocalStorage.Message -> Model -> Cmd Msg
localStorageSend message model =
    LocalStorage.send (getCmdPort LocalStorage.moduleName model)
        message
        model.funnelState.storage


pageTitle : String
pageTitle =
    "GabDecker"


view : Model -> Document Msg
view model =
    { title = pageTitle
    , body = [ Element.layout [] <| pageBody model ]
    }


pageBody : Model -> Element Msg
pageBody model =
    text "Hello, GabDecker!"



{-

   footerDiv : Model -> Html Msg
   footerDiv model =
       div []
           [ text "API docs: "
           , a [ href "https://developers.gab.com" ]
               [ text "developers.gab.com" ]
           , br
           , text "Creating an app (Pro Users only): "
           , a [ href "https://gab.com/settings/clients" ]
               [ text "gab.com/settings/clients" ]
           , br
           , text "Andrew Torba's announcement "
           , a [ href "https://gab.com/gab/posts/37368168" ]
               [ text "on October 3, 2018" ]
           , br
           , br
           , text (copyright ++ " 2017-2018 ")
           , a [ href "https://lisplog.org/" ]
               [ text "Bill St. Clair" ]
           , space
           , mailLink "billstclair@gmail.com"
           , br
           , br
           , span [ style "margin-left" "5em" ]
               [ logoLink "https://github.com/melon-love/elm-gab-api"
                   "GitHub-Mark-32px.png"
                   "GitHub source code"
                   32
               , space
               , logoLink "http://elm-lang.org/"
                   "elm-logo-125x125.png"
                   "Elm inside"
                   28
               , space
               , logoLink "https://gabdecker.com/"
                   "frog-32x32.jpg"
                   "GabDecker"
                   31
               ]
           ]
-}
