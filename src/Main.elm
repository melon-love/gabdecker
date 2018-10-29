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
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h2
        , h3
        , img
        , input
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , tr
        )
import Html.Attributes
    exposing
        ( accept
        , alt
        , checked
        , colspan
        , disabled
        , height
        , href
        , id
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (onClick, onInput)
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
    , scopes : List String
    , receivedScopes : List String
    , token : Maybe SavedToken
    , state : Maybe String
    , msg : Maybe String
    , request : Maybe (RequestParts JD.Value)
    , loggedInUser : Maybe String
    , replyType : String
    , replyThing : Thing
    , reply : Maybe Value
    , redirectBackUri : String
    , authorization : Maybe Authorization
    , tokenAuthorization : Maybe TokenAuthorization
    , prettify : Bool
    , username : String
    , userBefore : Int
    , userProfile : Maybe User
    , postUser : String
    , postGroup : String
    , postBefore : String
    , postId : String
    , postBody : String
    , post : Maybe Post
    , file : Maybe File
    , uploading : UploadingState
    , showRaw : Bool
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
    | Login
    | GetMe
    | GetUserProfile
    | GetUserFollowers
    | GetUserFollowing
    | GetPopularUsers
    | GetHomeFeed
    | GetUserFeed
    | GetGroupFeed
    | GetPopularFeed
    | DoOperation String String String Bool
    | ReceiveUser Bool (Result Http.Error Value)
    | ReceiveUserList (Result Http.Error Value)
    | ReceiveActivityLogList (Result Http.Error Value)
    | ReceiveValue (Result Http.Error Value)
    | ReceivePost (Result Http.Error Value)
    | ReceiveImageUpload (Result Http.Error Value)
    | AbortImageUpload
    | ReceivePosted (Result Http.Error Value)
    | ToggleScope String
    | SetUsername String
    | SetUserBefore String
    | SetPostBefore String
    | SetPostUser String
    | SetPostGroup String
    | SetPostId String
    | GetPost
    | SetPostBody String
    | SetFile File
    | NewPost
    | TogglePrettify
    | ShowHideDecoded
    | PersistResponseToken ResponseToken Posix
    | ProcessLocalStorage Value


{-| GitHub requires the "User-Agent" header.
-}
userAgentHeader : Http.Header
userAgentHeader =
    Http.header "User-Agent" "Xossbow"


type alias Api =
    { getUser : String
    }


apis : Dict String Api
apis =
    Dict.fromList
        [ ( "GitHub"
          , { getUser = "user"
            }
          )
        , ( "Gmail"
          , { getUser = "me/profile"
            }
          )
        , ( "Facebook"
          , { getUser = "me"
            }
          )
        , ( "Gab"
          , { getUser = "me" -- "users/{username}"
            }
          )
        ]


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
            , request = Nothing
            , loggedInUser = Nothing
            , replyType = "Token"
            , replyThing =
                ValueThing JE.null
            , reply = reply
            , redirectBackUri = locationToRedirectBackUri url
            , authorization = Nothing
            , scopes = scopes
            , receivedScopes = scopes
            , tokenAuthorization = Nothing
            , prettify = True
            , username = "xossbow"
            , userBefore = 0
            , userProfile = Nothing
            , postUser = "xossbow"
            , postGroup = "62c38c34-0559-4e2e-a382-98b1ba9acf18"
            , postBefore = ""
            , postId = ""
            , postBody = ""
            , post = Nothing
            , file = Nothing
            , uploading = NotUploading
            , showRaw = True
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


get : Model -> (Result Http.Error Value -> Msg) -> (OAuth.Token -> RequestParts Value) -> ( Model, Cmd Msg )
get model receiver makeRequest =
    case model.token of
        Nothing ->
            { model
                | msg = Just "You must login before getting logged-in user information."
            }
                |> withNoCmd

        Just token ->
            case model.authorization of
                Just auth ->
                    let
                        req =
                            makeRequest token.token
                    in
                    { model
                        | request = Just req
                        , replyThing = nullThing
                        , reply = Nothing
                    }
                        |> withCmd
                            (Http.send receiver <| Gab.request req)

                _ ->
                    { model | msg = Just "No authorization loaded." }
                        |> withNoCmd


getMe : Model -> ( Model, Cmd Msg )
getMe model =
    get model (ReceiveUser False) <|
        \token -> Gab.meParts JD.value token


getUserProfile : Model -> String -> ( Model, Cmd Msg )
getUserProfile model username =
    get model (ReceiveUser True) <|
        \token -> Gab.userProfileParts JD.value token username


getUserFollowers : Model -> String -> Int -> ( Model, Cmd Msg )
getUserFollowers model username before =
    get model ReceiveUserList <|
        \token -> Gab.userFollowersParts JD.value token username before


getUserFollowing : Model -> String -> Int -> ( Model, Cmd Msg )
getUserFollowing model username before =
    get model ReceiveUserList <|
        \token -> Gab.userFollowingParts JD.value token username before


getPopularUsers : Model -> ( Model, Cmd Msg )
getPopularUsers model =
    get model ReceiveUserList <|
        \token -> Gab.popularUsersParts JD.value token


getHomeFeed : Model -> String -> ( Model, Cmd Msg )
getHomeFeed model before =
    get model ReceiveActivityLogList <|
        \token -> Gab.homeFeedParts JD.value token before


getUserFeed : Model -> String -> String -> ( Model, Cmd Msg )
getUserFeed model user before =
    get model ReceiveActivityLogList <|
        \token -> Gab.userFeedParts JD.value token user before


getGroupFeed : Model -> String -> String -> ( Model, Cmd Msg )
getGroupFeed model group before =
    get model ReceiveActivityLogList <|
        \token -> Gab.groupFeedParts JD.value token group before


getPopularFeed : Model -> ( Model, Cmd Msg )
getPopularFeed model =
    get model ReceiveActivityLogList <|
        \token -> Gab.popularFeedParts JD.value token


{-| TODO: It would be good to refetch user or post after the operation is done.
-}
doOperation : String -> String -> String -> Bool -> Model -> ( Model, Cmd Msg )
doOperation prefix operation identifier undo model =
    let
        mod =
            case prefix of
                "users" ->
                    { model | userProfile = Nothing }

                "posts" ->
                    { model | post = Nothing }

                _ ->
                    model
    in
    get mod ReceiveValue <|
        \token -> Gab.doParts prefix operation JD.value token identifier undo


getPost : Model -> ( Model, Cmd Msg )
getPost model =
    get model ReceivePost <|
        \token -> Gab.getPostParts JD.value token model.postId


postFile : Model -> ( Model, Cmd Msg )
postFile model =
    case model.file of
        Just file ->
            get { model | uploading = Uploading } ReceiveImageUpload <|
                \token -> Gab.postImageParts JD.value token file

        _ ->
            -- Shouldn't happen
            model |> withNoCmd


newPost : Model -> ( Model, Cmd Msg )
newPost model =
    get { model | file = Nothing, uploading = NotUploading }
        ReceivePosted
    <|
        \token -> Gab.newPostParts JD.value token (makePostForm model)


makePostForm : Model -> PostForm
makePostForm model =
    let
        body =
            model.postBody
    in
    { body = body
    , reply_to = Nothing
    , is_quote = False
    , is_html = False
    , nsfw = False
    , is_premium = False
    , gif = Nothing
    , topic = Nothing
    , group = Nothing
    , media_attachments =
        case model.uploading of
            FinishedUploading id ->
                [ id ]

            _ ->
                []
    , premium_min_tier = Nothing
    , poll = False
    , poll_option_1 = Nothing
    , poll_option_2 = Nothing
    , poll_option_3 = Nothing
    , poll_option_4 = Nothing
    }


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

        ToggleScope scope ->
            let
                scopes =
                    if List.member scope model.scopes then
                        List.filter ((/=) scope) model.scopes

                    else
                        scope :: model.scopes
            in
            { model | scopes = scopes }
                |> withNoCmd

        SetUsername username ->
            { model
                | username = username
                , userProfile = Nothing
            }
                |> withNoCmd

        SetUserBefore before ->
            case String.toInt before of
                Nothing ->
                    if before == "" then
                        { model | userBefore = 0 }
                            |> withNoCmd

                    else
                        model |> withNoCmd

                Just a ->
                    { model | userBefore = a }
                        |> withNoCmd

        SetPostUser postUser ->
            { model | postUser = postUser }
                |> withNoCmd

        SetPostGroup postGroup ->
            { model | postGroup = postGroup }
                |> withNoCmd

        SetPostBefore before ->
            { model | postBefore = before }
                |> withNoCmd

        SetPostId id ->
            { model
                | postId = id
                , post = Nothing
            }
                |> withNoCmd

        SetPostBody postBody ->
            { model
                | postBody = postBody
                , post = Nothing
            }
                |> withNoCmd

        SetFile file ->
            postFile { model | file = Just file }

        TogglePrettify ->
            { model | prettify = not model.prettify }
                |> withNoCmd

        ShowHideDecoded ->
            { model | showRaw = not model.showRaw }
                |> withNoCmd

        ReceiveAuthorization result ->
            case result of
                Err err ->
                    { model | msg = Just <| Debug.toString err }
                        |> withNoCmd

                Ok authorization ->
                    let
                        ( replyType, reply, replyThing ) =
                            case ( model.reply, model.msg ) of
                                ( Nothing, Nothing ) ->
                                    ( "Authorization"
                                    , Just <|
                                        authorizationEncoder
                                            { authorization
                                                | clientId = "not telling"
                                                , redirectUri = "don't ask"
                                            }
                                    , ValueThing JE.null
                                    )

                                _ ->
                                    ( model.replyType
                                    , model.reply
                                    , model.replyThing
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
                            , request = Nothing
                            , replyType = replyType
                            , replyThing = replyThing
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

        Login ->
            case model.tokenAuthorization of
                Nothing ->
                    { model | msg = Just "No provider selected." }
                        |> withNoCmd

                Just authorization ->
                    model
                        |> withCmd
                            (case authorize { authorization | scope = model.scopes } of
                                Nothing ->
                                    Cmd.none

                                Just url ->
                                    Navigation.load <| Url.toString url
                            )

        GetMe ->
            getMe model

        GetUserProfile ->
            getUserProfile model model.username

        GetUserFollowers ->
            getUserFollowers model model.username model.userBefore

        GetUserFollowing ->
            getUserFollowing model model.username model.userBefore

        GetPopularUsers ->
            getPopularUsers model

        GetHomeFeed ->
            getHomeFeed model model.postBefore

        GetUserFeed ->
            getUserFeed model model.postUser model.postBefore

        GetGroupFeed ->
            getGroupFeed model model.postGroup model.postBefore

        GetPopularFeed ->
            getPopularFeed model

        DoOperation prefix operation identifier undo ->
            doOperation prefix operation identifier undo model

        GetPost ->
            getPost model

        NewPost ->
            newPost model

        ReceiveUser save result ->
            receiveUserThing save result model

        ReceiveUserList result ->
            receiveThing UserListThing result model

        ReceiveActivityLogList result ->
            receiveThing ActivityLogListThing result model

        ReceiveValue result ->
            receiveThing ValueThing result model

        ReceivePost result ->
            receivePost result model

        ReceivePosted result ->
            receiveThing PostedThing result model

        ReceiveImageUpload result ->
            receiveImageUpload result model

        AbortImageUpload ->
            { model | file = Nothing, uploading = NotUploading }
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


receiveUserThing : Bool -> Result Http.Error Value -> Model -> ( Model, Cmd Msg )
receiveUserThing save result model =
    let
        mod =
            if not save then
                model

            else
                case result of
                    Err _ ->
                        model

                    Ok value ->
                        case JD.decodeValue ED.userDecoder value of
                            Err _ ->
                                model

                            Ok user ->
                                { model | userProfile = Just user }
    in
    receiveThing UserThing result mod


receivePost : Result Http.Error Value -> Model -> ( Model, Cmd Msg )
receivePost result model =
    let
        mod =
            case result of
                Err _ ->
                    model

                Ok value ->
                    case JD.decodeValue ED.postDecoder value of
                        Err _ ->
                            model

                        Ok post ->
                            { model | post = Just post }
    in
    receiveThing PostThing result mod


receiveImageUpload : Result Http.Error Value -> Model -> ( Model, Cmd Msg )
receiveImageUpload result model =
    let
        mod =
            case result of
                Err err ->
                    { model
                        | uploading =
                            ErrorUploading <| Debug.toString err
                    }

                Ok value ->
                    case JD.decodeValue ED.mediaIdDecoder value of
                        Err err ->
                            { model
                                | uploading =
                                    ErrorUploading <| Debug.toString err
                            }

                        Ok id ->
                            case model.uploading of
                                Uploading ->
                                    { model
                                        | uploading = FinishedUploading id
                                    }

                                _ ->
                                    model
    in
    receiveThing ImageUploadThing result mod


receiveThing : (Value -> Thing) -> Result Http.Error Value -> Model -> ( Model, Cmd Msg )
receiveThing thingTag result model =
    case result of
        Err err ->
            { model
                | reply = Nothing
                , msg = Just <| Debug.toString err
            }
                |> withNoCmd

        Ok reply ->
            { model
                | replyType = "API Response"
                , reply = Just reply
                , replyThing = thingTag reply
                , msg = Nothing
            }
                |> withNoCmd


doDecodeEncode : Decoder a -> (a -> Value) -> Value -> Value
doDecodeEncode decoder encoder value =
    case JD.decodeValue decoder value of
        Err msg ->
            JE.string <| JD.errorToString msg

        Ok thing ->
            encoder thing


decodeEncode : Thing -> Value
decodeEncode thing =
    case thing of
        ValueThing value ->
            value

        UserThing value ->
            doDecodeEncode ED.userDecoder ED.userEncoder value

        UserListThing value ->
            doDecodeEncode ED.userListDecoder ED.userListEncoder value

        ActivityLogListThing value ->
            doDecodeEncode ED.activityLogListDecoder ED.activityLogListEncoder value

        PostThing value ->
            doDecodeEncode ED.postDecoder ED.postEncoder value

        PostedThing value ->
            doDecodeEncode ED.activityLogDecoder ED.activityLogEncoder value

        ImageUploadThing value ->
            doDecodeEncode ED.mediaIdDecoder ED.mediaIdEncoder value


link : String -> String -> Html Msg
link txt url =
    a
        [ href url
        , target "_blank"
        ]
        [ text txt ]


maybeLink : String -> Maybe String -> Html Msg
maybeLink txt url =
    case url of
        Nothing ->
            text txt

        Just u ->
            link txt u


userUrl : Maybe String -> Maybe String
userUrl user =
    case user of
        Nothing ->
            Nothing

        Just u ->
            Just <| "https://gab.ai/" ++ u


loggedInUserUrl : Model -> Maybe String
loggedInUserUrl model =
    userUrl model.loggedInUser


usernameUrl : Model -> Maybe String
usernameUrl model =
    userUrl <|
        if model.username == "" then
            Nothing

        else
            Just model.username


postUserUrl : Model -> Maybe String
postUserUrl model =
    userUrl <|
        if model.postUser == "" then
            Nothing

        else
            Just model.postUser


groupUrl : Maybe String -> Maybe String
groupUrl group =
    case group of
        Nothing ->
            Nothing

        Just g ->
            Just <| "https://gab.ai/groups/" ++ g


postGroupUrl : Model -> Maybe String
postGroupUrl model =
    groupUrl <|
        if model.postGroup == "" then
            Nothing

        else
            Just model.postGroup


isScopeSelected : String -> Model -> Bool
isScopeSelected scope model =
    List.filter ((==) scope) model.scopes
        |> List.isEmpty
        |> not


scopeCheckbox : ( String, String ) -> Model -> Html Msg
scopeCheckbox ( label, value ) model =
    span []
        [ input
            [ type_ "checkbox"
            , onClick <| ToggleScope value
            , checked <| isScopeSelected value model
            ]
            []
        , text " "
        , text label
        ]


scopeCheckboxes : Model -> Html Msg
scopeCheckboxes model =
    span [] <|
        List.concatMap
            (\scope ->
                [ scopeCheckbox scope model
                , text nbsp2
                ]
            )
            allScopes


pageTitle : String
pageTitle =
    "Gab API Explorer"


view : Model -> Document Msg
view model =
    { title = pageTitle
    , body = [ pageBody model ]
    }


pageBody : Model -> Html Msg
pageBody model =
    let
        engagePost =
            List.member "engage-post" model.receivedScopes

        engageUser =
            List.member "engage-user" model.receivedScopes
    in
    div
        [ style "margin-left" "3em"
        , style "width" "40em"
        ]
        [ div []
            [ h2 [] [ text pageTitle ]
            , p []
                [ scopeCheckboxes model
                , br
                , button [ onClick Login ]
                    [ text "Login" ]
                ]
            , if model.token == Nothing then
                text ""

              else
                p []
                    [ h3 [] [ text "User Information" ]
                    , table []
                        [ tr []
                            [ td []
                                [ b [ text "Logged-in User: " ]
                                , case model.loggedInUser of
                                    Nothing ->
                                        text ""

                                    Just userid ->
                                        maybeLink userid <| loggedInUserUrl model
                                ]
                            , td []
                                [ button [ onClick GetMe ]
                                    [ text "Get Profile" ]
                                ]
                            ]
                        , tr []
                            [ td []
                                [ b
                                    [ maybeLink "Username:" <| usernameUrl model
                                    , text " "
                                    ]
                                , input
                                    [ size 20
                                    , onInput SetUsername
                                    , value model.username
                                    ]
                                    []
                                ]
                            , td []
                                [ button
                                    [ onClick GetUserProfile
                                    , disabled <| model.username == ""
                                    , title "Fillin 'Username' and click"
                                    ]
                                    [ text "Get Profile" ]
                                ]
                            ]
                        , tr []
                            [ td []
                                [ b [ text <| nbsp2 ++ "before: " ]
                                , input
                                    [ size 3
                                    , onInput SetUserBefore
                                    , value <| String.fromInt model.userBefore
                                    ]
                                    []
                                ]
                            , td []
                                [ text nbsp2
                                , button
                                    [ onClick GetUserFollowers
                                    , disabled <| model.username == ""
                                    , title "Fillin 'Username' and click."
                                    ]
                                    [ text "Followers" ]
                                , text " "
                                , button
                                    [ onClick GetUserFollowing
                                    , disabled <| model.username == ""
                                    , title "Fillin 'Username' and click."
                                    ]
                                    [ text "Following" ]
                                ]
                            ]
                        , tr [] <|
                            let
                                ( ( isDisabled, theTitle, unfollow ), ( followText, unmute, muteText ) ) =
                                    case model.userProfile of
                                        Nothing ->
                                            ( ( True
                                              , "Click \"Get Profile\" to enable."
                                              , True
                                              )
                                            , ( "(Un)Follow"
                                              , True
                                              , "(Un)Mute"
                                              )
                                            )

                                        Just user ->
                                            let
                                                following =
                                                    user.following || user.follow_pending
                                            in
                                            ( ( False
                                              , ""
                                              , following
                                              )
                                            , ( if following then
                                                    "Unfollow"

                                                else
                                                    "Follow"
                                              , True
                                              , "Mute"
                                              )
                                            )
                            in
                            [ td [ colspan 2 ]
                                [ if engageUser then
                                    text ""

                                  else
                                    span []
                                        [ text nbsp2
                                        , text "(Since 'Engage User' scope is disabled, these will error)"
                                        , br
                                        ]
                                , text nbsp2
                                , button
                                    [ disabled isDisabled
                                    , title theTitle
                                    , onClick <|
                                        DoOperation "users" "follow" model.username unfollow
                                    ]
                                    [ text followText ]
                                , text " "

                                {--Mute doesn't current work
                                , button
                                    [ disabled isDisabled
                                    , title theTitle
                                    , onClick <|
                                        DoOperation "users" "mute" model.username False
                                    ]
                                    [ text muteText ]
                                --}
                                , if isDisabled then
                                    text ""

                                  else
                                    span []
                                        [ text " "
                                        , button
                                            [ onClick <|
                                                DoOperation "users" "mute" model.username True
                                            ]
                                            [ text "Unmute" ]
                                        ]
                                ]
                            ]
                        , tr []
                            [ td []
                                [ b [ text "Popular Users:" ] ]
                            , td []
                                [ button [ onClick GetPopularUsers ]
                                    [ text "Get List" ]
                                ]
                            ]
                        ]
                    , p []
                        [ h3 [] [ text "Posts" ]
                        , table []
                            [ tr []
                                [ td []
                                    [ b [ link "Popular Feed:" "https://gab.ai/popular" ] ]
                                , td []
                                    [ button [ onClick GetPopularFeed ]
                                        [ text "Get Posts" ]
                                    ]
                                ]
                            , tr []
                                [ td [ colspan 2 ]
                                    [ b [ text "Before: " ]
                                    , input
                                        [ size 40
                                        , onInput SetPostBefore
                                        , value model.postBefore
                                        ]
                                        []
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ b
                                        [ text nbsp2
                                        , link "Logged-in User:" "https://gab.ai/"
                                        ]
                                    ]
                                , td []
                                    [ button [ onClick GetHomeFeed ]
                                        [ text "Home Feed"
                                        ]
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ b
                                        [ text nbsp2
                                        , maybeLink "Username:" <| postUserUrl model
                                        , text " "
                                        ]
                                    , input
                                        [ size 20
                                        , onInput SetPostUser
                                        , value model.postUser
                                        ]
                                        []
                                    ]
                                , td []
                                    [ button
                                        [ onClick GetUserFeed
                                        , disabled <| model.postUser == ""
                                        , title "Fillin 'Username' and click."
                                        ]
                                        [ text "User Feed" ]
                                    ]
                                ]

                            {--Not yet
                            , tr []
                                [ td []
                                    [ b
                                        [ text nbsp2
                                        , maybeLink "Group ID:" <| postGroupUrl model
                                        , text " "
                                        ]
                                    , input
                                        [ size 38
                                        , onInput SetPostGroup
                                        , value model.postGroup
                                        ]
                                        []
                                    ]
                                , td []
                                    [ button
                                        [ onClick GetGroupFeed
                                        , disabled <| model.postGroup == ""
                                        , title "Fillin 'Group ID' and click."
                                        ]
                                        [ text "Group Feed" ]
                                    ]
                                ]
                            --}
                            , tr []
                                [ td []
                                    [ b
                                        [ text "Post Id: "
                                        ]
                                    , input
                                        [ size 38
                                        , onInput SetPostId
                                        , value model.postId
                                        ]
                                        []
                                    ]
                                , td []
                                    [ button
                                        [ disabled <| model.postId == ""
                                        , onClick GetPost
                                        , title "Fillin 'Post Id' and click."
                                        ]
                                        [ text "Get" ]
                                    ]
                                ]
                            , if engagePost then
                                text ""

                              else
                                tr []
                                    [ td [ colspan 2 ]
                                        [ text nbsp4
                                        , text "(Since 'Engage Post' scope is disabled, these will error)"
                                        ]
                                    ]
                            , tr [] <|
                                let
                                    ( ( isDisabled, theTitle, unlike ), ( likeText, undislike, dislikeText ), ( unrepost, repostText ) ) =
                                        case model.post of
                                            Nothing ->
                                                ( ( True
                                                  , "Click \"Get\" to enable."
                                                  , True
                                                  )
                                                , ( "(Un)Upvote"
                                                  , True
                                                  , "(Un)Downvote"
                                                  )
                                                , ( True
                                                  , "(Un)Repost"
                                                  )
                                                )

                                            Just post ->
                                                let
                                                    liked =
                                                        post.liked

                                                    disliked =
                                                        post.disliked

                                                    reposted =
                                                        post.repost
                                                in
                                                ( ( False
                                                  , ""
                                                  , liked
                                                  )
                                                , ( if liked then
                                                        "Unupvote"

                                                    else
                                                        "Upvote"
                                                  , disliked
                                                  , if disliked then
                                                        "Undownvote"

                                                    else
                                                        "Downvote"
                                                  )
                                                , ( reposted
                                                  , if reposted then
                                                        "Unrepost"

                                                    else
                                                        "Repost"
                                                  )
                                                )
                                in
                                [ td [ colspan 2 ]
                                    [ text nbsp4
                                    , button
                                        [ disabled isDisabled
                                        , title theTitle
                                        , onClick <|
                                            DoOperation "posts" "upvote" model.postId unlike
                                        ]
                                        [ text likeText ]
                                    , text " "
                                    , button
                                        [ disabled isDisabled
                                        , title theTitle
                                        , onClick <|
                                            DoOperation "posts" "downvote" model.postId undislike
                                        ]
                                        [ text dislikeText ]
                                    , text " "
                                    , button
                                        [ disabled isDisabled
                                        , title theTitle
                                        , onClick <|
                                            DoOperation "posts" "repost" model.postId unrepost
                                        ]
                                        [ text repostText ]
                                    ]
                                ]
                            ]
                        , tr []
                            [ td
                                [ colspan 2
                                , style "vertical-align" "top"
                                ]
                                [ b
                                    [ text "Post Body: "
                                    ]
                                , input
                                    [ size 38
                                    , onInput SetPostBody
                                    , value model.postBody
                                    ]
                                    []
                                , text " "
                                , button
                                    [ disabled <|
                                        (model.postBody == "")
                                            || (model.uploading == Uploading)
                                    , title <|
                                        if model.uploading == Uploading then
                                            "Wait for the image to upload"

                                        else
                                            ""
                                    , onClick NewPost
                                    , title "Fillin 'Post Body' and click."
                                    ]
                                    [ text "New Post" ]
                                , br
                                , File.fileInput "thefile"
                                    [ accept "image/*" ]
                                    [ File.onLoad SetFile ]
                                , br
                                , case model.uploading of
                                    NotUploading ->
                                        text ""

                                    Uploading ->
                                        text "Uploading..."

                                    FinishedUploading id ->
                                        text <| "Uploaded, id: " ++ id

                                    ErrorUploading err ->
                                        text <| "Error uploading: " ++ err
                                ]
                            , td []
                                [ case model.file of
                                    Nothing ->
                                        text ""

                                    Just file ->
                                        img
                                            [ src file.dataUrl
                                            , width 100
                                            , title <|
                                                if model.uploading == Uploading then
                                                    "Click to stop upload."

                                                else
                                                    ""
                                            , onClick AbortImageUpload
                                            ]
                                            []
                                ]
                            ]
                        ]
                    , p []
                        [ input
                            [ type_ "checkbox"
                            , onClick TogglePrettify
                            , checked model.prettify
                            ]
                            []
                        , b [ text " Prettify" ]
                        , text " (easier to read, may no longer be valid JSON)"
                        ]
                    ]
            , case model.request of
                Nothing ->
                    text ""

                Just req ->
                    span []
                        [ b [ text "Request:" ]
                        , br
                        , pre []
                            [ text <|
                                req.method
                                    ++ " "
                                    ++ req.url
                                    ++ "\n"
                                    ++ "\n"
                                    ++ (String.left 200 <|
                                            Gab.bodyToString 2 req.body
                                       )
                            ]
                        ]
            , case ( model.msg, model.reply, canHideRaw model ) of
                ( Just msg, _, _ ) ->
                    span []
                        [ b [ text "Error:" ]
                        , br
                        , pre [] [ text <| SE.softWrap 60 msg ]
                        ]

                ( _, Just reply, canHide ) ->
                    span []
                        [ b [ text <| model.replyType ++ ":" ]
                        , if canHide then
                            span []
                                [ text " "
                                , button
                                    [ onClick ShowHideDecoded
                                    , checked model.showRaw
                                    ]
                                    [ text <|
                                        if model.showRaw then
                                            " Hide"

                                        else
                                            " Show"
                                    ]
                                ]

                          else
                            text ""
                        , if not canHide || model.showRaw then
                            pre []
                                [ text <|
                                    encodeWrap model.prettify reply
                                ]

                          else
                            pre [] []
                        , if model.replyThing == nullThing then
                            text ""

                          else
                            span []
                                [ b [ text "Decoded and re-encoded:" ]
                                , pre []
                                    [ text <|
                                        encodeWrap
                                            model.prettify
                                            (decodeEncode model.replyThing)
                                    , br
                                    ]
                                ]
                        ]

                _ ->
                    p []
                        [ b [ text "Waiting for request..." ]
                        ]
            ]
        , footerDiv model
        ]


canHideRaw : Model -> Bool
canHideRaw model =
    model.reply /= Nothing && model.replyThing /= nullThing


convertJsonNewlines : String -> String
convertJsonNewlines json =
    String.replace "\\r" "" json
        |> String.replace "\\n" "\n"


wrapJsonLine : Int -> String -> List String
wrapJsonLine width line =
    let
        body =
            String.trimLeft line

        indentN =
            String.length line - String.length body + 2

        initialIndent =
            String.repeat (indentN - 2) " "

        indent =
            String.repeat indentN " "

        wrapped =
            convertJsonNewlines body
                |> String.split "\n"
                |> List.map (SE.softWrap <| max 20 (width - indentN))
                |> String.join "\n"

        lines =
            String.split "\n" wrapped
    in
    case lines of
        [] ->
            []

        first :: rest ->
            (initialIndent ++ first)
                :: List.map ((++) indent) rest


wrapJsonLines : Int -> String -> String
wrapJsonLines width string =
    String.split "\n" string
        |> List.concatMap (wrapJsonLine width)
        |> String.join "\n"


encodeWrap : Bool -> Value -> String
encodeWrap prettify value =
    JE.encode 2 value
        |> (if prettify then
                wrapJsonLines 80

            else
                identity
           )


b : List (Html a) -> Html a
b body =
    Html.b [] body


br : Html a
br =
    Html.br [] []


sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
    img
        [ src url
        , title name
        , alt name
        , width size
        , height size
        ]
        []


logoLink : String -> String -> String -> Int -> Html Msg
logoLink url img name size =
    a [ href url ]
        [ sqrimg ("images/" ++ img) name size ]


mailLink : String -> Html Msg
mailLink email =
    span []
        [ text "<"
        , a [ href ("mailto:" ++ email) ]
            [ text email ]
        , text ">"
        ]


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


nbsp : String
nbsp =
    -- \u00A0
    stringFromCode 160


nbsp2 : String
nbsp2 =
    nbsp ++ nbsp


nbsp4 : String
nbsp4 =
    nbsp2 ++ nbsp2


copyright : String
copyright =
    -- \u00A9
    stringFromCode 169


checkmark : String
checkmark =
    -- \u2714
    stringFromCode 10004


space : Html Msg
space =
    text " "


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
