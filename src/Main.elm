----------------------------------------------------------------------
--
-- Main.elm
-- GabDecker top-level
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- Search for TODO to see remaining work.
-- Also see ../TODO.md
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import CustomElement.FileListener as File exposing (File)
import DateFormat as DF
import Dict exposing (Dict)
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , alignBottom
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , height
        , image
        , link
        , padding
        , paddingEach
        , paragraph
        , px
        , row
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Gab
import Gab.EncodeDecode as ED
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , Attachment(..)
        , MediaRecord
        , Notification
        , NotificationType(..)
        , NotificationsLog
        , Post
        , PostForm
        , RelatedPosts(..)
        , RequestParts
        , SavedToken
        , Success
        , User
        )
import GabDecker.Api as Api exposing (Backend(..))
import GabDecker.Authorization as Auth
import GabDecker.Elements
    exposing
        ( colors
        , heightImage
        , newTabLink
        , simpleImage
        , simpleLink
        , styleColors
        , styledLink
        , widthImage
        )
import GabDecker.EncodeDecode as ED
import GabDecker.Parsers as Parsers
import GabDecker.Types as Types
    exposing
        ( ApiError
        , Feed
        , FeedData(..)
        , FeedGetter(..)
        , FeedResult
        , FeedType(..)
        , GangedNotification
        , Icons
        , LogList
        )
import Html exposing (Html)
import Html.Attributes as Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Iso8601
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra as LE
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
import Set exposing (Set)
import String
import String.Extra as SE
import Task
import Time exposing (Posix, Zone)
import Url exposing (Url)


allScopes : List ( String, String )
allScopes =
    [ ( "Read", "read" )
    , ( "Engage User", "engage-user" )
    , ( "Engage Post", "engage-post" )
    , ( "Post", "write-post" )
    , ( "Notifications", "notifications" )
    ]


type DialogType
    = NoDialog
    | AddFeedDialog
    | ImageDialog String
    | SaveFeedsDialog
    | OperationErrorDialog String


type alias Settings =
    { columnWidth : Int
    , fontSize : Float
    , here : Zone
    , windowWidth : Int
    , windowHeight : Int
    }


defaultSettings : Settings
defaultSettings =
    { columnWidth = defaultColumnWidth
    , fontSize = defaultFontSize
    , here = Time.utc
    , windowWidth = 1260
    , windowHeight = 1024
    }


type alias Model =
    { showDialog : DialogType
    , dialogError : Maybe String
    , useSimulator : Bool
    , settings : Settings
    , backend : Maybe Backend
    , key : Key
    , funnelState : PortFunnels.State
    , token : Maybe SavedToken
    , state : Maybe String
    , msg : Maybe String
    , loggedInUser : Maybe String
    , replyType : String
    , reply : Maybe Value
    , redirectBackUri : String
    , scopes : List String
    , receivedScopes : List String
    , tokenAuthorization : Maybe TokenAuthorization
    , username : String
    , dialogInput : String
    , icons : Icons

    -- This is only used at startup. It's not accurate after that.
    , feedTypes : List FeedType
    , nextId : Int
    , feeds : List (Feed Msg)
    , lastFeeds : Dict String (Feed Msg)
    , loadingFeeds : Set String
    , lastClosedFeed : Maybe (Feed Msg)
    }


type UploadingState
    = NotUploading
    | Uploading
    | FinishedUploading String
    | ErrorUploading String


type Msg
    = Noop
    | CloseDialog
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | ReceiveLoggedInUser (Result Http.Error User)
    | PersistResponseToken ResponseToken Posix
    | ProcessLocalStorage Value
    | WindowResize Int Int
    | Here Zone
    | Login
    | LoadMore Bool (Feed Msg)
    | LoadAll
    | CloseFeed (Feed Msg)
    | MoveFeedLeft FeedType
    | MoveFeedRight FeedType
    | ShowImageDialog String
    | ScrollToFeed FeedType
    | ScrollToViewport String (Result Dom.Error Dom.Element)
    | ScrollToControl String Dom.Element (Result Dom.Error Dom.Element)
    | ScrollToContent Dom.Element Dom.Element (Result Dom.Error Dom.Element)
    | AddFeed
    | SaveFeedTypes
    | RestoreFeedTypes
    | DialogInput String
    | AddNewFeed FeedType
    | Upvote FeedType Post
    | Downvote FeedType Post
    | Repost FeedType Post
    | ReceiveOperation Operation (FeedResult Success)
    | ReceivePostFeed Bool FeedType (FeedResult ActivityLogList)
    | ReceiveNotificationsFeed Bool FeedType (FeedResult NotificationsLog)


type Operation
    = UpvoteOperation FeedType Int Bool
    | DownvoteOperation FeedType Int Bool
    | RepostOperation FeedType Int


lookupUserIconUrl : String -> Icons -> String
lookupUserIconUrl username icons =
    case Dict.get username icons.user of
        Just icon ->
            icon

        Nothing ->
            iconUrls.user


storeIcons : Icons -> Model -> Cmd Msg
storeIcons icons model =
    localStorageSend
        (LocalStorage.put storageKeys.icons
            (Just <| ED.encodeIcons icons)
        )
        model


updateUserIconUrl : String -> String -> Model -> ( Model, Bool )
updateUserIconUrl username url model =
    case
        LE.find
            (\feedType ->
                case feedType of
                    UserFeed name ->
                        username == name

                    _ ->
                        False
            )
            (List.map .feedType model.feeds)
    of
        Nothing ->
            ( model, False )

        Just _ ->
            let
                icons =
                    model.icons

                updateUserIcons () =
                    let
                        newIcons =
                            { icons
                                | user =
                                    Dict.insert username url icons.user
                            }
                    in
                    ( { model
                        | icons = newIcons
                      }
                    , True
                    )
            in
            case Dict.get username icons.user of
                Just u ->
                    if u == url then
                        ( model, False )

                    else
                        updateUserIcons ()

                Nothing ->
                    updateUserIcons ()


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
        , subscriptions = subscriptions
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PortFunnels.subscriptions ProcessLocalStorage model
        , Events.onResize WindowResize
        , Events.onKeyDown <| keyDownDecoder keycodes.escape CloseDialog
        ]


localStoragePrefix : String
localStoragePrefix =
    "gab-api-example"


initialFeeds : List FeedType
initialFeeds =
    [ HomeFeed, NotificationsFeed, PopularFeed, UserFeed "a" ]


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        opts =
            Debug.log "optimizations" optimizationsToBits

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
                    ( Nothing, List.map Tuple.second allScopes )

                Just tok ->
                    ( Just <| responseTokenEncoder tok
                    , tok.scope
                    )

        model =
            let
                useSimulator =
                    Auth.useSimulator

                redirectBackUri =
                    locationToRedirectBackUri url

                ( backend, authorization ) =
                    if useSimulator then
                        ( Just SimulatedBackend, Nothing )

                    else
                        case Auth.authorization redirectBackUri of
                            Err _ ->
                                ( Just SimulatedBackend, Nothing )

                            Ok auth ->
                                let
                                    be =
                                        case savedToken of
                                            Nothing ->
                                                Nothing

                                            Just st ->
                                                Just <| RealBackend st.token
                                in
                                ( be, Just auth )

                feeds =
                    feedTypesToFeeds Nothing
                        backend
                        initialFeeds
                        0
            in
            { showDialog = NoDialog
            , dialogError = Nothing
            , useSimulator = useSimulator
            , backend = backend
            , key = key
            , settings = defaultSettings
            , funnelState = PortFunnels.initialState localStoragePrefix
            , token = savedToken
            , state = state
            , msg = msg
            , loggedInUser = Nothing
            , replyType = "Token"
            , reply = reply
            , redirectBackUri = redirectBackUri
            , scopes = scopes
            , receivedScopes = scopes
            , tokenAuthorization = authorization
            , username = "xossbow"
            , dialogInput = ""
            , icons = Types.emptyIcons
            , feedTypes = initialFeeds
            , nextId = List.length feeds
            , feeds = feeds
            , lastFeeds = Dict.empty
            , loadingFeeds = Set.empty
            , lastClosedFeed = Nothing
            }
    in
    List.foldl setLoadingFeed model model.feeds
        |> withCmds
            [ Navigation.replaceUrl key "#"
            , case token of
                Just t ->
                    Task.perform (PersistResponseToken t) Time.now

                Nothing ->
                    Cmd.none
            , localStorageSend (LocalStorage.get storageKeys.feeds) model
            , localStorageSend (LocalStorage.get storageKeys.icons) model
            , Task.perform getViewport Dom.getViewport
            , Task.perform Here Time.here
            , case savedToken of
                Nothing ->
                    Cmd.none

                Just st ->
                    Http.send ReceiveLoggedInUser <|
                        Gab.me st.token
            , loadAllCmd
            ]


loadAllCmd : Cmd Msg
loadAllCmd =
    Task.perform (\_ -> LoadAll) <| Task.succeed ()


loadMoreCmd : Bool -> Feed Msg -> Cmd Msg
loadMoreCmd appendResult feed =
    Task.perform (LoadMore appendResult) <| Task.succeed feed


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


feedTypesToFeeds : Maybe String -> Maybe Backend -> List FeedType -> Int -> List (Feed Msg)
feedTypesToFeeds username maybeBackend feedTypes startId =
    case maybeBackend of
        Nothing ->
            []

        Just backend ->
            List.map2 (feedTypeToFeed username backend)
                feedTypes
            <|
                List.range startId (startId + List.length feedTypes - 1)


defaultColumnWidth : Int
defaultColumnWidth =
    350


feedTypeToFeed : Maybe String -> Backend -> FeedType -> Int -> Feed Msg
feedTypeToFeed username backend feedType id =
    let
        ft =
            if feedType == LoggedInUserFeed then
                case username of
                    Nothing ->
                        feedType

                    Just name ->
                        UserFeed name

            else
                feedType
    in
    { getter = Api.feedTypeToGetter ft backend
    , feedType = ft
    , feed =
        { data =
            case ft of
                NotificationsFeed ->
                    NotificationFeedData []

                _ ->
                    PostFeedData []
        , no_more = False
        }
    , error = Nothing
    , id = id
    }


userUrl : User -> String
userUrl user =
    "https://gab.com/" ++ user.username


postUrl : Post -> String
postUrl post =
    "https://gab.com/"
        ++ post.user.username
        ++ "/posts/"
        ++ String.fromInt post.id


feedTypeDescription : FeedType -> Float -> Icons -> Element Msg
feedTypeDescription feedType baseFontSize icons =
    let
        iconHeight =
            smallUserIconHeight baseFontSize

        feedRow url elements =
            styledLink True [] url (row [] elements)
    in
    case feedType of
        HomeFeed ->
            feedRow "https://gab.com/"
                [ text "Home "
                , heightImage iconUrls.home "Home" iconHeight
                ]

        UserFeed user ->
            feedRow ("https://gab.com/" ++ user)
                [ text user
                , text " "
                , heightImage (lookupUserIconUrl user icons) "frob" iconHeight
                ]

        -- Need to look up group name
        GroupFeed groupid ->
            text <| "Group: " ++ groupid

        -- Need to look up topic name
        TopicFeed topicid ->
            text <| "Topic: " ++ topicid

        PopularFeed ->
            feedRow "https://gab.com/popular"
                [ text "Popular "
                , heightImage iconUrls.popular "Popular" iconHeight
                ]

        NotificationsFeed ->
            feedRow "https://gab.com/notifications"
                [ text "Notifications "
                , heightImage iconUrls.notifications "Notifications" iconHeight
                ]

        _ ->
            text "Shouldn't happen"


receiveToken : Maybe Value -> Model -> ( Model, Cmd Msg )
receiveToken mv model =
    case mv of
        Nothing ->
            model |> withNoCmd

        Just value ->
            case JD.decodeValue ED.savedTokenDecoder value of
                Err err ->
                    { model | msg = Just <| JD.errorToString err }
                        |> withNoCmd

                Ok st ->
                    let
                        savedToken =
                            { st | token = mungeToken st.token }

                        backend =
                            Just <|
                                RealBackend savedToken.token

                        feeds =
                            feedTypesToFeeds model.loggedInUser
                                backend
                                model.feedTypes
                                0
                    in
                    List.foldl setLoadingFeed
                        { model
                            | token = Just savedToken
                            , scopes = savedToken.scope
                            , receivedScopes = savedToken.scope
                            , backend = backend
                            , nextId = List.length feeds
                            , feeds = feeds
                        }
                        feeds
                        |> withCmds
                            [ if backend == Nothing then
                                Cmd.none

                              else
                                Http.send ReceiveLoggedInUser <|
                                    Gab.me savedToken.token
                            , loadAllCmd
                            ]


restoreFeedTypes : List FeedType -> Model -> ( Model, Cmd Msg )
restoreFeedTypes feedTypes model =
    let
        icons =
            Types.emptyIcons
    in
    case model.backend of
        Nothing ->
            { model
                | feedTypes = feedTypes
                , feeds = []
                , icons = icons
            }
                |> withNoCmd

        backend ->
            let
                feeds =
                    feedTypesToFeeds model.loggedInUser
                        backend
                        feedTypes
                        0
            in
            { model
                | feeds = feeds
                , icons = icons
            }
                |> withCmds
                    [ loadAllCmd
                    , saveFeeds feeds model
                    , storeIcons icons model
                    ]


receiveFeedTypes : Maybe Value -> Model -> ( Model, Cmd Msg )
receiveFeedTypes value model =
    let
        cmd =
            case model.token of
                Nothing ->
                    localStorageSend (LocalStorage.get storageKeys.token) model

                _ ->
                    Cmd.none
    in
    case value of
        Nothing ->
            model |> withCmd cmd

        Just v ->
            case ED.decodeFeedTypes v of
                Err err ->
                    model |> withCmd cmd

                Ok feedTypes ->
                    let
                        ( feeds, cmd2 ) =
                            case model.backend of
                                Nothing ->
                                    ( model.feeds, Cmd.none )

                                backend ->
                                    ( feedTypesToFeeds model.loggedInUser
                                        backend
                                        feedTypes
                                        0
                                    , loadAllCmd
                                    )
                    in
                    { model
                        | feedTypes = feedTypes
                        , feeds = feeds
                    }
                        |> withCmds [ cmd, cmd2 ]


receiveIcons : Maybe Value -> Model -> ( Model, Cmd Msg )
receiveIcons value model =
    case value of
        Nothing ->
            model |> withNoCmd

        Just v ->
            case ED.decodeIcons v of
                Err _ ->
                    model |> withNoCmd

                Ok icons ->
                    { model | icons = icons } |> withNoCmd


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    case response of
        LocalStorage.GetResponse { key, value } ->
            if key == storageKeys.token && model.backend == Nothing then
                receiveToken value model

            else if key == storageKeys.feeds then
                receiveFeedTypes value model

            else if key == storageKeys.icons then
                receiveIcons value model

            else
                model |> withNoCmd

        _ ->
            model |> withNoCmd


enableMungeToken : Bool
enableMungeToken =
    False


{-| For testing bad token errors.

Set `enableMungeToken` to `False` for delivery.

-}
mungeToken : Token -> Token
mungeToken token =
    if enableMungeToken then
        OAuth.tokenToString token
            |> (\s -> s ++ "x")
            |> OAuth.tokenFromString
            |> (\mt ->
                    case mt of
                        Nothing ->
                            token

                        Just t ->
                            t
               )

    else
        token


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        CloseDialog ->
            { model
                | showDialog = NoDialog
                , dialogError = Nothing
            }
                |> withNoCmd

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

        ReceiveLoggedInUser result ->
            case result of
                Err err ->
                    let
                        ( mdl, cmd ) =
                            processReceivedError err model
                    in
                    { mdl
                        | msg = Just "Error getting logged-in user name."
                    }
                        |> withCmd cmd

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
                (LocalStorage.put storageKeys.token <| Just value)
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

        WindowResize w h ->
            let
                settings =
                    model.settings
            in
            { model
                | settings =
                    { settings | windowWidth = w, windowHeight = h }
            }
                |> withNoCmd

        Here zone ->
            let
                settings =
                    model.settings
            in
            { model | settings = { settings | here = zone } }
                |> withNoCmd

        Login ->
            case model.tokenAuthorization of
                Nothing ->
                    { model | msg = Just "Failure to parse authorization." }
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

        LoadMore appendResult feed ->
            ( setLoadingFeed feed model
            , loadMore appendResult feed
            )

        LoadAll ->
            loadAll model

        CloseFeed feed ->
            let
                feedType =
                    feed.feedType

                feeds =
                    List.filter (\f -> feedType /= f.feedType)
                        model.feeds
            in
            { model
                | feeds = feeds
                , lastClosedFeed = Just feed
            }
                |> withCmd (saveFeeds feeds model)

        MoveFeedLeft feedType ->
            moveFeedLeft feedType model

        ShowImageDialog url ->
            { model | showDialog = ImageDialog url } |> withNoCmd

        MoveFeedRight feedType ->
            moveFeedRight feedType model

        ScrollToFeed feedType ->
            case findFeed feedType model of
                Nothing ->
                    model |> withNoCmd

                Just feed ->
                    let
                        colid =
                            columnId feed.id

                        firstcolId =
                            case List.head model.feeds of
                                Nothing ->
                                    ""

                                Just f ->
                                    columnId f.id
                    in
                    model
                        |> withCmd
                            (Task.attempt (ScrollToViewport firstcolId) <|
                                Dom.getElement <|
                                    Debug.log "colid" colid
                            )

        ScrollToViewport firstcolId result ->
            case result of
                Err _ ->
                    model |> withNoCmd

                Ok feedElement ->
                    model
                        |> withCmd
                            (Task.attempt (ScrollToControl firstcolId feedElement) <|
                                Dom.getElement controlColumnId
                            )

        ScrollToControl firstcolId feedElement result ->
            case result of
                Err _ ->
                    model |> withNoCmd

                Ok controlElement ->
                    model
                        |> withCmd
                            (Task.attempt
                                (ScrollToContent feedElement controlElement)
                             <|
                                Dom.getElement <|
                                    firstcolId
                            )

        ScrollToContent feedElement controlElement result ->
            case result of
                Err _ ->
                    model |> withNoCmd

                Ok firstElement ->
                    let
                        fe =
                            feedElement

                        ctrle =
                            controlElement

                        frst =
                            firstElement

                        ctrlw =
                            controlElement.element.width + 8

                        fx =
                            feedElement.element.x

                        fy =
                            feedElement.element.y

                        fw =
                            feedElement.element.width

                        sw =
                            feedElement.scene.width

                        frstx =
                            firstElement.element.x

                        targetLeft =
                            ctrlw

                        targetRight =
                            sw - fw

                        pos =
                            fx - frstx

                        nullMsg =
                            \_ -> Noop
                    in
                    if fx >= targetLeft then
                        if fx <= targetRight then
                            model |> withNoCmd

                        else
                            let
                                scrollx =
                                    -- 4 gets white space on the right in Chrome,
                                    -- and perfect in Brave and Safari
                                    pos - targetRight + ctrlw + 4
                            in
                            model
                                |> withCmd
                                    (Task.attempt nullMsg <|
                                        Dom.setViewportOf contentId scrollx fy
                                    )

                    else
                        let
                            scrollx =
                                pos
                        in
                        model
                            |> withCmd
                                (Task.attempt nullMsg <|
                                    Dom.setViewportOf contentId scrollx fy
                                )

        AddFeed ->
            { model
                | showDialog = AddFeedDialog
                , dialogError = Nothing
                , dialogInput = ""
            }
                |> withCmd
                    (Task.attempt (\_ -> Noop) <| Dom.focus userFeedInputId)

        SaveFeedTypes ->
            let
                typesString =
                    List.map .feedType model.feeds
                        |> ED.encodeFeedTypes
                        |> JE.encode 0
            in
            { model
                | showDialog = SaveFeedsDialog
                , dialogError = Nothing
                , dialogInput = typesString
            }
                |> withCmd
                    (Task.attempt (\_ -> Noop) <| Dom.focus feedsStringInputId)

        RestoreFeedTypes ->
            case JD.decodeString ED.feedTypesDecoder model.dialogInput of
                Err err ->
                    { model | dialogError = Just <| JD.errorToString err }
                        |> withNoCmd

                Ok feedTypes ->
                    let
                        mdl =
                            { model
                                | showDialog = NoDialog
                                , dialogError = Nothing
                            }
                    in
                    restoreFeedTypes feedTypes mdl

        DialogInput username ->
            { model | dialogInput = username } |> withNoCmd

        AddNewFeed feedType ->
            addNewFeed feedType model.settings.fontSize model

        Upvote feedType post ->
            upvote feedType post model

        Downvote feedType post ->
            downvote feedType post model

        Repost feedType post ->
            repost feedType post model

        ReceiveOperation operation result ->
            receiveOperation operation result model

        ReceivePostFeed scrollToTop feedType result ->
            receiveFeed scrollToTop
                feedType
                (boxActivityLogListResult result)
                model

        ReceiveNotificationsFeed scrollToTop feedType result ->
            receiveFeed scrollToTop
                feedType
                (boxNotificationsLogResult result)
                model


boxActivityLogListResult : FeedResult ActivityLogList -> FeedResult (LogList FeedData)
boxActivityLogListResult result =
    case result of
        Err err ->
            Err err

        Ok logList ->
            Ok
                { data = PostFeedData logList.data
                , no_more = logList.no_more
                }


boxNotificationsLogResult : FeedResult NotificationsLog -> FeedResult (LogList FeedData)
boxNotificationsLogResult result =
    case result of
        Err err ->
            Err err

        Ok logList ->
            let
                gangit notification =
                    { notification = notification
                    , users = []
                    }
            in
            Ok
                { data = NotificationFeedData <| List.map gangit logList.data
                , no_more = logList.no_more
                }


operationToString : Operation -> String
operationToString operation =
    case operation of
        UpvoteOperation _ _ _ ->
            "upvote"

        DownvoteOperation _ _ _ ->
            "downvote"

        RepostOperation _ _ ->
            "repost"


receiveOperation : Operation -> Result ApiError Success -> Model -> ( Model, Cmd Msg )
receiveOperation operation result model =
    let
        handleError err =
            let
                mdl =
                    case operation of
                        UpvoteOperation feedType postid wasDisliked ->
                            updatePost (togglePostLiked wasDisliked) feedType postid model

                        DownvoteOperation feedType postid wasLiked ->
                            updatePost (togglePostDisliked wasLiked) feedType postid model

                        RepostOperation feedType postid ->
                            updatePost toggleReposted feedType postid model
            in
            { mdl | showDialog = OperationErrorDialog err }
                |> withNoCmd
    in
    case result of
        Err err ->
            let
                opname =
                    " on " ++ operationToString operation
            in
            handleError <|
                case err.httpError of
                    Nothing ->
                        "Unknown error" ++ opname

                    Just httpError ->
                        case httpError of
                            Http.BadStatus response ->
                                if response.status.code == 400 then
                                    case operation of
                                        DownvoteOperation _ _ _ ->
                                            "Downvote error, probably unauthorized."

                                        _ ->
                                            "Bad Request error" ++ opname

                                else
                                    "Bad HTTP status: "
                                        ++ String.fromInt response.status.code
                                        ++ opname

                            _ ->
                                "HTTP error on" ++ opname

        Ok success ->
            if not success.state then
                handleError success.message

            else
                model |> withNoCmd


updatePost : (Post -> Post) -> FeedType -> Int -> Model -> Model
updatePost updater feedType postid model =
    let
        shouldUpdate lg =
            lg.post.id == postid

        modifier lg =
            { lg | post = updater lg.post }

        updateLogList : LogList FeedData -> LogList FeedData
        updateLogList logList =
            case logList.data of
                PostFeedData activityLogList ->
                    { logList
                        | data =
                            PostFeedData <|
                                LE.updateIf shouldUpdate
                                    modifier
                                    activityLogList
                    }

                _ ->
                    logList

        feeds : List (Feed Msg)
        feeds =
            LE.updateIf (\f -> feedType == f.feedType)
                (\feed ->
                    let
                        feed_feed =
                            feed.feed
                    in
                    { feed
                        | feed =
                            updateLogList feed.feed
                    }
                )
                model.feeds
    in
    { model | feeds = feeds }


upvote : FeedType -> Post -> Model -> ( Model, Cmd Msg )
upvote feedType post model =
    let
        updater =
            togglePostLiked post.disliked
    in
    case model.backend of
        Nothing ->
            model |> withNoCmd

        Just backend ->
            updatePost updater feedType post.id model
                |> withCmd
                    (Api.upvotePost backend
                        (ReceiveOperation <|
                            UpvoteOperation feedType post.id post.disliked
                        )
                        post.id
                        post.liked
                    )


togglePostLiked : Bool -> Post -> Post
togglePostLiked wasDisliked post =
    { post
        | liked = not post.liked
        , like_count =
            post.like_count
                + (if post.liked then
                    -1

                   else
                    1
                  )
        , disliked =
            if post.liked then
                wasDisliked

            else
                False
        , dislike_count =
            post.dislike_count
                + (if wasDisliked && post.liked then
                    1

                   else
                    0
                  )
    }


togglePostDisliked : Bool -> Post -> Post
togglePostDisliked wasLiked post =
    { post
        | disliked = not post.disliked
        , dislike_count =
            post.dislike_count
                + (if post.disliked then
                    -1

                   else
                    1
                  )
        , liked =
            if post.disliked then
                wasLiked

            else
                False
        , like_count =
            post.like_count
                + (if wasLiked && post.disliked then
                    1

                   else
                    0
                  )
    }


toggleReposted : Post -> Post
toggleReposted post =
    { post
        | repost = not post.repost
        , repost_count =
            post.repost_count
                + (if post.repost then
                    -1

                   else
                    1
                  )
    }


downvote : FeedType -> Post -> Model -> ( Model, Cmd Msg )
downvote feedType post model =
    let
        updater =
            togglePostDisliked post.liked
    in
    case model.backend of
        Nothing ->
            model |> withNoCmd

        Just backend ->
            updatePost updater feedType post.id model
                |> withCmd
                    (Api.downvotePost backend
                        (ReceiveOperation <|
                            DownvoteOperation feedType post.id post.liked
                        )
                        post.id
                        post.disliked
                    )


repost : FeedType -> Post -> Model -> ( Model, Cmd Msg )
repost feedType post model =
    case model.backend of
        Nothing ->
            model |> withNoCmd

        Just backend ->
            updatePost toggleReposted feedType post.id model
                |> withCmd
                    (Api.repost backend
                        (ReceiveOperation <|
                            RepostOperation feedType post.id
                        )
                        post.id
                        post.repost
                    )


moveFeedLeft : FeedType -> Model -> ( Model, Cmd Msg )
moveFeedLeft feedType model =
    let
        loop feeds res =
            case feeds of
                [] ->
                    model

                feed :: tail ->
                    if feedType /= feed.feedType then
                        loop tail <| feed :: res

                    else
                        case res of
                            [] ->
                                { model
                                    | feeds =
                                        List.concat [ tail, [ feed ] ]
                                }

                            f :: t ->
                                { model
                                    | feeds =
                                        List.concat
                                            [ List.reverse t
                                            , [ feed, f ]
                                            , tail
                                            ]
                                }

        mdl =
            loop model.feeds []
    in
    mdl |> withCmd (saveFeeds mdl.feeds mdl)


moveFeedRight : FeedType -> Model -> ( Model, Cmd Msg )
moveFeedRight feedType model =
    let
        loop feeds res =
            case feeds of
                [] ->
                    model

                feed :: tail ->
                    if feedType /= feed.feedType then
                        loop tail <| feed :: res

                    else
                        case tail of
                            [] ->
                                { model
                                    | feeds =
                                        List.concat
                                            [ [ feed ]
                                            , List.reverse res
                                            ]
                                }

                            f :: t ->
                                { model
                                    | feeds =
                                        List.concat
                                            [ List.reverse res
                                            , [ f, feed ]
                                            , t
                                            ]
                                }

        mdl =
            loop model.feeds []
    in
    mdl |> withCmd (saveFeeds mdl.feeds mdl)


cdr : List a -> List a
cdr list =
    Maybe.withDefault [] <| List.tail list


saveFeeds : List (Feed Msg) -> Model -> Cmd Msg
saveFeeds feeds model =
    let
        value =
            List.map .feedType feeds
                |> ED.encodeFeedTypes
    in
    localStorageSend
        (LocalStorage.put storageKeys.feeds <| Just value)
        model


addNewFeed : FeedType -> Float -> Model -> ( Model, Cmd Msg )
addNewFeed feedType baseFontSize model =
    let
        addit feed =
            let
                feeds =
                    List.concat [ model.feeds, [ feed ] ]
            in
            { model
                | feeds = feeds
                , showDialog = NoDialog
                , dialogError = Nothing
                , lastClosedFeed = Nothing
                , nextId = model.nextId + 1
            }
                |> withCmds
                    [ loadMoreCmd False feed
                    , saveFeeds feeds model
                    , Task.perform ScrollToFeed <| Task.succeed feed.feedType
                    ]
    in
    if feedType == LastClosedFeed then
        case model.lastClosedFeed of
            Nothing ->
                model |> withNoCmd

            Just feed ->
                addit feed

    else
        case model.backend of
            Nothing ->
                model |> withNoCmd

            Just backend ->
                case findFeed feedType model of
                    Just _ ->
                        { model
                            | dialogError =
                                Just "That feed is already displayed."
                        }
                            |> withNoCmd

                    Nothing ->
                        let
                            blankUser =
                                case feedType of
                                    UserFeed username ->
                                        username == ""

                                    _ ->
                                        False
                        in
                        if blankUser then
                            { model
                                | dialogError =
                                    Just "Username may not be blank."
                            }
                                |> withNoCmd

                        else
                            addit
                                (feedTypeToFeed model.loggedInUser
                                    backend
                                    feedType
                                    model.nextId
                                )


loadAll : Model -> ( Model, Cmd Msg )
loadAll model =
    let
        getone feed ( m, cs ) =
            ( setLoadingFeed feed m
            , loadMore False feed :: cs
            )

        ( mdl, cmds ) =
            List.foldr getone ( model, [] ) model.feeds
    in
    mdl |> withCmds cmds


feedBefore : Feed Msg -> String
feedBefore feed =
    case feed.feed.data of
        PostFeedData activityLogList ->
            case LE.last activityLogList of
                Nothing ->
                    ""

                Just activityLog ->
                    activityLog.published_at

        NotificationFeedData gangedNotificationList ->
            case LE.last gangedNotificationList of
                Nothing ->
                    ""

                Just gangedNotification ->
                    gangedNotification.notification.id


loadMore : Bool -> Feed Msg -> Cmd Msg
loadMore appendResult feed =
    let
        before =
            if appendResult then
                feedBefore feed

            else
                ""
    in
    case feed.getter of
        PostFeedGetter getter ->
            getter (ReceivePostFeed (not appendResult) feed.feedType) before

        NotificationFeedGetter getter ->
            getter (ReceiveNotificationsFeed (not appendResult) feed.feedType) before


updateIcons : LogList FeedData -> Model -> ( Model, Cmd Msg )
updateIcons logList model =
    case logList.data of
        PostFeedData logLists ->
            let
                folder log ( mdl, bool ) =
                    let
                        ( m, b ) =
                            updateUserIconUrl log.actuser.username
                                log.actuser.picture_url
                                mdl
                    in
                    ( m, bool || b )

                ( model2, needsUpdate ) =
                    List.foldl folder ( model, False ) logLists
            in
            model2
                |> withCmd
                    (if needsUpdate then
                        storeIcons model2.icons model

                     else
                        Cmd.none
                    )

        _ ->
            model |> withNoCmd


receiveFeed : Bool -> FeedType -> FeedResult (LogList FeedData) -> Model -> ( Model, Cmd Msg )
receiveFeed scrollToTop feedType result model =
    let
        ( mdl, cmd ) =
            case result of
                Err err ->
                    case err.httpError of
                        Just httpError ->
                            processReceivedError httpError model

                        _ ->
                            model |> withNoCmd

                Ok logList ->
                    updateIcons logList model

        ( id, feeds ) =
            updateFeeds (not scrollToTop) feedType result mdl.feeds
    in
    { mdl
        | feeds = feeds
        , loadingFeeds =
            Set.remove (feedTypeToString feedType) model.loadingFeeds
    }
        |> withCmds
            [ cmd
            , if scrollToTop && id /= "" then
                Task.attempt (\_ -> Noop) (Dom.setViewportOf id 0 0)

              else
                Cmd.none
            ]


processReceivedError : Http.Error -> Model -> ( Model, Cmd Msg )
processReceivedError err model =
    case err of
        -- I don't know why we get BadPayload for a bad token, but we do.
        -- Currently, this remove the saved token and forces a new login.
        BadPayload _ _ ->
            let
                mdl =
                    { model | backend = Nothing }
            in
            mdl
                |> withCmd
                    (localStorageSend
                        (LocalStorage.put storageKeys.token Nothing)
                        mdl
                    )

        _ ->
            model |> withNoCmd


updateFeeds : Bool -> FeedType -> FeedResult (LogList FeedData) -> List (Feed Msg) -> ( String, List (Feed Msg) )
updateFeeds appendResult feedType result feeds =
    let
        loop tail res =
            case tail of
                [] ->
                    ( "", List.reverse res )

                feed :: rest ->
                    if feedType == feed.feedType then
                        ( columnId feed.id
                        , List.concat
                            [ List.reverse res
                            , [ updateFeed appendResult
                                    result
                                    feed
                              ]
                            , rest
                            ]
                        )

                    else
                        loop rest (feed :: res)
    in
    loop feeds []


{-| This is here to try to preserve === on the whole feed or some of its elements.
-}
canonicalizeFeed : Feed Msg -> Feed Msg -> Feed Msg
canonicalizeFeed newFeed feed =
    let
        newData =
            newFeed.feed.data

        feedData =
            feed.feed.data
    in
    if newData == feedData then
        let
            ignore =
                --Debug.log "Canonical" feed.feedType
                1
        in
        feed

    else
        let
            data =
                case feedData of
                    PostFeedData feedList ->
                        case newData of
                            PostFeedData newList ->
                                PostFeedData <|
                                    let
                                        ignore =
                                            --logDifferences feedList newList
                                            0
                                    in
                                    canonicalizeData .id
                                        newList
                                        feedList

                            _ ->
                                newData

                    NotificationFeedData feedList ->
                        case newData of
                            NotificationFeedData newList ->
                                NotificationFeedData <|
                                    canonicalizeData (.notification >> .id)
                                        newList
                                        feedList

                            _ ->
                                newData

            feed_feed =
                feed.feed
        in
        { feed
            | feed =
                { feed_feed | data = data }
        }


logDifferences : List ActivityLog -> List ActivityLog -> ()
logDifferences feedList newList =
    let
        len =
            Debug.log "feedList len" <| List.length feedList

        len2 =
            Debug.log "newList len" <| List.length feedList

        doone feed new idx =
            let
                is =
                    String.fromInt idx ++ ": "

                id =
                    if feed.id /= new.id then
                        Debug.log (is ++ ", id: " ++ feed.id ++ ", ") new.id

                    else
                        ""

                pub =
                    if feed.published_at /= new.published_at then
                        Debug.log (is ++ ", published_at: " ++ feed.published_at ++ ", ") new.published_at

                    else
                        ""

                type_ =
                    if feed.type_ /= new.type_ then
                        Debug.log (is ++ ", id: " ++ feed.type_ ++ ", ") new.type_

                    else
                        ""

                user =
                    if feed.actuser /= new.actuser then
                        Debug.log (is ++ ", user: " ++ Debug.toString feed.actuser ++ ", ") new.actuser

                    else
                        feed.actuser

                post =
                    if feed.post /= new.post then
                        Debug.log (is ++ ", post: " ++ Debug.toString feed.post ++ ", ") new.post

                    else
                        feed.post
            in
            ()

        map =
            List.map3 doone
                feedList
                newList
                (List.range 1 <| List.length feedList)
    in
    ()


canonicalizeData : (x -> String) -> List x -> List x -> List x
canonicalizeData key new old =
    let
        dict =
            List.foldl
                (\log d ->
                    Dict.insert (key log) log d
                )
                Dict.empty
                old

        mapper dat =
            case Dict.get (key dat) dict of
                Just v ->
                    if dat == v then
                        v

                    else
                        dat

                Nothing ->
                    dat
    in
    List.map mapper new


updateFeed : Bool -> FeedResult (LogList FeedData) -> Feed Msg -> Feed Msg
updateFeed appendResult result feed =
    case result of
        Err err ->
            { feed | error = Just err }

        Ok resultLogList ->
            let
                feed_feed =
                    feed.feed

                newFeed =
                    { feed
                        | feed =
                            { feed_feed
                                | data =
                                    reprocessFeedData <|
                                        updateFeedData appendResult
                                            resultLogList.data
                                            feed_feed.data
                            }
                    }
            in
            canonicalizeFeed newFeed feed


updateFeedData : Bool -> FeedData -> FeedData -> FeedData
updateFeedData appendResult resultData feedData =
    if not appendResult then
        resultData

    else
        case resultData of
            PostFeedData resultList ->
                case feedData of
                    PostFeedData feedList ->
                        PostFeedData <|
                            List.append feedList resultList

                    _ ->
                        resultData

            NotificationFeedData gangedNotificationList ->
                case feedData of
                    NotificationFeedData feedList ->
                        NotificationFeedData <|
                            List.append feedList
                                gangedNotificationList

                    _ ->
                        resultData


reprocessFeedData : FeedData -> FeedData
reprocessFeedData data =
    case data of
        PostFeedData activityLogList ->
            PostFeedData <| trimComments activityLogList

        NotificationFeedData gangedNotificationList ->
            NotificationFeedData <|
                gangNotifications (ungangNotifications gangedNotificationList)


{-| (<same id>, <published\_at compare>)
-}
type alias PostOrder =
    ( Bool, Order )


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


focusStyle : Element.Option
focusStyle =
    Element.focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }


view : Model -> Document Msg
view model =
    { title = pageTitle
    , body =
        [ Element.layoutWith
            { options = [ focusStyle ] }
            [ Element.inFront <|
                case model.showDialog of
                    AddFeedDialog ->
                        addFeedDialog model

                    ImageDialog url ->
                        imageDialog url model

                    SaveFeedsDialog ->
                        saveFeedsDialog model

                    OperationErrorDialog err ->
                        operationErrorDialog err model

                    _ ->
                        text ""
            ]
            (case model.backend of
                Nothing ->
                    loginPage model.settings

                Just backend ->
                    optimizers.mainPage
                        model.settings
                        model.icons
                        model.loadingFeeds
                        model.feeds
            )
        ]
    }


defaultFontSize : Float
defaultFontSize =
    15


fontSize : Float -> Float -> Element.Attr decorative msg
fontSize baseFontSize scale =
    Font.size <| round (scale * baseFontSize)


fillWidth : Attribute msg
fillWidth =
    width Element.fill


mainPage : Settings -> Icons -> Set String -> List (Feed Msg) -> Element Msg
mainPage settings icons loadingFeeds feeds =
    let
        baseFontSize =
            settings.fontSize

        ccw =
            controlColumnWidth baseFontSize

        contentWidth =
            settings.columnWidth * List.length feeds
    in
    row
        [ fontSize baseFontSize 1
        , onMousedownAttribute CloseDialog
        , Border.widthEach
            { zeroes
                | left = 5
                , right =
                    if [] == feeds then
                        5

                    else
                        0
            }
        , Border.color styleColors.border
        ]
        [ column []
            [ row [ height <| px settings.windowHeight ]
                [ controlColumn ccw settings icons feeds
                ]
            ]
        , column []
            [ optimizers.keyedFeedColumn
                [ height <| px settings.windowHeight
                , Element.scrollbarY
                , width <| px (min (settings.windowWidth - ccw) contentWidth)
                , idAttribute contentId
                ]
                (List.map
                    (\feed ->
                        ( feedTypeToString feed.feedType
                        , optimizers.feedColumn
                            (feedIsLoading feed loadingFeeds)
                            settings
                            icons
                            feed
                        )
                    )
                    feeds
                )
            ]
        ]


findFeed : FeedType -> Model -> Maybe (Feed Msg)
findFeed feedType model =
    LE.find (\f -> feedType == f.feedType) model.feeds


addFeedChoices : Model -> List ( String, FeedType )
addFeedChoices model =
    let
        homeFeed =
            findFeed HomeFeed model

        popularFeed =
            findFeed PopularFeed model

        notificationsFeed =
            findFeed NotificationsFeed model

        ( username, userFeed ) =
            case model.loggedInUser of
                Nothing ->
                    ( "", Nothing )

                Just name ->
                    ( name
                    , findFeed (UserFeed name) model
                    )
    in
    List.concat
        [ case homeFeed of
            Nothing ->
                [ ( "Home", HomeFeed ) ]

            _ ->
                []
        , case popularFeed of
            Nothing ->
                [ ( "Popular", PopularFeed ) ]

            _ ->
                []
        , case notificationsFeed of
            Nothing ->
                [ ( "Notifications", NotificationsFeed ) ]

            _ ->
                []
        , case userFeed of
            Nothing ->
                if username == "" then
                    []

                else
                    [ ( "Your Posts", LoggedInUserFeed ) ]

            _ ->
                []
        , case model.lastClosedFeed of
            Just feed ->
                [ ( "Last Closed", LastClosedFeed ) ]

            _ ->
                []
        ]


dialogAttributes : List (Attribute msg)
dialogAttributes =
    [ Border.width 5
    , centerX
    , centerY
    , Background.color colors.verylightgray
    , paddingEach { top = 10, bottom = 20, left = 20, right = 20 }
    ]


operationErrorDialog : String -> Model -> Element Msg
operationErrorDialog err model =
    let
        baseFontSize =
            model.settings.fontSize

        iconHeight =
            userIconHeight baseFontSize
    in
    column dialogAttributes
        [ let
            closeIcon =
                heightImage iconUrls.close "Close" iconHeight
          in
          row [ width Element.fill ]
            [ row
                [ centerX
                , centerY
                , Font.bold
                , fontSize baseFontSize 1.5
                ]
                [ el [ Font.color colors.red ] <| text "Operation Error " ]
            , el [ alignRight ]
                (standardButton "Close Dialog" CloseDialog closeIcon)
            ]
        , row [ paddingEach { zeroes | top = 20 } ]
            [ text err ]
        ]


styleAttribute : String -> String -> Attribute msg
styleAttribute name value =
    Attributes.style name value
        |> Element.htmlAttribute


{-| Should probably put a transparent layer over the main view,
so clicks intended to close the dialog don't hit mouse-sensitive regions there.

Or not.

-}
imageDialog : String -> Model -> Element Msg
imageDialog url model =
    let
        settings =
            model.settings

        maxw =
            9 * settings.windowWidth // 10

        maxws =
            String.fromInt maxw ++ "px"

        maxh =
            9 * settings.windowHeight // 10

        maxhs =
            String.fromInt maxh ++ "px"
    in
    column
        -- This is black magic.
        -- It took much play with the CSS to get it right.
        [ centerX
        , centerY
        ]
        [ standardButton "" CloseDialog <|
            (Html.img
                [ Attributes.style "object-fit" "contain"
                , Attributes.style "max-width" maxws
                , Attributes.style "max-height" maxhs
                , Attributes.style "border" "2px solid black"
                , Attributes.style "width" "auto"
                , Attributes.style "height" "auto"
                , Attributes.src url
                ]
                []
                |> Element.html
            )
        ]


onMousedownAttribute : msg -> Attribute msg
onMousedownAttribute msg =
    Html.Events.on "mousedown" (JD.succeed msg)
        |> Element.htmlAttribute


onKeyDownAttribute : Int -> msg -> Attribute msg
onKeyDownAttribute keycode msg =
    onKeysDownAttribute [ ( keycode, msg ) ]


onKeysDownAttribute : List ( Int, msg ) -> Attribute msg
onKeysDownAttribute pairs =
    Html.Events.on "keydown"
        (Html.Events.keyCode |> JD.andThen (isKeycode pairs))
        |> Element.htmlAttribute


keyDownDecoder : Int -> msg -> Decoder msg
keyDownDecoder keycode msg =
    keysDownDecoder [ ( keycode, msg ) ]


keysDownDecoder : List ( Int, msg ) -> Decoder msg
keysDownDecoder pairs =
    Html.Events.keyCode |> JD.andThen (isKeycode pairs)


isKeycode : List ( Int, msg ) -> Int -> Decoder msg
isKeycode pairs keycode =
    case LE.find (\( kc, _ ) -> kc == keycode) pairs of
        Just ( _, msg ) ->
            JD.succeed msg

        Nothing ->
            JD.fail "These are not the keycodes you're looking for."


dialogTitleBar : Float -> String -> Element Msg
dialogTitleBar baseFontSize title =
    let
        iconHeight =
            userIconHeight baseFontSize

        closeIcon =
            heightImage iconUrls.close "Close" iconHeight
    in
    row [ width Element.fill ]
        [ row
            [ centerX
            , centerY
            , Font.bold
            , fontSize baseFontSize 1.5
            ]
            [ text title ]
        , el [ alignRight ]
            (standardButton "Close Dialog" CloseDialog closeIcon)
        ]


dialogErrorRow : Model -> Element msg
dialogErrorRow model =
    case model.dialogError of
        Nothing ->
            text ""

        Just err ->
            row
                [ paddingEach { zeroes | top = 10 }
                , Font.color colors.red
                ]
                [ text err ]


saveFeedsDialog : Model -> Element Msg
saveFeedsDialog model =
    let
        baseFontSize =
            model.settings.fontSize

        iconHeight =
            userIconHeight (2 * baseFontSize)
    in
    column dialogAttributes
        [ dialogTitleBar baseFontSize "Save/Restore Feeds "
        , dialogErrorRow model
        , row [ paddingEach { zeroes | top = 10 } ]
            [ Input.text
                [ width <| px 400
                , idAttribute feedsStringInputId
                , onKeysDownAttribute
                    [ ( keycodes.escape, CloseDialog )
                    , ( keycodes.enter, RestoreFeedTypes )
                    ]
                ]
                { onChange = DialogInput
                , text = model.dialogInput
                , placeholder =
                    Just <|
                        Input.placeholder [] (text "username")
                , label = Input.labelHidden "User Feed Name"
                }
            , el [ paddingEach { zeroes | left = 10 } ]
                (standardButton "Restore" RestoreFeedTypes <|
                    heightImage iconUrls.save "Restore" iconHeight
                )
            ]
        ]


addFeedDialog : Model -> Element Msg
addFeedDialog model =
    let
        baseFontSize =
            model.settings.fontSize

        iconHeight =
            userIconHeight baseFontSize

        addButton feedType =
            el [ Font.size <| 7 * iconHeight // 4 ] <|
                textButton "Add Feed"
                    (AddNewFeed feedType)
                    "+"

        addUserFeedRow =
            let
                feedType =
                    UserFeed model.dialogInput
            in
            { label = "User: "
            , element =
                Input.text
                    [ width <| px 200
                    , idAttribute userFeedInputId
                    , onKeysDownAttribute
                        [ ( keycodes.escape, CloseDialog )
                        , ( keycodes.enter, AddNewFeed feedType )
                        ]
                    ]
                    { onChange = DialogInput
                    , text = model.dialogInput
                    , placeholder =
                        Just <|
                            Input.placeholder [] (text "username")
                    , label = Input.labelHidden "User Feed Name"
                    }
            , feedType = Just feedType
            }

        makeRow ( label, feedType ) =
            { label = ""
            , element =
                el [ Font.bold ] <|
                    textButton "Add Feed"
                        (AddNewFeed feedType)
                        label
            , feedType = Nothing
            }

        choices =
            addFeedChoices model

        data =
            addUserFeedRow :: List.map makeRow choices
    in
    column dialogAttributes
        [ dialogTitleBar baseFontSize "Add Feed"
        , dialogErrorRow model
        , row []
            [ Element.table
                [ spacing 10, centerX ]
                { data = data
                , columns =
                    [ { header = text ""
                      , width = Element.shrink
                      , view =
                            \x ->
                                el [ Font.bold, Element.centerY ]
                                    (text x.label)
                      }
                    , { header = text ""
                      , width = Element.shrink
                      , view = \x -> x.element
                      }
                    , { header = text ""
                      , width = Element.shrink
                      , view =
                            \x ->
                                case x.feedType of
                                    Nothing ->
                                        text ""

                                    Just ft ->
                                        addButton ft
                      }
                    ]
                }
            ]
        ]


addFeedHeader : Model -> Element Msg
addFeedHeader model =
    el
        [ centerX
        , Font.bold
        , fontSize model.settings.fontSize 1.5
        ]
    <|
        text "Add Feed"


okButton : Html Msg
okButton =
    Html.button
        [ class "btn btn-success"
        , onClick CloseDialog
        ]
        [ Html.text "OK" ]


controlColumnWidth : Float -> Int
controlColumnWidth baseFontSize =
    smallUserIconHeight baseFontSize + 2 * columnPadding


controlColumnId : String
controlColumnId =
    "controlColumn"


contentId : String
contentId =
    "contentColumn"


userFeedInputId : String
userFeedInputId =
    "userFeedInput"


feedsStringInputId : String
feedsStringInputId =
    "feedsStringInput"


controlColumn : Int -> Settings -> Icons -> List (Feed Msg) -> Element Msg
controlColumn columnWidth settings icons feeds =
    let
        colw =
            width <| px columnWidth

        iconHeight =
            smallUserIconHeight settings.fontSize
    in
    column
        (List.concat
            [ [ colw
              , height Element.fill
              , padding columnPadding
              , Border.widthEach
                    { zeroes
                        | top = columnBorderWidth
                        , bottom = columnBorderWidth
                        , right = columnBorderWidth
                    }
              , Border.color styleColors.border
              , Background.color styleColors.headerBackground
              , spacing 10
              , idAttribute controlColumnId
              ]
            , columnBorderAttributes True
            ]
        )
    <|
        List.concat
            [ [ row
                    [ centerX
                    , paddingEach { zeroes | bottom = iconHeight // 2 }
                    ]
                    [ standardButton
                        "Refresh All Feeds"
                        LoadAll
                        (widthImage iconUrls.reload "Refresh" iconHeight)
                    ]
              ]
            , List.map (feedSelectorButton iconHeight icons) feeds
            , [ row
                    [ Font.size <| 7 * iconHeight // 4
                    , centerX
                    ]
                    [ standardButton "Add New Feed" AddFeed (text "+") ]
              , el
                    [ alignBottom
                    , paddingEach { zeroes | bottom = 10 }
                    ]
                    (standardButton
                        "Restore Feeds"
                        SaveFeedTypes
                        (widthImage iconUrls.save "Save" iconHeight)
                    )
              ]
            ]


feedTypeIconUrl : FeedType -> Icons -> ( String, String )
feedTypeIconUrl feedType icons =
    case feedType of
        HomeFeed ->
            ( iconUrls.home, "Home" )

        UserFeed username ->
            ( lookupUserIconUrl username icons, "User: " ++ username )

        GroupFeed groupid ->
            ( "", "Group: " ++ groupid )

        TopicFeed topicid ->
            ( "", "Topic: " ++ topicid )

        PopularFeed ->
            ( iconUrls.popular, "Popular" )

        NotificationsFeed ->
            ( iconUrls.notifications, "Notifications" )

        _ ->
            ( "", "" )


feedSelectorButton : Int -> Icons -> Feed Msg -> Element Msg
feedSelectorButton iconHeight icons feed =
    case feedTypeIconUrl feed.feedType icons of
        ( "", _ ) ->
            text ""

        ( url, label ) ->
            row
                [ centerX ]
                [ standardButtonWithDontHover False
                    label
                    (ScrollToFeed feed.feedType)
                  <|
                    heightImage url label iconHeight
                ]


zeroes =
    { right = 0
    , left = 0
    , top = 0
    , bottom = 0
    }


{-| 40 has no rhyme or reason other than it works.
-}
headerHeight : Float -> Int
headerHeight baseFontSize =
    round <| 1.5 * baseFontSize


idAttribute : String -> Attribute msg
idAttribute id =
    Element.htmlAttribute <| Attributes.id id


titleAttribute : String -> Attribute msg
titleAttribute string =
    Element.htmlAttribute <| Attributes.title string


columnId : Int -> String
columnId idx =
    "column" ++ String.fromInt idx


columnIdAttribute : Int -> Attribute msg
columnIdAttribute idx =
    idAttribute <| columnId idx


columnBorderAttributes : Bool -> List (Attribute msg)
columnBorderAttributes isControlColumn =
    [ if isControlColumn then
        Border.widthEach { zeroes | top = 3, bottom = 3 }

      else
        Border.width 3
    , Border.color styleColors.border
    ]


columnPadding : Int
columnPadding =
    10


userIconHeight : Float -> Int
userIconHeight baseFontSize =
    round (4 * baseFontSize / 3)


smallUserIconHeight : Float -> Int
smallUserIconHeight baseFontSize =
    round (3 * baseFontSize / 2)


columnBorderWidth : Int
columnBorderWidth =
    5


setLoadingFeed : Feed Msg -> Model -> Model
setLoadingFeed feed model =
    { model
        | loadingFeeds =
            Set.insert (feedTypeToString feed.feedType)
                model.loadingFeeds
    }


feedTypeToString : FeedType -> String
feedTypeToString feedType =
    case feedType of
        HomeFeed ->
            "home"

        UserFeed username ->
            "user." ++ username

        GroupFeed groupid ->
            "group." ++ groupid

        TopicFeed topicid ->
            "topic." ++ topicid

        PopularFeed ->
            "popular"

        NotificationsFeed ->
            "notifications"

        _ ->
            "unknown"


feedIsLoading : Feed Msg -> Set String -> Bool
feedIsLoading feed loadingFeeds =
    Set.member (feedTypeToString feed.feedType) loadingFeeds


feedColumn : Bool -> Settings -> Icons -> Feed Msg -> Element Msg
feedColumn isLoading settings icons feed =
    let
        baseFontSize =
            settings.fontSize

        colw =
            width <| px settings.columnWidth

        iconHeight =
            userIconHeight baseFontSize
    in
    column
        (List.concat
            [ [ colw
              , height Element.fill
              , Border.width columnBorderWidth
              ]
            , columnBorderAttributes False
            ]
        )
        [ row
            [ fillWidth
            , Border.widthEach { zeroes | bottom = 1 }
            , Border.color styleColors.border
            , Background.color styleColors.headerBackground
            ]
            [ column [ colw ]
                [ let
                    closeIcon =
                        heightImage iconUrls.close "Close" iconHeight

                    prevIcon =
                        heightImage iconUrls.previous "Move Left" iconHeight

                    nextIcon =
                        heightImage iconUrls.next "Move Right" iconHeight
                  in
                  row
                    [ padding columnPadding
                    , fontSize baseFontSize 1.5
                    , Font.bold
                    , centerX
                    , width Element.fill
                    ]
                    [ row [ alignLeft ]
                        [ standardButton "Move Feed Left"
                            (MoveFeedLeft feed.feedType)
                            prevIcon
                        , text " "
                        , standardButton "Move Feed Right"
                            (MoveFeedRight feed.feedType)
                            nextIcon
                        ]
                    , row
                        [ centerX ]
                        [ el
                            (if isLoading then
                                [ Background.color colors.orange ]

                             else
                                []
                            )
                          <|
                            standardButtonWithDontHover isLoading
                                "Refresh Feed"
                                (LoadMore False feed)
                                (heightImage iconUrls.reload "Refresh" iconHeight)
                        , text " "
                        , feedTypeDescription feed.feedType baseFontSize icons
                        ]
                    , el [ alignRight ]
                        (standardButton "Close Feed" (CloseFeed feed) closeIcon)
                    ]
                ]
            ]
        , optimizers.renderRowContents
            settings
            feed
        ]


renderRowContents : Settings -> Feed Msg -> Element Msg
renderRowContents settings feed =
    let
        baseFontSize =
            settings.fontSize

        windowHeight =
            settings.windowHeight

        colw =
            width <| px settings.columnWidth

        iconHeight =
            userIconHeight baseFontSize
    in
    row []
        [ column
            [ colw
            , height <|
                px
                    (windowHeight
                        - headerHeight baseFontSize
                        - (2 * columnPadding + 10)
                    )
            , columnIdAttribute feed.id
            , Element.scrollbarX
            , Element.clipX
            ]
          <|
            let
                data =
                    feed.feed.data

                --                ignore =
                --                    Debug.log
                --                        "Rendering"
                --                        feed.feedType
                rows =
                    case data of
                        PostFeedData activityLogList ->
                            optimizers.keyedPostRow
                                []
                            <|
                                List.map
                                    (\log ->
                                        ( log.id
                                        , optimizers.postRow
                                            settings
                                            feed
                                            True
                                            log
                                        )
                                    )
                                    activityLogList

                        NotificationFeedData gangedNotificationList ->
                            optimizers.keyedNotificationRow
                                []
                            <|
                                List.map
                                    (\notification ->
                                        ( notification.notification.id
                                        , optimizers.notificationRow
                                            settings
                                            True
                                            notification
                                        )
                                    )
                                    gangedNotificationList
            in
            let
                typeString =
                    feedTypeToString feed.feedType
            in
            [ if False then
                undoneRow typeString

              else
                text ""
            , rows
            , moreRow colw feed
            ]
        ]


{-| Same signature as Keyed.column, to allow quick switch between keyed and unkeyed.
-}
keyedColumn : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
keyedColumn attributes pairs =
    column attributes <| List.map Tuple.second pairs


{-| Same signature as Keyed.row, to allow quick switch between keyed and unkeyed.
-}
keyedRow : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
keyedRow attributes pairs =
    row attributes <| List.map Tuple.second pairs


{-| Not currently used. Saved for the next incomplete column
-}
undoneRow : String -> Element Msg
undoneRow typeString =
    row
        [ fillWidth
        , paddingEach
            { zeroes
                | left = columnPadding
                , top = 5
                , bottom = 5
            }
        , Font.bold
        , Border.widthEach
            { zeroes | bottom = 2 }
        , Border.color styleColors.border
        ]
        [ text "Comment parents are coming."
        ]


moreRow : Attribute Msg -> Feed Msg -> Element Msg
moreRow colw feed =
    if feed.feed.no_more then
        text ""

    else if
        case feed.feed.data of
            PostFeedData activityLogList ->
                activityLogList == []

            NotificationFeedData gangedNotificationList ->
                gangedNotificationList == []
    then
        text ""

    else
        row
            [ centerX
            , paddingEach
                { zeroes
                    | top = 20
                    , bottom = 20
                    , left = 5
                    , right = 5
                }
            ]
            [ let
                nbsps =
                    String.repeat 3 chars.nbsp

                msg =
                    LoadMore True feed
              in
              textButton ""
                msg
                (nbsps ++ "Load More" ++ nbsps)
            ]


userPadding : Attribute msg
userPadding =
    paddingEach
        { top = 0
        , right = 0
        , bottom = 4
        , left = 0
        }


nameBottomPadding : Attribute msg
nameBottomPadding =
    paddingEach { zeroes | bottom = 3 }


postRow : Settings -> Feed Msg -> Bool -> ActivityLog -> Element Msg
postRow settings feed isToplevel log =
    let
        baseFontSize =
            settings.fontSize

        cw =
            settings.columnWidth

        pad =
            5

        colpad =
            if isToplevel then
                columnPadding

            else
                5

        cwp =
            cw - 2 * colpad - 6

        colw =
            width <| px cw

        colwp =
            width <| px cwp

        mediaw =
            colwp

        post =
            log.post

        user =
            post.user

        username =
            user.username

        actuser =
            log.actuser

        actusername =
            actuser.username

        ( repostString, iconUrl ) =
            if log.type_ == "repost" then
                if post.is_quote then
                    ( "quoted", chars.leftCurlyQuote )

                else
                    ( "reposted", iconUrls.refresh )

            else if post.is_reply then
                ( "commented", iconUrls.comment )

            else
                ( "", "" )
    in
    row
        [ colw
        , paddingEach
            { zeroes
                | left = colpad
                , right = colpad
            }
        , Border.widthEach <|
            if isToplevel then
                { zeroes | bottom = 2 }

            else
                zeroes
        , Border.color styleColors.border
        ]
        [ column []
            [ if repostString == "" then
                text ""

              else
                row
                    [ Border.widthEach { zeroes | bottom = 1 }
                    , Border.color styleColors.border
                    , colwp
                    ]
                    [ row
                        [ paddingEach { zeroes | top = 5, bottom = 5 }
                        ]
                        [ if iconUrl == chars.leftCurlyQuote then
                            row
                                [ Font.bold
                                , paddingEach
                                    { zeroes
                                        | top =
                                            round <| 0.9 * baseFontSize
                                    }
                                , height (px <| round <| 0.9 * baseFontSize)
                                , Font.size (round <| 1.8 * baseFontSize)
                                ]
                                [ text chars.leftCurlyQuote ]

                          else
                            heightImage iconUrl "refresh" 10
                        , newTabLink ("https://gab.com/" ++ actusername)
                            (" " ++ actuser.name ++ " " ++ repostString)
                        ]
                    ]
            , postUserRow colwp settings.here post
            , row []
                [ Element.textColumn
                    [ paragraphSpacing baseFontSize
                    , colwp
                    ]
                  <|
                    case post.body_html of
                        Nothing ->
                            htmlBodyElements baseFontSize <|
                                newlinesToPs post.body

                        Just html ->
                            htmlBodyElements baseFontSize html
                ]
            , row []
                [ column [ colwp ] <|
                    case post.attachment of
                        MediaAttachment records ->
                            List.map (mediaRow mediaw) records

                        _ ->
                            [ text "" ]
                ]
            , row []
                [ column
                    [ paddingEach { zeroes | bottom = 5 } ]
                    [ row
                        [ paddingEach { zeroes | left = 5 }
                        , Background.color styleColors.quotedPost
                        , Border.width 1
                        , Border.color styleColors.quotedPostBorder
                        ]
                        [ case post.related of
                            RelatedPosts { parent } ->
                                case parent of
                                    Nothing ->
                                        text ""

                                    Just parentPost ->
                                        postRow
                                            { settings | columnWidth = cwp - 10 }
                                            feed
                                            False
                                            { log
                                                | post =
                                                    { parentPost
                                                        | body_html =
                                                            if post.is_quote then
                                                                parentPost.body_html

                                                            else
                                                                parentPost.body_html_summary
                                                        , body =
                                                            truncatePost parentPost.body
                                                    }
                                                , type_ = "post"
                                            }
                        ]
                    ]
                ]
            , if not isToplevel then
                text ""

              else
                interactionRow baseFontSize colwp feed.feedType post
            ]
        ]


postUserRow : Attribute Msg -> Zone -> Post -> Element Msg
postUserRow colwp here post =
    let
        user =
            post.user

        username =
            user.username
    in
    row
        [ Font.bold
        , paddingEach { zeroes | top = 5 }
        , colwp
        ]
        [ column
            [ paddingEach
                { zeroes | right = 5 }
            ]
            [ row []
                [ styledLink True
                    [ titleAttribute user.name ]
                    (userUrl user)
                  <|
                    heightImage user.picture_url username 40
                ]
            ]
        , column []
            [ row [ nameBottomPadding ]
                [ newTabLink (userUrl user) <|
                    embiggen user.name
                , text <| " (" ++ username ++ ")"
                ]
            , case post.group of
                Just { id, title } ->
                    let
                        url =
                            "http://gab.com/groups/" ++ id
                    in
                    row [ nameBottomPadding ]
                        [ newTabLink url <| "Group: " ++ title ]

                _ ->
                    case post.topic of
                        Just { id, title } ->
                            let
                                url =
                                    "http://gab.com/topic/" ++ id
                            in
                            row [ nameBottomPadding ]
                                [ newTabLink url <| "Topic: " ++ title ]

                        _ ->
                            text ""
            , let
                url =
                    "https://gab.com/"
                        ++ username
                        ++ "/posts/"
                        ++ String.fromInt post.id
              in
              row []
                [ newTabLink url <|
                    iso8601ToString here post.created_at
                ]
            ]
        ]


userNameLink : User -> Element el
userNameLink user =
    newTabLink (userUrl user) user.name


notificationDescriptionLine : User -> String -> Maybe Post -> String -> Element Msg
notificationDescriptionLine user middle maybePost postName =
    row []
        [ userNameLink user
        , text middle
        , case maybePost of
            Nothing ->
                text postName

            Just post ->
                newTabLink (postUrl post) postName
        , text "."
        ]


postCreatedLink : Post -> Zone -> Element Msg
postCreatedLink post here =
    newTabLink (postUrl post) <| iso8601ToString here post.created_at


notificationTypeToDescription : NotificationType -> Bool -> Notification -> List User -> Element Msg
notificationTypeToDescription typ isComment notification otherUsers =
    -- "comment", "follow", "like", "mention", "repost", "comment-reply"
    let
        postOrComment =
            if isComment then
                "comment"

            else
                "post"

        maybePost =
            notification.post

        actuser =
            notification.actuser

        otherUsersString =
            if otherUsers == [] then
                ""

            else
                let
                    len =
                        List.length otherUsers
                in
                " and "
                    ++ String.fromInt len
                    ++ (if len == 1 then
                            " others"

                        else
                            " others"
                       )
    in
    case typ of
        LikeNotification ->
            notificationDescriptionLine actuser
                (otherUsersString ++ " liked ")
                maybePost
                ("your " ++ postOrComment)

        RepostNotification ->
            notificationDescriptionLine actuser
                (otherUsersString ++ " reposted ")
                maybePost
                ("your " ++ postOrComment)

        FollowNotification ->
            notificationDescriptionLine actuser
                (otherUsersString ++ " followed you")
                Nothing
                ""

        MentionNotification ->
            notificationDescriptionLine actuser
                " mentioned you "
                maybePost
                -- Sometimes this should be "comment"
                ("in a " ++ postOrComment)

        UnknownNotification "comment" ->
            notificationDescriptionLine actuser
                " commented on your "
                maybePost
                postOrComment

        UnknownNotification "comment-reply" ->
            notificationDescriptionLine actuser
                " replied to your "
                maybePost
                postOrComment

        UnknownNotification message ->
            case maybePost of
                Nothing ->
                    text notification.message

                Just post ->
                    newTabLink (postUrl post) notification.message


style : String -> Html msg
style css =
    Html.node "style" [] [ Html.text css ]


gangedTypes : List NotificationType
gangedTypes =
    [ LikeNotification, RepostNotification, FollowNotification ]


trimComments : List ActivityLog -> List ActivityLog
trimComments data =
    let
        isSameCommentedPost post1 parent1Id log2 =
            let
                post2 =
                    log2.post
            in
            ("post" == log2.type_)
                && post2.is_reply
                && (case post2.related of
                        RelatedPosts { parent } ->
                            case parent of
                                Nothing ->
                                    False

                                Just parent2 ->
                                    parent2.id == parent1Id
                   )

        loop rest res =
            case rest of
                [] ->
                    List.reverse res

                head :: tail ->
                    if head.type_ /= "post" then
                        loop tail (head :: res)

                    else
                        let
                            post =
                                head.post
                        in
                        if not post.is_reply then
                            loop tail (head :: res)

                        else
                            case post.related of
                                RelatedPosts { parent } ->
                                    case parent of
                                        Nothing ->
                                            loop tail res

                                        Just p ->
                                            loop
                                                (LE.filterNot
                                                    (isSameCommentedPost post p.id)
                                                    tail
                                                )
                                                (head :: res)
    in
    loop data []


ungangNotifications : List GangedNotification -> List Notification
ungangNotifications data =
    let
        loop rest res =
            case rest of
                [] ->
                    List.reverse res

                head :: tail ->
                    loop tail <|
                        let
                            notification =
                                head.notification
                        in
                        List.concat
                            [ List.map
                                (\user ->
                                    { notification | actuser = user }
                                )
                                head.users
                            , notification :: res
                            ]
    in
    loop data []


gangNotifications : List Notification -> List GangedNotification
gangNotifications data =
    let
        notesAreSimilar note1 note2 =
            if
                (note1.type_ == note2.type_)
                    && List.member note1.type_ gangedTypes
            then
                case note1.post of
                    Nothing ->
                        note2.post == Nothing

                    Just post1 ->
                        case note2.post of
                            Nothing ->
                                False

                            Just post2 ->
                                post1.id == post2.id

            else
                False

        loop : List Notification -> List GangedNotification -> List GangedNotification
        loop rest res =
            case rest of
                [] ->
                    List.reverse res

                head :: tail ->
                    let
                        similars =
                            List.filter (notesAreSimilar head) tail

                        users =
                            List.map .actuser similars

                        ganged =
                            GangedNotification head <|
                                List.reverse users
                    in
                    loop (LE.filterNot (notesAreSimilar head) tail)
                        (ganged :: res)
    in
    loop data []


notificationRow : Settings -> Bool -> GangedNotification -> Element Msg
notificationRow settings isToplevel gangedNotification =
    let
        baseFontSize =
            settings.fontSize

        here =
            settings.here

        notification =
            gangedNotification.notification

        otherUsers =
            gangedNotification.users

        pad =
            5

        colpad =
            if isToplevel then
                columnPadding

            else
                5

        cw =
            settings.columnWidth

        cwp =
            cw - 2 * colpad - 6

        colw =
            width <| px cw

        colwp =
            width <| px cwp

        maybePost =
            notification.post

        actuser =
            notification.actuser

        actusername =
            actuser.username

        maybeParent =
            case maybePost of
                Just post ->
                    case post.related of
                        RelatedPosts { parent } ->
                            parent

                Nothing ->
                    Nothing
    in
    row
        [ colw
        , paddingEach
            { zeroes
                | left = colpad
                , right = colpad
            }
        , Border.widthEach <|
            if isToplevel then
                { zeroes | bottom = 2 }

            else
                zeroes
        , Border.color styleColors.border
        ]
        [ column []
            [ row
                [ paddingEach { zeroes | top = 5, bottom = 5 }
                , Font.bold
                ]
                [ notificationTypeToDescription notification.type_
                    (maybeParent /= Nothing)
                    notification
                    otherUsers
                ]
            , row []
                [ case maybePost of
                    Nothing ->
                        text ""

                    Just post ->
                        Element.textColumn
                            [ paragraphSpacing baseFontSize
                            , colwp
                            ]
                        <|
                            List.concat
                                [ [ let
                                        spacing =
                                            negate (paragraphSpacingAmount baseFontSize)

                                        css =
                                            ".moveup { margin-bottom: "
                                                ++ String.fromInt spacing
                                                ++ "px !important;"
                                    in
                                    Element.html <| style css
                                  , row
                                        [ paddingEach { zeroes | top = 4 }
                                        , Element.htmlAttribute <|
                                            Attributes.class "moveup"
                                        , Font.bold
                                        ]
                                        [ postCreatedLink post here ]
                                  ]
                                , notificationsBody settings post
                                ]
                ]
            , case maybePost of
                Nothing ->
                    text ""

                Just post ->
                    case maybeParent of
                        Just pp ->
                            notificationParentRow (cw - colpad)
                                settings
                                pp

                        Nothing ->
                            text ""
            , if not isToplevel then
                text ""

              else
                let
                    allUsers =
                        notification.actuser :: otherUsers

                    height =
                        30

                    userImage user =
                        styledLink True
                            [ titleAttribute user.name ]
                            (userUrl user)
                            (heightImage user.picture_url "" height)
                in
                row
                    [ paddingEach { zeroes | bottom = 4 }
                    , spacing 4
                    ]
                <|
                    List.map userImage allUsers
            ]
        ]


notificationParentRow : Int -> Settings -> Post -> Element Msg
notificationParentRow cw settings post =
    let
        colpad =
            5

        cwp =
            cw - 2 * colpad - 6

        colwp =
            width <| px cwp

        user =
            post.user
    in
    row [ paddingEach { zeroes | bottom = 5 } ]
        [ column
            [ colwp
            , paddingEach { zeroes | bottom = 5, left = 5 }
            , Background.color styleColors.quotedPost
            , Border.width 1
            , Border.color styleColors.quotedPostBorder
            ]
          <|
            List.concat
                [ [ postUserRow colwp settings.here post ]
                , notificationsBody settings post
                ]
        ]


truncatePost : String -> String
truncatePost body =
    String.left 200 body


notificationsBody : Settings -> Post -> List (Element Msg)
notificationsBody settings post =
    case post.body_html_summary of
        Just html ->
            htmlBodyElements settings.fontSize html

        Nothing ->
            let
                body1 =
                    truncatePost post.body

                body =
                    if
                        String.length body1
                            == String.length post.body
                    then
                        body1

                    else
                        body1 ++ "..."
            in
            htmlBodyElements settings.fontSize <|
                newlinesToPs body


type HighlightColor
    = NoHighlight
    | RedHighlight
    | GreenHighlight


highlightElement : HighlightColor -> Element msg -> Element msg
highlightElement color element =
    let
        attrs =
            case color of
                NoHighlight ->
                    []

                RedHighlight ->
                    [ Background.color colors.red ]

                GreenHighlight ->
                    [ Background.color colors.green ]
    in
    el attrs element


interactionRow : Float -> Attribute Msg -> FeedType -> Post -> Element Msg
interactionRow baseFontSize colwp feedType post =
    let
        fsize =
            round <| baseFontSize * 0.8

        onecol image ( isHighlighted, highlight ) label count buttonLabel msg =
            column [ fillWidth ]
                [ row []
                    [ if isHighlighted then
                        highlightElement highlight image

                      else
                        image
                    , text " "
                    , if label == "" then
                        text ""

                      else
                        text label
                    , text " "
                    , if count < 0 then
                        text ""

                      else
                        let
                            str =
                                chars.nbsp ++ String.fromInt count ++ chars.nbsp
                        in
                        el [ Background.color styleColors.postcountBackground ]
                            (text str)
                    ]
                    |> (if msg == Noop then
                            identity

                        else
                            standardButton buttonLabel msg
                       )
                ]

        postid =
            post.id
    in
    row
        [ paddingEach { zeroes | top = 5, bottom = 5 }
        , colwp
        , Font.size fsize
        ]
        [ onecol (heightImage iconUrls.like "upvote" fsize)
            ( post.liked, GreenHighlight )
            ""
            post.like_count
            "upvote"
            (Upvote feedType post)
        , onecol (heightImage iconUrls.dislike "downvote" fsize)
            ( post.disliked, RedHighlight )
            ""
            post.dislike_count
            "downvote"
            (Downvote feedType post)
        , onecol (heightImage iconUrls.comment "comment" fsize)
            ( False, NoHighlight )
            ""
            post.reply_count
            ""
            Noop
        , onecol (heightImage iconUrls.refresh "reposted" fsize)
            ( post.repost, GreenHighlight )
            ""
            post.repost_count
            ""
            (Repost feedType post)
        , onecol
            (column [ height (px <| fsize) ]
                [ row
                    [ Font.bold
                    , Font.size (round <| 1.5 * baseFontSize)
                    ]
                    [ text chars.leftCurlyQuote ]
                ]
            )
            ( False, NoHighlight )
            "Quote"
            -1
            ""
            Noop
        ]


mediaRow : Attribute Msg -> MediaRecord -> Element Msg
mediaRow colw record =
    row
        [ paddingEach
            { zeroes
                | top = 5
                , bottom = 5
            }
        ]
        [ standardButtonWithDontHover True
            ""
            (ShowImageDialog record.url_full)
          <|
            image
                [ colw ]
                { src = record.url_thumbnail
                , description = "image"
                }
        ]


embiggen : String -> String
embiggen string =
    if 3 <= String.length string then
        string

    else
        " " ++ string ++ " "


stringRemove : String -> String -> String
stringRemove substring string =
    String.split substring string
        |> String.concat


removeEmptyHead : List String -> List String
removeEmptyHead strings =
    case strings of
        [] ->
            []

        first :: rest ->
            if first == "" then
                rest

            else
                strings


paragraphPadding : Attribute msg
paragraphPadding =
    paddingEach <| { zeroes | top = 4, bottom = 4 }


paragraphPaddingNoTop : Attribute msg
paragraphPaddingNoTop =
    paddingEach <| { zeroes | bottom = 4 }


paragraphSpacingAmount : Float -> Int
paragraphSpacingAmount baseFontSize =
    round (0.6 * baseFontSize)


paragraphSpacing : Float -> Attribute msg
paragraphSpacing baseFontSize =
    spacing <| paragraphSpacingAmount baseFontSize


paragraphLineSpacing : Float -> Attribute msg
paragraphLineSpacing baseFontSize =
    spacing <| round (0.25 * baseFontSize)


newlinesToPs : String -> String
newlinesToPs string =
    String.split "\n\n" string
        |> String.join "<p>"
        |> String.split "\n"
        |> String.join "<br />"


htmlBodyElements : Float -> String -> List (Element Msg)
htmlBodyElements baseFontSize html =
    let
        par : List (Element Msg) -> Element Msg
        par elements =
            paragraph
                [ paragraphPadding
                , paragraphLineSpacing baseFontSize
                ]
                elements
    in
    -- may want to convert single <br/ > to row instead of paragraph.
    splitIntoParagraphs html
        |> List.map Parsers.parseElements
        |> List.map par


splitIntoParagraphs : String -> List String
splitIntoParagraphs string =
    stringRemove "</p>" string
        |> String.split "<p>"
        |> removeEmptyHead
        |> List.map (\s -> String.split "<br /><br />" s)
        |> List.concat
        |> List.map (\s -> String.split "<br />" s)
        |> List.concat


loginPage : Settings -> Element Msg
loginPage settings =
    let
        baseFontSize =
            settings.fontSize
    in
    row
        [ fillWidth
        , fontSize baseFontSize 2
        ]
        [ column [ centerX, spacing 10 ]
            [ row
                [ centerX
                , padding 20
                , fontSize baseFontSize 3
                , Font.bold
                ]
                [ text "GabDecker" ]
            , row [ centerX ]
                [ simpleLink "./" "GabDecker"
                , text " is a "
                , simpleLink "https://tweetdeck.twitter.com" "TweetDeck"
                , text "-like interface to "
                , simpleLink "https://gab.com" "Gab.com"
                , text "."
                ]
            , row [ centerX ]
                [ text "This is a work in progress." ]
            , row [ centerX ]
                [ textButton "" Login "Login" ]
            , row [ centerX ]
                [ simpleImage "images/deck-with-frog-671x425.jpg"
                    "Deck with Frog"
                    ( 671, 425 )
                ]
            , row [ centerX ]
                [ simpleLink "news/" "News" ]
            , row [ centerX ]
                [ simpleLink "api/" "Gab API Explorer" ]
            , row [ centerX ]
                [ column [ centerX, spacing 6, fontSize baseFontSize 1 ]
                    [ row [ centerX ]
                        [ text "Icons by "
                        , simpleLink "https://www.flaticon.com/authors/gregor-cresnar"
                            "Gregor Cresnar"
                        ]
                    , row [ centerX ]
                        [ text <| chars.copyright ++ " 2018 Bill St. Clair" ]
                    , row [ centerX ]
                        [ simpleLink "https://github.com/melon-love/gabdecker"
                            "GitHub"
                        ]
                    ]
                ]
            ]
        ]


standardButton : String -> Msg -> Element Msg -> Element Msg
standardButton =
    standardButtonWithDontHover False


standardButtonWithDontHover : Bool -> String -> Msg -> Element Msg -> Element Msg
standardButtonWithDontHover dontHover title msg label =
    button
        (List.concat
            [ [ Font.color styleColors.link
              , titleAttribute title
              ]
            , if dontHover then
                []

              else
                [ Element.mouseOver [ Background.color styleColors.linkHover ] ]
            ]
        )
        { onPress = Just msg
        , label = label
        }


textButton : String -> Msg -> String -> Element Msg
textButton title msg label =
    standardButton title msg (text label)


iso8601ToString : Zone -> String -> String
iso8601ToString zone iso8601 =
    case Iso8601.toTime iso8601 of
        Err _ ->
            iso8601

        Ok time ->
            timeString zone time ++ " - " ++ dateString zone time


colonToken : DF.Token
colonToken =
    DF.text ":"


spaceToken : DF.Token
spaceToken =
    DF.text " "


dateString : Zone -> Posix -> String
dateString =
    DF.format
        [ DF.dayOfMonthNumber
        , spaceToken
        , DF.monthNameAbbreviated
        , spaceToken
        , DF.yearNumber
        ]


{-| Convert a zoned time to a string in the format "HH:MM"
-}
timeString : Zone -> Posix -> String
timeString =
    DF.format
        [ DF.hourMilitaryFixed
        , colonToken
        , DF.minuteFixed
        ]


codestr : Int -> String
codestr code =
    String.fromList [ Char.fromCode code ]


storageKeys =
    { token = "token"
    , feeds = "feeds"
    , icons = "icons"
    }


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    }


keycodes =
    { enter = 13
    , escape = 27
    }


iconUrls =
    { reload = "icon/reload.svg"
    , refresh = "icon/refresh-arrow.svg"
    , close = "icon/cancel.svg"
    , like = "icon/like.svg"
    , dislike = "icon/dislike.svg"
    , settings = "icon/settings.svg"
    , user = "icon/avatar.svg"
    , home = "icon/house.svg"
    , popular = "icon/star.svg"
    , notifications = "icon/glasses.svg"
    , next = "icon/next-1.svg"
    , previous = "icon/back.svg"
    , comment = "icon/chat-2.svg"
    , notification = "icon/hand.svg"
    , save = "icon/folder.svg"
    }


optimizers =
    { mainPage =
        if optimizations.lazyMainPage then
            Lazy.lazy4 mainPage

        else
            mainPage
    , renderRowContents =
        if optimizations.lazyRenderRowContents then
            Lazy.lazy2 renderRowContents

        else
            renderRowContents
    , keyedFeedColumn =
        if optimizations.keyedFeedColumn then
            Keyed.row

        else
            keyedRow
    , feedColumn =
        if optimizations.lazyFeedColumn then
            Lazy.lazy4 feedColumn

        else
            feedColumn
    , keyedPostRow =
        if optimizations.keyedPostRow then
            Keyed.column

        else
            keyedColumn
    , postRow =
        if optimizations.lazyPostRow then
            Lazy.lazy4 postRow

        else
            postRow
    , keyedNotificationRow =
        if optimizations.keyedPostRow then
            Keyed.column

        else
            keyedColumn
    , notificationRow =
        if optimizations.lazyPostRow then
            Lazy.lazy3 notificationRow

        else
            notificationRow
    }


optimizationsToBits =
    let
        bit accessor =
            if accessor optimizations then
                "1"

            else
                "0"
    in
    bit .lazyMainPage
        ++ bit .lazyRenderRowContents
        ++ bit .keyedFeedColumn
        ++ bit .lazyFeedColumn
        ++ bit .keyedPostRow
        ++ bit .lazyPostRow


{-| These allow quick switching of all the Keyed and Lazy uses.
-}
optimizations =
    { lazyMainPage = True
    , lazyRenderRowContents = True
    , keyedFeedColumn = True
    , lazyFeedColumn = False
    , keyedPostRow = True
    , lazyPostRow = False
    }
