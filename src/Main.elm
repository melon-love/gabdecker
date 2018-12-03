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
    | OperationErrorDialog String


type alias Model =
    { showDialog : DialogType
    , dialogError : Maybe String
    , useSimulator : Bool
    , windowWidth : Int
    , windowHeight : Int
    , columnWidth : Int
    , fontSize : Float
    , maxPosts : Int
    , backend : Maybe Backend
    , key : Key
    , funnelState : PortFunnels.State
    , controlColumnState : ExpandedState
    , here : Zone
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
    , addedUserFeedName : String
    , feedTypes : List FeedType
    , nextId : Int
    , feeds : List (Feed Msg)
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
    | AddFeed
    | AddedUserFeedName String
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
                    feedTypesToFeeds defaultFontSize
                        Nothing
                        defaultColumnWidth
                        backend
                        initialFeeds
                        0
            in
            { showDialog = NoDialog
            , dialogError = Nothing
            , useSimulator = useSimulator
            , backend = backend
            , key = key
            , windowWidth = 1260
            , windowHeight = 1024
            , columnWidth = defaultColumnWidth
            , fontSize = defaultFontSize
            , maxPosts = defaultMaxPosts
            , funnelState = PortFunnels.initialState localStoragePrefix
            , controlColumnState = ContractedState
            , here = Time.utc
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
            , addedUserFeedName = ""
            , feedTypes = initialFeeds
            , nextId = List.length feeds
            , feeds = feeds
            , lastClosedFeed = Nothing
            }
    in
    model
        |> withCmds
            [ Navigation.replaceUrl key "#"
            , case token of
                Just t ->
                    Task.perform (PersistResponseToken t) Time.now

                Nothing ->
                    Cmd.none
            , localStorageSend (LocalStorage.get feedsKey) model
            , Task.perform getViewport Dom.getViewport
            , Task.perform Here Time.here
            , case savedToken of
                Nothing ->
                    Cmd.none

                Just st ->
                    Http.send ReceiveLoggedInUser <|
                        Gab.me st.token
            , Cmd.batch <|
                List.map (loadMore False) model.feeds
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


feedTypesToFeeds : Float -> Maybe String -> Int -> Maybe Backend -> List FeedType -> Int -> List (Feed Msg)
feedTypesToFeeds baseFontSize username columnWidth maybeBackend feedTypes startId =
    case maybeBackend of
        Nothing ->
            []

        Just backend ->
            List.map2 (feedTypeToFeed baseFontSize username columnWidth backend)
                feedTypes
            <|
                List.range startId (startId + List.length feedTypes - 1)


defaultColumnWidth : Int
defaultColumnWidth =
    350


feedTypeToFeed : Float -> Maybe String -> Int -> Backend -> FeedType -> Int -> Feed Msg
feedTypeToFeed baseFontSize username columnWidth backend feedType id =
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
    , description = feedTypeDescription ft baseFontSize
    , feed = { data = [], no_more = False }
    , error = Nothing
    , columnWidth = columnWidth
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


feedTypeDescription : FeedType -> Float -> Element Msg
feedTypeDescription feedType baseFontSize =
    let
        iconHeight =
            userIconHeight baseFontSize

        feedRow url elements =
            styledLink True [] url (row [] elements)
    in
    case feedType of
        HomeFeed ->
            feedRow "https://gab.com/"
                [ text "Home "
                , heightImage icons.home "Home" iconHeight
                ]

        UserFeed user ->
            feedRow ("https://gab.com/" ++ user)
                [ text user
                , text " "
                , heightImage icons.user "frob" iconHeight
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
                , heightImage icons.popular "Popular" iconHeight
                ]

        NotificationsFeed ->
            feedRow "https://gab.com/notifications"
                [ text "Notifications "
                , heightImage icons.notifications "Notifications" iconHeight
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
                            feedTypesToFeeds model.fontSize
                                model.loggedInUser
                                model.columnWidth
                                backend
                                model.feedTypes
                                0
                    in
                    { model
                        | token = Just savedToken
                        , scopes = savedToken.scope
                        , receivedScopes = savedToken.scope
                        , backend = backend
                        , nextId = List.length feeds
                        , feeds = feeds
                    }
                        |> withCmds
                            [ if backend == Nothing then
                                Cmd.none

                              else
                                Http.send ReceiveLoggedInUser <|
                                    Gab.me savedToken.token
                            , Cmd.batch <|
                                List.map (loadMore False) feeds
                            ]


receiveFeedTypes : Maybe Value -> Model -> ( Model, Cmd Msg )
receiveFeedTypes value model =
    let
        cmd =
            case model.token of
                Nothing ->
                    localStorageSend (LocalStorage.get tokenKey) model

                _ ->
                    Cmd.none
    in
    case value of
        Nothing ->
            model |> withCmd cmd

        Just v ->
            case ED.decodeFeedTypes v of
                Err _ ->
                    model |> withCmd cmd

                Ok feedTypes ->
                    { model | feedTypes = feedTypes }
                        |> withCmd cmd


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    case response of
        LocalStorage.GetResponse { key, value } ->
            if key == tokenKey && model.backend == Nothing then
                receiveToken value model

            else if key == feedsKey then
                receiveFeedTypes value model

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

        WindowResize w h ->
            { model
                | windowWidth = w
                , windowHeight = h
            }
                |> withNoCmd

        Here zone ->
            { model | here = zone } |> withNoCmd

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
            ( model
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

        MoveFeedRight feedType ->
            moveFeedRight feedType model

        AddFeed ->
            { model
                | showDialog = AddFeedDialog
                , dialogError = Nothing
                , addedUserFeedName = ""
            }
                |> withNoCmd

        AddedUserFeedName username ->
            { model | addedUserFeedName = username } |> withNoCmd

        AddNewFeed feedType ->
            addNewFeed feedType model.fontSize model

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
                { data = List.map PostFeedData logList.data
                , no_more = logList.no_more
                }


boxNotificationsLogResult : FeedResult NotificationsLog -> FeedResult (LogList FeedData)
boxNotificationsLogResult result =
    case result of
        Err err ->
            Err err

        Ok logList ->
            Ok
                { data = List.map NotificationFeedData logList.data
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
            case lg of
                PostFeedData al ->
                    al.post.id == postid

                _ ->
                    False

        modifier lg =
            case lg of
                PostFeedData al ->
                    PostFeedData { al | post = updater al.post }

                _ ->
                    lg

        updateLogList : LogList FeedData -> LogList FeedData
        updateLogList logList =
            { logList | data = LE.updateIf shouldUpdate modifier logList.data }

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
        (LocalStorage.put feedsKey <| Just value)
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
                    [ loadMore False feed
                    , saveFeeds feeds model
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
                                (feedTypeToFeed baseFontSize
                                    model.loggedInUser
                                    model.columnWidth
                                    backend
                                    feedType
                                    model.nextId
                                )


loadAll : Model -> ( Model, Cmd Msg )
loadAll model =
    let
        getone feed res =
            loadMore False feed
                :: res
    in
    model |> withCmds (List.foldr getone [] model.feeds)


feedBefore : Feed Msg -> String
feedBefore feed =
    case LE.last feed.feed.data of
        Nothing ->
            ""

        Just log ->
            case log of
                PostFeedData d ->
                    d.published_at

                NotificationFeedData d ->
                    d.id

                _ ->
                    ""


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

                _ ->
                    model |> withNoCmd
    in
    let
        ( id, feeds ) =
            updateFeeds (not scrollToTop) feedType result mdl.feeds
    in
    { mdl | feeds = feeds }
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
                        (LocalStorage.put tokenKey Nothing)
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


updateFeed : Bool -> FeedResult (LogList FeedData) -> Feed Msg -> Feed Msg
updateFeed appendResult result feed =
    case result of
        Err err ->
            { feed | error = Just err }

        Ok logList ->
            let
                feed_feed =
                    feed.feed
            in
            { feed
                | feed =
                    { feed_feed
                        | data =
                            if appendResult then
                                List.append feed_feed.data logList.data

                            else
                                logList.data
                        , no_more = logList.no_more
                    }
            }


{-| (<same id>, <published\_at compare>)
-}
type alias PostOrder =
    ( Bool, Order )


tokenKey : String
tokenKey =
    "token"


feedsKey : String
feedsKey =
    "feeds"


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
    let
        body =
            pageBody model
    in
    { title = pageTitle
    , body =
        [ Element.layoutWith
            { options = [ focusStyle ] }
            (case model.showDialog of
                AddFeedDialog ->
                    [ Element.inFront <| addFeedDialog model ]

                OperationErrorDialog err ->
                    [ Element.inFront <| operationErrorDialog err model ]

                _ ->
                    []
            )
            body
        ]
    }


defaultMaxPosts : Int
defaultMaxPosts =
    100


defaultFontSize : Float
defaultFontSize =
    15


fontSize : Float -> Float -> Element.Attr decorative msg
fontSize baseFontSize scale =
    Font.size <| round (scale * baseFontSize)


pageBody : Model -> Element Msg
pageBody model =
    case model.backend of
        Nothing ->
            loginPage model

        Just _ ->
            mainPage model


fillWidth : Attribute msg
fillWidth =
    width Element.fill


mainPage : Model -> Element Msg
mainPage model =
    let
        ccw =
            controlColumnWidth model.controlColumnState model.fontSize

        contentWidth =
            model.columnWidth * List.length model.feeds
    in
    row
        [ fontSize model.fontSize 1
        , Border.widthEach
            { zeroes
                | left = 5
                , right =
                    if [] == model.feeds then
                        5

                    else
                        0
            }
        , Border.color styleColors.border
        ]
        [ column []
            [ row [ height <| px model.windowHeight ]
                [ controlColumn ccw model -- Doesn't work in Brave or Safari
                ]
            ]
        , column []
            [ row
                [ height <| px model.windowHeight
                , Element.scrollbarY
                , width <| px (min (model.windowWidth - ccw) contentWidth)
                ]
                (List.map (feedColumn model.windowHeight model.fontSize model.here)
                    model.feeds
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


operationErrorDialog : String -> Model -> Element Msg
operationErrorDialog err model =
    let
        iconHeight =
            userIconHeight model.fontSize
    in
    column
        [ Border.width 5
        , centerX
        , centerY
        , Background.color colors.verylightgray
        , paddingEach { top = 10, bottom = 20, left = 20, right = 20 }
        ]
        [ let
            closeIcon =
                heightImage icons.close "Close" iconHeight
          in
          row [ width Element.fill ]
            [ row
                [ centerX
                , centerY
                , Font.bold
                , fontSize model.fontSize 1.5
                ]
                [ el [ Font.color colors.red ] <| text "Operation Error " ]
            , el [ alignRight ]
                (standardButton "Close Dialog" CloseDialog closeIcon)
            ]
        , row [ paddingEach { zeroes | top = 20 } ]
            [ text err ]
        ]


addFeedDialog : Model -> Element Msg
addFeedDialog model =
    let
        iconHeight =
            userIconHeight model.fontSize

        addButton feedType =
            el [ Font.size <| 7 * iconHeight // 4 ] <|
                textButton "Add Feed"
                    (AddNewFeed feedType)
                    "+"

        addUserFeedRow =
            { label = "User: "
            , element =
                Input.text [ width <| px 200 ]
                    { onChange = AddedUserFeedName
                    , text = model.addedUserFeedName
                    , placeholder =
                        Just <|
                            Input.placeholder []
                                (text "username")
                    , label = Input.labelHidden "User Feed Name"
                    }
            , feedType = Just <| UserFeed model.addedUserFeedName
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
    column
        [ Border.width 5
        , centerX
        , centerY
        , Background.color colors.verylightgray
        , paddingEach { top = 10, bottom = 20, left = 20, right = 20 }
        ]
        [ let
            closeIcon =
                heightImage icons.close "Close" iconHeight
          in
          row [ width Element.fill ]
            [ row
                [ centerX
                , centerY
                , Font.bold
                , fontSize model.fontSize 1.5
                ]
                [ text "Add Feed" ]
            , el [ alignRight ]
                (standardButton "Close Dialog" CloseDialog closeIcon)
            ]
        , case model.dialogError of
            Nothing ->
                text ""

            Just err ->
                row
                    [ paddingEach { zeroes | top = 10 }
                    , Font.color colors.red
                    ]
                    [ text err ]
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
        , fontSize model.fontSize 1.5
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


type ExpandedState
    = ContractedState
    | ExpandedState


controlColumnWidth : ExpandedState -> Float -> Int
controlColumnWidth state baseFontSize =
    case state of
        ContractedState ->
            userIconHeight baseFontSize + 2 * columnPadding

        ExpandedState ->
            round (baseFontSize * 20)


controlColumn : Int -> Model -> Element Msg
controlColumn columnWidth model =
    let
        state =
            model.controlColumnState

        colw =
            width <| px columnWidth

        iconHeight =
            userIconHeight model.fontSize
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
                    }
              , Background.color styleColors.headerBackground
              , spacing 10
              ]
            , columnBorderAttributes True
            ]
        )
        [ row [ centerX ]
            [ standardButton
                "Refresh All Feeds"
                LoadAll
                (widthImage icons.reload "Refresh" iconHeight)
            ]
        , row
            [ Font.size <| 7 * iconHeight // 4
            , centerX
            ]
            [ standardButton "Add New Feed" AddFeed (text "+") ]
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


columnBorderWidth : Int
columnBorderWidth =
    5


feedColumn : Int -> Float -> Zone -> Feed Msg -> Element Msg
feedColumn windowHeight baseFontSize here feed =
    Lazy.lazy4 feedColumnInternal windowHeight baseFontSize here feed


feedColumnInternal : Int -> Float -> Zone -> Feed Msg -> Element Msg
feedColumnInternal windowHeight baseFontSize here feed =
    let
        colw =
            width <| px feed.columnWidth

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
                        heightImage icons.close "Close" iconHeight

                    prevIcon =
                        heightImage icons.previous "Move Left" iconHeight

                    nextIcon =
                        heightImage icons.next "Move Right" iconHeight
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
                    , row [ centerX ]
                        [ standardButton
                            "Refresh Feed"
                            (LoadMore False feed)
                            (heightImage icons.reload "Refresh" iconHeight)
                        , text " "
                        , feed.description
                        ]
                    , el [ alignRight ]
                        (standardButton "Close Feed" (CloseFeed feed) closeIcon)
                    ]
                ]
            ]
        , row []
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
                        if feed.feedType == NotificationsFeed then
                            gangNotifications feed.feed.data

                        else
                            feed.feed.data

                    rows =
                        List.map (feedDataRow baseFontSize feed True here) <|
                            data
                in
                List.concat
                    [ if False then
                        undoneRows

                      else
                        []
                    , rows
                    , [ moreRow colw feed ]
                    ]
            ]
        ]


{-| Not currently used. Saved for the next incomplete column
-}
undoneRows : List (Element Msg)
undoneRows =
    [ row
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
    ]


moreRow : Attribute Msg -> Feed Msg -> Element Msg
moreRow colw feed =
    if feed.feed.no_more then
        text ""

    else if feed.feed.data == [] then
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


feedDataRow : Float -> Feed Msg -> Bool -> Zone -> FeedData -> Element Msg
feedDataRow baseFontSize feed isToplevel here data =
    case data of
        PostFeedData log ->
            postRow baseFontSize feed isToplevel here log

        GangedNotificationData notification ->
            notificationRow baseFontSize feed isToplevel here notification

        _ ->
            text ""


postRow : Float -> Feed Msg -> Bool -> Zone -> ActivityLog -> Element Msg
postRow baseFontSize feed isToplevel here log =
    let
        cw =
            feed.columnWidth

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
                ( "reposted", icons.refresh )

            else if post.is_reply then
                ( "commented", icons.comment )

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
                        [ heightImage iconUrl "refresh" 10
                        , newTabLink ("https://gab.com/" ++ actusername)
                            (" " ++ actuser.name ++ " " ++ repostString)
                        ]
                    ]
            , postUserRow colwp here post
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
                                        postRow baseFontSize
                                            { feed | columnWidth = cwp - 10 }
                                            False
                                            here
                                        <|
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
                interactionRow baseFontSize colwp feed post
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
                [ heightImage user.picture_url username 40 ]
            ]
        , column []
            [ row [ nameBottomPadding ]
                [ newTabLink ("https://gab.com/" ++ username) <|
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


gangNotifications : List FeedData -> List FeedData
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

        loop : List Notification -> List FeedData -> List FeedData
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
                            GangedNotificationData
                                (GangedNotification head <|
                                    List.reverse users
                                )
                    in
                    loop (LE.filterNot (notesAreSimilar head) tail)
                        (ganged :: res)

        getNote feedData =
            case feedData of
                NotificationFeedData note ->
                    Just note

                _ ->
                    Nothing

        notes =
            List.filterMap getNote data
    in
    loop notes []


notificationRow : Float -> Feed Msg -> Bool -> Zone -> GangedNotification -> Element Msg
notificationRow baseFontSize feed isToplevel here gangedNotification =
    let
        notification =
            gangedNotification.notification

        otherUsers =
            gangedNotification.users

        cw =
            feed.columnWidth

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
                                , notificationsBody baseFontSize post
                                ]
                ]
            , case maybePost of
                Nothing ->
                    text ""

                Just post ->
                    case maybeParent of
                        Just pp ->
                            notificationParentRow (cw - colpad)
                                baseFontSize
                                here
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


notificationParentRow : Int -> Float -> Zone -> Post -> Element Msg
notificationParentRow cw baseFontSize here post =
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
                [ [ postUserRow colwp here post ]
                , notificationsBody baseFontSize post
                ]
        ]


truncatePost : String -> String
truncatePost body =
    String.left 200 body


notificationsBody : Float -> Post -> List (Element Msg)
notificationsBody baseFontSize post =
    case post.body_html_summary of
        Just html ->
            htmlBodyElements baseFontSize html

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
            htmlBodyElements baseFontSize <|
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


interactionRow : Float -> Attribute Msg -> Feed Msg -> Post -> Element Msg
interactionRow baseFontSize colwp feed post =
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

        feedType =
            feed.feedType

        postid =
            post.id
    in
    row
        [ paddingEach { zeroes | top = 5, bottom = 5 }
        , colwp
        , Font.size fsize
        ]
        [ onecol (heightImage icons.like "upvote" fsize)
            ( post.liked, GreenHighlight )
            ""
            post.like_count
            "upvote"
            (Upvote feedType post)
        , onecol (heightImage icons.dislike "downvote" fsize)
            ( post.disliked, RedHighlight )
            ""
            post.dislike_count
            "downvote"
            (Downvote feedType post)
        , onecol (heightImage icons.comment "comment" fsize)
            ( False, NoHighlight )
            ""
            post.reply_count
            ""
            Noop
        , onecol (heightImage icons.refresh "reposted" fsize)
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


mediaRow : Attribute msg -> MediaRecord -> Element msg
mediaRow colw record =
    row
        [ paddingEach
            { zeroes
                | top = 5
                , bottom = 5
            }
        ]
        [ image
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


loginPage : Model -> Element Msg
loginPage model =
    let
        baseFontSize =
            model.fontSize
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
standardButton title msg label =
    button
        [ Font.color styleColors.link
        , Element.mouseOver [ Background.color styleColors.linkHover ]
        , titleAttribute title
        ]
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


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    }


icons =
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
    }
