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
        , Post
        , PostForm
        , RelatedPosts(..)
        , RequestParts
        , SavedToken
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
        , widthImage
        )
import GabDecker.EncodeDecode as ED
import GabDecker.Parsers as Parsers
import GabDecker.Types as Types
    exposing
        ( ApiError
        , Feed
        , FeedGetter(..)
        , FeedType(..)
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


type alias Model =
    { showDialog : Bool
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
    = NoOp
    | CloseDialog
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | ReceiveLoggedInUser (Result Http.Error User)
    | PersistResponseToken ResponseToken Posix
    | ProcessLocalStorage Value
    | WindowResize Int Int
    | Here Zone
    | Login
    | LoadMore String (Feed Msg)
    | LoadAll
    | CloseFeed (Feed Msg)
    | MoveFeedLeft FeedType
    | MoveFeedRight FeedType
    | AddFeed
    | AddedUserFeedName String
    | AddNewFeed FeedType
    | RefreshFeed FeedType (Result ApiError ActivityLogList)
    | ReceiveFeed FeedType (Result ApiError ActivityLogList)


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
    [ HomeFeed, UserFeed "a", PopularFeed ]


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
                    ( Nothing, [ "read" ] )

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
                        defaultColumnWidth
                        backend
                        initialFeeds
                        0
            in
            { showDialog = False
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
                List.map feedGetMore model.feeds
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


feedTypesToFeeds : Maybe String -> Int -> Maybe Backend -> List FeedType -> Int -> List (Feed Msg)
feedTypesToFeeds username columnWidth maybeBackend feedTypes startId =
    case maybeBackend of
        Nothing ->
            []

        Just backend ->
            List.map2 (feedTypeToFeed username columnWidth backend)
                feedTypes
            <|
                List.range startId (startId + List.length feedTypes - 1)


defaultColumnWidth : Int
defaultColumnWidth =
    350


feedTypeToFeed : Maybe String -> Int -> Backend -> FeedType -> Int -> Feed Msg
feedTypeToFeed username columnWidth backend feedType id =
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
    , description = feedTypeDescription ft
    , feed = { data = [], no_more = False }
    , error = Nothing
    , columnWidth = columnWidth
    , id = id
    }


feedTypeDescription : FeedType -> Element Msg
feedTypeDescription feedType =
    case feedType of
        HomeFeed ->
            newTabLink "https://gab.com/" "Home"

        UserFeed user ->
            newTabLink ("https://gab.com/" ++ user) <| "User: " ++ user

        -- Need to look up group name
        GroupFeed groupid ->
            text <| "Group: " ++ groupid

        -- Need to look up topic name
        TopicFeed topicid ->
            text <| "Topic: " ++ topicid

        PopularFeed ->
            newTabLink "https://gab.com/popular" "Popular"

        _ ->
            text "Shouldn't happen"


feedGetMore : Feed Msg -> Cmd Msg
feedGetMore feed =
    if feed.feed.no_more then
        Cmd.none

    else
        let
            receiver =
                ReceiveFeed feed.feedType
        in
        case feed.getter of
            FeedGetter f ->
                f receiver

            FeedGetterWithBefore f ->
                let
                    before =
                        case LE.last feed.feed.data of
                            Nothing ->
                                ""

                            Just log ->
                                log.published_at
                in
                f receiver before

            FeedGetterUnused ->
                Cmd.none


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
                                List.map feedGetMore feeds
                            ]


receiveFeeds : Maybe Value -> Model -> ( Model, Cmd Msg )
receiveFeeds value model =
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
                receiveFeeds value model

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
        NoOp ->
            model |> withNoCmd

        CloseDialog ->
            { model
                | showDialog = False
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

        LoadMore before feed ->
            loadMore before feed model

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
                | showDialog = True
                , dialogError = Nothing
                , addedUserFeedName = ""
            }
                |> withNoCmd

        AddedUserFeedName username ->
            { model | addedUserFeedName = username } |> withNoCmd

        AddNewFeed feedType ->
            addNewFeed feedType model

        RefreshFeed feedType result ->
            receiveFeed True feedType result model

        ReceiveFeed feedType result ->
            receiveFeed False feedType result model


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


addNewFeed : FeedType -> Model -> ( Model, Cmd Msg )
addNewFeed feedType model =
    let
        addit feed =
            let
                feeds =
                    List.concat [ model.feeds, [ feed ] ]
            in
            { model
                | feeds = feeds
                , showDialog = False
                , dialogError = Nothing
                , lastClosedFeed = Nothing
                , nextId = model.nextId + 1
            }
                |> withCmds
                    [ feedGetMore feed
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
                                (feedTypeToFeed model.loggedInUser
                                    model.columnWidth
                                    backend
                                    feedType
                                    model.nextId
                                )


loadAll : Model -> ( Model, Cmd Msg )
loadAll model =
    let
        getone feed res =
            (loadMore "" feed model
                |> Tuple.second
            )
                :: res
    in
    model |> withCmds (List.foldr getone [] model.feeds)


loadMore : String -> Feed Msg -> Model -> ( Model, Cmd Msg )
loadMore before feed model =
    case feed.getter of
        FeedGetter getter ->
            model |> withCmd (getter (RefreshFeed feed.feedType))

        FeedGetterWithBefore getter ->
            let
                tagger =
                    if before == "" then
                        RefreshFeed

                    else
                        ReceiveFeed
            in
            model |> withCmd (getter (tagger feed.feedType) before)

        FeedGetterUnused ->
            model |> withNoCmd


receiveFeed : Bool -> FeedType -> Result ApiError ActivityLogList -> Model -> ( Model, Cmd Msg )
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
            updateFeeds model.maxPosts feedType result mdl.feeds
    in
    { mdl | feeds = feeds }
        |> withCmds
            [ cmd
            , if scrollToTop && id /= "" then
                Task.attempt (\_ -> NoOp) (Dom.setViewportOf id 0 0)

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


updateFeeds : Int -> FeedType -> Result ApiError ActivityLogList -> List (Feed Msg) -> ( String, List (Feed Msg) )
updateFeeds maxPosts feedType result feeds =
    let
        loop tail res =
            case tail of
                [] ->
                    ( "", List.reverse res )

                feed :: rest ->
                    if feed.feedType == feedType then
                        ( columnId feed.id
                        , List.concat
                            [ List.reverse res
                            , [ updateFeed maxPosts result feed ]
                            , rest
                            ]
                        )

                    else
                        loop rest (feed :: res)
    in
    loop feeds []


updateFeed : Int -> Result ApiError ActivityLogList -> Feed Msg -> Feed Msg
updateFeed maxPosts result feed =
    case result of
        Err err ->
            { feed | error = Just err }

        Ok activities ->
            { feed
                | error = Nothing
                , feed =
                    { data = mergeFeedData maxPosts activities.data feed
                    , no_more = activities.no_more
                    }
            }


{-| (<same id>, <published\_at compare>)
-}
type alias PostOrder =
    ( Bool, Order )


merge : (a -> a -> PostOrder) -> List a -> List a -> List a
merge comparef a b =
    let
        loop : List a -> List a -> List a -> List a
        loop ar br res =
            case ( ar, br ) of
                ( [], _ ) ->
                    List.concat [ List.reverse res, br ]

                ( _, [] ) ->
                    List.concat [ List.reverse res, ar ]

                ( af :: atail, bf :: btail ) ->
                    case comparef af bf of
                        ( True, _ ) ->
                            loop atail btail (af :: res)

                        ( False, GT ) ->
                            loop ar btail (bf :: res)

                        _ ->
                            loop atail br (af :: res)
    in
    loop a b []


compareActivityLogs : ActivityLog -> ActivityLog -> PostOrder
compareActivityLogs a b =
    if a.id == b.id then
        ( True, EQ )

    else
        let
            pa =
                a.published_at

            pb =
                b.published_at
        in
        if pa > pb then
            ( False, LT )

        else if pa == pb then
            ( False, EQ )

        else
            ( False, GT )


mergeFeedData : Int -> List ActivityLog -> Feed Msg -> List ActivityLog
mergeFeedData maxPosts data feed =
    case feed.feedType of
        PopularFeed ->
            data

        _ ->
            let
                res =
                    merge compareActivityLogs data feed.feed.data
            in
            case ( List.head data, List.head res ) of
                ( Just d, Just r ) ->
                    if d.id == r.id then
                        List.take maxPosts res

                    else
                        res

                _ ->
                    res


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
            (if model.showDialog then
                [ Element.inFront <| dialog model ]

             else
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


dialog : Model -> Element Msg
dialog model =
    let
        iconHeight =
            userIconHeight model.fontSize

        addButton feedType =
            el [ Font.size <| 7 * iconHeight // 4 ] <|
                textButton "+"
                    "Add Feed"
                    (AddNewFeed feedType)

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
                    textButton label
                        "Add Feed"
                        (AddNewFeed feedType)
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
                (standardButton closeIcon "Close Feed" CloseDialog)
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
                (widthImage icons.reload "Refresh" iconHeight)
                "Refresh All Feeds"
                LoadAll
            ]
        , row
            [ Font.size <| 7 * iconHeight // 4
            , centerX
            ]
            [ standardButton (text "+") "Add New Feed" AddFeed ]
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
                        [ standardButton prevIcon
                            "Move Feed Left"
                            (MoveFeedLeft feed.feedType)
                        , text " "
                        , standardButton nextIcon
                            "Move Feed Right"
                            (MoveFeedRight feed.feedType)
                        ]
                    , row [ centerX ]
                        [ standardButton
                            (heightImage icons.reload "Refresh" iconHeight)
                            "Refresh Feed"
                            (LoadMore "" feed)
                        , text " "
                        , feed.description
                        ]
                    , el [ alignRight ]
                        (standardButton closeIcon "Close Feed" (CloseFeed feed))
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
                List.concat
                    [ List.map
                        (postRow baseFontSize feed.columnWidth True here)
                        feed.feed.data
                    , [ moreRow colw feed ]
                    ]
            ]
        ]


moreRow : Attribute Msg -> Feed Msg -> Element Msg
moreRow colw feed =
    if feed.feed.no_more then
        text ""

    else
        case LE.last feed.feed.data of
            Nothing ->
                text ""

            Just log ->
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
                      in
                      textButton (nbsps ++ "Load More" ++ nbsps)
                        ""
                        (LoadMore log.published_at feed)
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


postRow : Float -> Int -> Bool -> Zone -> ActivityLog -> Element Msg
postRow baseFontSize cw isToplevel here log =
    let
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

        repost =
            log.type_ == "repost"
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
            [ if not repost then
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
                        [ heightImage icons.refresh "refresh" 10
                        , newTabLink ("https://gab.com/" ++ actusername)
                            (" " ++ actuser.name ++ " reposted")
                        ]
                    ]
            , row
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
            , row []
                [ Element.textColumn
                    [ paragraphSpacing baseFontSize
                    , colwp
                    ]
                  <|
                    case post.body_html of
                        Nothing ->
                            htmlBodyElements baseFontSize <| newlinesToPs post.body

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
                [ if not post.is_quote then
                    text ""

                  else
                    column
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
                                                (cwp - 10)
                                                False
                                                here
                                            <|
                                                { log
                                                    | post = parentPost
                                                    , type_ = "post"
                                                }
                            ]
                        ]
                ]
            , if not isToplevel then
                text ""

              else
                interactionRow baseFontSize colwp post
            ]
        ]


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


interactionRow : Float -> Attribute Msg -> Post -> Element Msg
interactionRow baseFontSize colwp post =
    let
        fsize =
            round <| baseFontSize * 0.8

        onecol image ( isHighlighted, highlight ) label count =
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
                ]
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
        , onecol (heightImage icons.dislike "downvote" fsize)
            ( post.disliked, RedHighlight )
            ""
            post.dislike_count
        , onecol (heightImage icons.comment "comment" fsize)
            ( False, NoHighlight )
            ""
            post.reply_count
        , onecol (heightImage icons.refresh "reposted" fsize)
            ( post.repost, GreenHighlight )
            ""
            post.repost_count
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


paragraphSpacing : Float -> Attribute msg
paragraphSpacing baseFontSize =
    spacing <| round (0.6 * baseFontSize)


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
                [ Element.clipY
                , paragraphPadding

                -- This works nicely in Chrome, but not in other browsers.
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
                [ textButton "Login" "" Login ]
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


standardButton : Element Msg -> String -> Msg -> Element Msg
standardButton label title msg =
    button
        [ Font.color styleColors.link
        , Element.mouseOver [ Background.color styleColors.linkHover ]
        , titleAttribute title
        ]
        { onPress = Just msg
        , label = label
        }


textButton : String -> String -> Msg -> Element Msg
textButton label title msg =
    standardButton (text label) title msg


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
    }
