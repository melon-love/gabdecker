---------------------------------------------------------------------
--
-- Main.elm
-- GabDecker top-level
-- Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
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
import CustomElement.TextAreaTracker as Tracker
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
import File exposing (File)
import File.Select as Select
import Gab
import Gab.EncodeDecode as ED
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , Attachment(..)
        , Embed
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
        ( circularHeightImage
        , circularHeightImageWithAttributes
        , circularRadiusAttribute
        , colors
        , darkStyle
        , defaultColumnWidth
        , defaultFontSize
        , defaultSettings
        , defaultStyles
        , getIconUrl
        , heightImage
        , lightStyle
        , newTabLink
        , simpleImage
        , simpleLink
        , styledLink
        , widthImage
        )
import GabDecker.EncodeDecode as ED
import GabDecker.Parsers as Parsers exposing (ReplaceOrPrefix(..))
import GabDecker.Types as Types
    exposing
        ( ApiError
        , Feed
        , FeedData(..)
        , FeedGetter(..)
        , FeedResult
        , FeedSet
        , FeedType(..)
        , GangedNotification
        , IconUrls
        , Icons
        , LogList
        , Settings
        , SizeOption(..)
        , Style
        , StyleOption(..)
        )
import Html exposing (Html)
import Html.Attributes as Attributes exposing (class, href, rel)
import Html.Events exposing (onClick)
import Html.Parser as HP
import Http exposing (Error(..))
import IdSearch
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


type PostResponseType
    = NormalPost (Maybe Post) FeedType
    | QuotePost Post FeedType
    | CommentOnPost Post FeedType


type alias ReceiveFeedError =
    { feedType : FeedType
    , updateType : UpdateType
    , error : Http.Error
    }


type DialogType
    = NoDialog
    | NewPostDialog PostResponseType
    | AddFeedDialog
    | ImageDialog String
    | UserDialog User Bool
    | SaveFeedsDialog
    | SettingsDialog
    | FeedSetChooserDialog
    | FeedSetsDialog
    | OperationErrorDialog String
    | ReceiveFeedErrorDialog (List ReceiveFeedError)


type alias PostImage =
    { file : File
    , url : Maybe String
    , mediaId : Maybe String
    }


type alias UserChoice =
    { name : String
    , username : String
    , picture_url : String
    }


type alias UserChoiceTable =
    IdSearch.Table UserChoice


type alias DialogInputs =
    { feedTypes : List FeedType
    , feedSets : List (FeedSet Msg)
    , currentFeedSet : String
    , feedSetColumnWidths : Dict String String
    , isLastClosedFeed : Bool
    , icons : Icons
    , showDialog : DialogType
    , dialogError : Maybe String
    , dialogInput : String
    , postInput : String
    , completions : List UserChoice
    , coordinates : Maybe Tracker.Coordinates
    , postSelection : ( Int, Int )
    , postImages : List PostImage
    , feedSetsInput : String
    , fontSizeInput : String
    , columnWidthInput : String
    , fontSizeOption : Maybe SizeOption
    , columnWidthOption : Maybe SizeOption
    , autoSizeColumns : Int
    }


initialDialogInputs : DialogInputs
initialDialogInputs =
    { feedTypes = initialFeedTypes
    , feedSets = []
    , currentFeedSet = ""
    , feedSetColumnWidths = Dict.empty
    , isLastClosedFeed = False
    , icons = Types.emptyIcons
    , showDialog = NoDialog
    , dialogError = Nothing
    , dialogInput = ""
    , postInput = ""
    , completions = []
    , coordinates = Nothing
    , postSelection = ( 0, 0 )
    , postImages = []
    , feedSetsInput = ""
    , fontSizeInput = String.fromFloat defaultFontSize
    , columnWidthInput = String.fromInt defaultColumnWidth
    , fontSizeOption = Just MediumSize
    , columnWidthOption = Just MediumSize
    , autoSizeColumns = 1
    }


type alias Model =
    { useSimulator : Bool
    , styleOption : StyleOption
    , settings : Settings
    , backend : Maybe Backend
    , url : Url
    , key : Key
    , funnelState : PortFunnels.State
    , dialogInputs : DialogInputs
    , token : Maybe SavedToken
    , state : Maybe String
    , msg : Maybe String
    , replyType : String
    , reply : Maybe Value
    , redirectBackUri : String
    , scopes : List String
    , receivedScopes : List String
    , tokenAuthorization : Maybe TokenAuthorization
    , users : Dict String User
    , userChoices : UserChoiceTable
    , triggerSelection : Int
    , triggerCoordinates : Int
    , nextId : Int
    , feeds : List (Feed Msg)
    , loadingFeeds : Set String
    , loadingAll : Bool
    , lastClosedFeed : Maybe ( Feed Msg, Int )
    , scrollToFeed : Maybe FeedType
    , draggingInfo : Maybe DraggingInfo
    }


type alias DraggingInfo =
    { feedType : FeedType
    , index : Int
    , point : ( Int, Int )
    , feeds : List (Feed Msg)
    }


type UploadingState
    = NotUploading
    | Uploading
    | FinishedUploading String
    | ErrorUploading String


type UpdateType
    = MergeUpdate
    | AppendUpdate
    | FeedSetUpdate String


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
    | Logout
    | LoadMore UpdateType (Feed Msg)
    | LoadAll
    | NewPost PostResponseType
    | OnNewPostSelection Tracker.Selection
    | OnNewPostCoordinates Tracker.Coordinates
    | CompletePostChoice UserChoice
    | MakePost
    | CloseFeed (Feed Msg)
    | MoveFeedLeft FeedType
    | MoveFeedRight FeedType
    | MouseDown ( Int, Int )
    | MouseUp ( Int, Int )
    | MouseMoved ( Int, Int )
    | ShowImageDialog String
    | ShowUserDialog String
    | ScrollToFeed FeedType
    | ScrollToNewFeed FeedType
    | ScrollToViewport String (Result Dom.Error Dom.Element)
    | ScrollToControl String Dom.Element (Result Dom.Error Dom.Element)
    | ScrollToContent Dom.Element Dom.Element (Result Dom.Error Dom.Element)
    | AddFeed
    | FeedSetChooser
    | FeedSets
    | SaveFeedTypes
    | ShowSettings
    | SetStyle StyleOption
    | RestoreFeedTypes
    | DialogInput String
    | PostInput String
    | ChoosePostImage
    | ReceiveFile File
    | ReceiveFileUrl File String
    | ReceiveImageUpload File (FeedResult String)
    | RemovePostImage File
    | PostButton FeedType WhichPostButton
    | RestoreFeedSets
    | SetFeedSetsInput String
    | FontSizeInput String
    | ColumnWidthInput String
    | FontSizeOption SizeOption
    | ColumnWidthOption SizeOption
    | SetAutoSizeColumns Int
    | AutoSize
    | ShowHiddenText Bool
    | SaveSettings
    | ClearFeeds
    | NewFeedSet
    | SetCurrentFeedSet String
    | SetFeedSetColumnWidth String String
    | ReloadFeedSet String
    | SaveToFeedSet String
    | RestoreFromFeedSet String
    | DeleteFeedSet String
    | RestoreDefaultSettings
    | AddNewFeed FeedType
    | RemoveFeedError FeedType
    | ReloadFeedError FeedType UpdateType
    | Upvote FeedType Post
    | Downvote FeedType Post
    | Repost FeedType Post
    | ReceiveOperation Operation (FeedResult Success)
    | ReceivePostFeed UpdateType FeedType (FeedResult ActivityLogList)
    | ReceiveNotificationsFeed UpdateType FeedType (FeedResult NotificationsLog)
    | ReceiveUser (FeedResult User)
    | ReceivePost (FeedResult ActivityLog)


type Operation
    = UpvoteOperation FeedType String Bool
    | DownvoteOperation FeedType String Bool
    | RepostOperation FeedType String


lookupUserIconUrl : Style -> String -> Icons -> String
lookupUserIconUrl style username icons =
    case Dict.get username icons.user of
        Just icon ->
            icon

        Nothing ->
            getIconUrl style .user


storeIcons : Icons -> Model -> Cmd Msg
storeIcons icons model =
    localStorageSend
        (LocalStorage.put storageKeys.icons
            (Just <| ED.encodeIcons icons)
        )
        model


storeSettings : Settings -> Model -> Cmd Msg
storeSettings settings model =
    localStorageSend
        (LocalStorage.put storageKeys.style
            (Just <| ED.encodeSettings settings)
        )
        model


updateUser : User -> Model -> ( Model, Bool )
updateUser user model =
    let
        username =
            user.username

        url =
            user.picture_url

        users =
            model.users

        mdl =
            if Just user == Dict.get username users then
                model

            else
                let
                    userChoices =
                        IdSearch.insert
                            { name = user.name
                            , username = user.username
                            , picture_url = user.picture_url
                            }
                            model.userChoices
                in
                { model
                    | users = Dict.insert username user users
                    , userChoices = userChoices
                }
    in
    case
        LE.find
            (\feedType ->
                let
                    uname =
                        String.toLower username
                in
                case feedType of
                    UserFeed name ->
                        uname == String.toLower name

                    _ ->
                        False
            )
            (List.map .feedType mdl.feeds)
    of
        Nothing ->
            ( mdl, False )

        Just _ ->
            let
                dialogInputs =
                    mdl.dialogInputs

                icons =
                    dialogInputs.icons

                updateUserIcons () =
                    let
                        newIcons =
                            { icons
                                | user =
                                    Dict.insert username url icons.user
                            }
                    in
                    ( { mdl
                        | dialogInputs = { dialogInputs | icons = newIcons }
                      }
                    , True
                    )
            in
            case Dict.get username icons.user of
                Just u ->
                    if u == url then
                        ( mdl, False )

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
        , Events.onMouseDown <| mousePositionDecoder MouseDown
        , Events.onMouseUp <| mousePositionDecoder MouseUp
        , case model.draggingInfo of
            Just _ ->
                Events.onMouseMove <| mousePositionDecoder MouseMoved

            Nothing ->
                Sub.none
        , case model.scrollToFeed of
            Just feedType ->
                Time.every 100 (\_ -> ScrollToNewFeed feedType)

            Nothing ->
                Sub.none
        ]


localStoragePrefix : String
localStoragePrefix =
    "gab-api-example"


initialFeedTypes : List FeedType
initialFeedTypes =
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

                settings =
                    { defaultSettings
                        | loggedInUser =
                            if useSimulator then
                                Just "billstclair"

                            else
                                Nothing
                    }

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
                        initialFeedTypes
                        0
            in
            { useSimulator = useSimulator
            , styleOption = LightStyle
            , settings = settings
            , backend = backend
            , url = url
            , key = key
            , funnelState = PortFunnels.initialState localStoragePrefix
            , dialogInputs = initialDialogInputs
            , token = savedToken
            , state = state
            , msg = msg
            , replyType = "Token"
            , reply = reply
            , redirectBackUri = redirectBackUri
            , scopes = scopes
            , receivedScopes = scopes
            , tokenAuthorization = authorization
            , users = Dict.empty
            , userChoices =
                IdSearch.makeTable 3
                    (\user ->
                        [ String.toLower user.username
                        , String.toLower user.name
                        ]
                    )
            , triggerSelection = 0
            , triggerCoordinates = 0
            , nextId = List.length feeds
            , feeds = feeds
            , loadingFeeds = Set.empty
            , loadingAll = False
            , lastClosedFeed = Nothing
            , scrollToFeed = Nothing
            , draggingInfo = Nothing
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
            , localStorageSend (LocalStorage.get storageKeys.feedsets) model
            , localStorageSend (LocalStorage.get storageKeys.icons) model
            , localStorageSend (LocalStorage.get storageKeys.style) model
            , Task.perform getViewport Dom.getViewport
            , Task.perform Here Time.here
            , case savedToken of
                Nothing ->
                    if model.useSimulator then
                        loadAllCmd

                    else
                        Cmd.none

                Just st ->
                    Gab.me ReceiveLoggedInUser st.token
            ]


loadAllCmd : Cmd Msg
loadAllCmd =
    Task.perform (\_ -> LoadAll) <| Task.succeed ()


loadMoreCmd : UpdateType -> Feed Msg -> Cmd Msg
loadMoreCmd updateType feed =
    Task.perform (LoadMore updateType) <| Task.succeed feed


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
    , newPosts = 0
    , error = Nothing
    , id = id
    }


genericUserButton : Style -> Bool -> (user -> Element Msg) -> String -> String -> user -> Element Msg
genericUserButton style dontHover renderer title username user =
    standardButtonWithDontHover dontHover
        style
        (if title == "" then
            "Show user profile"

         else
            title
        )
        (ShowUserDialog username)
        (renderer user)


userButton : Style -> Bool -> (User -> Element Msg) -> String -> User -> Element Msg
userButton style dontHover renderer title user =
    genericUserButton style dontHover renderer title user.username user


userNameButton : Style -> String -> User -> Element Msg
userNameButton style title user =
    userButton style False (\u -> text u.name) title user


userIconButton : Style -> Int -> String -> User -> Element Msg
userIconButton style height title user =
    userButton style
        True
        (\u -> circularHeightImage u.picture_url "" height)
        title
        user


userUrl : User -> String
userUrl user =
    "https://gab.com/" ++ user.username


postUrl : Post -> String
postUrl post =
    "https://gab.com/"
        ++ post.user.username
        ++ "/posts/"
        ++ post.id


feedTypeDescription : Style -> Int -> FeedType -> Float -> Icons -> Element Msg
feedTypeDescription style newPosts feedType baseFontSize icons =
    let
        iconHeight =
            bigUserIconHeight baseFontSize

        feedRow url elements =
            styledLink True style [] url (row [] elements)
    in
    case feedType of
        HomeFeed ->
            feedRow "https://gab.com/"
                [ text "Home "
                , heightImageWithCount newPosts
                    (getIconUrl style .home)
                    "Home"
                    iconHeight
                ]

        UserFeed username ->
            standardButton style
                "Show user profile"
                (ShowUserDialog username)
            <|
                row []
                    [ text username
                    , text " "
                    , circularHeightImageWithCount newPosts
                        (lookupUserIconUrl style username icons)
                        ("User: " ++ username)
                        (adjustUserIconHeight iconHeight)
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
                , heightImage (getIconUrl style .popular) "Popular" iconHeight
                ]

        NotificationsFeed ->
            feedRow "https://gab.com/notifications"
                [ text "Notifications "
                , heightImageWithCount newPosts
                    (getIconUrl style .notifications)
                    "Notifications"
                    iconHeight
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
                            feedTypesToFeeds model.settings.loggedInUser
                                backend
                                model.dialogInputs.feedTypes
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
                                Gab.me ReceiveLoggedInUser savedToken.token
                            ]


restoreFeedTypes : List FeedType -> Model -> ( Model, Cmd Msg )
restoreFeedTypes feedTypes model =
    let
        icons =
            Types.emptyIcons

        dialogInputs =
            model.dialogInputs
    in
    case model.backend of
        Nothing ->
            setFeedTypes feedTypes
                { model
                    | feeds = []
                    , dialogInputs = { dialogInputs | icons = icons }
                }
                |> withNoCmd

        backend ->
            let
                feeds =
                    feedTypesToFeeds model.settings.loggedInUser
                        backend
                        feedTypes
                        0
            in
            { model
                | feeds = feeds
                , dialogInputs = { dialogInputs | icons = icons }
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

                Ok types ->
                    let
                        ( feeds, cmd2 ) =
                            case model.backend of
                                Nothing ->
                                    ( model.feeds, Cmd.none )

                                backend ->
                                    ( feedTypesToFeeds model.settings.loggedInUser
                                        backend
                                        types
                                        0
                                    , loadAllCmd
                                    )
                    in
                    setFeedTypes types
                        { model
                            | feeds = feeds
                        }
                        |> withCmds [ cmd, cmd2 ]


receiveFeedSets : Maybe Value -> Model -> ( Model, Cmd Msg )
receiveFeedSets value model =
    case value of
        Nothing ->
            model |> withNoCmd

        Just v ->
            case ED.decodeFeedSets v of
                Err _ ->
                    model |> withNoCmd

                Ok feedSets ->
                    setFeedSets feedSets model
                        |> withNoCmd


receiveStyle : Maybe Value -> Model -> ( Model, Cmd Msg )
receiveStyle value model =
    case value of
        Nothing ->
            model |> withNoCmd

        Just v ->
            case ED.decodeSettings v of
                Err _ ->
                    model |> withNoCmd

                Ok set ->
                    let
                        settings =
                            model.settings

                        dialogInputs =
                            model.dialogInputs
                    in
                    { model
                        | settings =
                            { settings
                                | columnWidth = set.columnWidth
                                , fontSize = set.fontSize
                                , styleOption = set.styleOption
                                , style = set.style
                            }
                        , dialogInputs =
                            { dialogInputs
                                | fontSizeInput = String.fromFloat set.fontSize
                                , columnWidthInput = String.fromInt set.columnWidth
                            }
                    }
                        |> adjustOptions
                        |> withNoCmd


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
                    let
                        dialogInputs =
                            model.dialogInputs
                    in
                    { model | dialogInputs = { dialogInputs | icons = icons } }
                        |> withNoCmd


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    case response of
        LocalStorage.GetResponse { key, value } ->
            if key == storageKeys.token && model.backend == Nothing then
                receiveToken value model

            else if key == storageKeys.feeds then
                receiveFeedTypes value model

            else if key == storageKeys.feedsets then
                receiveFeedSets value model

            else if key == storageKeys.icons then
                receiveIcons value model

            else if key == storageKeys.style then
                receiveStyle value model

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


setDialogInputs : (DialogInputs -> DialogInputs) -> Model -> Model
setDialogInputs setter model =
    { model | dialogInputs = setter model.dialogInputs }


setShowDialog : DialogType -> Maybe String -> Model -> Model
setShowDialog showDialog dialogError model =
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | showDialog = showDialog
                , dialogError = dialogError
            }
        )
        model


setDialogError : Maybe String -> Model -> Model
setDialogError dialogError model =
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | dialogError = dialogError
            }
        )
        model


setOnlyShowDialog : DialogType -> Model -> Model
setOnlyShowDialog showDialog model =
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | showDialog = showDialog
            }
        )
        model


setFeedTypes : List FeedType -> Model -> Model
setFeedTypes feedTypes model =
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | feedTypes = feedTypes
            }
        )
        model


updateFeedTypes : Model -> Model
updateFeedTypes model =
    setFeedTypes (List.map .feedType model.feeds) model


setFeedSets : List (FeedSet Msg) -> Model -> Model
setFeedSets feedSets model =
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | feedSets = feedSets
            }
        )
        model


setCurrentFeedSet : String -> Model -> Model
setCurrentFeedSet currentFeedSet model =
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | currentFeedSet = currentFeedSet
            }
        )
        model


setFieldSetColumnWidth : String -> String -> Model -> Model
setFieldSetColumnWidth name width model =
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | feedSetColumnWidths =
                    Dict.insert name width dialogInputs.feedSetColumnWidths
            }
        )
        model


setDialogInput : String -> Model -> Model
setDialogInput dialogInput model =
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | dialogInput = dialogInput
            }
        )
        model


setPostInput : String -> Model -> Model
setPostInput postInput mdl =
    let
        model =
            if String.contains "@" postInput then
                { mdl
                    | triggerSelection = mdl.triggerSelection + 1
                }

            else
                mdl
    in
    setDialogInputs
        (\dialogInputs ->
            { dialogInputs
                | postInput = postInput
                , completions = []
                , coordinates = Nothing
            }
        )
        model


setPostButton : FeedType -> WhichPostButton -> Model -> ( Model, Cmd Msg )
setPostButton feedType whichPost model =
    case model.dialogInputs.showDialog of
        NewPostDialog responseType ->
            let
                maybePost =
                    case responseType of
                        NormalPost mp _ ->
                            mp

                        QuotePost p _ ->
                            Just p

                        CommentOnPost p _ ->
                            Just p

                newDialog =
                    case maybePost of
                        Nothing ->
                            NewPostDialog <| NormalPost Nothing feedType

                        Just p ->
                            case whichPost of
                                NormalButton ->
                                    NewPostDialog <| NormalPost maybePost feedType

                                QuoteButton ->
                                    NewPostDialog <| QuotePost p feedType

                                CommentButton ->
                                    NewPostDialog <| CommentOnPost p feedType
            in
            setDialogInputs
                (\dialogInputs ->
                    { dialogInputs
                        | showDialog = newDialog
                        , dialogError = Nothing
                    }
                )
                model
                |> withNoCmd

        _ ->
            model |> withNoCmd


imageMimeTypes : List String
imageMimeTypes =
    [ "image/png", "image/jpg", "image/gif" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        dialogInputs =
            model.dialogInputs

        settings =
            model.settings
    in
    case msg of
        Noop ->
            model |> withNoCmd

        CloseDialog ->
            setShowDialog NoDialog Nothing model
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
                    { model
                        | backend = Nothing
                        , feeds = []
                        , msg = Just "Error getting logged-in user name."
                    }
                        |> withCmd
                            (localStorageSend
                                (LocalStorage.put storageKeys.token Nothing)
                                model
                            )

                Ok user ->
                    { model
                        | settings =
                            { settings
                                | loggedInUser = Just user.username
                            }
                    }
                        |> withCmd loadAllCmd

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
            { model
                | settings =
                    { settings | windowWidth = w, windowHeight = h }
            }
                |> withNoCmd

        Here zone ->
            { model | settings = { settings | here = zone } }
                |> withNoCmd

        Login ->
            if model.useSimulator then
                init JE.null model.url model.key

            else
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

        Logout ->
            { model
                | backend = Nothing
                , feeds = []
            }
                |> withCmd
                    (localStorageSend
                        (LocalStorage.put storageKeys.token
                            Nothing
                        )
                        model
                    )

        LoadMore updateType feed ->
            ( setLoadingFeed feed model
            , loadMore updateType feed
            )

        LoadAll ->
            loadAll model

        NewPost responseType ->
            setShowDialog (NewPostDialog responseType) Nothing model
                |> withCmd (focusId postInputId)

        OnNewPostSelection selection ->
            onNewPostSelection selection model

        OnNewPostCoordinates coordinates ->
            { model
                | dialogInputs =
                    { dialogInputs
                        | coordinates = Just coordinates
                    }
            }
                |> withNoCmd

        CompletePostChoice choice ->
            completePostChoice choice model

        MakePost ->
            makePost model

        CloseFeed feed ->
            let
                feedType =
                    feed.feedType

                findNeighbor rest idx =
                    case rest of
                        [] ->
                            idx

                        head :: tail ->
                            if head.feedType == feedType then
                                idx

                            else
                                findNeighbor tail (idx + 1)

                index =
                    findNeighbor model.feeds 0

                feeds =
                    List.filter (\f -> feedType /= f.feedType)
                        model.feeds
            in
            updateFeedTypes
                { model
                    | feeds = feeds
                    , lastClosedFeed = Just ( feed, index )
                    , dialogInputs =
                        { dialogInputs | isLastClosedFeed = True }
                }
                |> withCmd (saveFeeds feeds model)

        MoveFeedLeft feedType ->
            moveFeedLeft feedType model

        ShowImageDialog url ->
            setShowDialog (ImageDialog url) Nothing model
                |> withNoCmd

        ShowUserDialog username ->
            let
                dialogUser =
                    case Dict.get username model.users of
                        Just user ->
                            user

                        Nothing ->
                            Types.emptyUser username
            in
            setShowDialog (UserDialog dialogUser True)
                (Just "Loading...")
                model
                |> withCmd
                    (case model.backend of
                        Nothing ->
                            Cmd.none

                        Just be ->
                            Api.userProfile be ReceiveUser username
                    )

        MoveFeedRight feedType ->
            moveFeedRight feedType model

        MouseDown point ->
            mouseDown point model

        MouseUp point ->
            mouseUp point model

        MouseMoved point ->
            mouseMoved point model

        ScrollToFeed feedType ->
            scrollToFeed feedType model

        ScrollToNewFeed feedType ->
            scrollToFeed feedType { model | scrollToFeed = Nothing }

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
                                Dom.getElement firstcolId
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
            setShowDialog AddFeedDialog Nothing (setDialogInput "" model)
                |> withCmd (focusId addFeedInputId)

        FeedSetChooser ->
            if 2 > List.length dialogInputs.feedSets then
                update FeedSets model

            else
                setShowDialog FeedSetChooserDialog Nothing model
                    |> withNoCmd

        FeedSets ->
            { model
                | dialogInputs =
                    { dialogInputs
                        | showDialog = FeedSetsDialog
                        , dialogError = Nothing
                        , currentFeedSet = ""
                        , feedSetColumnWidths =
                            feedSetColumnWidths dialogInputs.feedSets
                    }
            }
                |> withCmd (focusId newFeedSetInputId)

        SaveFeedTypes ->
            let
                typesString =
                    List.map .feedType model.feeds
                        |> ED.encodeFeedTypes
                        |> JE.encode 0

                feedSetsString =
                    ED.encodeFeedSets dialogInputs.feedSets
                        |> JE.encode 0
            in
            { model
                | dialogInputs =
                    { dialogInputs
                        | showDialog = SaveFeedsDialog
                        , dialogError = Nothing
                        , dialogInput = typesString
                        , feedSetsInput = feedSetsString
                    }
            }
                |> withCmd (focusId saveFeedInputId)

        ShowSettings ->
            { model
                | dialogInputs =
                    { dialogInputs
                        | showDialog = SettingsDialog
                        , dialogError = Nothing
                        , fontSizeInput = String.fromFloat model.settings.fontSize
                        , columnWidthInput = String.fromInt model.settings.columnWidth
                    }
            }
                |> withCmd (focusId settingsInputId)

        SetStyle option ->
            let
                style =
                    case option of
                        LightStyle ->
                            lightStyle

                        DarkStyle ->
                            darkStyle

                newSettings =
                    { settings
                        | styleOption = option
                        , style = style
                    }
            in
            { model
                | settings = newSettings
                , styleOption = option
            }
                |> withCmd (storeSettings newSettings model)

        RestoreFeedTypes ->
            case JD.decodeString ED.feedTypesDecoder dialogInputs.dialogInput of
                Err err ->
                    setDialogError (Just <| JD.errorToString err) model
                        |> withNoCmd

                Ok feeds ->
                    let
                        mdl =
                            setShowDialog NoDialog Nothing model
                    in
                    restoreFeedTypes feeds mdl

        DialogInput username ->
            setDialogInput username model |> withNoCmd

        PostInput postInput ->
            setPostInput postInput model |> withNoCmd

        ChoosePostImage ->
            setDialogError Nothing model
                |> withCmd (Select.file imageMimeTypes ReceiveFile)

        ReceiveFile file ->
            let
                images =
                    List.concat
                        [ dialogInputs.postImages
                        , [ PostImage file Nothing Nothing ]
                        ]
            in
            { model
                | dialogInputs =
                    { dialogInputs
                        | postImages = images
                    }
            }
                |> withCmds
                    [ Task.perform (ReceiveFileUrl file) <|
                        File.toUrl file
                    , case model.backend of
                        Nothing ->
                            Cmd.none

                        Just be ->
                            Api.postImage be (ReceiveImageUpload file) file
                    ]

        ReceiveFileUrl file url ->
            let
                images =
                    LE.updateIf (\image -> image.file == file)
                        (\image -> { image | url = Just url })
                        dialogInputs.postImages
            in
            { model
                | dialogInputs =
                    { dialogInputs | postImages = images }
            }
                |> withNoCmd

        ReceiveImageUpload file result ->
            receiveImageUpload file result model

        RemovePostImage file ->
            let
                images =
                    List.filter (\image -> image.file /= file)
                        dialogInputs.postImages
            in
            { model
                | dialogInputs =
                    { dialogInputs | postImages = images }
            }
                |> withNoCmd

        PostButton feedType whichButton ->
            setPostButton feedType whichButton model

        RestoreFeedSets ->
            case JD.decodeString ED.feedSetsDecoder dialogInputs.feedSetsInput of
                Err err ->
                    setDialogError (Just <| JD.errorToString err) model
                        |> withNoCmd

                Ok feedSets ->
                    let
                        mdl =
                            setShowDialog NoDialog
                                Nothing
                                { model
                                    | dialogInputs =
                                        { dialogInputs
                                            | feedSets = feedSets
                                            , feedSetColumnWidths =
                                                feedSetColumnWidths feedSets
                                        }
                                }
                    in
                    mdl |> withCmd (saveFeedSets feedSets mdl)

        SetFeedSetsInput feedSetsInput ->
            { model
                | dialogInputs =
                    { dialogInputs | feedSetsInput = feedSetsInput }
            }
                |> withNoCmd

        FontSizeInput fontSizeInput ->
            { model
                | dialogInputs =
                    { dialogInputs
                        | fontSizeInput = fontSizeInput
                    }
            }
                |> withNoCmd

        SaveSettings ->
            saveSettings model

        ClearFeeds ->
            setFeedTypes []
                { model
                    | feeds = []
                }
                |> withCmd (saveFeeds [] model)

        NewFeedSet ->
            newFeedSet model

        SetCurrentFeedSet name ->
            setCurrentFeedSet name model |> withNoCmd

        SetFeedSetColumnWidth name width ->
            setFieldSetColumnWidth name width model |> withNoCmd

        ReloadFeedSet name ->
            reloadFeedSet name model

        SaveToFeedSet name ->
            saveToFeedSet name model

        RestoreFromFeedSet name ->
            restoreFromFeedSet name model

        DeleteFeedSet name ->
            deleteFeedSet name model

        ColumnWidthInput columnWidth ->
            { model
                | dialogInputs =
                    { dialogInputs | columnWidthInput = columnWidth }
            }
                |> withNoCmd

        FontSizeOption option ->
            case LE.find (\( _, o ) -> option == o) fontSizeOptions of
                Nothing ->
                    model |> withNoCmd

                Just ( size, _ ) ->
                    saveSettings
                        { model
                            | dialogInputs =
                                { dialogInputs
                                    | fontSizeOption = Just option
                                    , fontSizeInput = String.fromFloat size
                                }
                        }

        ColumnWidthOption option ->
            case LE.find (\( _, o ) -> option == o) columnWidthOptions of
                Nothing ->
                    model |> withNoCmd

                Just ( size, _ ) ->
                    saveSettings
                        { model
                            | dialogInputs =
                                { dialogInputs
                                    | columnWidthOption = Just option
                                    , columnWidthInput = String.fromInt size
                                }
                        }

        SetAutoSizeColumns columns ->
            { model
                | dialogInputs =
                    { dialogInputs | autoSizeColumns = columns }
            }
                |> withNoCmd

        AutoSize ->
            autoSize model

        ShowHiddenText showHidden ->
            let
                newSettings =
                    { settings | showHidden = showHidden }
            in
            { model
                | settings = newSettings
            }
                |> withCmd (storeSettings newSettings model)

        RestoreDefaultSettings ->
            let
                newSettings =
                    { settings
                        | fontSize = defaultFontSize
                        , columnWidth = defaultColumnWidth
                    }
            in
            { model
                | settings = newSettings
                , dialogInputs =
                    { dialogInputs
                        | fontSizeInput = String.fromFloat defaultFontSize
                        , columnWidthInput = String.fromInt defaultColumnWidth
                    }
            }
                |> adjustOptions
                |> withCmd (storeSettings newSettings model)

        AddNewFeed feedType ->
            addNewFeed feedType model.settings.fontSize model

        RemoveFeedError feedType ->
            removeFeedError feedType model

        ReloadFeedError feedType updateType ->
            let
                ( mdl, _ ) =
                    removeFeedError feedType model
            in
            case findFeed feedType mdl of
                Just feed ->
                    update (LoadMore updateType feed) mdl

                Nothing ->
                    mdl |> withNoCmd

        Upvote feedType post ->
            upvote feedType post model

        Downvote feedType post ->
            downvote feedType post model

        Repost feedType post ->
            repost feedType post model

        ReceiveOperation operation result ->
            receiveOperation operation result model

        ReceivePostFeed updateType feedType result ->
            receiveFeed updateType
                feedType
                (boxActivityLogListResult result)
                model

        ReceiveNotificationsFeed updateType feedType result ->
            receiveFeed updateType
                feedType
                (boxNotificationsLogResult result)
                model

        ReceiveUser result ->
            receiveUser result model

        ReceivePost result ->
            receivePost result model


feedSetColumnWidths : List (FeedSet msg) -> Dict String String
feedSetColumnWidths feedSets =
    List.map
        (\feedSet ->
            ( feedSet.name
            , case feedSet.columnWidth of
                Nothing ->
                    ""

                Just w ->
                    String.fromInt w
            )
        )
        feedSets
        |> Dict.fromList


{-| TODO
-}
receiveImageUpload : File -> FeedResult String -> Model -> ( Model, Cmd Msg )
receiveImageUpload file result model =
    case model.dialogInputs.showDialog of
        NewPostDialog _ ->
            case result of
                Ok mediaId ->
                    setDialogInputs
                        (\inputs ->
                            { inputs
                                | postImages =
                                    LE.updateIf (\image -> image.file == file)
                                        (\image ->
                                            { image | mediaId = Just mediaId }
                                        )
                                        inputs.postImages
                            }
                        )
                        model
                        |> withNoCmd

                Err _ ->
                    setDialogError
                        (Just <|
                            "Error uploading image: "
                                ++ (truncateString 20 <| File.name file)
                        )
                        (setDialogInputs
                            (\inputs ->
                                { inputs
                                    | postImages =
                                        List.filter (\image -> image.file /= file)
                                            inputs.postImages
                                }
                            )
                            model
                        )
                        |> withNoCmd

        _ ->
            model |> withNoCmd


truncateString : Int -> String -> String
truncateString maxlen string =
    if String.length string <= maxlen then
        string

    else
        String.left maxlen string ++ "..."


removeFeedError : FeedType -> Model -> ( Model, Cmd Msg )
removeFeedError feedType model =
    let
        dialogInputs =
            model.dialogInputs
    in
    case dialogInputs.showDialog of
        ReceiveFeedErrorDialog errors ->
            let
                errs =
                    List.filter
                        (\err -> err.feedType /= feedType)
                        errors
            in
            { model
                | dialogInputs =
                    { dialogInputs
                        | showDialog =
                            if errs == [] then
                                NoDialog

                            else
                                ReceiveFeedErrorDialog errs
                    }
            }
                |> withNoCmd

        _ ->
            model |> withNoCmd


mouseDown : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
mouseDown point model =
    let
        ( index, inControlColumn ) =
            mouseFeedIndex point model

        ( feed, idx ) =
            case index of
                Nothing ->
                    ( Nothing, -1 )

                Just i ->
                    ( LE.getAt i model.feeds, i )

        draggingInfo =
            case feed of
                Nothing ->
                    Nothing

                Just f ->
                    Just <| DraggingInfo f.feedType idx point model.feeds

        dialogInputs =
            model.dialogInputs

        showDialog =
            if inControlColumn then
                NoDialog

            else
                dialogInputs.showDialog

        mdl =
            if
                (draggingInfo == model.draggingInfo)
                    && (showDialog == dialogInputs.showDialog)
            then
                model

            else
                { model
                    | draggingInfo = draggingInfo
                    , dialogInputs =
                        { dialogInputs
                            | showDialog = showDialog
                        }
                }
    in
    mdl |> withNoCmd


mouseUp : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
mouseUp point model =
    case model.draggingInfo of
        Nothing ->
            model |> withNoCmd

        Just info ->
            let
                mdl =
                    { model | draggingInfo = Nothing }
            in
            if List.map .feedType model.feeds == List.map .feedType info.feeds then
                mdl |> withNoCmd

            else
                updateFeedTypes
                    { mdl
                        | feeds = info.feeds
                        , scrollToFeed = Just info.feedType
                    }
                    |> withNoCmd


mouseMoved : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
mouseMoved point model =
    case model.draggingInfo of
        Nothing ->
            -- can't happen
            model |> withNoCmd

        Just info ->
            case justMouseFeedIndex point model of
                Nothing ->
                    { model
                        | draggingInfo =
                            Just
                                { info
                                    | index = -1
                                    , feeds = model.feeds
                                }
                    }
                        |> withNoCmd

                Just idx ->
                    case findFeed info.feedType model of
                        Nothing ->
                            model |> withNoCmd

                        Just feed ->
                            if idx == info.index then
                                model |> withNoCmd

                            else
                                let
                                    feedType =
                                        info.feedType

                                    feeds =
                                        List.filter (\f -> f.feedType /= feedType)
                                            info.feeds

                                    newFeeds =
                                        List.concat
                                            [ List.take idx feeds
                                            , [ feed ]
                                            , List.drop idx feeds
                                            ]
                                in
                                { model
                                    | draggingInfo =
                                        Just
                                            { info
                                                | index = idx
                                                , point = point
                                                , feeds = newFeeds
                                            }
                                }
                                    |> withNoCmd


justMouseFeedIndex : ( Int, Int ) -> Model -> Maybe Int
justMouseFeedIndex point model =
    mouseFeedIndex point model |> Tuple.first


mouseFeedIndex : ( Int, Int ) -> Model -> ( Maybe Int, Bool )
mouseFeedIndex point model =
    let
        ( x, y ) =
            point

        settings =
            model.settings

        baseFontSize =
            settings.fontSize

        iconHeight =
            bigUserIconHeight baseFontSize

        topPad =
            13

        space =
            10

        topy =
            topPad + iconHeight + (space // 2)

        deltay =
            iconHeight + space

        inControlColumn =
            x < controlColumnWidth baseFontSize
    in
    if x < 15 || x > 15 + iconHeight then
        ( Nothing, inControlColumn )

    else
        let
            index =
                (y - topy) // deltay

            cnt =
                List.length model.feeds
        in
        ( Just <| min (cnt - 1) index, inControlColumn )


autoSize : Model -> ( Model, Cmd Msg )
autoSize model =
    let
        settings =
            model.settings

        baseFontSize =
            settings.fontSize

        ccw =
            controlColumnWidth baseFontSize

        dialogInputs =
            model.dialogInputs

        columnWidth =
            (settings.windowWidth - ccw - 5) // dialogInputs.autoSizeColumns

        newFontSize =
            toFloat (round <| toFloat columnWidth / 22.5)

        realCcw =
            controlColumnWidth newFontSize

        realCw =
            (settings.windowWidth - realCcw - 5) // dialogInputs.autoSizeColumns
    in
    { model
        | dialogInputs =
            { dialogInputs
                | fontSizeInput = String.fromFloat newFontSize
                , columnWidthInput = String.fromInt realCw
            }
    }
        |> saveSettings


adjustOptions : Model -> Model
adjustOptions model =
    let
        settings =
            model.settings

        dialogInputs =
            model.dialogInputs
    in
    { model
        | dialogInputs =
            { dialogInputs
                | fontSizeOption = fontSizeOption settings.fontSize
                , columnWidthOption = columnWidthOption settings.columnWidth
            }
    }


fontSizeOptions : List ( Float, SizeOption )
fontSizeOptions =
    [ ( 10, SmallSize ), ( 15, MediumSize ), ( 20, LargeSize ) ]


fontSizeOption : Float -> Maybe SizeOption
fontSizeOption size =
    case LE.find (\( s, _ ) -> size == s) fontSizeOptions of
        Just ( _, option ) ->
            Just option

        Nothing ->
            Nothing


columnWidthOptions : List ( Int, SizeOption )
columnWidthOptions =
    [ ( 250, SmallSize ), ( 350, MediumSize ), ( 450, LargeSize ) ]


columnWidthOption : Int -> Maybe SizeOption
columnWidthOption size =
    case LE.find (\( s, _ ) -> size == s) columnWidthOptions of
        Just ( _, option ) ->
            Just option

        Nothing ->
            Nothing


focusId : String -> Cmd Msg
focusId id =
    Task.attempt (\_ -> Noop) <| Dom.focus id


saveSettings : Model -> ( Model, Cmd Msg )
saveSettings model =
    case String.toFloat model.dialogInputs.fontSizeInput of
        Nothing ->
            setDialogError (Just "Font Size is not a number.") model
                |> withNoCmd

        Just size ->
            case String.toInt model.dialogInputs.columnWidthInput of
                Nothing ->
                    setDialogError (Just "Column Width is not a number.") model
                        |> withNoCmd

                Just width ->
                    let
                        settings =
                            model.settings
                    in
                    if
                        (size == settings.fontSize)
                            && (width == settings.columnWidth)
                            && (width == settings.defaultColumnWidth)
                    then
                        model |> withNoCmd

                    else
                        let
                            newSettings =
                                { settings
                                    | fontSize = max 10 size
                                    , columnWidth = max 100 width
                                    , defaultColumnWidth = max 100 width
                                }

                            dialogInputs =
                                model.dialogInputs
                        in
                        { model
                            | settings = newSettings
                            , dialogInputs =
                                { dialogInputs
                                    | fontSizeInput =
                                        String.fromFloat newSettings.fontSize
                                    , columnWidthInput =
                                        String.fromInt newSettings.columnWidth
                                    , dialogError = Nothing
                                }
                        }
                            |> adjustOptions
                            |> withCmd (storeSettings newSettings model)


newFeedSet : Model -> ( Model, Cmd Msg )
newFeedSet model =
    let
        name =
            model.dialogInputs.currentFeedSet
    in
    if name == "" then
        setDialogError (Just "Feed set name may not be blank.") model
            |> withNoCmd

    else
        case LE.find (\fs -> fs.name == name) model.dialogInputs.feedSets of
            Just _ ->
                setDialogError
                    (Just <| "There is already a feed set named \"" ++ name ++ "\"")
                    model
                    |> withNoCmd

            Nothing ->
                saveToFeedSet name
                    (setCurrentFeedSet "" model)


reloadFeedSet : String -> Model -> ( Model, Cmd Msg )
reloadFeedSet name model =
    case findFeedSet name model of
        Nothing ->
            model |> withNoCmd

        Just feedSet ->
            let
                updateType =
                    FeedSetUpdate name

                feeds =
                    case feedSet.feeds of
                        Just fs ->
                            fs

                        Nothing ->
                            feedTypesToFeeds Nothing
                                model.backend
                                feedSet.feedTypes
                                0

                cmds =
                    List.map (loadMore updateType) feeds

                loadingFeeds =
                    Set.fromList <|
                        List.map feedTypeToString feedSet.feedTypes
            in
            model
                |> updateFeedSet { feedSet | loadingFeeds = loadingFeeds }
                |> withCmds cmds


saveToFeedSet : String -> Model -> ( Model, Cmd Msg )
saveToFeedSet name model =
    let
        oldColumnWidth =
            case findFeedSet name model of
                Nothing ->
                    Nothing

                Just set ->
                    set.columnWidth

        columnWidth =
            case Dict.get name model.dialogInputs.feedSetColumnWidths of
                Nothing ->
                    oldColumnWidth

                Just s ->
                    if s == "" then
                        Nothing

                    else
                        case String.toInt s of
                            Nothing ->
                                oldColumnWidth

                            w ->
                                w

        feedSet =
            { name = name
            , feedTypes = List.map .feedType model.feeds
            , feeds =
                Just <|
                    List.map (\feed -> { feed | newPosts = 0 })
                        model.feeds
            , loadingFeeds = Set.empty
            , columnWidth = columnWidth
            }

        mdl =
            setDialogError Nothing model
                |> updateFeedSet feedSet
    in
    mdl
        |> withCmd (saveFeedSets mdl.dialogInputs.feedSets mdl)


updateFeedSet : FeedSet Msg -> Model -> Model
updateFeedSet feedSet model =
    let
        name =
            feedSet.name

        feedSets =
            List.filter (\fs -> fs.name /= name) model.dialogInputs.feedSets
    in
    setFeedSets (List.sortBy .name <| feedSet :: feedSets) model


isFeedSetInstalled : FeedSet msg -> List FeedType -> Settings -> Bool
isFeedSetInstalled feedSet feedTypes settings =
    (feedTypes == feedSet.feedTypes)
        && (case feedSet.columnWidth of
                Nothing ->
                    settings.columnWidth == settings.defaultColumnWidth

                Just w ->
                    w == settings.columnWidth
           )


restoreFromFeedSet : String -> Model -> ( Model, Cmd Msg )
restoreFromFeedSet name model =
    let
        feedTypes =
            List.map .feedType model.feeds

        settings =
            model.settings

        isInstalled feedSet =
            isFeedSetInstalled feedSet feedTypes settings

        dialogInputs =
            model.dialogInputs
    in
    case findFeedSet name model of
        Nothing ->
            model |> withNoCmd

        Just feedSet ->
            if isInstalled feedSet then
                setOnlyShowDialog NoDialog model
                    |> withNoCmd

            else
                let
                    feeds =
                        case feedSet.feeds of
                            Nothing ->
                                feedTypesToFeeds Nothing
                                    model.backend
                                    feedSet.feedTypes
                                    0

                            Just fs ->
                                fs

                    newPosts =
                        totalNewPosts feeds

                    -- This makes swapping a feedset back in
                    -- preserve its new post counts.
                    maybeOldFeeds fs =
                        if isInstalled fs then
                            { fs | feeds = Just model.feeds }

                        else
                            fs

                    feedSets =
                        List.map maybeOldFeeds dialogInputs.feedSets

                    ( mdl, _ ) =
                        updateFeedTypes
                            { model
                                | feeds = feeds
                                , settings =
                                    { settings
                                        | columnWidth =
                                            case feedSet.columnWidth of
                                                Nothing ->
                                                    settings.defaultColumnWidth

                                                Just w ->
                                                    w
                                    }
                                , dialogInputs =
                                    { dialogInputs
                                        | showDialog = NoDialog
                                        , feedSets = feedSets
                                    }
                            }
                            |> saveToFeedSet name

                    scrollToTop feed =
                        let
                            colid =
                                columnId feed.id
                        in
                        Task.attempt (\_ -> Noop) (Dom.setViewportOf colid 0 0)
                in
                { mdl
                    | scrollToFeed =
                        case List.head feeds of
                            Nothing ->
                                Nothing

                            Just feed ->
                                Just feed.feedType
                }
                    |> withCmds
                        [ saveFeeds feeds mdl
                        , if newPosts > 0 then
                            List.map scrollToTop feeds
                                |> Cmd.batch

                          else
                            loadAllCmd
                        ]


totalNewPosts : List (Feed msg) -> Int
totalNewPosts feeds =
    List.foldr (\feed sum -> sum + feed.newPosts) 0 feeds


findFeedSet : String -> Model -> Maybe (FeedSet Msg)
findFeedSet name model =
    LE.find (\fs -> fs.name == name) model.dialogInputs.feedSets


deleteFeedSet : String -> Model -> ( Model, Cmd Msg )
deleteFeedSet name model =
    let
        dialogInputs =
            model.dialogInputs

        feedSets =
            List.filter (\fs -> fs.name /= name) dialogInputs.feedSets
    in
    setFeedSets feedSets model
        |> withCmd (saveFeedSets feedSets model)


scrollToFeed : FeedType -> Model -> ( Model, Cmd Msg )
scrollToFeed feedType model =
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
                        Dom.getElement colid
                    )


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
            setOnlyShowDialog (OperationErrorDialog err) mdl
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
                            Http.BadStatus code ->
                                if code == 400 then
                                    case operation of
                                        DownvoteOperation _ _ _ ->
                                            "Downvote error, probably unauthorized."

                                        _ ->
                                            "Bad Request error" ++ opname

                                else
                                    "Bad HTTP status: "
                                        ++ String.fromInt code
                                        ++ opname

                            _ ->
                                "HTTP error on" ++ opname

        Ok success ->
            if not success.state then
                handleError success.message

            else
                model |> withNoCmd


updatePost : (Post -> Post) -> FeedType -> String -> Model -> Model
updatePost updater feedType postid model =
    let
        shouldUpdate lg =
            lg.post.id == postid

        modifier lg =
            { lg | post = updater lg.post }

        shouldUpdateNotification { notification } =
            case notification.post of
                Nothing ->
                    False

                Just post ->
                    post.id == postid

        notificationModifier gangedNotification =
            let
                notification =
                    gangedNotification.notification
            in
            case notification.post of
                Nothing ->
                    gangedNotification

                Just post ->
                    { gangedNotification
                        | notification =
                            { notification
                                | post = Just <| updater post
                            }
                    }

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

                NotificationFeedData gangedNotifications ->
                    { logList
                        | data =
                            NotificationFeedData <|
                                LE.updateIf shouldUpdateNotification
                                    notificationModifier
                                    gangedNotifications
                    }

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
                                updateFeedTypes
                                    { model
                                        | feeds =
                                            List.concat [ tail, [ feed ] ]
                                    }

                            f :: t ->
                                updateFeedTypes
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
    { mdl | scrollToFeed = Just feedType }
        |> withCmd (saveFeeds mdl.feeds mdl)


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
                                updateFeedTypes
                                    { model
                                        | feeds =
                                            List.concat
                                                [ [ feed ]
                                                , List.reverse res
                                                ]
                                    }

                            f :: t ->
                                updateFeedTypes
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
    { mdl | scrollToFeed = Just feedType }
        |> withCmd (saveFeeds mdl.feeds mdl)


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


saveFeedSets : List (FeedSet Msg) -> Model -> Cmd Msg
saveFeedSets feedSets model =
    let
        value =
            ED.encodeFeedSets feedSets
    in
    localStorageSend
        (LocalStorage.put storageKeys.feedsets <| Just value)
        model


addNewFeed : FeedType -> Float -> Model -> ( Model, Cmd Msg )
addNewFeed feedType baseFontSize model =
    let
        addit feed index =
            let
                feeds =
                    if index < 0 then
                        List.concat [ model.feeds, [ feed ] ]

                    else
                        List.concat
                            [ List.take index model.feeds
                            , [ feed ]
                            , List.drop index model.feeds
                            ]

                dialogInputs =
                    model.dialogInputs
            in
            setShowDialog NoDialog
                Nothing
                (updateFeedTypes
                    { model
                        | feeds = feeds
                        , lastClosedFeed = Nothing
                        , dialogInputs =
                            { dialogInputs | isLastClosedFeed = False }
                        , nextId = model.nextId + 1
                        , scrollToFeed = Just feed.feedType
                    }
                )
                |> withCmds
                    [ if feedType == LastClosedFeed then
                        Cmd.none

                      else
                        loadMoreCmd MergeUpdate feed
                    , saveFeeds feeds model
                    ]
    in
    if feedType == LastClosedFeed then
        case model.lastClosedFeed of
            Nothing ->
                model |> withNoCmd

            Just ( feed, index ) ->
                addit feed index

    else
        case model.backend of
            Nothing ->
                model |> withNoCmd

            Just backend ->
                case findFeed feedType model of
                    Just _ ->
                        setDialogError (Just "That feed is already displayed.")
                            model
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
                            setDialogError (Just "Username may not be blank.") model
                                |> withNoCmd

                        else
                            addit
                                (feedTypeToFeed model.settings.loggedInUser
                                    backend
                                    feedType
                                    model.nextId
                                )
                                -1


loadAll : Model -> ( Model, Cmd Msg )
loadAll model =
    let
        getone feed ( m, cs ) =
            ( setLoadingFeed feed m
            , loadMore MergeUpdate feed :: cs
            )

        ( mdl, cmds ) =
            List.foldr getone ( model, [] ) model.feeds
    in
    { mdl | loadingAll = True } |> withCmds cmds


emptyPostForm : PostForm
emptyPostForm =
    { body = ""
    , reply_to = Nothing
    , is_quote = False
    , is_html = False
    , nsfw = False
    , is_premium = False
    , gif = Nothing
    , topic = Nothing
    , group = Nothing
    , media_attachments = []
    , premium_min_tier = Nothing
    , poll = False
    , poll_option_1 = Nothing
    , poll_option_2 = Nothing
    , poll_option_3 = Nothing
    , poll_option_4 = Nothing
    }


spliceInAtName : Int -> String -> String -> ( String, Int )
spliceInAtName cursor atName string =
    let
        left =
            String.left cursor string

        right =
            String.dropLeft cursor string

        toAt =
            if
                String.startsWith "@" left
                    && not (String.contains " " left)
            then
                "@"

            else
                SE.leftOfBack " @" left ++ " @"
    in
    ( toAt ++ atName ++ right, String.length toAt + String.length atName )


findAtName : Int -> String -> Maybe String
findAtName cursor string =
    let
        beginning =
            String.left cursor string
                |> String.toLower
                |> String.replace "\n" " "
    in
    if
        String.startsWith "@" beginning
            && not (String.contains " " beginning)
    then
        Just <| String.dropLeft 1 beginning

    else
        let
            res =
                SE.rightOfBack " @" beginning
        in
        if res /= "" && not (String.contains " " res) then
            Just res

        else
            Nothing


trimCompletions : String -> List UserChoice -> List UserChoice
trimCompletions prefix choices =
    let
        tuples =
            List.map
                (\choice ->
                    ( ( if String.startsWith prefix choice.username then
                            0

                        else
                            1
                      , choice.username
                      )
                    , choice
                    )
                )
                choices
    in
    List.sortBy Tuple.first tuples
        |> List.take 5
        |> List.map Tuple.second


onNewPostSelection : Tracker.Selection -> Model -> ( Model, Cmd Msg )
onNewPostSelection selection model =
    let
        cursor =
            selection.selectionEnd

        dialogInputs =
            model.dialogInputs
    in
    if cursor /= selection.selectionStart then
        model |> withNoCmd

    else
        case findAtName cursor dialogInputs.postInput of
            Nothing ->
                model |> withNoCmd

            Just complete ->
                case
                    IdSearch.lookup complete model.userChoices
                        |> trimCompletions complete
                of
                    [] ->
                        model |> withNoCmd

                    completions ->
                        { model
                            | dialogInputs =
                                { dialogInputs | completions = completions }
                            , triggerCoordinates = model.triggerCoordinates + 1
                        }
                            |> withNoCmd


completePostChoice : UserChoice -> Model -> ( Model, Cmd Msg )
completePostChoice choice model =
    let
        dialogInputs =
            model.dialogInputs

        ( postInput, postSelection ) =
            case dialogInputs.coordinates of
                Nothing ->
                    ( dialogInputs.postInput, dialogInputs.postSelection )

                Just coordinates ->
                    let
                        ( input, start ) =
                            spliceInAtName coordinates.selectionEnd
                                choice.username
                                dialogInputs.postInput

                        ( _, count ) =
                            dialogInputs.postSelection
                    in
                    ( input, ( start, count + 1 ) )
    in
    { model
        | dialogInputs =
            { dialogInputs
                | postInput = postInput
                , postSelection = postSelection
                , completions = []
                , coordinates = Nothing
            }
    }
        |> withCmd (focusId postInputId)


makePost : Model -> ( Model, Cmd Msg )
makePost model =
    let
        dialogInputs =
            model.dialogInputs

        body =
            dialogInputs.postInput

        ( reply_to, is_quote ) =
            case dialogInputs.showDialog of
                NewPostDialog responseType ->
                    case responseType of
                        NormalPost _ _ ->
                            ( Nothing, False )

                        QuotePost qp _ ->
                            ( Just qp.id, True )

                        CommentOnPost qp _ ->
                            ( Just qp.id, False )

                _ ->
                    ( Nothing, False )

        media =
            List.map
                (\image ->
                    case image.mediaId of
                        Just id ->
                            id

                        Nothing ->
                            ""
                )
                dialogInputs.postImages
                |> List.filter ((==) "")

        postForm =
            { emptyPostForm
                | body = body
                , reply_to = reply_to
                , is_quote = is_quote
                , media_attachments = Debug.log "media_attachments" media
            }
    in
    case LE.find (\image -> image.mediaId == Nothing) dialogInputs.postImages of
        Just _ ->
            setDialogError (Just "Wait for images to upload.") model
                |> withNoCmd

        Nothing ->
            case model.backend of
                Nothing ->
                    model |> withNoCmd

                Just backend ->
                    model
                        |> withCmd (Api.newPost backend ReceivePost postForm)


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


loadMore : UpdateType -> Feed Msg -> Cmd Msg
loadMore updateType feed =
    let
        before =
            case updateType of
                AppendUpdate ->
                    feedBefore feed

                _ ->
                    ""
    in
    case feed.getter of
        PostFeedGetter getter ->
            getter (ReceivePostFeed updateType feed.feedType) before

        NotificationFeedGetter getter ->
            getter (ReceiveNotificationsFeed updateType feed.feedType) before


updateIcons : LogList FeedData -> Model -> ( Model, Cmd Msg )
updateIcons logList model =
    let
        doPost post m =
            let
                ( m2, b2 ) =
                    updateUser post.user m

                ( m3, b3 ) =
                    case post.related of
                        RelatedPosts { parent } ->
                            case parent of
                                Nothing ->
                                    ( m2, False )

                                Just p ->
                                    updateUser p.user m2
            in
            ( m3, b2 || b3 )

        ( model2, needsUpdate ) =
            case logList.data of
                PostFeedData logLists ->
                    let
                        foldPosts log ( mdl, bool ) =
                            let
                                ( m, b ) =
                                    updateUser log.actuser mdl

                                ( m2, b2 ) =
                                    doPost log.post m
                            in
                            ( m2, bool || b || b2 )
                    in
                    List.foldl foldPosts ( model, False ) logLists

                NotificationFeedData notifications ->
                    let
                        foldNotifications notification ( mdl, bool ) =
                            let
                                ( m, b ) =
                                    updateUser
                                        notification.notification.actuser
                                        mdl

                                ( m2, b2 ) =
                                    case notification.notification.post of
                                        Nothing ->
                                            ( m, b )

                                        Just p ->
                                            doPost p m
                            in
                            ( m2, bool || b || b2 )
                    in
                    List.foldl foldNotifications ( model, False ) notifications
    in
    model2
        |> withCmd
            (if needsUpdate then
                storeIcons model2.dialogInputs.icons model2

             else
                Cmd.none
            )


receiveUser : FeedResult User -> Model -> ( Model, Cmd Msg )
receiveUser result model =
    let
        dialogInputs =
            model.dialogInputs
    in
    case result of
        Err _ ->
            ( case dialogInputs.showDialog of
                UserDialog _ _ ->
                    setDialogError (Just "Error getting user profile.") model

                _ ->
                    model
            , Cmd.none
            )

        Ok user ->
            ( case dialogInputs.showDialog of
                UserDialog _ _ ->
                    setShowDialog (UserDialog user False) Nothing model

                _ ->
                    model
            , Cmd.none
            )


replaceFeed : Feed Msg -> Model -> Model
replaceFeed feed model =
    let
        feeds =
            LE.setIf (\f -> f.feedType == feed.feedType) feed model.feeds
    in
    { model | feeds = feeds }


addFeedPost : FeedType -> ActivityLog -> Model -> Model
addFeedPost feedType activityLog model =
    case findFeed feedType model of
        Nothing ->
            model

        Just feed ->
            let
                logList =
                    feed.feed
            in
            case logList.data of
                NotificationFeedData _ ->
                    model

                PostFeedData data ->
                    let
                        newFeed =
                            { feed
                                | feed =
                                    { logList
                                        | data =
                                            PostFeedData <| activityLog :: data
                                    }
                                , newPosts =
                                    feed.newPosts + 1
                            }
                    in
                    replaceFeed newFeed model


receivePost : FeedResult ActivityLog -> Model -> ( Model, Cmd Msg )
receivePost result model =
    let
        dialogInputs =
            model.dialogInputs

        settings =
            model.settings
    in
    case result of
        Err _ ->
            ( case dialogInputs.showDialog of
                NewPostDialog _ ->
                    setDialogError (Just "Error posting.") model

                _ ->
                    model
            , Cmd.none
            )

        Ok activityLog ->
            let
                mdl =
                    case settings.loggedInUser of
                        Nothing ->
                            model

                        Just user ->
                            addFeedPost (UserFeed user) activityLog model

                mdl2 =
                    addFeedPost HomeFeed activityLog mdl
                        |> setPostInput ""
            in
            ( case dialogInputs.showDialog of
                NewPostDialog _ ->
                    setShowDialog NoDialog Nothing mdl2

                _ ->
                    mdl2
            , Cmd.none
            )


receiveFeed : UpdateType -> FeedType -> FeedResult (LogList FeedData) -> Model -> ( Model, Cmd Msg )
receiveFeed updateType feedType result model =
    let
        ( mdl, cmd ) =
            case result of
                Err err ->
                    case err.httpError of
                        Just httpError ->
                            processReceiveFeedError updateType
                                feedType
                                httpError
                                model

                        _ ->
                            model |> withNoCmd

                Ok logList ->
                    updateIcons logList model

        ( mdl2, cmd2 ) =
            case updateType of
                FeedSetUpdate setName ->
                    receiveFeedSet setName feedType result mdl

                _ ->
                    receiveDisplayedFeed updateType feedType result mdl
    in
    mdl2 |> withCmds [ cmd, cmd2 ]


receiveFeedSet : String -> FeedType -> FeedResult (LogList FeedData) -> Model -> ( Model, Cmd Msg )
receiveFeedSet setName feedType result model =
    case findFeedSet setName model of
        Nothing ->
            model |> withNoCmd

        Just feedSet ->
            let
                feeds =
                    case feedSet.feeds of
                        Just fs ->
                            fs

                        Nothing ->
                            feedTypesToFeeds Nothing
                                model.backend
                                feedSet.feedTypes
                                0

                ( _, feeds2 ) =
                    updateFeeds (FeedSetUpdate setName) feedType result feeds

                loadingFeeds =
                    Set.remove (feedTypeToString feedType) feedSet.loadingFeeds
            in
            model
                |> updateFeedSet
                    { feedSet
                        | feeds = Just feeds2
                        , loadingFeeds = loadingFeeds
                    }
                |> withNoCmd


receiveDisplayedFeed : UpdateType -> FeedType -> FeedResult (LogList FeedData) -> Model -> ( Model, Cmd Msg )
receiveDisplayedFeed updateType feedType result model =
    let
        ( id, feeds ) =
            updateFeeds updateType feedType result model.feeds

        loadingFeeds =
            Set.remove (feedTypeToString feedType) model.loadingFeeds

        isLoading =
            not <| Set.isEmpty loadingFeeds

        firstNewFeed =
            if isLoading || not model.loadingAll then
                Nothing

            else
                LE.find (\feed -> feed.newPosts > 0) feeds
    in
    { model
        | feeds = feeds
        , loadingFeeds =
            loadingFeeds
        , loadingAll =
            if isLoading then
                model.loadingAll

            else
                False
        , scrollToFeed =
            case firstNewFeed of
                Nothing ->
                    model.scrollToFeed

                Just feed ->
                    Just feed.feedType
    }
        |> withCmd
            (if updateType == MergeUpdate && id /= "" then
                Task.attempt (\_ -> Noop) (Dom.setViewportOf id 0 0)

             else
                Cmd.none
            )


processReceiveFeedError : UpdateType -> FeedType -> Http.Error -> Model -> ( Model, Cmd Msg )
processReceiveFeedError updateType feedType err model =
    let
        dialogInputs =
            model.dialogInputs

        error =
            { feedType = feedType
            , updateType = updateType
            , error = err
            }

        mdl =
            case dialogInputs.showDialog of
                ReceiveFeedErrorDialog errors ->
                    setOnlyShowDialog (ReceiveFeedErrorDialog <| error :: errors)
                        model

                NoDialog ->
                    setOnlyShowDialog (ReceiveFeedErrorDialog [ error ]) model

                _ ->
                    model
    in
    mdl |> withNoCmd


updateFeeds : UpdateType -> FeedType -> FeedResult (LogList FeedData) -> List (Feed Msg) -> ( String, List (Feed Msg) )
updateFeeds updateType feedType result feeds =
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
                            , [ updateFeed updateType result feed ]
                            , rest
                            ]
                        )

                    else
                        loop rest (feed :: res)
    in
    loop feeds []


{-| This is here to try to preserve === on the whole feed or some of its elements.
-}
canonicalizeFeed : UpdateType -> Feed Msg -> Feed Msg -> Feed Msg
canonicalizeFeed updateType newFeed feed =
    let
        newData =
            newFeed.feed.data

        feedData =
            feed.feed.data
    in
    if newData == feedData then
        case updateType of
            FeedSetUpdate _ ->
                feed

            _ ->
                if feed.newPosts == 0 then
                    feed

                else
                    { feed | newPosts = 0 }

    else
        let
            ( data, newPosts, totalPosts ) =
                case feedData of
                    PostFeedData feedList ->
                        case newData of
                            PostFeedData newList ->
                                let
                                    ( dat, cnt ) =
                                        canonicalizeData .id newList feedList
                                in
                                ( PostFeedData dat, cnt, List.length dat )

                            _ ->
                                ( newData, 0, 0 )

                    NotificationFeedData feedList ->
                        case newData of
                            NotificationFeedData newList ->
                                let
                                    ( dat, cnt ) =
                                        canonicalizeData (.notification >> .id)
                                            newList
                                            feedList
                                in
                                ( NotificationFeedData dat, cnt, List.length dat )

                            _ ->
                                ( newData, 0, 0 )

            feed_feed =
                feed.feed

            res =
                { feed
                    | feed =
                        { feed_feed | data = data }
                }

            np =
                if List.member feed.feedType nonMergingFeedTypes then
                    0

                else
                    case updateType of
                        FeedSetUpdate _ ->
                            min (newPosts + res.newPosts) totalPosts

                        _ ->
                            newPosts
        in
        if np == res.newPosts then
            res

        else
            { res | newPosts = np }


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


canonicalizeData : (x -> String) -> List x -> List x -> ( List x, Int )
canonicalizeData key new old =
    let
        dict =
            List.foldl
                (\log d ->
                    Dict.insert (key log) log d
                )
                Dict.empty
                old

        loop : List x -> Int -> List x -> ( List x, Int )
        loop rest count result =
            case rest of
                [] ->
                    ( List.reverse result
                    , if old == [] then
                        0

                      else
                        count
                    )

                head :: tail ->
                    case Dict.get (key head) dict of
                        Just v ->
                            if head == v then
                                loop tail count <| v :: result

                            else
                                loop tail count <| head :: result

                        Nothing ->
                            loop tail (count + 1) <| head :: result
    in
    loop new 0 []


nonMergingFeedTypes : List FeedType
nonMergingFeedTypes =
    [ PopularFeed ]


updateFeed : UpdateType -> FeedResult (LogList FeedData) -> Feed Msg -> Feed Msg
updateFeed updateType result feed =
    case result of
        Err err ->
            { feed | error = Just err }

        Ok resultLogList ->
            let
                feed_feed =
                    feed.feed

                mergeType =
                    if List.member feed.feedType nonMergingFeedTypes then
                        MergeUpdate

                    else
                        updateType

                newFeed =
                    { feed
                        | feed =
                            { feed_feed
                                | data =
                                    reprocessFeedData <|
                                        updateFeedData mergeType
                                            resultLogList.data
                                            feed_feed.data
                            }
                    }
            in
            let
                res =
                    canonicalizeFeed updateType newFeed feed
            in
            case updateType of
                AppendUpdate ->
                    if res.newPosts == 0 then
                        res

                    else
                        { res | newPosts = 0 }

                _ ->
                    res


mergeLists : (a -> String) -> (a -> String) -> List a -> List a -> List a
mergeLists toDate toId list1 list2 =
    let
        loop : List a -> List a -> List a -> List a
        loop rest1 rest2 res =
            case rest1 of
                [] ->
                    List.append (List.reverse res) rest2

                head1 :: tail1 ->
                    case rest2 of
                        [] ->
                            List.append (List.reverse res) rest1

                        head2 :: tail2 ->
                            let
                                date1 =
                                    toDate head1

                                date2 =
                                    toDate head2
                            in
                            if date1 > date2 then
                                loop tail1 rest2 <| head1 :: res

                            else if date1 < date2 then
                                loop rest1 tail2 <| head2 :: res

                            else if toId head1 == toId head2 then
                                loop tail1 tail2 <| head1 :: res

                            else
                                loop tail1 tail2 <| head2 :: head1 :: res
    in
    loop list1 list2 []


{-| A switch used below to make it easy to change my mind.
-}
keepOldFeedSetData : Bool
keepOldFeedSetData =
    False


updateFeedData : UpdateType -> FeedData -> FeedData -> FeedData
updateFeedData updateType resultData feedData =
    case updateType of
        AppendUpdate ->
            case resultData of
                PostFeedData resultList ->
                    case feedData of
                        PostFeedData feedList ->
                            PostFeedData <|
                                List.append feedList resultList

                        _ ->
                            resultData

                NotificationFeedData resultList ->
                    case feedData of
                        NotificationFeedData feedList ->
                            NotificationFeedData <|
                                List.append feedList resultList

                        _ ->
                            resultData

        -- I'm not entirely convinced that this is a good idea.
        -- It can leave gaps in the feed, and doing enough fetches
        -- to close those gaps is probably an even worse idea.
        FeedSetUpdate _ ->
            if not keepOldFeedSetData then
                resultData

            else
                case resultData of
                    PostFeedData resultList ->
                        case feedData of
                            PostFeedData feedList ->
                                PostFeedData <|
                                    mergeLists .published_at .id resultList feedList

                            _ ->
                                resultData

                    NotificationFeedData resultList ->
                        case feedData of
                            NotificationFeedData feedList ->
                                NotificationFeedData <|
                                    mergeLists (.notification >> .created_at)
                                        (.notification >> .id)
                                        resultList
                                        feedList

                            _ ->
                                resultData

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


dialogPositionAttributes : DialogInputs -> Settings -> List (Attribute Msg)
dialogPositionAttributes dialogInputs settings =
    case dialogInputs.showDialog of
        FeedSetChooserDialog ->
            let
                baseFontSize =
                    settings.fontSize

                ccw =
                    controlColumnWidth baseFontSize

                iconHeight =
                    bigUserIconHeight baseFontSize

                cnt =
                    List.length dialogInputs.feedTypes + 1

                x =
                    ccw + 10

                y =
                    cnt * (iconHeight + 10) + 10
            in
            [ paddingEach { zeroes | top = y, left = x } ]

        _ ->
            [ centerX, centerY ]


view : Model -> Document Msg
view model =
    let
        settings =
            model.settings

        style =
            settings.style

        dialogInputs =
            model.dialogInputs
    in
    { title = pageTitle
    , body =
        [ Element.layoutWith
            { options = [ focusStyle ] }
            [ Element.inFront <|
                optimizers.keyedShowDialog
                    (dialogPositionAttributes dialogInputs settings)
                    ( "dialog"
                    , optimizers.lazyShowDialog dialogInputs settings
                    )
            , Background.color <| style.background
            , Font.color <| style.text
            ]
            (case model.backend of
                Nothing ->
                    loginPage settings

                Just backend ->
                    optimizers.mainPage
                        settings
                        dialogInputs.icons
                        model.draggingInfo
                        model.loadingFeeds
                        model.feeds
            )
        , let
            ( start, count ) =
                dialogInputs.postSelection
          in
          Tracker.textAreaTracker
            [ Tracker.textAreaId postInputId
            , Tracker.setSelection start start count
            , Tracker.triggerSelection model.triggerSelection
            , Tracker.onSelection OnNewPostSelection
            , Tracker.triggerCoordinates model.triggerCoordinates
            , Tracker.onCoordinates OnNewPostCoordinates
            ]
            []
        ]
    }


{-| Currently unused, until I figure out how to get it to update.
-}
dragImage : Settings -> Maybe DraggingInfo -> Icons -> Maybe (Element Msg)
dragImage settings draggingInfo icons =
    case draggingInfo of
        Nothing ->
            Nothing

        Just info ->
            let
                style =
                    settings.style

                iconHeight =
                    bigUserIconHeight settings.fontSize

                image =
                    feedSelectorImage style iconHeight icons info.feedType

                ( x, y ) =
                    info.point
            in
            Just <|
                el
                    [ paddingEach { zeroes | left = x - 10, top = y - 10 }
                    , classAttribute "gabdecker-undraggable"
                    ]
                    image


showTheDialog : DialogInputs -> Settings -> Element Msg
showTheDialog dialogInputs settings =
    case dialogInputs.showDialog of
        AddFeedDialog ->
            addFeedDialog dialogInputs settings

        NewPostDialog responseType ->
            newPostDialog responseType dialogInputs settings

        ImageDialog url ->
            imageDialog url dialogInputs settings

        UserDialog username isLoading ->
            userDialog username isLoading dialogInputs settings

        SaveFeedsDialog ->
            saveFeedsDialog dialogInputs settings

        SettingsDialog ->
            settingsDialog dialogInputs settings

        FeedSetChooserDialog ->
            feedSetChooserDialog dialogInputs settings

        FeedSetsDialog ->
            feedSetsDialog dialogInputs settings

        OperationErrorDialog err ->
            operationErrorDialog err dialogInputs settings

        ReceiveFeedErrorDialog errors ->
            receiveFeedErrorDialog errors dialogInputs settings

        _ ->
            Element.none


scaleFontSize : Float -> Float -> Int
scaleFontSize baseFontSize scale =
    round (scale * baseFontSize)


fontSize : Float -> Float -> Element.Attr decorative msg
fontSize baseFontSize scale =
    Font.size <| scaleFontSize baseFontSize scale


fillWidth : Attribute msg
fillWidth =
    width Element.fill


mainPage : Settings -> Icons -> Maybe DraggingInfo -> Set String -> List (Feed Msg) -> Element Msg
mainPage settings icons draggingInfo loadingFeeds feeds =
    let
        style =
            settings.style

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
        , Border.color style.border
        ]
        [ Element.html undraggableImageStyleNode
        , column []
            [ row [ height <| px settings.windowHeight ]
                [ controlColumn ccw
                    draggingInfo
                    (not <| Set.isEmpty loadingFeeds)
                    settings
                    icons
                    feeds
                ]
            ]
        , column []
            [ optimizers.keyedFeedColumn
                [ --, Element.scrollbarX
                  styleAttribute "overflow-x" "scroll"
                , styleAttribute "-webkit-overflow-scrolling" "touch"
                , height <| px settings.windowHeight
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


addFeedChoices : DialogInputs -> Settings -> List ( String, FeedType )
addFeedChoices dialogInputs settings =
    let
        feedTypes =
            dialogInputs.feedTypes

        homeFeed =
            List.member HomeFeed feedTypes

        popularFeed =
            List.member PopularFeed feedTypes

        notificationsFeed =
            List.member NotificationsFeed feedTypes

        ( username, userFeed ) =
            case settings.loggedInUser of
                Nothing ->
                    ( "", False )

                Just name ->
                    ( name
                    , List.member (UserFeed name) feedTypes
                    )
    in
    List.concat
        [ if not homeFeed then
            [ ( "Home", HomeFeed ) ]

          else
            []
        , if not popularFeed then
            [ ( "Popular", PopularFeed ) ]

          else
            []
        , if not notificationsFeed then
            [ ( "Notifications", NotificationsFeed ) ]

          else
            []
        , if not userFeed then
            if username == "" then
                []

            else
                [ ( "Your Posts", LoggedInUserFeed ) ]

          else
            []
        , if dialogInputs.isLastClosedFeed then
            [ ( "Last Closed", LastClosedFeed ) ]

          else
            []
        ]


dialogAttributes : Settings -> List (Attribute msg)
dialogAttributes settings =
    List.concat
        [ dialogAttributesNoPadding settings
        , [ paddingEach { top = 10, bottom = 20, left = 20, right = 20 }
          , width <| Element.maximum settings.windowWidth Element.shrink
          , Element.scrollbarX
          ]
        ]


dialogFontScale : Float
dialogFontScale =
    1.2


dialogAttributesNoPadding : Settings -> List (Attribute msg)
dialogAttributesNoPadding settings =
    [ Border.width 5
    , spacing 10
    , centerX
    , centerY
    , Background.color settings.style.dialogBackground
    , fontSize settings.fontSize dialogFontScale
    ]


operationErrorDialog : String -> DialogInputs -> Settings -> Element Msg
operationErrorDialog err dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight baseFontSize
    in
    column (dialogAttributes settings)
        [ let
            closeIcon =
                heightImage (getIconUrl style .close) "Close" iconHeight
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
                (standardButton style "Close Dialog" CloseDialog closeIcon)
            ]
        , row [ paddingEach { zeroes | top = 20 } ]
            [ text err ]
        ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl string ->
            "BadUrl: " ++ string

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus code ->
            "Bad Status: " ++ String.fromInt code

        Http.BadBody _ ->
            "Bad Body"


receiveFeedErrorDialog : List ReceiveFeedError -> DialogInputs -> Settings -> Element Msg
receiveFeedErrorDialog errors dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight baseFontSize

        icons =
            dialogInputs.icons

        closeIcon =
            heightImage (getIconUrl style .close) "Close" iconHeight

        reloadIcon =
            heightImage (getIconUrl style .reload) "Reload" iconHeight
    in
    column (dialogAttributes settings)
        [ row [ width Element.fill ]
            [ row
                [ centerX
                , centerY
                , Font.bold
                , fontSize baseFontSize 1.5
                ]
                [ el [ Font.color colors.red ] <| text "Receive Feed Errors " ]
            , el [ alignRight ]
                (standardButton style "Close Dialog" CloseDialog closeIcon)
            ]
        , Element.table
            [ spacing 10
            , width Element.fill
            ]
            { data = errors
            , columns =
                [ { header = Element.none
                  , width = Element.shrink
                  , view =
                        \{ feedType, error } ->
                            column []
                                [ feedTypeDescription style
                                    0
                                    feedType
                                    baseFontSize
                                    icons
                                , text <| httpErrorToString error
                                ]
                  }
                , { header = Element.none
                  , width = Element.fill
                  , view =
                        \{ feedType, updateType } ->
                            row
                                [ spacing 10
                                , alignRight
                                ]
                                [ el [ width Element.fill ] <| text " "
                                , standardButton style
                                    "Remove"
                                    (RemoveFeedError feedType)
                                    closeIcon
                                , standardButton style
                                    "Reload"
                                    (ReloadFeedError feedType updateType)
                                    reloadIcon
                                ]
                  }
                ]
            }
        ]


styleAttribute : String -> String -> Attribute msg
styleAttribute name value =
    Attributes.style name value
        |> Element.htmlAttribute


verifiedBadge : Style -> Int -> Int -> Bool -> User -> List (Attribute Msg)
verifiedBadge style offset size isButton user =
    if not user.verified then
        []

    else
        let
            element =
                el [ paddingEach { zeroes | top = offset, left = offset } ]
                    (circularHeightImageWithAttributes
                        [ Attributes.style "background-color" "#607CF5"
                        , Attributes.style "border" "1px solid black"
                        ]
                        (getIconUrl style .checkmark)
                        "Verified"
                        size
                    )

            button =
                if isButton then
                    -- I don't know why I have to do this, but I do.
                    -- Otherwise, user icons with verified badges are
                    -- not clickable.
                    standardButtonWithDontHover True
                        style
                        "Show user profile"
                        (ShowUserDialog user.username)
                        element

                else
                    element
        in
        [ Element.inFront button ]


{-| TODO: scale to baseFontSize
-}
userIconAndInfoOverlay : Style -> Float -> Attribute Msg -> User -> Element Msg
userIconAndInfoOverlay style baseFontSize smallFont user =
    row [ paddingEach { zeroes | left = 20 } ]
        [ column [ paddingEach { zeroes | top = 315 - 80 } ]
            [ el (verifiedBadge style 80 20 False user) <|
                circularHeightImageWithAttributes
                    [ Attributes.style
                        "border"
                        "4px solid #fbfbfb"
                    ]
                    user.picture_url
                    "picture"
                    100
            ]
        , let
            descriptionRows =
                userProfileDescriptionRows smallFont user

            delta =
                if List.length descriptionRows == 2 then
                    80

                else
                    60
          in
          column
            [ paddingEach
                { zeroes
                    | left = 5
                    , top = 315 - delta
                }
            ]
            [ row []
                [ column
                    [ spacing 5
                    , paddingEach
                        { zeroes
                            | left = 10
                            , right = 10
                            , top = 10
                            , bottom = 10
                        }
                    , Font.color colors.white
                    , Element.htmlAttribute <|
                        Attributes.style "border-radius" "30%"
                    , styleAttribute "background" "rgb(0,0,0,0)"
                    , styleAttribute "background" "radial-gradient(circle, rgba(0,0,0,1) 10%, rgba(0,0,0,0) 99%)"
                    ]
                    descriptionRows
                ]
            ]
        ]


userProfileDescriptionRows : Attribute Msg -> User -> List (Element Msg)
userProfileDescriptionRows smallFont user =
    let
        userRow =
            [ row []
                [ text user.name
                , text " · @"
                , text user.username
                ]
            ]

        prostrs =
            [ if user.is_pro then
                "PRO"

              else
                ""
            , if user.is_premium then
                "*PREMIUM"

              else
                ""
            , if user.is_private then
                "PRIVATE"

              else
                ""
            , if user.is_donor then
                "DONOR"

              else
                ""
            , if user.is_investor then
                "INVESTOR"

              else
                ""
            ]

        proline =
            List.filter (\x -> x /= "") prostrs
                |> String.join " "

        followStr =
            if user.following then
                if user.followed then
                    "You follow each other"

                else
                    "Followed"

            else if user.followed then
                "Follows you"

            else
                ""

        profollow =
            [ if proline == "" then
                []

              else
                [ proline ]
            , if followStr == "" then
                []

              else
                [ followStr ]
            ]
                |> List.concat
                |> String.join " · "

        prorow =
            if profollow == "" then
                []

            else
                [ row [ smallFont ] [ text profollow ] ]
    in
    List.concat [ userRow, prorow ]


formatInt : Int -> String
formatInt int =
    let
        commify substr res =
            if substr == "" then
                String.join "," res

            else
                let
                    len =
                        String.length substr
                in
                if len <= 3 then
                    commify "" <| substr :: res

                else
                    commify (String.dropRight 3 substr)
                        (String.right 3 substr :: res)
    in
    commify (String.fromInt int) []


userCountColumn : Attribute Msg -> String -> Maybe Int -> Element Msg
userCountColumn smallFont label value =
    case value of
        Nothing ->
            Element.none

        Just val ->
            column []
                [ row [ centerX ] [ text <| formatInt val ]
                , row [ centerX, smallFont ] [ text label ]
                ]


userDialog : User -> Bool -> DialogInputs -> Settings -> Element Msg
userDialog user isLoading dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        smallFont =
            fontSize baseFontSize 0.9

        iconHeight =
            userIconHeight (2 * baseFontSize)
    in
    if isLoading then
        column (dialogAttributes settings) <|
            [ dialogTitleBar style baseFontSize <| user.name
            , dialogErrorRow dialogInputs
            , row []
                [ newTabLink style
                    (userUrl user)
                    "Open profile at Gab.com"
                ]
            ]

    else
        column
            (dialogAttributesNoPadding settings)
            [ case user.cover_url of
                Nothing ->
                    Element.none

                Just url ->
                    row
                        [ centerX
                        , Element.inFront <|
                            standardButtonWithDontHover True
                                style
                                ""
                                CloseDialog
                            <|
                                userIconAndInfoOverlay
                                    style
                                    baseFontSize
                                    smallFont
                                    user
                        ]
                        [ standardButton style "" CloseDialog <|
                            image
                                [ width <| px 850
                                , height <| px 315
                                ]
                                { src = url
                                , description = "Cover image."
                                }
                        ]
            , row
                [ paddingEach
                    { zeroes
                        | left = 10
                        , right = 10
                        , bottom = 10
                    }
                , centerX
                ]
                [ column [ spacing 5 ]
                    [ row
                        [ paddingEach { zeroes | left = 70 }
                        , spacing 20
                        ]
                        [ userCountColumn smallFont "Score" user.score
                        , userCountColumn smallFont "Posts" user.post_count
                        , userCountColumn smallFont "Followers" user.follower_count
                        , userCountColumn smallFont "Following" user.following_count
                        ]
                    , row
                        [ centerX
                        , paddingEach { zeroes | top = 5 }
                        ]
                        [ newTabLink style
                            (userUrl user)
                            "Open profile at Gab.com"
                        ]
                    , let
                        feedType =
                            UserFeed user.username
                      in
                      if List.member feedType dialogInputs.feedTypes then
                        Element.none

                      else
                        row [ centerX ]
                            [ textButton style
                                ""
                                (AddNewFeed feedType)
                                "Add feed for user"
                            ]
                    , case user.bio of
                        Nothing ->
                            Element.none

                        Just bio ->
                            row [ paddingEach { zeroes | top = 5 } ]
                                [ column
                                    [ width <| px 700
                                    , paragraphSpacing baseFontSize
                                    ]
                                    (htmlBodyElements style baseFontSize <|
                                        Debug.log "bio" <|
                                            newlinesToPs bio
                                    )
                                ]
                    ]
                ]
            ]


{-| Should probably put a transparent layer over the main view,
so clicks intended to close the dialog don't hit mouse-sensitive regions there.

Or not.

-}
imageDialog : String -> DialogInputs -> Settings -> Element Msg
imageDialog url dialogInputs settings =
    let
        style =
            settings.style

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
        [ centerX
        , centerY
        ]
        [ standardButton style "" CloseDialog <|
            (Html.img
                -- This is black magic.
                -- It took much play with the CSS to get it right.
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


mousePositionDecoder : (( Int, Int ) -> Msg) -> Decoder Msg
mousePositionDecoder wrapper =
    JD.map2 (\x y -> wrapper ( x, y ))
        (JD.field "clientX" JD.int)
        (JD.field "clientY" JD.int)


dialogTitleBar : Style -> Float -> String -> Element Msg
dialogTitleBar style baseFontSize title =
    let
        iconHeight =
            userIconHeight baseFontSize

        closeIcon =
            heightImage (getIconUrl style .close) "Close" iconHeight
    in
    row
        [ width Element.fill
        , paddingEach { zeroes | bottom = 10 }
        ]
        [ row
            [ centerX
            , centerY
            , Font.bold
            , fontSize baseFontSize 1.5
            ]
            [ text <| title ++ " " ]
        , el [ alignRight ]
            (standardButton style "Close Dialog" CloseDialog closeIcon)
        ]


dialogErrorRow : DialogInputs -> Element msg
dialogErrorRow dialogInputs =
    case dialogInputs.dialogError of
        Nothing ->
            Element.none

        Just err ->
            row
                [ paddingEach { zeroes | bottom = 10 }
                , Font.color colors.red
                ]
                [ text err ]


settingsDialog : DialogInputs -> Settings -> Element Msg
settingsDialog dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight (1.5 * baseFontSize)
    in
    column (dialogAttributes settings)
        [ dialogTitleBar style baseFontSize "Settings"
        , dialogErrorRow dialogInputs
        , let
            styleRow =
                { label = "Style:"
                , element =
                    Input.radioRow
                        [ spacing 10
                        ]
                        { onChange = SetStyle
                        , selected = Just settings.styleOption
                        , label = Input.labelLeft [] Element.none
                        , options =
                            [ Input.option LightStyle (text "Light")
                            , Input.option DarkStyle (text "Dark")
                            ]
                        }
                }

            fontSizeRow =
                { label = "Font Size:"
                , element =
                    row []
                        [ Input.radioRow
                            [ spacing 10
                            ]
                            { onChange = FontSizeOption
                            , selected = dialogInputs.fontSizeOption
                            , label = Input.labelLeft [] Element.none
                            , options =
                                [ Input.option SmallSize (text "S")
                                , Input.option MediumSize (text "M")
                                , Input.option LargeSize (text "L")
                                ]
                            }
                        , el [ paddingEach { zeroes | left = 10 } ] Element.none
                        , textInput style
                            baseFontSize
                            { label = ""
                            , text = dialogInputs.fontSizeInput
                            , scale = 4
                            , id = settingsInputId
                            , buttonTitle = ""
                            , doit = SaveSettings
                            , saveit = FontSizeInput
                            , placeholder = Nothing
                            }
                        ]
                }

            columnWidthRow =
                { label = "Column Width:"
                , element =
                    row []
                        [ Input.radioRow
                            [ spacing 10
                            ]
                            { onChange = ColumnWidthOption
                            , selected = dialogInputs.columnWidthOption
                            , label = Input.labelLeft [] Element.none
                            , options =
                                [ Input.option SmallSize (text "S")
                                , Input.option MediumSize (text "M")
                                , Input.option LargeSize (text "L")
                                ]
                            }
                        , el [ paddingEach { zeroes | left = 10 } ] Element.none
                        , textInput style
                            baseFontSize
                            { label = ""
                            , text = dialogInputs.columnWidthInput
                            , scale = 4
                            , id = ""
                            , buttonTitle = ""
                            , doit = SaveSettings
                            , saveit = ColumnWidthInput
                            , placeholder = Nothing
                            }
                        ]
                }
          in
          row
            [ Border.color style.border
            , Border.width 1
            , padding 10
            ]
            [ column
                []
                [ Element.table
                    [ spacing 10 ]
                    { data = [ styleRow, fontSizeRow, columnWidthRow ]
                    , columns =
                        [ { header = Element.none
                          , width = Element.shrink
                          , view =
                                \x ->
                                    row
                                        [ Font.bold
                                        , Element.centerY
                                        ]
                                        [ column [ Element.alignRight ]
                                            [ text x.label ]
                                        ]
                          }
                        , { header = Element.none
                          , width = Element.shrink
                          , view = .element
                          }
                        ]
                    }
                ]
            , column
                [ paddingEach { zeroes | left = 10, top = 10 }
                , centerY
                ]
                [ standardButton style "Save Numbers" SaveSettings <|
                    heightImage (getIconUrl style .save)
                        "Save Numbers"
                        iconHeight
                ]
            ]
        , row
            [ Border.color style.border
            , Border.width 1
            , padding 10
            ]
            [ el
                [ Font.bold
                , paddingEach { zeroes | right = 10 }
                ]
                (text "Auto-size columns:")
            , Input.radioRow
                [ spacing 10
                , paddingEach { zeroes | right = 10 }
                ]
                { onChange = SetAutoSizeColumns
                , selected = Just dialogInputs.autoSizeColumns
                , label = Input.labelLeft [] Element.none
                , options =
                    [ Input.option 1 (text "one")
                    , Input.option 2 (text "two")
                    ]
                }
            , standardButton style "Auto Size" AutoSize <|
                heightImage (getIconUrl style .save)
                    "Auto Size"
                    iconHeight
            ]
        , row [ Font.bold ]
            [ textButton style
                "Restore font size & column width defaults"
                RestoreDefaultSettings
                "Restore Defaults"
            ]
        ]


type alias InputRowParameters =
    { label : String
    , text : String
    , scale : Float
    , id : String
    , buttonTitle : String
    , doit : Msg
    , saveit : String -> Msg
    , placeholder : Maybe String
    }


textInputRow : Style -> Float -> InputRowParameters -> Element Msg
textInputRow style baseFontSize params =
    let
        iconHeight =
            userIconHeight (1.5 * baseFontSize)
    in
    row []
        [ inputLabel params.label
        , textInput style baseFontSize params
        , case params.buttonTitle of
            "" ->
                Element.none

            title ->
                el
                    [ paddingEach { zeroes | left = 10 }
                    , spacing 5
                    ]
                    (standardButton style params.buttonTitle params.doit <|
                        heightImage (getIconUrl style .save)
                            params.buttonTitle
                            iconHeight
                    )
        ]


textInput : Style -> Float -> InputRowParameters -> Element Msg
textInput style baseFontSize params =
    let
        placeholder =
            case params.placeholder of
                Nothing ->
                    Nothing

                Just p ->
                    Just <| Input.placeholder [] (text p)
    in
    Input.text
        [ width
            (px <|
                scaleFontSize baseFontSize
                    (params.scale * dialogFontScale)
            )
        , idAttribute params.id
        , onKeysDownAttribute
            [ ( keycodes.escape, CloseDialog )
            , ( keycodes.enter, params.doit )
            ]
        , Background.color style.quotedPostBackground
        , Font.color style.text
        ]
        { onChange = params.saveit
        , text = params.text
        , placeholder = placeholder
        , label = Input.labelHidden params.label
        }


inputLabel string =
    el
        [ paddingEach { zeroes | right = 5 }
        , Font.bold
        ]
        (text string)


saveFeedsDialog : DialogInputs -> Settings -> Element Msg
saveFeedsDialog dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight (2 * baseFontSize)
    in
    column (dialogAttributes settings)
        [ dialogTitleBar style baseFontSize "Save/Restore Feeds"
        , dialogErrorRow dialogInputs
        , textInputRow
            style
            baseFontSize
            { label = "Feeds:"
            , text = dialogInputs.dialogInput
            , scale = 15
            , id = saveFeedInputId
            , buttonTitle = "Restore Feeds"
            , doit = RestoreFeedTypes
            , saveit = DialogInput
            , placeholder = Just ""
            }
        , textInputRow
            style
            baseFontSize
            { label = "Sets:"
            , text = dialogInputs.feedSetsInput
            , scale = 15.7
            , id = ""
            , buttonTitle = "Restore Feed Sets"
            , doit = RestoreFeedSets
            , saveit = SetFeedSetsInput
            , placeholder = Just "username"
            }
        ]


feedSetChooserDialog : DialogInputs -> Settings -> Element Msg
feedSetChooserDialog dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight (1.5 * baseFontSize)

        feedTypes =
            dialogInputs.feedTypes

        isInstalled feedSet =
            isFeedSetInstalled feedSet feedTypes settings

        feedSetRow feedSet =
            let
                name =
                    feedSet.name
            in
            row []
                [ (if isInstalled feedSet then
                    generalButton
                        [ Font.color style.selected
                        , titleAttribute name
                        ]
                        False

                   else
                    standardButton
                  )
                    style
                    name
                    (RestoreFromFeedSet name)
                    (text name)
                ]
    in
    column
        (dialogAttributes settings)
    <|
        List.concat
            [ [ row [] [ textButton style "Edit Feed Sets" FeedSets "Edit Feed Sets" ] ]
            , List.map feedSetRow dialogInputs.feedSets
            ]


feedSetsDialog : DialogInputs -> Settings -> Element Msg
feedSetsDialog dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight (1.5 * baseFontSize)

        feedTypes =
            dialogInputs.feedTypes

        isInstalled feedSet =
            isFeedSetInstalled feedSet feedTypes settings
    in
    column
        (dialogAttributes settings)
        [ dialogTitleBar style baseFontSize "Feed Sets"
        , dialogErrorRow dialogInputs
        , row []
            [ textButton style "Clear Feeds" ClearFeeds "Clear Feeds" ]
        , textInputRow style
            baseFontSize
            { label = ""
            , text = dialogInputs.currentFeedSet
            , scale = 10
            , id = newFeedSetInputId
            , buttonTitle = "Save"
            , doit = NewFeedSet
            , saveit = SetCurrentFeedSet
            , placeholder = Just "Feed Set Name"
            }
        , row []
            [ Element.table
                [ spacing 10, centerX ]
                { data =
                    List.map (feedSetDialogRow style iconHeight isInstalled dialogInputs)
                        dialogInputs.feedSets
                , columns =
                    [ { header = Element.none
                      , width = Element.shrink
                      , view =
                            \x ->
                                el
                                    [ Font.bold
                                    , Element.alignRight
                                    , centerY
                                    ]
                                    x.name
                      }
                    , { header = Element.none
                      , width = Element.shrink
                      , view =
                            \x ->
                                countBox False x.count (2 * iconHeight)
                      }
                    , { header = Element.none
                      , width = Element.shrink
                      , view =
                            \x ->
                                textInput style
                                    baseFontSize
                                    { label = ""
                                    , text = x.columnWidth
                                    , scale = 4
                                    , id = ""
                                    , buttonTitle = ""
                                    , doit = Noop
                                    , saveit = SetFeedSetColumnWidth x.nameString
                                    , placeholder = Nothing
                                    }
                      }
                    , { header = Element.none
                      , width = Element.shrink
                      , view =
                            \x -> row [ spacing 10, centerY ] x.buttons
                      }
                    ]
                }
            ]
        ]


type alias FeedSetDialogRow =
    { nameString : String
    , name : Element Msg
    , count : Int
    , columnWidth : String
    , buttons : List (Element Msg)
    }


feedSetDialogRow : Style -> Int -> (FeedSet Msg -> Bool) -> DialogInputs -> FeedSet Msg -> FeedSetDialogRow
feedSetDialogRow style iconHeight isInstalled dialogInputs feedSet =
    let
        name =
            feedSet.name
    in
    { nameString = name
    , name =
        (if not <| isInstalled feedSet then
            standardButton

         else
            generalButton
                [ Font.color style.selected
                , titleAttribute name
                ]
                False
        )
            style
            name
            (RestoreFromFeedSet name)
        <|
            text name
    , count =
        case feedSet.feeds of
            Nothing ->
                0

            Just feeds ->
                totalNewPosts feeds
    , columnWidth =
        Dict.get feedSet.name dialogInputs.feedSetColumnWidths
            |> Maybe.withDefault ""
    , buttons =
        let
            makeButton label wrapper url dontHover =
                standardButtonWithDontHover dontHover
                    style
                    label
                    (wrapper name)
                <|
                    heightImage (getIconUrl style url) label iconHeight

            isLoading =
                not <| Set.isEmpty feedSet.loadingFeeds

            reloadButton =
                makeButton "Reload" ReloadFeedSet .reload isLoading
        in
        [ makeButton "Save" SaveToFeedSet .save False
        , if isLoading then
            el [ isLoadingHighlight ] reloadButton

          else
            reloadButton
        , makeButton "Delete" DeleteFeedSet .close False
        ]
    }


isLoadingHighlight : Attribute Msg
isLoadingHighlight =
    -- Border.glow colors.orange 3
    Background.color colors.orange


mouseOverHighlight : Style -> Element.Decoration
mouseOverHighlight style =
    Border.glow style.linkHover 3


addFeedDialog : DialogInputs -> Settings -> Element Msg
addFeedDialog dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight baseFontSize

        addButton feedType =
            el [ Font.size <| 7 * iconHeight // 4 ] <|
                textButton style
                    "Add Feed"
                    (AddNewFeed feedType)
                    "+"

        addUserFeedRow =
            let
                feedType =
                    UserFeed dialogInputs.dialogInput
            in
            { label = "User: "
            , element =
                Input.text
                    [ width
                        (px <|
                            scaleFontSize baseFontSize
                                (200 / defaultFontSize)
                        )
                    , idAttribute addFeedInputId
                    , onKeysDownAttribute
                        [ ( keycodes.escape, CloseDialog )
                        , ( keycodes.enter, AddNewFeed feedType )
                        ]
                    , Background.color style.quotedPostBackground
                    , Font.color style.text
                    ]
                    { onChange = DialogInput
                    , text = dialogInputs.dialogInput
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
                    textButton style
                        "Add Feed"
                        (AddNewFeed feedType)
                        label
            , feedType = Nothing
            }

        choices =
            addFeedChoices dialogInputs settings

        data =
            addUserFeedRow :: List.map makeRow choices
    in
    column (dialogAttributes settings)
        [ dialogTitleBar style baseFontSize "Add Feed"
        , dialogErrorRow dialogInputs
        , row []
            [ Element.table
                [ spacing 10, centerX ]
                { data = data
                , columns =
                    [ { header = Element.none
                      , width = Element.shrink
                      , view =
                            \x ->
                                el [ Font.bold, Element.centerY ]
                                    (text x.label)
                      }
                    , { header = Element.none
                      , width = Element.shrink
                      , view = \x -> x.element
                      }
                    , { header = Element.none
                      , width = Element.shrink
                      , view =
                            \x ->
                                case x.feedType of
                                    Nothing ->
                                        Element.none

                                    Just ft ->
                                        addButton ft
                      }
                    ]
                }
            ]
        ]


chooseUserDialog : List UserChoice -> Settings -> Element Msg
chooseUserDialog completions settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight baseFontSize

        choiceRow choice =
            button
                [ fontSize baseFontSize 0.9
                ]
                { onPress = Just <| CompletePostChoice choice
                , label =
                    row []
                        [ row []
                            [ circularHeightImage choice.picture_url "" iconHeight
                            , text " "
                            ]
                        , row
                            [ Element.mouseOver
                                [ mouseOverHighlight style ]
                            , Font.color style.link
                            ]
                            [ text choice.name
                            , text " (@"
                            , text choice.username
                            , text ")"
                            ]
                        ]
                }
    in
    column
        [ padding 5
        , Border.width 1
        , spacing 5
        , Background.color style.background
        ]
    <|
        List.map choiceRow completions


type WhichPostButton
    = NormalButton
    | QuoteButton
    | CommentButton


postImageShadow : Attribute msg
postImageShadow =
    Border.shadow
        { offset = ( 10, 10 )
        , size = 0
        , blur = 20
        , color = colors.gray
        }


postImageGlow : Element.Decoration
postImageGlow =
    Border.glow colors.red 5


newPostDialog : PostResponseType -> DialogInputs -> Settings -> Element Msg
newPostDialog responseType dialogInputs settings =
    let
        style =
            settings.style

        baseFontSize =
            settings.fontSize

        iconHeight =
            userIconHeight baseFontSize

        ( maybePost, whichButton, feedType ) =
            case responseType of
                NormalPost mp t ->
                    ( mp, NormalButton, t )

                QuotePost p t ->
                    ( Just p, QuoteButton, t )

                CommentOnPost p t ->
                    ( Just p, CommentButton, t )

        renderIcon icon msg title alt alwaysActive dontHover =
            let
                buttonImage =
                    widthImage (getIconUrl style icon) alt iconHeight
            in
            if not alwaysActive && dialogInputs.postInput == "" then
                buttonImage

            else
                standardButtonWithDontHover dontHover
                    style
                    title
                    msg
                    buttonImage

        postImages =
            dialogInputs.postImages

        imageCount =
            List.length postImages

        postImg url mediaId =
            let
                opacity =
                    case mediaId of
                        Nothing ->
                            0.5

                        Just _ ->
                            1.0
            in
            el
                [ width <| Element.maximum (2 * iconHeight) Element.fill
                , Element.clipX
                , Element.alpha opacity
                ]
            <|
                (el
                    [ centerX
                    ]
                 <|
                    heightImage url "image" (2 * iconHeight)
                )

        postImageButton postImage =
            case postImage.url of
                Nothing ->
                    Element.none

                Just url ->
                    button [ Element.mouseOver [ postImageGlow ] ]
                        { onPress = Just <| RemovePostImage postImage.file
                        , label = postImg url postImage.mediaId
                        }

        imageButtons =
            List.map postImageButton postImages

        chooseImageButton =
            if imageCount < 4 then
                renderIcon .camera ChoosePostImage "Choose Image" "Choose" True False

            else
                Element.none

        postButton =
            renderIcon .edit MakePost "Post" "Post" (imageCount > 0) False

        w =
            min (settings.windowWidth * 3 // 4) (scaleFontSize baseFontSize 40)

        h =
            max (settings.windowHeight * 3 // 4)
                (w * settings.windowHeight // settings.windowWidth)
    in
    column (dialogAttributes settings)
        [ dialogTitleBar style baseFontSize "Post"
        , dialogErrorRow dialogInputs
        , row []
            [ column [ height <| px h ]
                [ case maybePost of
                    Nothing ->
                        Element.none

                    Just _ ->
                        row [ Element.centerX ]
                            [ Input.radioRow
                                [ paddingEach { zeroes | bottom = 10 }
                                , spacing 10
                                ]
                                { onChange = PostButton feedType
                                , options =
                                    [ Input.option NormalButton (text "post")
                                    , Input.option QuoteButton (text "quote")
                                    , Input.option CommentButton (text "comment")
                                    ]
                                , selected = Just whichButton
                                , label = Input.labelHidden "Type of post"
                                }
                            ]
                , if whichButton == NormalButton then
                    Element.none

                  else
                    case maybePost of
                        Nothing ->
                            Element.none

                        Just post ->
                            el
                                [ height <| Element.maximum (h // 2) Element.shrink
                                , Element.scrollbarY
                                ]
                            <|
                                postRow
                                    { settings | columnWidth = w - 10 }
                                    feedType
                                    False
                                    { id = ""
                                    , published_at = ""
                                    , type_ = "post"
                                    , actuser = post.user
                                    , post =
                                        { post
                                            | body_html =
                                                post.body_html_summary
                                            , body =
                                                truncatePost post.body
                                            , related =
                                                RelatedPosts
                                                    { parent = Nothing
                                                    , replies = []
                                                    }
                                            , is_quote = False
                                            , is_reply = False
                                            , embed = Nothing
                                            , attachment = NoAttachment
                                        }
                                    }
                                    False
                , row
                    [ height Element.fill
                    , Element.inFront <|
                        case dialogInputs.coordinates of
                            Nothing ->
                                Element.none

                            Just coordinates ->
                                let
                                    caret =
                                        coordinates.caretCoordinates
                                in
                                el
                                    [ paddingEach
                                        { zeroes
                                            | top =
                                                -- May need to put this above
                                                caret.top
                                                    + round (1.5 * baseFontSize)
                                        }
                                    , centerX
                                    ]
                                <|
                                    chooseUserDialog dialogInputs.completions
                                        settings
                    ]
                    [ Input.multiline
                        [ width <| px w
                        , height Element.fill
                        , idAttribute postInputId
                        , Background.color <| style.background
                        , Attributes.value dialogInputs.postInput
                            |> Element.htmlAttribute
                        ]
                        { onChange = PostInput
                        , text = dialogInputs.postInput
                        , placeholder = Nothing
                        , label = Input.labelHidden "Post Text"
                        , spellcheck = True
                        }
                    ]
                ]
            ]
        , row
            [ width Element.fill
            , spacing 10
            ]
          <|
            List.concat
                [ imageButtons
                , [ chooseImageButton
                  , el [ Element.alignRight ] postButton
                  ]
                ]
        ]


okButton : Html Msg
okButton =
    Html.button
        [ class "btn btn-success"
        , onClick CloseDialog
        ]
        [ Html.text "OK" ]


controlColumnWidth : Float -> Int
controlColumnWidth baseFontSize =
    bigUserIconHeight baseFontSize + 2 * columnPadding


controlColumnId : String
controlColumnId =
    "controlColumn"


contentId : String
contentId =
    "contentColumn"


dialogInputId : String
dialogInputId =
    "dialogInput"


postInputId : String
postInputId =
    "postInput"


newFeedSetInputId : String
newFeedSetInputId =
    "newFeedSet"


addFeedInputId : String
addFeedInputId =
    "addFeedInput"


saveFeedInputId : String
saveFeedInputId =
    "feedsStringInput"


settingsInputId : String
settingsInputId =
    "settingsInput"


controlColumn : Int -> Maybe DraggingInfo -> Bool -> Settings -> Icons -> List (Feed Msg) -> Element Msg
controlColumn columnWidth draggingInfo isLoading settings icons feeds =
    let
        isDragging =
            draggingInfo /= Nothing

        style =
            settings.style

        colw =
            width <| px columnWidth

        iconHeight =
            bigUserIconHeight settings.fontSize

        renderIcon icon msg title alt dontHover =
            standardButtonWithDontHover (isDragging || dontHover)
                style
                title
                msg
                (widthImage (getIconUrl style icon) alt iconHeight)
    in
    column
        (List.concat
            [ [ classAttribute "gabdecker-undraggable"
              , colw
              , height Element.fill
              , padding columnPadding
              , Border.widthEach
                    { zeroes
                        | top = columnBorderWidth
                        , bottom = columnBorderWidth
                        , right = columnBorderWidth
                    }
              , Border.color style.border
              , Background.color style.headerBackground
              , spacing 10
              , idAttribute controlColumnId
              ]
            , columnBorderAttributes style True
            , case dragImage settings draggingInfo icons of
                Nothing ->
                    []

                Just image ->
                    -- This only moves when something else changes.
                    --[ Element.inFront image ]
                    []
            ]
        )
    <|
        List.concat
            [ [ row
                    (List.concat
                        [ [ centerX
                          , Background.color style.headerBackground
                          ]
                        , if isLoading then
                            [ isLoadingHighlight ]

                          else
                            []
                        ]
                    )
                    [ renderIcon .reload
                        LoadAll
                        "Refresh All Feeds"
                        "Refresh"
                        isLoading
                    ]
              , row
                    [ centerX
                    , Background.color style.headerBackground
                    , paddingEach { zeroes | bottom = 10 }
                    ]
                    [ renderIcon .edit
                        (NewPost (NormalPost Nothing HomeFeed))
                        "Create a New Post"
                        "Post"
                        False
                    ]
              ]
            , List.map (feedSelectorButton isDragging style iconHeight icons) <|
                case draggingInfo of
                    Nothing ->
                        feeds

                    Just info ->
                        info.feeds
            , [ row
                    [ paddingEach { zeroes | top = 10 }
                    , centerX
                    , Font.size <| 7 * iconHeight // 4
                    ]
                    [ column [ spacing 0 ]
                        [ renderIcon .feedsets
                            FeedSetChooser
                            "FeedSets"
                            "FeedSet Chooser"
                            False
                        , standardButtonWithDontHover isDragging
                            style
                            "Add New Feed"
                            AddFeed
                            (text "+")
                        ]
                    ]
              , row
                    [ alignBottom
                    , paddingEach { zeroes | bottom = 10 }
                    ]
                    [ column [ spacing 10 ]
                        [ renderIcon .settings
                            ShowSettings
                            "Settings"
                            "Settings"
                            False
                        , renderIcon .save
                            SaveFeedTypes
                            "Restore Feeds"
                            "Save"
                            False
                        , renderIcon .logout
                            Logout
                            "Logout"
                            "Logout"
                            False
                        ]
                    ]
              ]
            ]


feedTypeIconUrl : Style -> FeedType -> Icons -> ( String, String, Bool )
feedTypeIconUrl style feedType icons =
    case feedType of
        HomeFeed ->
            ( getIconUrl style .home, "Home", False )

        UserFeed username ->
            ( lookupUserIconUrl style username icons, "User: " ++ username, True )

        GroupFeed groupid ->
            ( "", "Group: " ++ groupid, False )

        TopicFeed topicid ->
            ( "", "Topic: " ++ topicid, False )

        PopularFeed ->
            ( getIconUrl style .popular, "Popular", False )

        NotificationsFeed ->
            ( getIconUrl style .notifications, "Notifications", False )

        _ ->
            ( "", "", False )


feedSelectorButton : Bool -> Style -> Int -> Icons -> Feed Msg -> Element Msg
feedSelectorButton dontHover style iconHeight icons feed =
    case feedTypeIconUrl style feed.feedType icons of
        ( "", _, _ ) ->
            Element.none

        ( url, label, isCircular ) ->
            row
                [ centerX ]
                [ standardButtonWithDontHover dontHover
                    style
                    label
                    (ScrollToFeed feed.feedType)
                  <|
                    (if isCircular then
                        circularHeightImageWithCount

                     else
                        heightImageWithCount
                    )
                        feed.newPosts
                        url
                        label
                        (if isCircular then
                            adjustUserIconHeight iconHeight

                         else
                            iconHeight
                        )
                ]


feedSelectorImage : Style -> Int -> Icons -> FeedType -> Element Msg
feedSelectorImage style iconHeight icons feedType =
    case feedTypeIconUrl style feedType icons of
        ( "", _, _ ) ->
            Element.none

        ( url, label, isCircular ) ->
            (if isCircular then
                circularHeightImage

             else
                heightImage
            )
                url
                label
                (if isCircular then
                    adjustUserIconHeight iconHeight

                 else
                    iconHeight
                )


circularHeightImageWithCount : Int -> String -> String -> Int -> Element msg
circularHeightImageWithCount count src description h =
    heightImageWithCountInternal count
        (circularHeightImage src description h)
        h


heightImageWithCount : Int -> String -> String -> Int -> Element msg
heightImageWithCount count src description h =
    heightImageWithCountInternal count
        (heightImage src description h)
        h


heightImageWithCountInternal : Int -> Element msg -> Int -> Element msg
heightImageWithCountInternal count img h =
    el
        [ Element.inFront <| countBox True count h ]
        img


countBox : Bool -> Int -> Int -> Element msg
countBox doOffset count h =
    let
        size =
            h // 2
    in
    if count == 0 then
        Element.none

    else
        el
            (List.append
                [ width <| px size
                , height <| px size
                , Font.color colors.red
                , Background.color colors.white
                ]
                (if doOffset then
                    [ Element.moveRight <| toFloat size
                    , Element.moveDown <| toFloat size
                    ]

                 else
                    []
                )
            )
            (el
                [ Font.size <| 8 * h // 16
                , centerX
                , centerY
                ]
                (text <| String.fromInt count)
            )


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


classAttribute : String -> Attribute msg
classAttribute string =
    Element.htmlAttribute <| Attributes.class string


columnId : Int -> String
columnId idx =
    "column" ++ String.fromInt idx


columnIdAttribute : Int -> Attribute msg
columnIdAttribute idx =
    idAttribute <| columnId idx


columnBorderAttributes : Style -> Bool -> List (Attribute msg)
columnBorderAttributes style isControlColumn =
    [ if isControlColumn then
        Border.widthEach { zeroes | top = 3, bottom = 3 }

      else
        Border.width 3
    , Border.color style.border
    ]


columnPadding : Int
columnPadding =
    10


{-| Circular images look smaller. Accomodate.
-}
adjustUserIconHeight : Int -> Int
adjustUserIconHeight iconHeight =
    11 * iconHeight // 10


userIconHeight : Float -> Int
userIconHeight baseFontSize =
    round (4 * baseFontSize / 3)


bigUserIconHeight : Float -> Int
bigUserIconHeight baseFontSize =
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
        style =
            settings.style

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
            , columnBorderAttributes style False
            ]
        )
        [ row
            [ fillWidth
            , Border.widthEach { zeroes | bottom = 1 }
            , Border.color style.border
            , Background.color style.headerBackground
            ]
            [ column [ colw ]
                [ let
                    closeIcon =
                        heightImage (getIconUrl style .close) "Close" iconHeight

                    prevIcon =
                        heightImage (getIconUrl style .previous) "Move Left" iconHeight

                    nextIcon =
                        heightImage (getIconUrl style .next) "Move Right" iconHeight
                  in
                  row
                    [ padding columnPadding
                    , fontSize baseFontSize 1.5
                    , Font.bold
                    , centerX
                    , width Element.fill
                    ]
                    [ row [ alignLeft ]
                        [ standardButton style
                            "Move Feed Left"
                            (MoveFeedLeft feed.feedType)
                            prevIcon
                        , text " "
                        , standardButton style
                            "Move Feed Right"
                            (MoveFeedRight feed.feedType)
                            nextIcon
                        ]
                    , row
                        [ centerX ]
                        [ el
                            (if isLoading then
                                [ isLoadingHighlight ]

                             else
                                []
                            )
                          <|
                            standardButtonWithDontHover isLoading
                                style
                                "Refresh Feed"
                                (LoadMore MergeUpdate feed)
                                (heightImage (getIconUrl style .reload) "Refresh" iconHeight)
                        , text " "
                        , feedTypeDescription style
                            feed.newPosts
                            feed.feedType
                            baseFontSize
                            icons
                        ]
                    , el [ alignRight ]
                        (standardButton style "Close Feed" (CloseFeed feed) closeIcon)
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
        style =
            settings.style

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
            , Element.scrollbarY
            , Element.clipX
            ]
          <|
            let
                data =
                    feed.feed.data

                rows =
                    case data of
                        PostFeedData activityLogList ->
                            optimizers.keyedPostRow
                                []
                            <|
                                List.map2
                                    (\log idx ->
                                        ( log.id
                                        , optimizers.postRow
                                            settings
                                            feed.feedType
                                            True
                                            log
                                            (idx == feed.newPosts)
                                        )
                                    )
                                    activityLogList
                                    (List.range 1 <| List.length activityLogList)

                        NotificationFeedData gangedNotificationList ->
                            optimizers.keyedNotificationRow
                                []
                            <|
                                List.map2
                                    (\notification idx ->
                                        ( notification.notification.id
                                        , optimizers.notificationRow
                                            settings
                                            True
                                            notification
                                            (idx == feed.newPosts)
                                        )
                                    )
                                    gangedNotificationList
                                    (List.range 1 <|
                                        List.length gangedNotificationList
                                    )
            in
            let
                typeString =
                    feedTypeToString feed.feedType
            in
            [ if False then
                undoneRow style typeString

              else
                Element.none
            , rows
            , moreRow style colw feed
            ]
        ]


{-| Same signature as Keyed.el, to allow quick switch between keyed and unkeyed.
-}
keyedEl : List (Attribute msg) -> ( String, Element msg ) -> Element msg
keyedEl attributes pair =
    el attributes <| Tuple.second pair


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
undoneRow : Style -> String -> Element Msg
undoneRow style typeString =
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
        , Border.color style.border
        ]
        [ text "Comment parents are coming."
        ]


moreRow : Style -> Attribute Msg -> Feed Msg -> Element Msg
moreRow style colw feed =
    if feed.feed.no_more || List.member feed.feedType nonMergingFeedTypes then
        Element.none

    else if
        case feed.feed.data of
            PostFeedData activityLogList ->
                activityLogList == []

            NotificationFeedData gangedNotificationList ->
                gangedNotificationList == []
    then
        Element.none

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
                    LoadMore AppendUpdate feed
              in
              textButton style
                ""
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


postRow : Settings -> FeedType -> Bool -> ActivityLog -> Bool -> Element Msg
postRow settings feedType isToplevel log isLastNew =
    let
        style =
            settings.style

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

        post =
            log.post

        user =
            post.user

        username =
            user.username

        actuser =
            log.actuser

        ( repostString, iconUrl ) =
            if log.type_ == "repost" then
                ( "reposted", getIconUrl style .refresh )

            else if post.is_reply then
                ( "commented", getIconUrl style .comment )

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
        , Border.widthEach
            { zeroes
                | bottom =
                    if isToplevel then
                        if isLastNew then
                            10

                        else
                            2

                    else
                        0
            }
        , Border.color <|
            if isToplevel && isLastNew then
                colors.red

            else
                style.border
        ]
        [ column []
            [ if repostString == "" then
                Element.none

              else
                row
                    [ Border.widthEach { zeroes | bottom = 1 }
                    , Border.color style.border
                    , Font.bold
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
                        , userButton style
                            False
                            (\u -> text <| " " ++ u.name ++ " " ++ repostString)
                            ""
                            actuser
                        ]
                    ]
            , postUserRow style baseFontSize colwp settings.here post
            , row []
                [ Element.textColumn
                    [ paragraphSpacing baseFontSize
                    , colwp
                    , paddingEach { zeroes | top = 5 }
                    ]
                  <|
                    case post.body_html of
                        Nothing ->
                            let
                                body =
                                    addHiddenLink False settings post.body
                            in
                            htmlBodyElements style baseFontSize <|
                                newlinesToPs body

                        Just html ->
                            let
                                fixedHtml =
                                    if feedType == PopularFeed then
                                        fixBareHtml html

                                    else
                                        html

                                body =
                                    addHiddenLink False settings html
                            in
                            htmlBodyElements style baseFontSize body
                ]
            , row []
                [ column [ colwp ] <|
                    attachmentRows cwp style post
                ]
            , row []
                [ column
                    [ paddingEach { zeroes | top = 5, bottom = 5 } ]
                    [ row
                        [ paddingEach { zeroes | left = 5 }
                        , Background.color style.quotedPostBackground
                        , Border.width 1
                        , Border.color style.quotedPostBorder
                        ]
                        [ case post.related of
                            RelatedPosts { parent } ->
                                case parent of
                                    Nothing ->
                                        Element.none

                                    Just parentPost ->
                                        postRow
                                            { settings | columnWidth = cwp - 10 }
                                            feedType
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
                                                        , related =
                                                            RelatedPosts
                                                                { parent = Nothing
                                                                , replies = []
                                                                }
                                                        , is_quote = False
                                                        , is_reply = False
                                                    }
                                                , type_ = "post"
                                            }
                                            False
                        ]
                    ]
                ]
            , if not isToplevel then
                Element.none

              else
                interactionRow style baseFontSize colwp feedType post
            ]
        ]


addHiddenLink : Bool -> Settings -> String -> String
addHiddenLink forNotifications settings body =
    let
        suffix =
            if forNotifications then
                "<br />"

            else
                " "

        replaceOrPrefix =
            if settings.showHidden then
                Prefix <| "<br />" ++ hideHtml ++ suffix

            else
                Replace showHtml
    in
    Parsers.replaceAtUsers 5 replaceOrPrefix body


attachmentRows : Int -> Style -> Post -> List (Element Msg)
attachmentRows cwp style post =
    let
        mediaw =
            width <| px cwp
    in
    case post.attachment of
        MediaAttachment records ->
            List.map (mediaRow style mediaw) records

        YoutubeAttachment id ->
            [ embedYouTube cwp id ]

        GiphyAttachment useless ->
            case giphyIdFromEmbed post.embed of
                Nothing ->
                    []

                Just id ->
                    [ embedGiphy cwp id ]

        _ ->
            []


postUserRow : Style -> Float -> Attribute Msg -> Zone -> Post -> Element Msg
postUserRow style baseFontSize colwp here post =
    let
        user =
            post.user

        username =
            user.username

        badgeSize =
            scaleFontSize baseFontSize (25 / 15)

        badgeOffset =
            scaleFontSize baseFontSize 1

        iconSize =
            scaleFontSize baseFontSize (40 / 15)
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
            [ row (verifiedBadge style badgeSize badgeOffset True user)
                [ userIconButton style iconSize "" user ]
            ]
        , column []
            [ row [ nameBottomPadding ]
                [ userButton style
                    False
                    (\u -> text <| embiggen u.name)
                    ""
                    user
                , text <| " (@" ++ username ++ ")"
                ]
            , case post.group of
                Just { id, title } ->
                    let
                        url =
                            "https://gab.com/groups/" ++ id
                    in
                    row [ nameBottomPadding ]
                        [ newTabLink style url <| "Group: " ++ title ]

                _ ->
                    case post.topic of
                        Just { id, title } ->
                            let
                                url =
                                    "https://gab.com/topic/" ++ id
                            in
                            row [ nameBottomPadding ]
                                [ newTabLink style url <| "Topic: " ++ title ]

                        _ ->
                            Element.none
            , let
                url =
                    "https://gab.com/"
                        ++ username
                        ++ "/posts/"
                        ++ post.id
              in
              row []
                [ newTabLink style url <|
                    iso8601ToString here post.created_at
                ]
            ]
        ]


userNameLink : Style -> User -> Element Msg
userNameLink style user =
    userNameButton style "" user


notificationDescriptionLine : Style -> User -> String -> Maybe Post -> String -> Element Msg
notificationDescriptionLine style user middle maybePost postName =
    row
        [ Border.widthEach { zeroes | bottom = 1 }
        , Border.color style.border
        , paddingEach { zeroes | bottom = 5 }
        , fillWidth
        ]
        [ userNameLink style user
        , text middle
        , case maybePost of
            Nothing ->
                text postName

            Just post ->
                newTabLink style (postUrl post) postName
        , text "."
        ]


postCreatedLink : Style -> Post -> Zone -> Element Msg
postCreatedLink style post here =
    newTabLink style (postUrl post) <| iso8601ToString here post.created_at


notificationTypeToDescription : Settings -> NotificationType -> Bool -> Notification -> List User -> Element Msg
notificationTypeToDescription settings typ isComment notification otherUsers =
    -- "comment", "follow", "like", "mention", "repost", "comment-reply"
    let
        style =
            settings.style

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
            notificationDescriptionLine style
                actuser
                (otherUsersString ++ " liked ")
                maybePost
                ("your " ++ postOrComment)

        RepostNotification ->
            notificationDescriptionLine style
                actuser
                (otherUsersString ++ " reposted ")
                maybePost
                ("your " ++ postOrComment)

        FollowNotification ->
            notificationDescriptionLine style
                actuser
                (otherUsersString ++ " followed you")
                Nothing
                ""

        MentionNotification ->
            let
                ( commentedOn, maybeComment ) =
                    case maybePost of
                        Nothing ->
                            ( False, Nothing )

                        Just post ->
                            case post.related of
                                RelatedPosts { parent } ->
                                    case parent of
                                        Nothing ->
                                            ( False, Nothing )

                                        Just pp ->
                                            case settings.loggedInUser of
                                                Nothing ->
                                                    ( False, Nothing )

                                                Just you ->
                                                    ( you == pp.user.username
                                                    , parent
                                                    )
            in
            -- Sometimes this should be " commented on your post"
            if commentedOn then
                notificationDescriptionLine style
                    actuser
                    " commented on "
                    maybeComment
                    ("your " ++ postOrComment)

            else
                notificationDescriptionLine style
                    actuser
                    " mentioned you "
                    maybePost
                    ("in a " ++ postOrComment)

        UnknownNotification "comment" ->
            notificationDescriptionLine style
                actuser
                " commented on your "
                maybePost
                postOrComment

        UnknownNotification "comment-reply" ->
            notificationDescriptionLine style
                actuser
                " replied to your "
                maybePost
                postOrComment

        UnknownNotification message ->
            case maybePost of
                Nothing ->
                    text notification.message

                Just post ->
                    newTabLink style (postUrl post) notification.message


styleNode : String -> Html msg
styleNode css =
    Html.node "style" [] [ Html.text css ]


{-| <https://stackoverflow.com/a/35924844/1386989>
-}
undraggableImageStyleNode : Html msg
undraggableImageStyleNode =
    styleNode """.gabdecker-undraggable img {
  -moz-user-select: none;
  -webkit-user-select: none;
  -ms-user-select: none;
  user-select: none;
  -webkit-user-drag: none;
  user-drag: none;
  -webkit-touch-callout:
}"""


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


notificationRow : Settings -> Bool -> GangedNotification -> Bool -> Element Msg
notificationRow settings isToplevel gangedNotification isLastNew =
    let
        style =
            settings.style

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
            { zeroes
                | bottom =
                    if isToplevel then
                        if isLastNew then
                            10

                        else
                            2

                    else
                        0
            }
        , Border.color <|
            if isToplevel && isLastNew then
                colors.red

            else
                style.border
        ]
        [ column []
            [ row
                [ paddingEach { zeroes | top = 5, bottom = 0 }
                , Font.bold
                , fillWidth
                ]
                [ notificationTypeToDescription settings
                    notification.type_
                    (maybeParent /= Nothing)
                    notification
                    otherUsers
                ]
            , row []
                [ case maybePost of
                    Nothing ->
                        Element.none

                    Just post ->
                        Element.textColumn
                            [ paragraphSpacing baseFontSize
                            , colwp
                            ]
                        <|
                            List.concat
                                [ [ el
                                        [ paddingEach
                                            { zeroes
                                                | top = 5
                                                , bottom = 2
                                            }
                                        , Font.bold
                                        ]
                                        (postCreatedLink style post here)
                                  ]
                                , notificationsBody settings post
                                , case settings.loggedInUser of
                                    -- Maybe the interaction row should
                                    -- always be there, not just for other
                                    -- people's posts.
                                    Nothing ->
                                        []

                                    Just you ->
                                        if you == post.user.username then
                                            []

                                        else
                                            [ interactionRow style
                                                baseFontSize
                                                colwp
                                                NotificationsFeed
                                                post
                                            ]
                                , if notification.type_ == MentionNotification then
                                    attachmentRows cwp style post

                                  else
                                    []
                                ]
                ]
            , case maybeParent of
                Just pp ->
                    notificationParentRow (cw - colpad)
                        settings
                        baseFontSize
                        pp

                Nothing ->
                    Element.none
            , if not isToplevel then
                Element.none

              else
                let
                    allUsers =
                        notification.actuser :: otherUsers

                    height =
                        scaleFontSize baseFontSize (30 / 15)

                    userImage user =
                        userIconButton style height user.name user
                in
                Element.textColumn [ colwp ]
                    [ paragraph
                        [ paddingEach { zeroes | top = 5, bottom = 4 }
                        , spacing 4
                        ]
                        (List.take maxNotificationUserCount allUsers
                            |> List.map userImage
                        )
                    ]
            ]
        ]


maxNotificationUserCount : Int
maxNotificationUserCount =
    50


notificationParentRow : Int -> Settings -> Float -> Post -> Element Msg
notificationParentRow cw settings baseFontSize post =
    let
        style =
            settings.style

        colpad =
            5

        cwp =
            cw - 2 * colpad - 6

        colwp =
            width <| px cwp

        user =
            post.user
    in
    row [ paddingEach { zeroes | top = 5 } ]
        [ column
            [ colwp
            , paddingEach { zeroes | bottom = 5, left = 5 }
            , Background.color style.quotedPostBackground
            , Border.width 1
            , Border.color style.quotedPostBorder
            ]
          <|
            List.concat
                [ [ postUserRow style baseFontSize colwp settings.here post ]
                , notificationsBody settings post
                ]
        ]


truncatePost : String -> String
truncatePost body =
    String.left 200 body


notificationsBody : Settings -> Post -> List (Element Msg)
notificationsBody settings post =
    let
        style =
            settings.style
    in
    case post.body_html_summary of
        Just html ->
            let
                body =
                    addHiddenLink True settings html
            in
            htmlBodyElements style settings.fontSize body

        Nothing ->
            let
                body1 =
                    truncatePost post.body

                body =
                    (if
                        String.length body1
                            == String.length post.body
                     then
                        body1

                     else
                        body1 ++ "..."
                    )
                        |> addHiddenLink True settings
            in
            htmlBodyElements style settings.fontSize <|
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


interactionRow : Style -> Float -> Attribute Msg -> FeedType -> Post -> Element Msg
interactionRow style baseFontSize colwp feedType post =
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
                        Element.none

                      else
                        text label
                    , text " "
                    , if count < 0 then
                        Element.none

                      else
                        let
                            str =
                                chars.nbsp ++ String.fromInt count ++ chars.nbsp
                        in
                        el [ Background.color style.postcountBackground ]
                            (text str)
                    ]
                    |> (if msg == Noop then
                            identity

                        else
                            standardButton style buttonLabel msg
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
        [ onecol (heightImage (getIconUrl style .like) "upvote" fsize)
            ( post.liked, GreenHighlight )
            ""
            post.like_count
            "upvote"
            (Upvote feedType post)
        , onecol (heightImage (getIconUrl style .dislike) "downvote" fsize)
            ( post.disliked, RedHighlight )
            ""
            post.dislike_count
            "downvote"
            (Downvote feedType post)
        , onecol (heightImage (getIconUrl style .comment) "comment" fsize)
            ( False, NoHighlight )
            ""
            post.reply_count
            ""
            (NewPost (CommentOnPost post feedType))
        , onecol (heightImage (getIconUrl style .refresh) "reposted" fsize)
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
            (NewPost (QuotePost post feedType))
        ]


youTubeEmbedHeight : Int -> Int
youTubeEmbedHeight width =
    9 * width // 16


youTubeEmbedPrefix : String
youTubeEmbedPrefix =
    "https://www.youtube.com/embed/"


youTubeEmbedHtml : Int -> String -> Html msg
youTubeEmbedHtml cwp id =
    let
        height =
            youTubeEmbedHeight cwp
    in
    Html.iframe
        [ Attributes.width cwp
        , Attributes.height height
        , Attributes.src <| youTubeEmbedPrefix ++ id
        , frameborderAttr "1"
        , allowAttr "accelerometer; encrypted-media; gyroscope; picture-in-picture"
        , allowfullscreenAttr "1"
        ]
        []


giphyEmbedHeight : Int -> Int
giphyEmbedHeight width =
    360 * width // 480


giphyEmbedPrefix : String
giphyEmbedPrefix =
    "https://giphy.com/embed/"


{-| How Gihpy spells it:

    <iframe src="<https://giphy.com/embed/pUeXcg80cO8I8">
            width="480"
            height="360"
            frameBorder="0"
            class="giphy-embed"
            allowFullScreen>
    </iframe>

-}
giphyEmbedHtml : Int -> String -> Html msg
giphyEmbedHtml cwp id =
    let
        height =
            giphyEmbedHeight cwp
    in
    Html.iframe
        [ Attributes.width cwp
        , Attributes.height height
        , Attributes.src <| giphyEmbedPrefix ++ id
        , frameborderAttr "1"
        , allowfullscreenAttr "1"
        ]
        []


{-| The Gab API doesn't give us the Giphy ID directly.

It's encoded in Post.embed's html as:

    "<a \n  href=\"https://media3.giphy.com/media/guufsF0Az3Lpu/giphy.gif?cid=e1bb72ff5c0f82c0505147713611b129\" \n  target=\"_blank\" class=\"post__embed__body post__embed__body--photo \n  post__embed__body--gif\"><div class=\"post__embed__body__image\" \n  style=\"background-image: \n  url('https://ipr.gab.ai/d6d4dfcc1b5faa6426608fc1c85b464ec4d4488b/68747470733a2f2f6d65646961332e67697068792e636f6d2f6d656469612f67757566734630417a334c70752f67697068792e6769663f6369643d6531626237326666356330663832633035303531343737313336313162313239/')\"></div></a>"

-}
giphyIdFromEmbed : Maybe Embed -> Maybe String
giphyIdFromEmbed maybeEmbed =
    case maybeEmbed of
        Nothing ->
            Nothing

        Just embed ->
            case HP.run embed.html of
                Err _ ->
                    Nothing

                Ok elements ->
                    case elements of
                        (HP.Element "a" attrs _) :: _ ->
                            case attrs of
                                ( "href", url ) :: _ ->
                                    let
                                        id =
                                            SE.rightOf "media/" url
                                                |> SE.leftOf "/"
                                    in
                                    if id == "" then
                                        Nothing

                                    else
                                        Just id

                                _ ->
                                    Nothing

                        _ ->
                            Nothing


frameborderAttr =
    Attributes.attribute "frameborder"


allowAttr =
    Attributes.attribute "allow"


allowfullscreenAttr =
    Attributes.attribute "allowfullscreen"


embedYouTube : Int -> String -> Element msg
embedYouTube cwp id =
    row
        [ paddingEach
            { zeroes
                | top = 5
                , bottom = 5
            }
        ]
        [ youTubeEmbedHtml cwp id
            |> Element.html
        ]


embedGiphy : Int -> String -> Element msg
embedGiphy cwp id =
    row
        [ paddingEach
            { zeroes
                | top = 5
                , bottom = 5
            }
        ]
        [ giphyEmbedHtml cwp id
            |> Element.html
        ]


mediaRow : Style -> Attribute Msg -> MediaRecord -> Element Msg
mediaRow style colw record =
    row
        [ paddingEach
            { zeroes
                | top = 5
                , bottom = 5
            }
        ]
        [ standardButtonWithDontHover True
            style
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


paragraphSpacingAmount : Float -> Int
paragraphSpacingAmount baseFontSize =
    round (0.6 * baseFontSize)


paragraphSpacing : Float -> Attribute msg
paragraphSpacing baseFontSize =
    spacing <| paragraphSpacingAmount baseFontSize


paragraphLineSpacing : Float -> Attribute msg
paragraphLineSpacing baseFontSize =
    spacing <| round (0.25 * baseFontSize)


psep : String
psep =
    "</p><p>"


crlf : String
crlf =
    "\u{000D}\n"


replace : String -> String -> String -> String
replace old new string =
    String.split old string
        |> String.join new


brsep : String
brsep =
    "<br />"


newlinesToPs : String -> String
newlinesToPs string =
    replace "\n\n" psep string
        |> replace (crlf ++ crlf) psep
        |> replace "\n" brsep
        |> replace crlf brsep
        |> wrapWithP


wrapWithP : String -> String
wrapWithP string =
    case string of
        "" ->
            ""

        _ ->
            "<p>" ++ string ++ "</p>"


htmlBodyElements : Style -> Float -> String -> List (Element Msg)
htmlBodyElements style baseFontSize html =
    case HP.run html of
        Ok res ->
            List.map (nodeToElements style baseFontSize) res
                |> List.concat

        Err _ ->
            oldHtmlBodyElements style baseFontSize html


{-| The Popular feed currently doesn't really ship Html in body\_html.
-}
fixBareHtml : String -> String
fixBareHtml html =
    if String.startsWith "<p>" html then
        html

    else
        replace "\n" psep html
            |> replace crlf psep
            |> wrapWithP


atUserRenderer : Style -> String -> String -> Element Msg
atUserRenderer style username linkText =
    Html.a
        [ href "#"
        , onClick <| ShowUserDialog username
        , Attributes.title "Show user profile"
        ]
        [ Html.text linkText ]
        |> Element.html


{-| Like String.split for lists
-}
splitList : a -> List a -> List (List a)
splitList a list =
    let
        loop : List a -> List a -> List (List a) -> List (List a)
        loop rest accum res =
            case rest of
                [] ->
                    if accum == [] then
                        List.reverse res

                    else
                        List.reverse (List.reverse accum :: res)

                head :: tail ->
                    if head == a then
                        loop tail [] (List.reverse accum :: res)

                    else
                        loop tail (head :: accum) res
    in
    loop list [] []


showHideTag : String
showHideTag =
    "param"


showHideAttribute : String
showHideAttribute =
    "type"


showType : String
showType =
    "show"


hideType : String
hideType =
    "hide"


showHideHtml : Bool -> String
showHideHtml show =
    let
        type_ =
            if show then
                showType

            else
                hideType
    in
    "<" ++ showHideTag ++ " type='" ++ type_ ++ "' />"


showHtml : String
showHtml =
    showHideHtml True


hideHtml : String
hideHtml =
    showHideHtml False


hideElement : Style -> Element Msg
hideElement style =
    textButton style "Hide @user list" (ShowHiddenText False) "--hide @user list--"


showElement : Style -> Element Msg
showElement style =
    textButton style "Show @user list" (ShowHiddenText True) "--show @user list--"


nodeToElements : Style -> Float -> HP.Node -> List (Element Msg)
nodeToElements style baseFontSize theNode =
    let
        lineSpacing =
            paragraphLineSpacing baseFontSize

        brElement =
            HP.Element "br" [] []

        wrapParagraph : List HP.Node -> Element Msg
        wrapParagraph elements =
            paragraph [ lineSpacing ] <|
                mappedNodes elements

        mappedNodes nodes =
            List.map recurse nodes
                |> List.concat

        recurse : HP.Node -> List (Element Msg)
        recurse node =
            case node of
                HP.Text string ->
                    -- Gab gives us "</p>\n<p>" between paragraphs.
                    if string == "\n" then
                        [ Element.none ]

                    else
                        Parsers.parseElements style atUserRenderer string

                HP.Element tag attributes nodes ->
                    case tag of
                        "span" ->
                            mappedNodes nodes

                        "p" ->
                            splitList brElement nodes
                                |> List.map wrapParagraph

                        "br" ->
                            -- These should all be dealt with in the "p" branch
                            [ text "\n" ]

                        "blockquote" ->
                            [ row
                                [ padding 5
                                , Border.width 2
                                , Border.color colors.black
                                , Background.color style.quotedPostBackground
                                ]
                                [ Element.textColumn
                                    [ fillWidth
                                    , paragraphSpacing baseFontSize
                                    ]
                                  <|
                                    mappedNodes nodes
                                ]
                            ]

                        "param" ->
                            case LE.find (\( attr, _ ) -> attr == "type") attributes of
                                Nothing ->
                                    [ Element.none ]

                                Just ( _, type_ ) ->
                                    [ if type_ == showType then
                                        showElement style

                                      else
                                        hideElement style
                                    ]

                        _ ->
                            if
                                List.member tag emTags
                                    || String.contains ":" tag
                            then
                                List.map (\n -> el (emTagToAttributes tag) n) <|
                                    mappedNodes nodes

                            else
                                -- Shouldn't happen
                                mappedNodes nodes

                _ ->
                    []
    in
    recurse <| emify theNode


emTagToAttributes : String -> List (Attribute msg)
emTagToAttributes tag =
    String.split ":" tag
        |> List.map emTagToAttribute
        |> List.concat


emTagToAttribute : String -> List (Attribute msg)
emTagToAttribute tag =
    case tag of
        "strong" ->
            [ Font.bold ]

        "em" ->
            [ Font.italic ]

        "u" ->
            [ Font.underline ]

        _ ->
            []


emTags : List String
emTags =
    [ "strong", "em", "u" ]


emify : HP.Node -> HP.Node
emify node =
    case node of
        HP.Element tag attributes nodes ->
            if List.member tag emTags then
                HP.Element "span" [] <| List.map (subEmify tag) nodes

            else
                HP.Element tag attributes <|
                    List.map emify nodes

        _ ->
            node


subEmify : String -> HP.Node -> HP.Node
subEmify tag node =
    case node of
        HP.Text string ->
            HP.Element tag [] [ node ]

        HP.Element nodeTag attributes nodes ->
            if List.member nodeTag emTags then
                let
                    subtag =
                        tag ++ ":" ++ nodeTag
                in
                HP.Element "span" [] <|
                    List.map (subEmify subtag) nodes

            else
                HP.Element tag [] [ node ]

        _ ->
            HP.Text ""


oldHtmlBodyElements : Style -> Float -> String -> List (Element Msg)
oldHtmlBodyElements style baseFontSize html =
    let
        par : List (Element Msg) -> Element Msg
        par elements =
            paragraph
                [ paragraphPadding
                , paragraphLineSpacing baseFontSize
                ]
                elements
    in
    -- may want to convert single <br /> to row instead of paragraph.
    splitIntoParagraphs html
        |> List.map (Parsers.parseElements style atUserRenderer)
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
        style =
            settings.style

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
                [ simpleLink lightStyle "./" "GabDecker"
                , text " is a "
                , simpleLink lightStyle "https://tweetdeck.twitter.com" "TweetDeck"
                , text "-like interface to "
                , simpleLink lightStyle "https://gab.com" "Gab.com"
                , text "."
                ]
            , row [ centerX ]
                [ text "This is a work in progress." ]
            , row [ centerX ]
                [ textButton style "" Login "Login" ]
            , row [ centerX ]
                [ simpleImage "images/gabdecker-background-gahtannd.jpg"
                    "GabDecker Splash Image"
                    ( 768, 432 )
                ]
            , row [ centerX ]
                [ simpleLink lightStyle "news/" "News" ]
            , row [ centerX ]
                [ simpleLink lightStyle "api/" "Gab API Explorer" ]
            , row [ centerX ]
                [ column [ centerX, spacing 6, fontSize baseFontSize 1 ]
                    [ row [ centerX ]
                        [ text "Icons by "
                        , simpleLink lightStyle
                            "https://www.flaticon.com/authors/gregor-cresnar"
                            "Gregor Cresnar"
                        ]
                    , row [ centerX ]
                        [ text "Splash screen and favicon by "
                        , simpleLink lightStyle
                            "https://gab.com/gahtannd"
                            "Gabriel Tannd"
                        ]
                    , row [ centerX ]
                        [ text <| chars.copyright ++ " 2018-2019 Bill St. Clair" ]
                    , row [ centerX ]
                        [ simpleLink lightStyle
                            "https://github.com/melon-love/gabdecker"
                            "GitHub"
                        ]
                    ]
                ]
            ]
        ]


standardButton : Style -> String -> Msg -> Element Msg -> Element Msg
standardButton =
    standardButtonWithDontHover False


standardButtonWithDontHover : Bool -> Style -> String -> Msg -> Element Msg -> Element Msg
standardButtonWithDontHover dontHover style title =
    generalButton
        [ Font.color style.link
        , titleAttribute title
        ]
        dontHover
        style
        title


generalButton : List (Attribute Msg) -> Bool -> Style -> String -> Msg -> Element Msg -> Element Msg
generalButton attributes dontHover style title msg label =
    button
        (List.concat
            [ attributes
            , if dontHover then
                []

              else
                [ Element.mouseOver [ mouseOverHighlight style ] ]
            ]
        )
        { onPress = Just msg
        , label = label
        }


textButton : Style -> String -> Msg -> String -> Element Msg
textButton style title msg label =
    standardButton style title msg (text label)


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
    , feedsets = "feedsets"
    , icons = "icons"
    , style = "style"
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


optimizers =
    { mainPage =
        if optimizations.lazyMainPage then
            Lazy.lazy5 mainPage

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
            Lazy.lazy5 postRow

        else
            postRow
    , keyedNotificationRow =
        if optimizations.keyedPostRow then
            Keyed.column

        else
            keyedColumn
    , notificationRow =
        if optimizations.lazyPostRow then
            Lazy.lazy4 notificationRow

        else
            notificationRow
    , keyedShowDialog =
        if optimizations.keyedShowDialog then
            Keyed.el

        else
            keyedEl
    , lazyShowDialog =
        if optimizations.lazyShowDialog then
            Lazy.lazy2 showTheDialog

        else
            showTheDialog
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
    , keyedShowDialog = True
    , lazyShowDialog = True
    }
