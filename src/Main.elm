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
        , centerX
        , column
        , el
        , height
        , image
        , link
        , padding
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
import Element.Input exposing (button)
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
        )
import GabDecker.Parsers as Parsers
import GabDecker.Types as Types
    exposing
        ( ApiError
        , Feed
        , FeedGetter(..)
        , FeedType(..)
        )
import Html.Attributes
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
    { useSimulator : Bool
    , windowHeight : Int
    , columnWidth : Int
    , fontSize : Float
    , maxPosts : Int
    , backend : Maybe Backend
    , key : Key
    , funnelState : PortFunnels.State
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
    , feeds : List (Feed Msg)
    }


type UploadingState
    = NotUploading
    | Uploading
    | FinishedUploading String
    | ErrorUploading String


type Msg
    = NoOp
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | ReceiveLoggedInUser (Result Http.Error User)
    | PersistResponseToken ResponseToken Posix
    | ProcessLocalStorage Value
    | WindowResize Int Int
    | Here Zone
    | Login
    | LoadMore String (Feed Msg)
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
            in
            { useSimulator = useSimulator
            , backend = backend
            , key = key
            , windowHeight = 1024
            , columnWidth = defaultColumnWidth
            , fontSize = defaultFontSize
            , maxPosts = defaultMaxPosts
            , funnelState = PortFunnels.initialState localStoragePrefix
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
            , feeds =
                feedTypesToFeeds defaultColumnWidth initialFeeds backend
            }
    in
    model
        |> withCmds
            [ Navigation.replaceUrl key "#"
            , case token of
                Just t ->
                    Task.perform (PersistResponseToken t) Time.now

                Nothing ->
                    if tokenAndState == NoToken then
                        localStorageSend (LocalStorage.get tokenKey) model

                    else
                        Cmd.none
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


feedTypesToFeeds : Int -> List FeedType -> Maybe Backend -> List (Feed Msg)
feedTypesToFeeds columnWidth feedTypes maybeBackend =
    case maybeBackend of
        Nothing ->
            []

        Just backend ->
            List.map (feedTypeToFeed columnWidth backend) feedTypes


defaultColumnWidth : Int
defaultColumnWidth =
    350


feedTypeToFeed : Int -> Backend -> FeedType -> Feed Msg
feedTypeToFeed columnWidth backend feedType =
    { getter = Api.feedTypeToGetter feedType backend
    , feedType = feedType
    , description = feedTypeDescription feedType
    , feed = { data = [], no_more = False }
    , error = Nothing
    , columnWidth = columnWidth
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


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    case response of
        LocalStorage.GetResponse { key, value } ->
            if key /= tokenKey || model.backend /= Nothing then
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

                            Ok st ->
                                let
                                    savedToken =
                                        { st | token = mungeToken st.token }

                                    backend =
                                        Just <|
                                            RealBackend savedToken.token

                                    feeds =
                                        feedTypesToFeeds model.columnWidth
                                            initialFeeds
                                            backend
                                in
                                { model
                                    | token = Just savedToken
                                    , scopes = savedToken.scope
                                    , receivedScopes = savedToken.scope
                                    , backend = backend
                                    , feeds = feeds
                                }
                                    |> withCmds
                                        [ if model.backend == Nothing then
                                            Cmd.none

                                          else
                                            Http.send ReceiveLoggedInUser <|
                                                Gab.me savedToken.token
                                        , Cmd.batch <|
                                            List.map feedGetMore feeds
                                        ]

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

        WindowResize _ h ->
            { model | windowHeight = h } |> withNoCmd

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

        RefreshFeed feedType result ->
            receiveFeed True feedType result model

        ReceiveFeed feedType result ->
            receiveFeed False feedType result model


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
            , if scrollToTop && Debug.log "id" id /= "" then
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
        loop i tail res =
            case tail of
                [] ->
                    ( "", List.reverse res )

                feed :: rest ->
                    if feed.feedType == feedType then
                        ( columnId i
                        , List.concat
                            [ List.reverse res
                            , [ updateFeed maxPosts result feed ]
                            , rest
                            ]
                        )

                    else
                        loop (i + 1) rest (feed :: res)
    in
    loop 0 feeds []


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
    row
        [ height Element.fill
        , fontSize model.fontSize 1
        , Border.width 5
        , Border.color styleColors.border
        ]
    <|
        List.map2 (feedColumn model.windowHeight model.fontSize model.here)
            model.feeds
        <|
            List.range 0 (List.length model.feeds - 1)


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
    (round <| 1.5 * baseFontSize) + 40


idAttribute : String -> Attribute msg
idAttribute id =
    Element.htmlAttribute <| Html.Attributes.id id


columnId : Int -> String
columnId idx =
    "column" ++ String.fromInt idx


columnIdAttribute : Int -> Attribute msg
columnIdAttribute idx =
    idAttribute <| columnId idx


columnBorderAttributes : List (Attribute msg)
columnBorderAttributes =
    [ Border.width 3
    , Border.color styleColors.border
    ]


columnPadding : Int
columnPadding =
    10


feedColumn : Int -> Float -> Zone -> Feed Msg -> Int -> Element Msg
feedColumn windowHeight baseFontSize here feed id =
    let
        colw =
            width <| px feed.columnWidth
    in
    column
        (List.concat
            [ [ colw
              , height Element.fill
              , Border.width 5
              ]
            , columnBorderAttributes
            ]
        )
        [ row
            [ fillWidth
            , Border.widthEach { zeroes | bottom = 1 }
            , Border.color styleColors.border
            , Background.color styleColors.headerBackground
            ]
            [ column [ colw ]
                [ row
                    [ padding columnPadding
                    , fontSize baseFontSize 1.5
                    , Font.bold
                    , centerX
                    ]
                    [ standardButton
                        (heightImage icons.reload
                            "refresh"
                            (round (4 * baseFontSize / 3))
                        )
                        (LoadMore "" feed)
                    , text " "
                    , feed.description
                    ]
                ]
            ]
        , row []
            [ column
                [ colw
                , height <| px (windowHeight - headerHeight baseFontSize)
                , columnIdAttribute id
                , Element.paddingEach
                    { zeroes
                        | left = columnPadding
                        , right = columnPadding
                    }
                , Element.scrollbarX
                , Element.clipX
                ]
              <|
                List.concat
                    [ List.map (postRow baseFontSize feed.columnWidth here)
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
                    , Element.paddingEach
                        { zeroes
                            | top = 20
                            , bottom = 20
                            , left = 5
                            , right = 5
                        }
                    ]
                    [ let
                        nbsps =
                            nbsp ++ nbsp ++ nbsp
                      in
                      textButton (nbsps ++ "Load More" ++ nbsps)
                        (LoadMore log.published_at feed)
                    ]


userPadding : Attribute msg
userPadding =
    Element.paddingEach
        { top = 0
        , right = 0
        , bottom = 4
        , left = 0
        }


postBorder : Attribute msg
postBorder =
    Border.widthEach
        { bottom = 1
        , left = 0
        , right = 0
        , top = 1
        }


nameBottomPadding : Attribute msg
nameBottomPadding =
    Element.paddingEach { zeroes | bottom = 3 }


postRow : Float -> Int -> Zone -> ActivityLog -> Element Msg
postRow baseFontSize cw here log =
    let
        pad =
            5

        cwp =
            cw - 2 * pad - 6

        colw =
            width <| px cwp

        mediaw =
            width <| px (cwp - 2 * columnPadding)

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
        [ postBorder
        , Border.color styleColors.border
        , fillWidth
        , padding pad
        ]
        [ column [ colw ]
            [ if not repost then
                text ""

              else
                row
                    [ mediaw
                    , userPadding
                    , Border.widthEach { zeroes | bottom = 1 }
                    , Border.color styleColors.border
                    ]
                    [ heightImage icons.refresh "refresh" 10
                    , newTabLink ("https://gab.com/" ++ actusername)
                        (" " ++ actuser.name ++ " reposted")
                    ]
            , row
                [ Font.bold
                , mediaw
                , userPadding
                ]
                [ column
                    [ Element.paddingEach
                        { zeroes | right = 5 }
                    ]
                    [ row []
                        [ heightImage user.picture_url username 40 ]
                    ]
                , column []
                    [ row [ nameBottomPadding ]
                        [ newTabLink ("https://gab.com/" ++ username) <|
                            user.name
                                ++ " ("
                                ++ embiggen username
                                ++ ")"
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
            , row
                []
                [ Element.textColumn
                    [ mediaw
                    , paragraphSpacing baseFontSize
                    ]
                  <|
                    case post.body_html of
                        Nothing ->
                            htmlBodyElements baseFontSize <| newlinesToPs post.body

                        Just html ->
                            htmlBodyElements baseFontSize html
                ]
            , row []
                [ column [ colw ] <|
                    case post.attachment of
                        MediaAttachment records ->
                            List.map (mediaRow mediaw) records

                        _ ->
                            [ text "" ]
                ]
            , row []
                [ column
                    [ mediaw
                    , Element.paddingEach { zeroes | left = 5, right = 5 }
                    , Background.color styleColors.quotedPost
                    ]
                    [ if post.is_quote then
                        case post.related of
                            RelatedPosts { parent } ->
                                case parent of
                                    Nothing ->
                                        text ""

                                    Just parentPost ->
                                        postRow baseFontSize (cwp - 10) here <|
                                            { log
                                                | post = parentPost
                                                , type_ = "post"
                                            }

                      else
                        text ""
                    ]
                ]
            ]
        ]


mediaRow : Attribute msg -> MediaRecord -> Element msg
mediaRow colw record =
    row []
        [ image
            [ colw
            , Element.paddingEach { zeroes | top = 5 }
            ]
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
    Element.paddingEach <| { zeroes | top = 4, bottom = 4 }


paragraphSpacing : Float -> Attribute msg
paragraphSpacing baseFontSize =
    spacing <| round (0.6 * baseFontSize)


paragraphLineSpacing : Float -> Attribute msg
paragraphLineSpacing baseFontSize =
    spacing <| round (0.4 * baseFontSize)


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
                --, paragraphLineSpacing baseFontSize
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
                [ textButton "Login" Login ]
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
                        [ text <| copyright ++ " 2018 Bill St. Clair" ]
                    , row [ centerX ]
                        [ simpleLink "https://github.com/melon-love/gabdecker"
                            "GitHub"
                        ]
                    ]
                ]
            ]
        ]


standardButton : Element Msg -> Msg -> Element Msg
standardButton label msg =
    button
        [ Font.color styleColors.link
        , Element.mouseOver
            [ Background.color styleColors.linkHover
            ]
        ]
        { onPress = Just msg
        , label = label
        }


textButton : String -> Msg -> Element Msg
textButton label msg =
    standardButton (text label) msg


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


copyright : String
copyright =
    String.fromList [ Char.fromCode 0xA9 ]


nbsp : String
nbsp =
    String.fromList [ Char.fromCode 0xA0 ]


icons =
    { reload = "images/reload.svg"
    , refresh = "images/refresh-arrow.svg"
    , close = "images/cancel.svg"
    , like = "images/like.svg"
    , dislike = "images/dislike.svg"
    , settings = "images/settings.svg"
    , user = "images/avatar.svg"
    , home = "images/house.svg"
    , popular = "images/star.svg"
    , notifications = "images/glasses.svg"
    , next = "images/next-1.svg"
    , previous = "images/back.svg"
    }
