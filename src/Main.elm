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
--
-- Near-term TODO list:
--
-- Handle bad saved token error by returning to login page.
-- Load more, reload.
-- Linked Group/Topic below data line.
-- Allow configuration of the columns.
-- Post / reply / quote / upvote / downvote / repost
-- Font size and column width preferences.
-- Open clicked image in overlay pane.
-- Link user image to profile page.
-- HTML elements, "&amp;" -> "&".
-- Add Notifications feed to `Gab` module.
--
-- There is still no API for getting comments or group or topic feeds,
-- and posting still gets an error 429 (too many
-- requests). @developers?
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
        ( gray
        , heightImage
        , lightgray
        , newTabLink
        , simpleImage
        , simpleLink
        )
import GabDecker.Parsers as Parsers
import GabDecker.Types as Types exposing (Feed, FeedGetter(..), FeedType(..))
import Http
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
    = HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | ReceiveLoggedInUser (Result Http.Error User)
    | PersistResponseToken ResponseToken Posix
    | ProcessLocalStorage Value
    | WindowResize Int Int
    | Here Zone
    | Login
    | ReceiveFeed FeedType (Result Api.Error ActivityLogList)


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
                feedTypesToFeeds initialFeeds backend
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


feedTypesToFeeds : List FeedType -> Maybe Backend -> List (Feed Msg)
feedTypesToFeeds feedTypes maybeBackend =
    case maybeBackend of
        Nothing ->
            []

        Just backend ->
            List.map (feedTypeToFeed backend) feedTypes


columnWidth : Int
columnWidth =
    250


feedTypeToFeed : Backend -> FeedType -> Feed Msg
feedTypeToFeed backend feedType =
    { getter = Types.feedTypeToGetter feedType backend (ReceiveFeed feedType)
    , feedType = feedType
    , description = feedTypeDescription feedType
    , feed = { data = [], no_more = False }
    , error = Nothing
    , columnWidth = columnWidth
    }


feedTypeDescription : FeedType -> String
feedTypeDescription feedType =
    case feedType of
        HomeFeed ->
            "Home"

        UserFeed user ->
            "User: " ++ user

        -- Need to look up group name
        GroupFeed groupid ->
            "Group: " ++ groupid

        -- Need to look up topic name
        TopicFeed groupid ->
            "Topic: " ++ groupid

        PopularFeed ->
            "Popular"


feedGetMore : Feed Msg -> Cmd Msg
feedGetMore feed =
    if feed.feed.no_more then
        Cmd.none

    else
        case feed.getter of
            FeedGetter cmd ->
                cmd

            FeedGetterWithBefore f ->
                let
                    before =
                        case LE.last feed.feed.data of
                            Nothing ->
                                ""

                            Just log ->
                                log.published_at
                in
                f before


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
                                let
                                    backend =
                                        Just <|
                                            RealBackend savedToken.token

                                    feeds =
                                        feedTypesToFeeds initialFeeds backend
                                in
                                { model
                                    | token = Just savedToken
                                    , scopes = savedToken.scope
                                    , receivedScopes = savedToken.scope
                                    , backend = backend
                                    , feeds = feeds
                                }
                                    |> withCmds
                                        [ Http.send ReceiveLoggedInUser <|
                                            Gab.me savedToken.token
                                        , Cmd.batch <|
                                            List.map feedGetMore feeds
                                        ]

        _ ->
            model |> withNoCmd


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

        ReceiveFeed feedType result ->
            { model | feeds = updateFeeds feedType result model.feeds }
                |> withNoCmd


updateFeeds : FeedType -> Result Api.Error ActivityLogList -> List (Feed Msg) -> List (Feed Msg)
updateFeeds feedType result feeds =
    let
        loop tail res =
            case tail of
                [] ->
                    List.reverse res

                feed :: rest ->
                    if feed.feedType == feedType then
                        List.concat
                            [ List.reverse res
                            , [ updateFeed result feed ]
                            , rest
                            ]

                    else
                        loop rest (feed :: res)
    in
    loop feeds []


updateFeed : Result Api.Error ActivityLogList -> Feed Msg -> Feed Msg
updateFeed result feed =
    case result of
        Err err ->
            { feed | error = Just err }

        Ok activities ->
            { feed
                | error = Nothing
                , feed =
                    { data = List.append feed.feed.data activities.data
                    , no_more = activities.no_more
                    }
            }


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


baseFontSize : Float
baseFontSize =
    12


fontSize : Float -> Element.Attr decorative msg
fontSize scale =
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
        , fontSize 1
        , Border.widthEach { zeroes | right = 1 }
        ]
    <|
        List.map (feedColumn model.windowHeight model.here) model.feeds


zeroes =
    { right = 0
    , left = 0
    , top = 0
    , bottom = 0
    }


{-| 20 has no rhyme or reason other than it works.
-}
headerHeight : Int
headerHeight =
    (round <| 1.5 * baseFontSize) + 20


feedColumn : Int -> Zone -> Feed Msg -> Element Msg
feedColumn windowHeight here feed =
    let
        colw =
            width <| px feed.columnWidth
    in
    column
        [ colw
        , height Element.fill
        , Border.width 1
        ]
        [ row
            [ fillWidth
            , Border.widthEach { zeroes | bottom = 1 }
            ]
            [ column [ colw ]
                [ row
                    [ padding 10
                    , fontSize 1.5
                    , Font.bold
                    , centerX
                    ]
                    [ text feed.description ]
                ]
            ]
        , row []
            [ column
                [ colw
                , height <| px (windowHeight - headerHeight)
                , Element.scrollbarX
                ]
              <|
                List.map (postRow feed.columnWidth here) feed.feed.data
            ]
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


postRow : Int -> Zone -> ActivityLog -> Element Msg
postRow cw here log =
    let
        pad =
            5

        cwp =
            cw - 2 * pad

        colw =
            width <| px cwp

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
        , fillWidth
        , padding pad
        ]
        [ column [ colw ]
            [ if not repost then
                text ""

              else
                row
                    [ colw
                    , userPadding
                    , Border.widthEach { zeroes | bottom = 1 }
                    , Border.color gray
                    ]
                    [ heightImage "images/refresh-arrow.svg" "refresh" 10
                    , newTabLink ("https://gab.com/" ++ actusername)
                        (" " ++ actuser.name ++ " reposted")
                    ]
            , row
                [ Font.bold
                , userPadding
                ]
                [ column [ Element.paddingEach { zeroes | right = 5 } ]
                    [ row []
                        [ heightImage user.picture_url username 40 ]
                    ]
                , column []
                    [ row [ Element.paddingEach { zeroes | bottom = 5 } ]
                        [ text " "
                        , text user.name
                        , text " ("
                        , newTabLink ("https://gab.com/" ++ username) <|
                            embiggen username
                        , text ")"
                        ]
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
                    [ colw ]
                  <|
                    case post.body_html of
                        Nothing ->
                            [ paragraph
                                [ Element.clipY
                                , paragraphPadding
                                ]
                              <|
                                Parsers.parseElements post.body
                            ]

                        Just html ->
                            htmlBodyElements html
                ]
            , row []
                [ column [ colw ] <|
                    case post.attachment of
                        MediaAttachment records ->
                            List.map (mediaRow colw) records

                        _ ->
                            [ text "" ]
                ]
            , row []
                [ column
                    [ colw
                    , Element.paddingEach { zeroes | left = 5, right = 5 }
                    , Background.color lightgray
                    ]
                    [ if not post.is_quote then
                        text ""

                      else
                        case post.related of
                            RelatedPosts { parent } ->
                                case parent of
                                    Nothing ->
                                        text ""

                                    Just parentPost ->
                                        postRow (cwp - 10) here <|
                                            { log
                                                | post = parentPost
                                                , type_ = "post"
                                            }
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


htmlBodyElements : String -> List (Element Msg)
htmlBodyElements html =
    let
        par : List (Element Msg) -> Element Msg
        par elements =
            paragraph
                [ Element.clipY
                , paragraphPadding
                ]
                elements
    in
    -- Still have to deal with single <br />
    stringRemove "</p>" html
        |> String.split "<p>"
        |> removeEmptyHead
        |> List.map (\s -> String.split "<br /><br />" s)
        |> List.concat
        |> List.map Parsers.parseElements
        |> List.map par


loginPage : Model -> Element Msg
loginPage model =
    row
        [ fillWidth
        , fontSize 2
        ]
        [ column [ centerX, spacing 10 ]
            [ row
                [ centerX
                , padding 20
                , fontSize 3
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
                [ button []
                    { onPress = Just Login
                    , label = text "Login"
                    }
                ]
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
                [ column [ centerX, spacing 6, fontSize 1.5 ]
                    [ row [ centerX ]
                        [ text <| copyright ++ " 2018 Bill St. Clair" ]
                    , row [ centerX ]
                        [ simpleLink "https://github.com/melon-love/gabdecker"
                            "GitHub"
                        ]
                    ]
                ]
            ]
        ]


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
