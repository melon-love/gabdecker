module GabDecker.EncodeDecode exposing
    ( decodeFeedSets
    , decodeFeedType
    , decodeFeedTypes
    , decodeIcons
    , decodeSettings
    , encodeFeedSets
    , encodeFeedType
    , encodeFeedTypes
    , encodeIcons
    , encodeSettings
    , feedSetsDecoder
    , feedTypeDecoder
    , feedTypesDecoder
    , settingsDecoder
    )

import Dict exposing (Dict)
import GabDecker.Elements
    exposing
        ( darkStyle
        , defaultSettings
        , lightStyle
        )
import GabDecker.Types
    exposing
        ( FeedSet
        , FeedType(..)
        , Icons
        , Settings
        , Style
        , StyleOption(..)
        , newFeedSet
        )
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Set exposing (Set)


encodeFeedTypes : List FeedType -> Value
encodeFeedTypes types =
    JE.list encodeFeedType types


feedPropertiesDecoder : Decoder FeedType
feedPropertiesDecoder =
    JD.oneOf
        [ feedTypeDecoder

        -- Backward compatibilty
        , JD.field "feedType" feedTypeDecoder
        ]


feedTypesDecoder : Decoder (List FeedType)
feedTypesDecoder =
    JD.list feedPropertiesDecoder


decodeFeedTypes : Value -> Result String (List FeedType)
decodeFeedTypes value =
    JD.decodeValue feedTypesDecoder value
        |> fixDecodeResult


encodeFeedType : FeedType -> Value
encodeFeedType feedType =
    case feedType of
        HomeFeed ->
            JE.string "home"

        UserFeed username ->
            JE.object [ ( "user", JE.string username ) ]

        GroupFeed groupid ->
            JE.object [ ( "group", JE.string groupid ) ]

        TopicFeed topicid ->
            JE.object [ ( "topic", JE.string topicid ) ]

        PopularFeed ->
            JE.string "popular"

        NotificationsFeed ->
            JE.string "notification"

        _ ->
            JE.string "Unsaveable feed type"


oldFeedTypeDecoder : Decoder FeedType
oldFeedTypeDecoder =
    let
        fail =
            JD.fail "Missing param"

        decoder feedType maybeParam =
            JD.succeed ()
                |> JD.andThen
                    (\_ ->
                        case maybeParam of
                            Nothing ->
                                case feedType of
                                    "HomeFeed" ->
                                        JD.succeed HomeFeed

                                    "PopularFeed" ->
                                        JD.succeed PopularFeed

                                    "NotificationsFeed" ->
                                        JD.succeed NotificationsFeed

                                    _ ->
                                        fail

                            Just param ->
                                case feedType of
                                    "UserFeed" ->
                                        JD.succeed <| UserFeed param

                                    "GroupFeed" ->
                                        JD.succeed <| GroupFeed param

                                    "TopicFeed" ->
                                        JD.succeed <| TopicFeed param

                                    _ ->
                                        fail
                    )
    in
    JD.field "feedType" JD.string
        |> JD.andThen
            (\feedType ->
                (JD.maybe <| JD.field "param" JD.string)
                    |> JD.andThen
                        (\maybeParam ->
                            decoder feedType maybeParam
                        )
            )


decodeStringFeed : String -> Decoder FeedType
decodeStringFeed string =
    case string of
        "home" ->
            JD.succeed HomeFeed

        "popular" ->
            JD.succeed PopularFeed

        "notification" ->
            JD.succeed NotificationsFeed

        _ ->
            JD.fail "Not home, popular, or noticication."


feedTypeDecoder : Decoder FeedType
feedTypeDecoder =
    JD.oneOf
        [ JD.string |> JD.andThen decodeStringFeed
        , JD.map UserFeed <| JD.field "user" JD.string
        , JD.map GroupFeed <| JD.field "group" JD.string
        , JD.map TopicFeed <| JD.field "topic" JD.string
        , oldFeedTypeDecoder
        ]


fixDecodeResult : Result JD.Error a -> Result String a
fixDecodeResult result =
    case result of
        Err err ->
            Err <| JD.errorToString err

        Ok a ->
            Ok a


decodeFeedType : Value -> Result String FeedType
decodeFeedType value =
    JD.decodeValue feedTypeDecoder value
        |> fixDecodeResult


encodeIcons : Icons -> Value
encodeIcons icons =
    JE.object
        [ ( "user", JE.dict identity JE.string icons.user )
        , ( "group", JE.dict identity JE.string icons.group )
        , ( "topic", JE.dict identity JE.string icons.topic )
        ]


decodeIcons : Value -> Result String Icons
decodeIcons value =
    JD.decodeValue iconsDecoder value
        |> fixDecodeResult


iconsDecoder : Decoder Icons
iconsDecoder =
    JD.map3 Icons
        (JD.field "user" <| JD.dict JD.string)
        (JD.field "group" <| JD.dict JD.string)
        (JD.field "topic" <| JD.dict JD.string)


encodeStyleOption : StyleOption -> Value
encodeStyleOption option =
    case option of
        LightStyle ->
            JE.string "light"

        DarkStyle ->
            JE.string "dark"


decodeStyleOption : Value -> Result String StyleOption
decodeStyleOption value =
    JD.decodeValue styleOptionDecoder value
        |> fixDecodeResult


styleOptionDecoder : Decoder StyleOption
styleOptionDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "light" ->
                        JD.succeed LightStyle

                    "dark" ->
                        JD.succeed DarkStyle

                    _ ->
                        JD.fail <| "Not a style option name: " ++ s
            )


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object
        [ ( "columnWidth", JE.int settings.columnWidth )
        , ( "defaultColumnWidth", JE.int settings.defaultColumnWidth )
        , ( "fontSize", JE.float settings.fontSize )
        , ( "showHidden", JE.bool settings.showHidden )
        , ( "style", encodeStyleOption settings.styleOption )
        ]


decodeSettings : Value -> Result String Settings
decodeSettings value =
    JD.decodeValue settingsDecoder value
        |> fixDecodeResult


optionToStyle : StyleOption -> Style
optionToStyle option =
    case option of
        LightStyle ->
            lightStyle

        DarkStyle ->
            darkStyle


settingsDecoder : Decoder Settings
settingsDecoder =
    JD.oneOf
        [ -- Backward compatibilty
          JD.map
            (\option ->
                { defaultSettings
                    | styleOption = option
                    , style = optionToStyle option
                }
            )
            styleOptionDecoder
        , JD.map5
            (\columnWidth defaultColumnWidth fontSize showHidden option ->
                { defaultSettings
                    | columnWidth = columnWidth
                    , defaultColumnWidth =
                        case defaultColumnWidth of
                            Nothing ->
                                columnWidth

                            Just w ->
                                w
                    , fontSize = fontSize
                    , showHidden = showHidden
                    , styleOption = option
                    , style = optionToStyle option
                }
            )
            (JD.field "columnWidth" JD.int)
            (JD.oneOf
                [ JD.field "defaultColumnWidth" <| JD.nullable JD.int
                , JD.succeed Nothing
                ]
            )
            (JD.field "fontSize" JD.float)
            (JD.oneOf
                [ JD.field "showHidden" JD.bool
                , JD.succeed True
                ]
            )
            (JD.field "style" styleOptionDecoder)
        ]


encodeFeedSets : List (FeedSet msg) -> Value
encodeFeedSets feedSets =
    List.map
        (\feedSet ->
            ( feedSet.name
            , case feedSet.columnWidth of
                Nothing ->
                    encodeFeedTypes feedSet.feedTypes

                Just width ->
                    JE.object
                        [ ( "feedTypes", encodeFeedTypes feedSet.feedTypes )
                        , ( "columnWidth", JE.int width )
                        ]
            )
        )
        feedSets
        |> JE.object


type alias FeedTypesAndWidth =
    { feedTypes : List FeedType
    , columnWidth : Maybe Int
    }


feedTypesAndWidthDecoder : Decoder FeedTypesAndWidth
feedTypesAndWidthDecoder =
    JD.oneOf
        [ feedTypesDecoder
            |> JD.andThen
                (\feedTypes ->
                    FeedTypesAndWidth feedTypes Nothing
                        |> JD.succeed
                )
        , JD.map2 FeedTypesAndWidth
            (JD.field "feedTypes" feedTypesDecoder)
            (JD.field "columnWidth" <| JD.nullable JD.int)
        ]


feedSetsDecoder : Decoder (List (FeedSet msg))
feedSetsDecoder =
    JD.keyValuePairs feedTypesAndWidthDecoder
        |> JD.map
            (List.map
                (\( name, { feedTypes, columnWidth } ) ->
                    newFeedSet name feedTypes columnWidth
                )
                >> List.sortBy .name
            )


decodeFeedSets : Value -> Result String (List (FeedSet msg))
decodeFeedSets value =
    JD.decodeValue feedSetsDecoder value
        |> fixDecodeResult
