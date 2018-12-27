module GabDecker.EncodeDecode exposing
    ( decodeFeedSets
    , decodeFeedType
    , decodeFeedTypes
    , decodeIcons
    , decodeSettings
    , encodeFeedSet
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
        , ( "fontSize", JE.float settings.fontSize )
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
        , JD.map3
            (\columnWidth fontSize option ->
                { defaultSettings
                    | columnWidth = columnWidth
                    , fontSize = fontSize
                    , styleOption = option
                    , style = optionToStyle option
                }
            )
            (JD.field "columnWidth" JD.int)
            (JD.field "fontSize" JD.float)
            (JD.field "style" styleOptionDecoder)
        ]


encodeFeedSet : FeedSet msg -> Value
encodeFeedSet feedSet =
    JE.object
        [ ( feedSet.name, encodeFeedTypes feedSet.feedTypes ) ]


encodeFeedSets : List (FeedSet msg) -> Value
encodeFeedSets feedSets =
    JE.list encodeFeedSet feedSets


feedSetsDecoder : Decoder (List (FeedSet msg))
feedSetsDecoder =
    JD.map
        (\pairs ->
            List.map
                (\( name, feedTypes ) ->
                    { name = name
                    , feedTypes = feedTypes
                    , feeds = Nothing
                    , loadingFeeds = Set.empty
                    }
                )
                pairs
        )
        (JD.keyValuePairs feedTypesDecoder)


decodeFeedSets : Value -> Result String (List (FeedSet msg))
decodeFeedSets value =
    JD.decodeValue feedSetsDecoder value
        |> fixDecodeResult
