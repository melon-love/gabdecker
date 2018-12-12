module GabDecker.EncodeDecode exposing
    ( decodeFeedProperties
    , decodeFeedType
    , decodeFeedsProperties
    , decodeIcons
    , encodeFeedProperties
    , encodeFeedType
    , encodeFeedsProperties
    , encodeIcons
    , feedPropertiesDecoder
    , feedTypeDecoder
    , feedsPropertiesDecoder
    )

import Dict exposing (Dict)
import GabDecker.Types exposing (FeedProperties, FeedType(..), Icons)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


encodeFeedProperties : FeedProperties -> Value
encodeFeedProperties properties =
    if not properties.showProfile then
        encodeFeedType properties.feedType

    else
        JE.object
            [ ( "feedType", encodeFeedType properties.feedType )
            , ( "showProfile", JE.bool True )
            ]


encodeFeedsProperties : List FeedProperties -> Value
encodeFeedsProperties properties =
    JE.list encodeFeedProperties properties


feedPropertiesDecoder : Decoder FeedProperties
feedPropertiesDecoder =
    JD.oneOf
        [ JD.map (\feedType -> FeedProperties feedType False)
            feedTypeDecoder
        , JD.map2 FeedProperties
            (JD.field "feedType" feedTypeDecoder)
            (JD.field "showProfile" JD.bool)
        ]


decodeFeedProperties : Value -> Result String FeedProperties
decodeFeedProperties value =
    JD.decodeValue feedPropertiesDecoder value
        |> fixDecodeResult


feedsPropertiesDecoder : Decoder (List FeedProperties)
feedsPropertiesDecoder =
    JD.list feedPropertiesDecoder


decodeFeedsProperties : Value -> Result String (List FeedProperties)
decodeFeedsProperties value =
    JD.decodeValue feedsPropertiesDecoder value
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
