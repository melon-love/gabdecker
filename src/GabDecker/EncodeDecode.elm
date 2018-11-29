module GabDecker.EncodeDecode exposing
    ( decodeFeedType
    , decodeFeedTypes
    , encodeFeedType
    , encodeFeedTypes
    )

import GabDecker.Types exposing (FeedType(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


encodeFeedType : FeedType -> Value
encodeFeedType feedType =
    case feedType of
        HomeFeed ->
            JE.object [ ( "feedType", JE.string "HomeFeed" ) ]

        UserFeed username ->
            JE.object
                [ ( "feedType", JE.string "UserFeed" )
                , ( "param", JE.string username )
                ]

        GroupFeed groupid ->
            JE.object
                [ ( "feedType", JE.string "GroupFeed" )
                , ( "param", JE.string groupid )
                ]

        TopicFeed topicid ->
            JE.object
                [ ( "feedType", JE.string "TopicFeed" )
                , ( "param", JE.string topicid )
                ]

        PopularFeed ->
            JE.object [ ( "feedType", JE.string "PopularFeed" ) ]

        _ ->
            JE.object [ ( "feedType", JE.string "unknown" ) ]


encodeFeedTypes : List FeedType -> Value
encodeFeedTypes feedTypes =
    JE.list encodeFeedType feedTypes


feedTypeDecoder : Decoder FeedType
feedTypeDecoder =
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


feedTypesDecoder : Decoder (List FeedType)
feedTypesDecoder =
    JD.list feedTypeDecoder


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


decodeFeedTypes : Value -> Result String (List FeedType)
decodeFeedTypes value =
    JD.decodeValue feedTypesDecoder value
        |> fixDecodeResult
