module GabDecker.Types exposing
    ( Feed
    , FeedGetter(..)
    , FeedType(..)
    , feedTypeToGetter
    )

import Gab.Types exposing (ActivityLogList)
import GabDecker.Api as Api exposing (Backend)


type FeedGetter msg
    = FeedGetterWithBefore (String -> Cmd msg)
    | FeedGetter (Cmd msg)


type FeedType
    = HomeFeed
    | UserFeed String
    | GroupFeed String
    | TopicFeed String
    | PopularFeed


type alias Feed msg =
    { getter : FeedGetter msg
    , feedType : FeedType
    , description : String
    , feed : ActivityLogList
    , error : Maybe Api.Error
    }


feedTypeToGetter : FeedType -> Backend -> (Result Api.Error ActivityLogList -> msg) -> FeedGetter msg
feedTypeToGetter feedType backend tagger =
    case feedType of
        HomeFeed ->
            FeedGetterWithBefore <| Api.homeFeed backend tagger

        UserFeed username ->
            FeedGetterWithBefore <| Api.userFeed backend tagger username

        GroupFeed groupid ->
            FeedGetterWithBefore <| Api.groupFeed backend tagger groupid

        TopicFeed topicid ->
            FeedGetterWithBefore <| Api.topicFeed backend tagger topicid

        PopularFeed ->
            FeedGetter <| Api.popularFeed backend tagger
