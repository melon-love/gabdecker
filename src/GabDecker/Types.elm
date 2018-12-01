module GabDecker.Types exposing
    ( ApiError
    , Feed
    , FeedData(..)
    , FeedGetter(..)
    , FeedResult
    , FeedTagger
    , FeedType(..)
    , LogList
    )

import Element exposing (Element)
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , Notification
        , NotificationsLog
        , UserList
        )
import Http


type alias ApiError =
    { httpError : Maybe Http.Error
    , message : String
    }


type alias FeedResult x =
    Result ApiError x


type alias FeedTagger x msg =
    FeedResult x -> msg


type FeedGetter msg
    = PostFeedGetter (FeedTagger ActivityLogList msg -> String -> Cmd msg)
    | NotificationFeedGetter (FeedTagger NotificationsLog msg -> String -> Cmd msg)


type FeedType
    = HomeFeed
    | UserFeed String
    | GroupFeed String
    | TopicFeed String
    | PopularFeed
    | LastClosedFeed
    | LoggedInUserFeed
    | NotificationsFeed


type alias LogList x =
    { data : List x
    , no_more : Bool
    }


type alias Feed msg =
    { getter : FeedGetter msg
    , feedType : FeedType
    , description : Element msg
    , feed : LogList FeedData
    , error : Maybe ApiError
    , columnWidth : Int
    , id : Int
    }


type FeedData
    = PostFeedData ActivityLog
    | NotificationFeedData Notification
