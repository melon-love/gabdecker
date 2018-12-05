module GabDecker.Types exposing
    ( ApiError
    , Feed
    , FeedData(..)
    , FeedGetter(..)
    , FeedResult
    , FeedTagger
    , FeedType(..)
    , GangedNotification
    , Icons
    , LogList
    , emptyIcons
    )

import Dict exposing (Dict)
import Element exposing (Element)
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , Notification
        , NotificationsLog
        , User
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
    { data : x
    , no_more : Bool
    }


type alias Feed msg =
    { getter : FeedGetter msg
    , feedType : FeedType
    , feed : LogList FeedData
    , error : Maybe ApiError
    , columnWidth : Int
    , id : Int
    }


type alias GangedNotification =
    { notification : Notification
    , users : List User
    }


type FeedData
    = PostFeedData (List ActivityLog)
    | NotificationFeedData (List GangedNotification)


type alias Icons =
    { user : Dict String String
    , group : Dict String String
    , topic : Dict String String
    }


emptyIcons : Icons
emptyIcons =
    Icons Dict.empty Dict.empty Dict.empty
