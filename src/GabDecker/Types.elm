module GabDecker.Types exposing
    ( ApiError
    , Feed(..)
    , FeedGetter(..)
    , FeedRecord
    , FeedTagger
    , FeedType(..)
    )

import Element exposing (Element)
import Gab.Types exposing (ActivityLogList, NotificationsLog, UserList)
import Http


type alias ApiError =
    { httpError : Maybe Http.Error
    , message : String
    }


type alias FeedTagger msg kind =
    Result ApiError kind -> msg


type FeedGetter msg kind
    = FeedGetterWithBefore (FeedTagger msg kind -> String -> Cmd msg)
    | FeedGetter (FeedTagger msg kind -> Cmd msg)
    | FeedGetterUnused


type FeedType
    = HomeFeed
    | UserFeed String
    | GroupFeed String
    | TopicFeed String
    | PopularFeed
    | LastClosedFeed
    | LoggedInUserFeed
    | NotificationsFeed


type alias FeedRecord msg kind =
    { getter : FeedGetter msg kind
    , feedType : FeedType
    , description : Element msg
    , feed : kind
    , error : Maybe ApiError
    , columnWidth : Int
    , id : Int
    }


type Feed msg
    = ActivityLogListFeed (FeedRecord msg ActivityLogList)
    | NotificationsLogFeed (FeedRecord msg NotificationsLog)
