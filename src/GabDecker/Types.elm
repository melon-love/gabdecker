module GabDecker.Types exposing
    ( ApiError
    , Feed
    , FeedGetter(..)
    , FeedTagger
    , FeedType(..)
    )

import Element exposing (Element)
import Gab.Types exposing (ActivityLogList)
import Http


type alias ApiError =
    { httpError : Maybe Http.Error
    , message : String
    }


type alias FeedTagger msg =
    Result ApiError ActivityLogList -> msg


type FeedGetter msg
    = FeedGetterWithBefore (FeedTagger msg -> String -> Cmd msg)
    | FeedGetter (FeedTagger msg -> Cmd msg)
    | FeedGetterUnused


type FeedType
    = HomeFeed
    | UserFeed String
    | GroupFeed String
    | TopicFeed String
    | PopularFeed
    | LastClosedFeed
    | LoggedInUserFeed


type alias Feed msg =
    { getter : FeedGetter msg
    , feedType : FeedType
    , description : Element msg
    , feed : ActivityLogList
    , error : Maybe ApiError
    , columnWidth : Int
    }
