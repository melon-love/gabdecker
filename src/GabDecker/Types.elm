module GabDecker.Types exposing
    ( ApiError
    , Feed
    , FeedData(..)
    , FeedGetter(..)
    , FeedResult
    , FeedTagger
    , FeedType(..)
    , GangedNotification
    , IconUrls
    , Icons
    , LogList
    , Style
    , Styles
    , emptyIcons
    )

import Dict exposing (Dict)
import Element exposing (Color, Element)
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
    | NotificationsFeed
    | LastClosedFeed
    | LoggedInUserFeed


type alias LogList x =
    { data : x
    , no_more : Bool
    }


type alias Feed msg =
    { getter : FeedGetter msg
    , feedType : FeedType
    , feed : LogList FeedData
    , error : Maybe ApiError
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


type alias Style =
    { name : String
    , icondir : String
    , background : Color
    , dialogBackground : Color
    , text : Color
    , link : Color
    , linkHover : Color
    , border : Color
    , headerBackground : Color
    , quotedPostBackground : Color
    , quotedPostBorder : Color
    , postcountBackground : Color
    , loadingFeed : Color
    }


type alias Styles =
    { selected : String
    , styles : List Style
    }


type alias IconUrls =
    { close : String
    , comment : String
    , dislike : String
    , home : String
    , like : String
    , logout : String
    , next : String
    , notification : String
    , notifications : String
    , popular : String
    , previous : String
    , refresh : String
    , reload : String
    , save : String
    , settings : String
    , user : String
    }
