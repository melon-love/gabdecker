module GabDecker.Types exposing
    ( ApiError
    , Feed
    , FeedData(..)
    , FeedGetter(..)
    , FeedProperties
    , FeedResult
    , FeedTagger
    , FeedType(..)
    , GangedNotification
    , IconUrls
    , Icons
    , LogList
    , Profile(..)
    , Settings
    , Style
    , StyleOption(..)
    , Styles
    , emptyIcons
    , emptyUser
    , isEmptyUser
    , isUserProfile
    )

import Dict exposing (Dict)
import Element exposing (Color, Element)
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , Group
        , Notification
        , NotificationsLog
        , Topic
        , User
        , UserList
        )
import Http
import Time exposing (Zone)


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
    , showProfile : Bool
    , newPosts : Int
    , error : Maybe ApiError
    , id : Int
    }


type alias FeedProperties =
    { feedType : FeedType
    , showProfile : Bool
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


isEmptyUser : User -> Bool
isEmptyUser user =
    user.id == 0


isUserProfile : User -> Bool
isUserProfile user =
    -- Need to verify this for a private account
    user.post_count /= Nothing


emptyUser : String -> User
emptyUser username =
    { id = 0
    , name = "@" ++ username
    , username = username
    , picture_url = ""
    , verified = False
    , is_pro = False
    , is_donor = False
    , is_investor = False
    , is_premium = False
    , is_private = False
    , is_tippable = False
    , is_accessible = False
    , created_at_month_label = Nothing
    , follower_count = Nothing
    , following_count = Nothing
    , post_count = Nothing
    , picture_url_full = Nothing
    , following = False
    , followed = False
    , premium_price = Nothing
    , follow_pending = False
    , unread_notification_count = Nothing
    , stream = False
    , bio = Nothing
    , cover_url = Nothing
    , show_replies = False
    , sound_alerts = False
    , email = Nothing
    , notify_followers = False
    , notify_mentions = False
    , notify_likes = False
    , notify_reposts = False
    , broadcast_channel = Nothing
    , exclusive_features = False
    , social_facebook = False
    , social_twitter = False
    , is_pro_overdue = False
    , pro_expires_at = Nothing
    , has_chat = False
    , has_chat_unread = False
    , germany_law = False
    , language = Nothing
    , pinned_post_id = Nothing
    , nsfw_filter = False
    , hide_premium_content = False
    , score = Nothing
    , video_count = Nothing
    , is_favorited = False
    , subscribing = False
    , is_muted = False
    , can_downvote = False
    }


type Profile
    = NoProfile
    | UserProfile User
    | GroupProfile Group
    | TopicProfile Topic


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
    , checkmark : String
    }


type StyleOption
    = LightStyle
    | DarkStyle


type alias Settings =
    { columnWidth : Int
    , fontSize : Float
    , here : Zone
    , windowWidth : Int
    , windowHeight : Int
    , styleOption : StyleOption
    , style : Style
    }
