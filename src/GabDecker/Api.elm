module GabDecker.Api exposing
    ( Backend(..)
    , downvotePost
    , feedTypeToGetter
    , followUser
    , getPost
    , groupFeed
    , homeFeed
    , me
    , muteUser
    , newPost
    , notifications
    , popularFeed
    , popularUsers
    , postImage
    , repost
    , topicFeed
    , upvotePost
    , userFeed
    , userFollowers
    , userFollowing
    , userProfile
    )

import CustomElement.FileListener as File exposing (File)
import Gab
import Gab.EncodeDecode
    exposing
        ( activityLogDecoder
        , activityLogListDecoder
        , notificationsLogDecoder
        , postDecoder
        , userDecoder
        , userListDecoder
        )
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , NotificationsLog
        , Post
        , PostForm
        , Success
        , User
        , UserList
        )
import GabDecker.Types
    exposing
        ( ApiError
        , FeedGetter(..)
        , FeedTagger
        , FeedType(..)
        )
import Http
import Json.Encode exposing (Value)
import OAuth exposing (Token)
import Task exposing (Task)


type Backend
    = RealBackend Token
    | SimulatedBackend


feedTypeToGetter : FeedType -> Backend -> FeedGetter msg
feedTypeToGetter feedType backend =
    case feedType of
        HomeFeed ->
            PostFeedGetter <| homeFeed backend

        UserFeed username ->
            PostFeedGetter <| userFeed backend username

        GroupFeed groupid ->
            PostFeedGetter <| groupFeed backend groupid

        TopicFeed topicid ->
            PostFeedGetter <| topicFeed backend topicid

        PopularFeed ->
            PostFeedGetter (\tagger _ -> popularFeed backend tagger)

        NotificationsFeed ->
            NotificationFeedGetter <| notifications backend

        _ ->
            PostFeedGetter (\tagger _ -> unimplemented tagger)


unimplemented : FeedTagger x msg -> Cmd msg
unimplemented tagger =
    Task.perform tagger
        (Task.succeed <|
            Err
                { httpError = Nothing
                , message = "Unimplemented"
                }
        )


wrapHttpReturn : FeedTagger x msg -> Http.Request x -> Cmd msg
wrapHttpReturn tagger request =
    let
        tag : Result Http.Error x -> msg
        tag result =
            case result of
                Ok x ->
                    tagger <| Ok x

                Err err ->
                    tagger <|
                        Err
                            { httpError = Just err
                            , message = "Http Error"
                            }
    in
    Http.send tag request


simulatedUser : String
simulatedUser =
    "billstclair"


me : Backend -> FeedTagger User msg -> Cmd msg
me backend tagger =
    case backend of
        RealBackend token ->
            wrapHttpReturn tagger <|
                Gab.me token

        SimulatedBackend ->
            userProfile backend tagger simulatedUser


userProfile : Backend -> FeedTagger User msg -> String -> Cmd msg
userProfile backend tagger username =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userProfile token username

            SimulatedBackend ->
                Http.get ("json/users/" ++ username ++ ".json") userDecoder


userFollowers : Backend -> FeedTagger UserList msg -> String -> Int -> Cmd msg
userFollowers backend tagger username before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userFollowers token username before

            SimulatedBackend ->
                Http.get ("json/followers/" ++ username ++ ".json") userListDecoder


userFollowing : Backend -> FeedTagger UserList msg -> String -> Int -> Cmd msg
userFollowing backend tagger username before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userFollowing token username before

            SimulatedBackend ->
                Http.get ("json/following/" ++ username ++ ".json") userListDecoder


followUser : Backend -> FeedTagger Success msg -> String -> Cmd msg
followUser backend tagger username =
    unimplemented tagger


muteUser : Backend -> FeedTagger Success msg -> String -> Cmd msg
muteUser backend tagger username =
    unimplemented tagger


whichJson : String -> String
whichJson before =
    if before == "" then
        "1.json"

    else
        "2.json"


homeFeed : Backend -> FeedTagger ActivityLogList msg -> String -> Cmd msg
homeFeed backend tagger before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.homeFeed token before

            SimulatedBackend ->
                let
                    json =
                        whichJson before
                in
                Http.get ("json/feeds/home/" ++ json) activityLogListDecoder


userFeed : Backend -> String -> FeedTagger ActivityLogList msg -> String -> Cmd msg
userFeed backend username tagger before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userFeed token username before

            SimulatedBackend ->
                let
                    json =
                        whichJson before
                in
                Http.get ("json/feeds/" ++ username ++ "/" ++ json)
                    activityLogListDecoder


groupFeed : Backend -> String -> FeedTagger ActivityLogList msg -> String -> Cmd msg
groupFeed backend groupid tagger before =
    unimplemented tagger


topicFeed : Backend -> String -> FeedTagger ActivityLogList msg -> String -> Cmd msg
topicFeed backend topicid tagger before =
    unimplemented tagger


popularFeed : Backend -> FeedTagger ActivityLogList msg -> Cmd msg
popularFeed backend tagger =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.popularFeed token

            SimulatedBackend ->
                Http.get "json/feeds/popular.json" activityLogListDecoder


popularUsers : Backend -> FeedTagger UserList msg -> Cmd msg
popularUsers backend tagger =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.popularUsers token

            SimulatedBackend ->
                Http.get "json/users/popular.json" userListDecoder


notifications : Backend -> FeedTagger NotificationsLog msg -> String -> Cmd msg
notifications backend tagger before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.notifications token before

            SimulatedBackend ->
                Http.get ("json/notifications/" ++ whichJson before)
                    notificationsLogDecoder


upvotePost : Backend -> FeedTagger Success msg -> Int -> Bool -> Cmd msg
upvotePost backend tagger postid unupvote =
    case backend of
        RealBackend token ->
            wrapHttpReturn tagger <|
                Gab.upvotePost token postid unupvote

        SimulatedBackend ->
            unimplemented tagger


downvotePost : Backend -> FeedTagger Success msg -> Int -> Bool -> Cmd msg
downvotePost backend tagger postid undownvote =
    case backend of
        RealBackend token ->
            wrapHttpReturn tagger <|
                Gab.downvotePost token postid undownvote

        SimulatedBackend ->
            unimplemented tagger


repost : Backend -> FeedTagger Success msg -> Int -> Bool -> Cmd msg
repost backend tagger postid unrepost =
    case backend of
        RealBackend token ->
            wrapHttpReturn tagger <|
                Gab.repost token postid unrepost

        SimulatedBackend ->
            unimplemented tagger


getPost : Backend -> FeedTagger Post msg -> Cmd msg
getPost backend tagger =
    unimplemented tagger


newPost : Backend -> FeedTagger PostForm msg -> Cmd msg
newPost backend tagger =
    unimplemented tagger


postImage : Backend -> FeedTagger File msg -> Cmd msg
postImage backend tagger =
    unimplemented tagger
