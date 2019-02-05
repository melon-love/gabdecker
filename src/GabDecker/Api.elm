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
import Json.Decode exposing (Decoder)
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


wrapHttpReturn : FeedTagger x msg -> Result Http.Error x -> msg
wrapHttpReturn tagger result =
    case result of
        Ok x ->
            tagger <| Ok x

        Err err ->
            tagger <|
                Err
                    { httpError = Just err
                    , message = "Http Error"
                    }


simulatedUser : String
simulatedUser =
    "billstclair"


me : Backend -> FeedTagger User msg -> Cmd msg
me backend tagger =
    case backend of
        RealBackend token ->
            Gab.me (wrapHttpReturn tagger) token

        SimulatedBackend ->
            userProfile backend tagger simulatedUser


httpGet : String -> FeedTagger a msg -> Decoder a -> Cmd msg
httpGet url tagger decoder =
    Http.get
        { url = url
        , expect = Http.expectJson (wrapHttpReturn tagger) decoder
        }


userProfile : Backend -> FeedTagger User msg -> String -> Cmd msg
userProfile backend tagger username =
    case backend of
        RealBackend token ->
            Gab.userProfile (wrapHttpReturn tagger) token username

        SimulatedBackend ->
            httpGet ("json/users/" ++ username ++ ".json")
                tagger
                userDecoder


userFollowers : Backend -> FeedTagger UserList msg -> String -> Int -> Cmd msg
userFollowers backend tagger username before =
    case backend of
        RealBackend token ->
            Gab.userFollowers (wrapHttpReturn tagger) token username before

        SimulatedBackend ->
            httpGet ("json/followers/" ++ username ++ ".json")
                tagger
                userListDecoder


userFollowing : Backend -> FeedTagger UserList msg -> String -> Int -> Cmd msg
userFollowing backend tagger username before =
    case backend of
        RealBackend token ->
            Gab.userFollowing (wrapHttpReturn tagger) token username before

        SimulatedBackend ->
            httpGet ("json/following/" ++ username ++ ".json")
                tagger
                userListDecoder


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
    case backend of
        RealBackend token ->
            Gab.homeFeed (wrapHttpReturn tagger) token before

        SimulatedBackend ->
            let
                json =
                    whichJson before
            in
            httpGet ("json/feeds/home/" ++ json)
                tagger
                activityLogListDecoder


userFeed : Backend -> String -> FeedTagger ActivityLogList msg -> String -> Cmd msg
userFeed backend username tagger before =
    case backend of
        RealBackend token ->
            Gab.userFeed (wrapHttpReturn tagger) token username before

        SimulatedBackend ->
            let
                json =
                    whichJson before
            in
            httpGet ("json/feeds/" ++ username ++ "/" ++ json)
                tagger
                activityLogListDecoder


groupFeed : Backend -> String -> FeedTagger ActivityLogList msg -> String -> Cmd msg
groupFeed backend groupid tagger before =
    unimplemented tagger


topicFeed : Backend -> String -> FeedTagger ActivityLogList msg -> String -> Cmd msg
topicFeed backend topicid tagger before =
    unimplemented tagger


popularFeed : Backend -> FeedTagger ActivityLogList msg -> Cmd msg
popularFeed backend tagger =
    case backend of
        RealBackend token ->
            Gab.popularFeed (wrapHttpReturn tagger) token

        SimulatedBackend ->
            httpGet "json/feeds/popular.json"
                tagger
                activityLogListDecoder


popularUsers : Backend -> FeedTagger UserList msg -> Cmd msg
popularUsers backend tagger =
    case backend of
        RealBackend token ->
            Gab.popularUsers (wrapHttpReturn tagger) token

        SimulatedBackend ->
            httpGet "json/users/popular.json"
                tagger
                userListDecoder


notifications : Backend -> FeedTagger NotificationsLog msg -> String -> Cmd msg
notifications backend tagger before =
    case backend of
        RealBackend token ->
            Gab.notifications (wrapHttpReturn tagger) token before

        SimulatedBackend ->
            httpGet ("json/notifications/" ++ whichJson before)
                tagger
                notificationsLogDecoder


upvotePost : Backend -> FeedTagger Success msg -> Int -> Bool -> Cmd msg
upvotePost backend tagger postid unupvote =
    case backend of
        RealBackend token ->
            Gab.upvotePost (wrapHttpReturn tagger) token postid unupvote

        SimulatedBackend ->
            unimplemented tagger


downvotePost : Backend -> FeedTagger Success msg -> Int -> Bool -> Cmd msg
downvotePost backend tagger postid undownvote =
    case backend of
        RealBackend token ->
            Gab.downvotePost (wrapHttpReturn tagger) token postid undownvote

        SimulatedBackend ->
            unimplemented tagger


repost : Backend -> FeedTagger Success msg -> Int -> Bool -> Cmd msg
repost backend tagger postid unrepost =
    case backend of
        RealBackend token ->
            Gab.repost (wrapHttpReturn tagger) token postid unrepost

        SimulatedBackend ->
            unimplemented tagger


getPost : Backend -> FeedTagger Post msg -> Cmd msg
getPost backend tagger =
    unimplemented tagger


newPost : Backend -> FeedTagger ActivityLog msg -> PostForm -> Cmd msg
newPost backend tagger postForm =
    case backend of
        RealBackend token ->
            Gab.newPost (wrapHttpReturn tagger) token postForm

        SimulatedBackend ->
            unimplemented tagger


postImage : Backend -> FeedTagger File msg -> Cmd msg
postImage backend tagger =
    unimplemented tagger
