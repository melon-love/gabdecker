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
        , postDecoder
        , userDecoder
        , userListDecoder
        )
import Gab.Types
    exposing
        ( ActivityLog
        , ActivityLogList
        , Post
        , PostForm
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
            FeedGetterWithBefore <| homeFeed backend

        UserFeed username ->
            FeedGetterWithBefore <| userFeed backend username

        GroupFeed groupid ->
            FeedGetterWithBefore <| groupFeed backend groupid

        TopicFeed topicid ->
            FeedGetterWithBefore <| topicFeed backend topicid

        PopularFeed ->
            FeedGetter <| popularFeed backend


unimplemented : (Result ApiError x -> msg) -> Cmd msg
unimplemented tagger =
    Task.perform tagger
        (Task.succeed <|
            Err
                { httpError = Nothing
                , message = "Unimplemented"
                }
        )


wrapHttpReturn : (Result ApiError x -> msg) -> Http.Request x -> Cmd msg
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


me : Backend -> (Result ApiError User -> msg) -> Cmd msg
me backend tagger =
    case backend of
        RealBackend token ->
            wrapHttpReturn tagger <|
                Gab.me token

        SimulatedBackend ->
            userProfile backend tagger simulatedUser


userProfile : Backend -> (Result ApiError User -> msg) -> String -> Cmd msg
userProfile backend tagger username =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userProfile token username

            SimulatedBackend ->
                Http.get ("json/users/" ++ username ++ ".json") userDecoder


userFollowers : Backend -> (Result ApiError UserList -> msg) -> String -> Int -> Cmd msg
userFollowers backend tagger username before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userFollowers token username before

            SimulatedBackend ->
                Http.get ("json/followers/" ++ username ++ ".json") userListDecoder


userFollowing : Backend -> (Result ApiError UserList -> msg) -> String -> Int -> Cmd msg
userFollowing backend tagger username before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userFollowing token username before

            SimulatedBackend ->
                Http.get ("json/following/" ++ username ++ ".json") userListDecoder


followUser : Backend -> (Result ApiError Value -> msg) -> String -> Cmd msg
followUser backend tagger username =
    unimplemented tagger


muteUser : Backend -> (Result ApiError User -> msg) -> String -> Cmd msg
muteUser backend tagger username =
    unimplemented tagger


whichJson : String -> String
whichJson before =
    if before == "" then
        "1.json"

    else
        "2.json"


homeFeed : Backend -> FeedTagger msg -> String -> Cmd msg
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


userFeed : Backend -> String -> FeedTagger msg -> String -> Cmd msg
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


groupFeed : Backend -> String -> FeedTagger msg -> String -> Cmd msg
groupFeed backend groupid tagger before =
    unimplemented tagger


topicFeed : Backend -> String -> FeedTagger msg -> String -> Cmd msg
topicFeed backend topicid tagger before =
    unimplemented tagger


popularFeed : Backend -> FeedTagger msg -> Cmd msg
popularFeed backend tagger =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.popularFeed token

            SimulatedBackend ->
                Http.get "json/feeds/popular.json" activityLogListDecoder


popularUsers : Backend -> (Result ApiError UserList -> msg) -> Cmd msg
popularUsers backend tagger =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.popularUsers token

            SimulatedBackend ->
                Http.get "json/users/popular.json" userListDecoder


upvotePost : Backend -> (Result ApiError Value -> msg) -> String -> Bool -> Cmd msg
upvotePost backend tagger postid unupvote =
    unimplemented tagger


downvotePost : Backend -> (Result ApiError Value -> msg) -> String -> Bool -> Cmd msg
downvotePost backend tagger postid undownvote =
    unimplemented tagger


repost : Backend -> (Result ApiError Value -> msg) -> String -> Bool -> Cmd msg
repost backend tagger postid unrepost =
    unimplemented tagger


getPost : Backend -> (Result ApiError Post -> msg) -> Cmd msg
getPost backend tagger =
    unimplemented tagger


newPost : Backend -> (Result ApiError PostForm -> msg) -> Cmd msg
newPost backend tagger =
    unimplemented tagger


postImage : Backend -> (Result ApiError File -> msg) -> Cmd msg
postImage backend tagger =
    unimplemented tagger
