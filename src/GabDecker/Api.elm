module GabDecker.Api exposing
    ( Backend(..)
    , Error
    , downvotePost
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
import Http
import Json.Encode exposing (Value)
import OAuth exposing (Token)
import Task exposing (Task)


type Backend
    = RealBackend Token
    | SimulatedBackend


type alias Error =
    { httpError : Maybe Http.Error
    , message : String
    }


unimplemented : (Result Error x -> msg) -> Cmd msg
unimplemented tagger =
    Task.perform tagger
        (Task.succeed <|
            Err
                { httpError = Nothing
                , message = "Unimplemented"
                }
        )


wrapHttpReturn : (Result Error x -> msg) -> Http.Request x -> Cmd msg
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


me : Backend -> (Result Error User -> msg) -> Cmd msg
me backend tagger =
    case backend of
        RealBackend token ->
            wrapHttpReturn tagger <|
                Gab.me token

        SimulatedBackend ->
            userProfile backend tagger simulatedUser


userProfile : Backend -> (Result Error User -> msg) -> String -> Cmd msg
userProfile backend tagger username =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userProfile token username

            SimulatedBackend ->
                Http.get ("json/users/" ++ username ++ ".json") userDecoder


userFollowers : Backend -> (Result Error UserList -> msg) -> String -> Int -> Cmd msg
userFollowers backend tagger username before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userFollowers token username before

            SimulatedBackend ->
                Http.get ("json/followers/" ++ username ++ ".json") userListDecoder


userFollowing : Backend -> (Result Error UserList -> msg) -> String -> Int -> Cmd msg
userFollowing backend tagger username before =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.userFollowing token username before

            SimulatedBackend ->
                Http.get ("json/following/" ++ username ++ ".json") userListDecoder


followUser : Backend -> (Result Error Value -> msg) -> String -> Cmd msg
followUser backend tagger username =
    unimplemented tagger


muteUser : Backend -> (Result Error User -> msg) -> String -> Cmd msg
muteUser backend tagger username =
    unimplemented tagger


whichJson : String -> String
whichJson before =
    if before == "" then
        "1.json"

    else
        "2.json"


homeFeed : Backend -> (Result Error ActivityLogList -> msg) -> String -> Cmd msg
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
                Http.get ("json/feeds/home" ++ json) activityLogListDecoder


userFeed : Backend -> (Result Error ActivityLogList -> msg) -> String -> String -> Cmd msg
userFeed backend tagger username before =
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


groupFeed : Backend -> (Result Error ActivityLogList -> msg) -> String -> String -> Cmd msg
groupFeed backend tagger groupid before =
    unimplemented tagger


popularFeed : Backend -> (Result Error ActivityLogList -> msg) -> Cmd msg
popularFeed backend tagger =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.popularFeed token

            SimulatedBackend ->
                Http.get "json/feeds/popular.json" activityLogListDecoder


popularUsers : Backend -> (Result Error UserList -> msg) -> Cmd msg
popularUsers backend tagger =
    wrapHttpReturn tagger <|
        case backend of
            RealBackend token ->
                Gab.popularUsers token

            SimulatedBackend ->
                Http.get "json/users/popular.json" userListDecoder


upvotePost : Backend -> (Result Error Value -> msg) -> String -> Bool -> Cmd msg
upvotePost backend tagger postid unupvote =
    unimplemented tagger


downvotePost : Backend -> (Result Error Value -> msg) -> String -> Bool -> Cmd msg
downvotePost backend tagger postid undownvote =
    unimplemented tagger


repost : Backend -> (Result Error Value -> msg) -> String -> Bool -> Cmd msg
repost backend tagger postid unrepost =
    unimplemented tagger


getPost : Backend -> (Result Error Post -> msg) -> Cmd msg
getPost backend tagger =
    unimplemented tagger


newPost : Backend -> (Result Error PostForm -> msg) -> Cmd msg
newPost backend tagger =
    unimplemented tagger


postImage : Backend -> (Result Error File -> msg) -> Cmd msg
postImage backend tagger =
    unimplemented tagger
