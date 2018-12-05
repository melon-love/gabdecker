----------------------------------------------------------------------
--
-- Parsers.elm
-- GabDecker parsers.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module GabDecker.Parsers exposing
    ( PartialParse(..)
    , allOneParsers
    , atUserOneParser
    , atUserParser
    , fullyParse
    , htmlOneParser
    , htmlParser
    , htmlStringParser
    , multiParseString
    , parseElements
    , parseOne
    , parseString
    , subParse
    , subParseOne
    , wrapPartialParses
    )

import Element as E exposing (Element, text)
import GabDecker.Elements
    exposing
        ( heightImage
        , newTabLink
        , simpleImage
        , simpleLink
        )
import Parser as P exposing ((|.), (|=), Parser, Step(..))
import Set exposing (Set)


type PartialParse a
    = Unparsed String
    | Parsed a


type alias OneParser a =
    Parser ( String, Maybe a, String )


{-| This is the only function you'll probably ever need.

The others are exported for testing in `elm repl`.

-}
parseElements : String -> List (Element msg)
parseElements string =
    fullyParse text allOneParsers string


fullyParse : (String -> a) -> List (OneParser a) -> String -> List a
fullyParse wrapper parsers string =
    multiParseString parsers string
        |> wrapPartialParses wrapper


wrapPartialParses : (String -> a) -> List (PartialParse a) -> List a
wrapPartialParses wrapper parses =
    let
        realize p =
            case p of
                Unparsed string ->
                    wrapper string

                Parsed a ->
                    a
    in
    List.map realize parses


multiParseString : List (OneParser a) -> String -> List (PartialParse a)
multiParseString parsers string =
    case parsers of
        [] ->
            []

        parser :: rest ->
            List.foldl (\p res -> subParse p res)
                (parseString parser string)
                rest


subParse : OneParser a -> List (PartialParse a) -> List (PartialParse a)
subParse parser parses =
    List.map (subParseOne parser) parses
        |> List.concat


subParseOne : OneParser a -> PartialParse a -> List (PartialParse a)
subParseOne parser parse =
    case parse of
        Unparsed string ->
            parseString parser string

        Parsed _ ->
            [ parse ]


parseString : OneParser a -> String -> List (PartialParse a)
parseString parser string =
    let
        loop s res =
            if s == "" then
                List.reverse res

            else
                case P.run parser s of
                    Err _ ->
                        List.reverse <| Unparsed s :: res

                    Ok ( prefix, ma, suffix ) ->
                        loop suffix <|
                            List.concat
                                [ case ma of
                                    Nothing ->
                                        []

                                    Just a ->
                                        [ Parsed a ]
                                , if prefix == "" then
                                    []

                                  else
                                    [ Unparsed prefix ]
                                , res
                                ]
    in
    loop string []


parseOne : Parser a -> Parser ( String, Maybe a, String )
parseOne parser =
    P.loop () (parseOneHelp parser)


parseOneHelp : Parser a -> () -> Parser (Step () ( String, Maybe a, String ))
parseOneHelp parser _ =
    P.oneOf
        [ P.succeed
            (\start a end whole ->
                P.Done
                    ( String.slice 0 start whole
                    , a
                    , String.slice end (String.length whole) whole
                    )
            )
            |= P.getOffset
            |= (parser |> P.andThen (\a -> P.succeed <| Just a))
            |= P.getOffset
            |= P.getSource
        , P.chompIf (\_ -> True)
            |> P.andThen (\_ -> P.succeed <| P.Loop ())
        , P.succeed
            (\source -> P.Done ( source, Nothing, "" ))
            |= P.getSource
        ]


atUserParser : Parser (Element msg)
atUserParser =
    P.variable
        { start = (==) '@'
        , inner = \c -> c == '_' || Char.isAlphaNum c
        , reserved = Set.empty
        }
        |> P.andThen
            (\s ->
                P.succeed <|
                    newTabLink ("https://gab.com/" ++ String.dropLeft 1 s)
                        s
            )


atUserOneParser : OneParser (Element msg)
atUserOneParser =
    parseOne atUserParser


sharpParser : Parser (Element msg)
sharpParser =
    P.variable
        { start = (==) '#'
        , inner = \c -> c == '_' || Char.isAlphaNum c
        , reserved = Set.empty
        }
        |> P.andThen
            (\s ->
                P.succeed <|
                    newTabLink ("https://gab.com/hash/" ++ String.dropLeft 1 s)
                        s
            )


sharpOneParser : OneParser (Element msg)
sharpOneParser =
    parseOne sharpParser


htmlParser : Parser (Element msg)
htmlParser =
    htmlStringParser
        |> P.andThen (\s -> P.succeed <| newTabLink s s)


htmlStringParser : Parser String
htmlStringParser =
    P.succeed String.slice
        |= P.getOffset
        |. P.symbol "http"
        |. P.oneOf [ P.chompIf ((==) 's'), P.succeed () ]
        |. P.symbol "://"
        |. nonWhitespace
        |= P.getOffset
        |= P.getSource


htmlOneParser : OneParser (Element msg)
htmlOneParser =
    parseOne htmlParser


ctrlR : Char
ctrlR =
    Char.fromCode 0x0D


allOneParsers : List (OneParser (Element msg))
allOneParsers =
    [ htmlOneParser, atUserOneParser, sharpOneParser ]


nonWhitespace : Parser ()
nonWhitespace =
    P.chompWhile (\c -> not <| List.member c [ ' ', '\t', '\n', ctrlR ])
