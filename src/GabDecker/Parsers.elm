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
import GabDecker.Types exposing (Style)
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
parseElements : Style -> (Style -> String -> String -> Element msg) -> String -> List (Element msg)
parseElements style renderer string =
    fullyParse text (allOneParsers style renderer) string


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


atUserParser : Style -> (Style -> String -> String -> Element msg) -> Parser (Element msg)
atUserParser style renderer =
    P.variable
        { start = (==) '@'
        , inner = \c -> c == '_' || Char.isAlphaNum c
        , reserved = Set.empty
        }
        |> P.andThen
            (\s ->
                P.succeed <|
                    renderer style (String.dropLeft 1 s) s
            )


atUserOneParser : Style -> (Style -> String -> String -> Element msg) -> OneParser (Element msg)
atUserOneParser style renderer =
    parseOne (atUserParser style renderer)


sharpParser : Style -> Parser (Element msg)
sharpParser style =
    P.variable
        { start = (==) '#'
        , inner = \c -> c == '_' || Char.isAlphaNum c
        , reserved = Set.empty
        }
        |> P.andThen
            (\s ->
                P.succeed <|
                    newTabLink style
                        ("https://gab.com/hash/" ++ String.dropLeft 1 s)
                        s
            )


sharpOneParser : Style -> OneParser (Element msg)
sharpOneParser style =
    parseOne (sharpParser style)


htmlOneParser : Style -> Parser ( String, Maybe (Element msg), String )
htmlOneParser style =
    P.loop () (htmlOneHelp style)


htmlOneHelp : Style -> () -> Parser (Step () ( String, Maybe (Element msg), String ))
htmlOneHelp style _ =
    P.oneOf
        [ P.succeed
            (\start ( a, tail ) end whole ->
                P.Done
                    ( String.slice 0 start whole
                    , Just a
                    , tail ++ String.slice end (String.length whole) whole
                    )
            )
            |= P.getOffset
            |= htmlParser style
            |= P.getOffset
            |= P.getSource
        , P.chompIf (\_ -> True)
            |> P.andThen (\_ -> P.succeed <| P.Loop ())
        , P.succeed
            (\source -> P.Done ( source, Nothing, "" ))
            |= P.getSource
        ]


htmlParser : Style -> Parser ( Element msg, String )
htmlParser style =
    htmlStringParser
        |> P.andThen
            (\( url, tail ) ->
                P.succeed ( newTabLink style url url, tail )
            )


htmlStringParser : Parser ( String, String )
htmlStringParser =
    P.succeed
        (\start end whole ( body, tail ) ->
            ( String.slice start end whole ++ body
            , tail
            )
        )
        |= P.getOffset
        |. P.symbol "http"
        |. P.oneOf [ P.chompIf ((==) 's'), P.succeed () ]
        |. P.symbol "://"
        |= P.getOffset
        |= P.getSource
        |= htmlCharacters


allOneParsers : Style -> (Style -> String -> String -> Element msg) -> List (OneParser (Element msg))
allOneParsers style renderer =
    [ htmlOneParser style, atUserOneParser style renderer, sharpOneParser style ]


ctrlR : Char
ctrlR =
    Char.fromCode 0x0D


nbsp : Char
nbsp =
    Char.fromCode 0xA0


whitespaceChars : List Char
whitespaceChars =
    [ ' '
    , '\t'
    , '\n'
    , ctrlR
    , nbsp
    , ','
    ]


isWhitespaceChar : Char -> Bool
isWhitespaceChar c =
    List.member c whitespaceChars


isNonWhitespaceChar : Char -> Bool
isNonWhitespaceChar c =
    not <| isWhitespaceChar c


htmlEndingChars : List Char
htmlEndingChars =
    [ '!'
    , '.'
    , '?'
    ]


isHtmlEndingChar : Char -> Bool
isHtmlEndingChar c =
    List.member c htmlEndingChars


htmlEndingStrings : List String
htmlEndingStrings =
    List.map String.fromChar htmlEndingChars


isHtmlEndingString : String -> Bool
isHtmlEndingString s =
    List.member s htmlEndingStrings


htmlEndingString : Parser String
htmlEndingString =
    P.succeed String.slice
        |= P.getOffset
        |. P.chompWhile isHtmlEndingChar
        |= P.getOffset
        |= P.getSource


htmlEnding : Parser String
htmlEnding =
    P.succeed identity
        |= htmlEndingString
        |. P.oneOf
            [ P.end
            , P.getChompedString (P.chompWhile isNonWhitespaceChar)
                |> P.andThen
                    (\s ->
                        if s == "" then
                            P.succeed ()

                        else
                            P.problem "Non-whitespace after ending chars."
                    )
            ]


{-| Need to make this stop at a `nonEndingHtmlChars` followed only
by more of them until a `nonHtmlChars`.
-}
htmlCharacters : Parser ( String, String )
htmlCharacters =
    P.getChompedString
        (P.succeed ()
            |. P.chompWhile isNonWhitespaceChar
        )
        |> P.andThen
            (\str ->
                let
                    loop s end =
                        if s == "" then
                            P.problem "No body to URL."

                        else
                            let
                                ch =
                                    String.right 1 s
                            in
                            if isHtmlEndingString ch then
                                loop (String.dropRight 1 s) <| ch ++ end

                            else
                                P.succeed ( s, end )
                in
                loop str ""
            )
