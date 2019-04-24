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
    ( AtUser(..)
    , PartialParse(..)
    , ReplaceOrPrefix(..)
    , allOneParsers
    , atUserOneParser
    , atUserParser
    , atVariableList
    , atVariablePs
    , atVariables
    , fullyParse
    , fullyParseAtUsers
    , htmlOneParser
    , htmlParser
    , htmlStringParser
    , multiParseString
    , nAtVariables
    , parseElements
    , parseOne
    , parseString
    , replaceAtUsers
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


atVariable : Parser String
atVariable =
    P.variable
        { start = (==) '@'
        , inner = \c -> c == '_' || c == '-' || Char.isAlphaNum c
        , reserved = Set.empty
        }
        |> P.andThen
            (\v ->
                if v == "@" then
                    P.problem "bare @"

                else
                    P.succeed v
            )


atUserParser : Style -> (Style -> String -> String -> Element msg) -> Parser (Element msg)
atUserParser style renderer =
    atVariable
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


keywordString : String -> Parser String
keywordString string =
    P.keyword string |> P.andThen (\_ -> P.succeed string)


whitespace : Parser ()
whitespace =
    P.chompWhile isWhitespaceChar


savedWhitespace : Parser String
savedWhitespace =
    P.getChompedString <| whitespace


pSep : Parser ()
pSep =
    P.succeed ()
        |. whitespace
        |. P.symbol "</p>"
        |. whitespace
        |. P.symbol "<p>"
        |. whitespace


brSep : Parser ()
brSep =
    P.succeed ()
        |. whitespace
        |. P.oneOf [ P.symbol "<br />", P.symbol "<br/>", P.symbol "<br>" ]
        |. whitespace


atVariableList : Parser (List String)
atVariableList =
    P.loop [] variableListHelp


variableListHelp : List String -> Parser (Step (List String) (List String))
variableListHelp res =
    P.oneOf
        [ P.backtrackable
            (P.succeed (\at -> Loop (at :: res))
                |= P.backtrackable atVariable
                |. whitespace
            )
        , P.succeed ()
            |> P.andThen
                (\_ ->
                    if res == [] then
                        P.problem "Empty list"

                    else
                        P.succeed ()
                            |> P.map (\_ -> P.Done (List.reverse res))
                )
        ]


atVariablePs : Parser (List String)
atVariablePs =
    P.loop [] variablePsHelp


variablePsHelp : List String -> Parser (Step (List String) (List String))
variablePsHelp res =
    P.oneOf
        [ P.succeed (\at -> Loop (at :: res))
            |= P.backtrackable atVariable
            |. P.backtrackable pSep
        , P.succeed (\at -> Loop (at :: res))
            |= P.backtrackable atVariable
            |. P.backtrackable brSep
        , P.succeed (\at -> Loop (at :: res))
            |= P.backtrackable atVariable
            |. whitespace
        , P.succeed ()
            |> P.andThen
                (\_ ->
                    if res == [] then
                        P.problem "Empty list"

                    else
                        P.succeed ()
                            |> P.map (\_ -> P.Done (List.reverse res))
                )
        ]


atVariables : Parser (List String)
atVariables =
    P.oneOf
        [ P.backtrackable atVariableList
        , P.succeed identity
            |. P.backtrackable (P.symbol "<p>")
            |. P.backtrackable whitespace
            |= P.backtrackable atVariablePs
            |. P.backtrackable whitespace
            |. P.backtrackable (P.symbol "</p>")
        ]


nAtVariables : Int -> Parser String
nAtVariables n =
    P.getChompedString
        (atVariables
            |> P.andThen
                (\vs ->
                    if List.length vs >= n then
                        P.succeed vs

                    else
                        P.problem "Fewer than n @vars"
                )
        )



-- TODO
-- Needs to start with <p>, and end with </p>


type AtUser
    = AtUser String
    | NotAtUser String


nAtUserParser : Int -> Parser AtUser
nAtUserParser n =
    nAtVariables n
        |> P.andThen (P.succeed << AtUser)


nAtUserOneParser : Int -> OneParser AtUser
nAtUserOneParser n =
    parseOne <| nAtUserParser n


fullyParseAtUsers : Int -> String -> List AtUser
fullyParseAtUsers n string =
    Debug.log "fullyParseAtUsers" <|
        fullyParse NotAtUser [ nAtUserOneParser n ] string


type ReplaceOrPrefix
    = Replace String
    | Prefix String


replaceAtUsers : Int -> ReplaceOrPrefix -> String -> String
replaceAtUsers n action string =
    let
        folder : AtUser -> String -> String
        folder atUser res =
            case atUser of
                NotAtUser s ->
                    s ++ res

                AtUser s ->
                    case action of
                        Replace r ->
                            r ++ res

                        Prefix p ->
                            p ++ s ++ res
    in
    List.foldr folder "" <| fullyParseAtUsers n string
