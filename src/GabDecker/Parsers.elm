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
    , parseString
    , subParse
    , subParseOne
    )

import Element as E exposing (Element)
import GabDecker.Elements
    exposing
        ( heightImage
        , newTabLink
        , simpleImage
        , simpleLink
        )
import Parser as P exposing (Parser)


type PartialParse a
    = Unparsed String
    | Parsed a


type alias OneParser a =
    Parser ( String, a, String )


fullyParse : (String -> a) -> List (PartialParse a) -> List a
fullyParse wrapper parses =
    let
        realize p =
            case p of
                Unparsed string ->
                    wrapper string

                Parsed a ->
                    a
    in
    List.map realize parses


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

                    Ok ( prefix, a, suffix ) ->
                        loop suffix <|
                            List.concat
                                [ if prefix == "" then
                                    []

                                  else
                                    [ Unparsed prefix ]
                                , res
                                ]
    in
    loop string []
