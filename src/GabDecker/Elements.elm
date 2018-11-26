----------------------------------------------------------------------
--
-- Elements.elm
-- Useful wrappers around elm-ui primitives.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- Search for TODO to see remaining work.
--
----------------------------------------------------------------------


module GabDecker.Elements exposing
    ( blue
    , gray
    , green
    , heightImage
    , lightBlue
    , lightgray
    , linkColor
    , linkHoverColor
    , newTabLink
    , rgbi
    , simpleImage
    , simpleLink
    , styledLink
    , turquoise
    , verylightgray
    , widthImage
    )

import Element
    exposing
        ( Attribute
        , Color
        , Element
        , height
        , image
        , link
        , px
        , text
        , width
        )
import Element.Font as Font


simpleLink : String -> String -> Element msg
simpleLink url label =
    styledLink False [] url label


newTabLink : String -> String -> Element msg
newTabLink url label =
    styledLink True [] url label


itou : Int -> Float
itou i =
    toFloat i / 255


rgbi : Int -> Int -> Int -> Color
rgbi r g b =
    Element.rgb (itou r) (itou g) (itou b)


turquoise : Color
turquoise =
    rgbi 0 0xFF 0xFF


green : Color
green =
    rgbi 0 0xFF 0


lightBlue : Color
lightBlue =
    rgbi 0xAD 0xD8 0xE6


blue : Color
blue =
    Element.rgb 0 0 1


gray : Color
gray =
    rgbi 0x80 0x80 0x80


lightgray : Color
lightgray =
    rgbi 0xE0 0xE0 0xE0


verylightgray : Color
verylightgray =
    rgbi 0xF4 0xF4 0xF4


linkColor =
    blue


linkHoverColor =
    lightBlue


{-| Color highlighting is temporary, until Font.underline becomes decorative.
-}
styledLink : Bool -> List (Attribute msg) -> String -> String -> Element msg
styledLink newTab attributes url label =
    (if newTab then
        Element.newTabLink

     else
        link
    )
        (List.concat
            [ [ Font.color blue
              , Element.mouseOver [ Font.color lightBlue ]
              ]
            , attributes
            ]
        )
        { url = url
        , label = text label
        }


simpleImage : String -> String -> ( Int, Int ) -> Element msg
simpleImage src description ( w, h ) =
    image
        [ width (px w)
        , height (px h)
        ]
        { src = src
        , description = description
        }


heightImage : String -> String -> Int -> Element msg
heightImage src description h =
    image
        [ height (px h)
        ]
        { src = src
        , description = description
        }


widthImage : String -> String -> Int -> Element msg
widthImage src description w =
    image
        [ width (px w)
        ]
        { src = src
        , description = description
        }
