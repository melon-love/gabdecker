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
    ( circularHeightImage
    , circularHeightImageWithAttributes
    , colors
    , darkStyle
    , defaultColumnWidth
    , defaultFontSize
    , defaultSettings
    , defaultStyles
    , getIconUrl
    , heightImage
    , lightStyle
    , newTabLink
    , rgbi
    , simpleImage
    , simpleLink
    , styledLink
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
import GabDecker.Types
    exposing
        ( IconUrls
        , Settings
        , Style
        , StyleOption(..)
        , Styles
        )
import Html
import Html.Attributes as Attributes
import Time


simpleLink : Style -> String -> String -> Element msg
simpleLink style url label =
    styledLink False style [] url (text label)


newTabLink : Style -> String -> String -> Element msg
newTabLink style url label =
    styledLink True style [] url (text label)


itou : Int -> Float
itou i =
    toFloat i / 255


rgbi : Int -> Int -> Int -> Color
rgbi r g b =
    Element.rgb (itou r) (itou g) (itou b)


colors =
    { turquoise = rgbi 0 0xFF 0xFF
    , green = rgbi 0 0xFF 0
    , lightblue = rgbi 0xAD 0xD8 0xE6
    , blue = Element.rgb 0 0 1
    , darkgray = rgbi 0x40 0x40 0x40
    , gray = rgbi 0x80 0x80 0x80
    , lightgray = rgbi 0xE0 0xE0 0xE0
    , verylightgray = rgbi 0xF4 0xF4 0xF4
    , white = rgbi 0xFF 0xFF 0xFF
    , red = rgbi 0xFF 0 0
    , orange = rgbi 0xFF 0xA5 0
    , black = rgbi 0 0 0
    , darkmodegray = rgbi 0x20 0x21 0x25
    , darkmodeblack = rgbi 0x16 0x17 0x1B
    , darkmodelightgray = rgbi 0x2B 0x2F 0x32
    , darkmodetext = rgbi 0xDD 0xDD 0xDD
    , gabGreen = rgbi 0 0xD1 0x77
    , verifiedBlue = rgbi 0x60 0x7C 0xF5
    }


defaultStyles : Styles
defaultStyles =
    { selected = "Light"
    , styles = [ lightStyle, darkStyle ]
    }


lightStyle : Style
lightStyle =
    { name = "Light"
    , icondir = "light"
    , background = colors.white
    , dialogBackground = colors.verylightgray
    , text = colors.black
    , link = colors.blue
    , linkHover = colors.lightblue
    , border = colors.lightgray
    , headerBackground = colors.verylightgray
    , quotedPostBackground = colors.verylightgray
    , quotedPostBorder = colors.lightgray
    , postcountBackground = colors.lightgray
    , loadingFeed = colors.orange
    }


darkStyle : Style
darkStyle =
    { name = "Dark"
    , icondir = "dark"
    , background = colors.darkmodeblack
    , dialogBackground = colors.darkmodegray
    , text = colors.darkmodetext
    , link = colors.gabGreen
    , linkHover = colors.lightblue
    , border = colors.darkmodegray
    , headerBackground = colors.darkmodegray
    , quotedPostBackground = colors.darkmodelightgray
    , quotedPostBorder = colors.darkmodegray
    , postcountBackground = colors.darkmodeblack
    , loadingFeed = colors.orange
    }


defaultFontSize : Float
defaultFontSize =
    15


defaultColumnWidth : Int
defaultColumnWidth =
    350


defaultSettings : Settings
defaultSettings =
    { columnWidth = defaultColumnWidth
    , fontSize = defaultFontSize
    , here = Time.utc
    , windowWidth = 1260
    , windowHeight = 1024
    , styleOption = LightStyle
    , style = lightStyle
    }


{-| Color highlighting is temporary, until Font.underline becomes decorative.
-}
styledLink : Bool -> Style -> List (Attribute msg) -> String -> Element msg -> Element msg
styledLink newTab style attributes url label =
    (if newTab then
        Element.newTabLink

     else
        link
    )
        (List.concat
            [ [ Font.color style.link
              , Element.mouseOver [ Font.color style.linkHover ]
              ]
            , attributes
            ]
        )
        { url = url
        , label = label
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


circularRadiusAttribute : Attribute msg
circularRadiusAttribute =
    Element.htmlAttribute <|
        Attributes.style "border-radius" "50%"


circularHeightImage : String -> String -> Int -> Element msg
circularHeightImage =
    circularHeightImageWithAttributes []


circularHeightImageWithAttributes : List (Html.Attribute msg) -> String -> String -> Int -> Element msg
circularHeightImageWithAttributes attributes src description h =
    let
        d =
            description
    in
    Html.img
        (List.concat
            [ [ Attributes.height h
              , Attributes.style "border-radius" "50%"
              , Attributes.src src
              , Attributes.alt description
              ]
            , attributes
            ]
        )
        []
        |> Element.html


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


iconUrls : IconUrls
iconUrls =
    { close = "cancel.svg"
    , comment = "chat-2.svg"
    , dislike = "dislike.svg"
    , home = "house.svg"
    , like = "like.svg"
    , logout = "logout.svg"
    , next = "next-1.svg"
    , notification = "hand.svg"
    , notifications = "glasses.svg"
    , popular = "star.svg"
    , previous = "back.svg"
    , refresh = "refresh-arrow.svg"
    , reload = "reload.svg"
    , save = "folder.svg"
    , settings = "settings.svg"
    , user = "avatar.svg"
    , checkmark = "checkmark.svg"
    }


getIconUrl : Style -> (IconUrls -> String) -> String
getIconUrl style accessor =
    "icon/" ++ style.icondir ++ "/" ++ accessor iconUrls
