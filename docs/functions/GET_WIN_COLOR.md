# GET_WIN_COLOR(1)

| | |
|---|---|
| **Category** | Function |
| **Name** | `GET_WIN_COLOR` |
| **Platform** | Windows Only |
| **Returns** | `R` |

## Purpose

This function returns the internal color value for an alpha specifier..

## Parts

| # | Type | Description |
|---|------|-------------|
| 1 | `f/c/e` | The color name. See below for available names. |

## Return Type

R Returns the color value. Will return 0 if no color has been found, even though this is a legitimate color.

## Comments

Colors are siginificantly different in Windows than they are in DOS. To help with this problem we have provided this function. You pass it a color name and it returns the appropriate value. Acceptable color names are: (the following names are standard Windows colors and return values you have set for Windows in general) ScrollBar, Background, ActiveCaption, InactiveCaption, Menu, Window, WindowFrame, MenuText, WindowText, CaptionText, ActiveBorder, InactiveBorder, AppWorkSpace, Highlight, HighlightText, BtnFace, BtnShadow, GrayText, BtnText, InactiveCaptionText, BtnHighlight. These next names are for regular colors: Black, Maroon, Green, Olive, Navy, Purple, Teal, Gray, Silver, Red, Lime, Yellow, Blue, Fuchsia, Aqua, LtGray, DkGray, White. The last are for values you set in the TP5WIN.INI file: NormalBkg, NormalText, BoxBkg, BoxText, EnterBkg, EnterText, MsgBkg, MsgText, WindowBkg, WindowTextSet, MenuBkg, MenuTextSet, ButtonBkg, ButtonText, ChoiceBkg, ChoiceText, EcolorBkg, EcolorText.
