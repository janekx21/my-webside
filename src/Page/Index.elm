module Page.Index exposing (Data, Model, Msg, data, head, page, view)

import DataSource exposing (DataSource)
import DataSource.Http
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Head
import Head.Seo as Seo
import Html.Attributes
import Material.Icons as Icons
import Material.Icons.Types exposing (Icon)
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Secrets as Secrets
import Pages.Url
import QueryParams
import Shared
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (d, points)
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


type alias Data =
    { aboutMe : String
    , projects : List Repository
    , games : List Game
    }


type alias Game =
    { name : String
    , shortText : String
    , thumbnailUrl : Maybe String
    , downloadsCount : Int
    , url : String
    , published : Bool
    }


type alias Repository =
    { name : String
    , fork : Bool
    , url : String
    , language : Maybe String
    , size : Int
    , description : Maybe String
    }


type Language
    = English
    | German


type TranslationKey
    = AboutMe
    | Projects
    | Games
    | ContactMe
    | ContactMeDetail


translate : TranslationKey -> Language -> String
translate translationKey language =
    case language of
        English ->
            case translationKey of
                AboutMe ->
                    "About Me"

                Projects ->
                    "Projects"

                Games ->
                    "Games"

                ContactMe ->
                    "Contact Me"

                ContactMeDetail ->
                    "Feel free so send me a message, I'll try to respond as quick as I can! I'm also on this social media thing, if you need me there:"

        German ->
            case translationKey of
                AboutMe ->
                    "Über mich"

                Projects ->
                    "Projekte"

                Games ->
                    "Spiele"

                ContactMe ->
                    "Kontakt"

                ContactMeDetail ->
                    "Sie können mir ruhig schreiben, ich werde so schnell antworten wie möglich! Ich bin auch auf den sozialen Medien Ding, wenn sie mach da brauchen:"


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


data : DataSource Data
data =
    DataSource.map3
        (\bio repoList gamesList ->
            { aboutMe = bio
            , projects = repoList
            , games = gamesList
            }
        )
        bioData
        repoData
        gamesData


bioData : DataSource String
bioData =
    DataSource.Http.request
        (Secrets.succeed
            (\githubTokenBase64 ->
                { url = "https://api.github.com/users/janekx21"
                , method = "GET"
                , body = DataSource.Http.emptyBody
                , headers = [ ( "Authorization", "Basic " ++ githubTokenBase64 ) ]
                }
            )
            |> Secrets.with "GITHUB_TOKEN_BASE64"
        )
        (Decode.field "bio" Decode.string)


gamesData : DataSource (List Game)
gamesData =
    DataSource.Http.request
        (Secrets.succeed
            (\apiKey ->
                { url = "https://itch.io/api/1/" ++ apiKey ++ "/my-games"
                , method = "GET"
                , body = DataSource.Http.emptyBody
                , headers = []
                }
            )
            |> Secrets.with "API_KEY"
        )
        itchGamesDecoder


itchGamesDecoder : Decoder (List Game)
itchGamesDecoder =
    Decode.field "games"
        (Decode.list
            (Decode.map6 Game
                (Decode.field "title" Decode.string)
                (Decode.optionalField "short_text" Decode.string |> Decode.map (Maybe.withDefault ""))
                (Decode.optionalField "cover_url" Decode.string)
                (Decode.field "downloads_count" Decode.int)
                (Decode.field "url" Decode.string)
                (Decode.field "published" Decode.bool)
            )
        )


repoData : DataSource (List Repository)
repoData =
    DataSource.Http.request
        (Secrets.succeed
            (\githubTokenBase64 ->
                { url = "https://api.github.com/users/janekx21/repos"
                , method = "GET"
                , body = DataSource.Http.emptyBody
                , headers = [ ( "Authorization", "Basic " ++ githubTokenBase64 ) ]
                }
            )
            |> Secrets.with "GITHUB_TOKEN_BASE64"
        )
        reposDecoder


reposDecoder : Decoder (List Repository)
reposDecoder =
    Decode.list
        (Decode.map6 Repository
            (Decode.field "name" Decode.string)
            (Decode.field "fork" Decode.bool)
            (Decode.field "html_url" Decode.string)
            (Decode.field "language" <| Decode.nullable Decode.string)
            (Decode.field "size" <| Decode.int)
            (Decode.field "description" <| Decode.nullable Decode.string)
        )


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    let
        lang =
            German

        -- todo
    in
    { title = "Janek Winkler"
    , body =
        el [ width fill, inFront overlay, scrollbarY ] <|
            column
                [ width fill
                , Font.color white
                , Font.size 18
                , Background.color black
                , inFront <| el [ width fill ] <| el [ alignRight, padding 32 ] <| materialIcon Icons.translate 24 white -- todo make functional
                ]
                [ video
                , column [ paddingXY 64 0 ]
                    -- todo padding nur bei desktop
                    [ aboutMe lang static.data.aboutMe
                    , projects lang static.data.projects
                    , games lang static.data.games
                    , contactMe lang
                    ]
                ]
    }


queryLanguage : Maybe PageUrl -> Language
queryLanguage maybePageUrl =
    maybePageUrl |> Maybe.andThen .query |> Maybe.andThen tryParse |> Maybe.withDefault English


tryParse : QueryParams.QueryParams -> Maybe Language
tryParse queryParams =
    queryParams
        |> QueryParams.parse
            (QueryParams.string "lang")
        |> Result.toMaybe
        |> Maybe.andThen parseLanguage


parseLanguage string =
    case string of
        "en" ->
            Just English

        "de" ->
            Just German

        _ ->
            Nothing


overlay =
    el
        [ Background.gradient
            { angle = degrees 45 + 90
            , steps =
                [ rgba255 255 151 72 0.2
                , rgba255 113 217 255 0.2
                ]
            }
        , style "filter" "blur(50px)"
        , style "pointer-events" "none"
        , style "mix-blend-mode" "hard-light"
        , style "position" "fixed"
        , style "width" "100vw"
        , style "height" "100vh"
        ]
    <|
        none


style key value =
    htmlAttribute <| Html.Attributes.style key value


video =
    el
        [ width fill
        , height (px 400)
        , Background.color gray
        ]
    <|
        el [ centerX, centerY ] <|
            text "video placeholder"


aboutMe lang content =
    column [ width fill, padding 32, spacing 32 ]
        [ title <| translate AboutMe lang
        , paragraph [ Font.center ] [ text content ]
        ]


title content =
    row [ width fill, spacing 16 ]
        [ el [ width fill, height (px 2), Background.gradient { angle = degrees -90, steps = [ primary, gone ] } ] <| none
        , el [ centerX, Font.size 32 ] <| text content
        , el [ width fill, height (px 2), Background.gradient { angle = degrees 90, steps = [ primary, gone ] } ] <| none
        ]


projects lang projectData =
    column [ width fill, padding 32, spacing 32 ]
        [ title <| translate Projects lang
        , wrappedRow [ spacing 64 ]
            (projectData
                |> List.filter (not << .fork)
                |> List.sortBy .size
                |> List.reverse
                |> List.map (\p -> el [ width fill ] <| repoCard p)
            )
        ]


repoCard : Repository -> Element msg
repoCard { name, url, language, description } =
    let
        -- todo add more
        pills =
            language |> Maybe.map List.singleton |> Maybe.withDefault []
    in
    column [ spacing 8, centerX, width (px 300) ]
        [ newTabLink [ mouseOver [ Font.color primary ] ] { label = paragraph [ centerX, Font.size 24, Font.bold ] [ text name ], url = url }
        , wrappedRow [ spacing 8 ]
            (pills
                |> List.map (pill secondary)
            )
        , description |> Maybe.map (\d -> paragraph [ paddingXY 16 0 ] [ text d ]) |> Maybe.withDefault none
        ]


games lang gameData =
    column [ width fill, padding 32, spacing 32 ]
        [ title <| translate Games lang
        , wrappedRow [ spacing 100, width fill ]
            (gameData
                |> List.filter .published
                |> List.sortBy .downloadsCount
                |> List.reverse
                |> List.map (\p -> el [ width fill ] <| gameCard p)
            )
        ]


gameCard : Game -> Element msg
gameCard { name, shortText, thumbnailUrl, url } =
    let
        thumbnail =
            case thumbnailUrl of
                Nothing ->
                    el [ width fill, height (px 240), Background.color gray ] <|
                        el [ centerX, centerY ] <|
                            text "image placeholder"

                Just url_ ->
                    el [ width fill, height (px 240), Background.image url_ ] <| none

        s =
            case String.split "|" shortText of
                description :: rest ->
                    ( description, rest )

                -- unreachable
                _ ->
                    ( "", [] )
    in
    column [ spacing 8, centerX, width (px 300) ]
        [ thumbnail
        , newTabLink [ mouseOver [ Font.color primary ] ] { label = paragraph [ centerX, Font.size 24, Font.bold ] [ text name ], url = url }
        , wrappedRow [ spacing 8 ]
            (Tuple.second s
                |> List.map (pill secondary)
            )
        , paragraph [ paddingXY 16 0 ] [ text <| Tuple.first s ]
        ]


pill color content =
    el [ paddingXY 8 4, Background.color color, Border.rounded 99 ] <| text content


contactMe lang =
    column [ width fill, padding 32, spacing 32 ]
        [ title <| translate ContactMe lang
        , paragraph [ Font.center ]
            [ text <| translate ContactMeDetail lang
            ]
        , row [ spacing 16, centerX ]
            [ newTabLink [] { url = "mailto:jane[delete this spam salt]kx21@gmail.com", label = mail 24 white }
            , newTabLink [] { url = "https://github.com/janekx21", label = github 24 white }
            , newTabLink [] { url = "https://twitter.com/janekx21", label = twitter 24 blue }
            , newTabLink [] { url = "https://www.youtube.com/channel/UCr1xaocoW6Z17HQehEvJoEg", label = youtube 24 red }
            ]
        ]


materialIcon : Icon msg -> Int -> Color -> Element msg
materialIcon icon size color =
    el [ Font.color color ] <| Element.html <| icon size Material.Icons.Types.Inherit


featherIcon : List (Svg msg) -> Int -> Color -> Element msg
featherIcon svg size color =
    el [ Font.color color ] <|
        html <|
            Svg.svg
                [ Attr.fill "none"
                , Attr.width (String.fromInt size)
                , Attr.height (String.fromInt size)
                , Attr.stroke "currentColor"
                , Attr.strokeLinecap "round"
                , Attr.strokeLinejoin "round"
                , Attr.strokeWidth "2"
                , Attr.viewBox "0 0 24 24"
                ]
                svg


mail =
    featherIcon
        [ Svg.path [ d "M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1 0-2-.9-2-2V6c0-1.1.9-2 2-2z" ] []
        , Svg.polyline [ points "22,6 12,13 2,6" ] []
        ]


twitter =
    featherIcon
        [ Svg.path [ d "M23 3a10.9 10.9 0 0 1-3.14 1.53 4.48 4.48 0 0 0-7.86 3v1A10.66 10.66 0 0 1 3 4s-4 9 5 13a11.64 11.64 0 0 1-7 2c9 5 20 0 20-11.5a4.5 4.5 0 0 0-.08-.83A7.72 7.72 0 0 0 23 3z" ] []
        ]


youtube =
    featherIcon
        [ Svg.path [ d "M22.54 6.42a2.78 2.78 0 0 0-1.94-2C18.88 4 12 4 12 4s-6.88 0-8.6.46a2.78 2.78 0 0 0-1.94 2A29 29 0 0 0 1 11.75a29 29 0 0 0 .46 5.33A2.78 2.78 0 0 0 3.4 19c1.72.46 8.6.46 8.6.46s6.88 0 8.6-.46a2.78 2.78 0 0 0 1.94-2 29 29 0 0 0 .46-5.25 29 29 0 0 0-.46-5.33z" ] []
        , Svg.polygon [ points "9.75 15.02 15.5 11.75 9.75 8.48 9.75 15.02" ] []
        ]


github =
    featherIcon
        [ Svg.path [ d "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22" ] []
        ]


white =
    rgb255 255 255 255


gray =
    rgb255 80 80 80


green =
    rgb255 121 152 62


blue =
    rgb255 66 123 201


red =
    rgb255 211 59 39


orange =
    rgb255 211 151 39


black =
    rgb255 0 0 0


primary =
    rgb255 242 46 98


secondary =
    rgb255 140 3 117


gone =
    rgba 0 0 0 0
