module Blog exposing (..)

import Browser
import Html exposing (Html, div, h1, p, span, text)
import Html.Attributes exposing (placeholder, class)
import Html.Events exposing (onClick, onInput)
import Html

-- <link rel="stylesheet" type="text/css" href="style.css">

-- Model
type alias Article =
    { title : String
    , content : String
    , isFavorited : Bool
    }

type alias Model =
    { articles : List Article
    , newArticleTitle : String
    , newArticleContent : String
    , showFavorites : Bool
    }

init : Model
init =
    { articles =
        [ { title = "🫡🫡🫡🫡", content = "prontos para arrasar no trabalho", isFavorited = False }
        , { title = "olá eu sou um título", content = "e eu o conteúdo", isFavorited = False }
        , { title = "outro títuloo", content = "e a ideia desse conteúdo é realmente fazer uma quebra de linha", isFavorited = False }
        ]
    , newArticleTitle = ""
    , newArticleContent = ""
    , showFavorites = False
    }

-- Msg
type Msg
    = NoOp
    | UpdateNewArticleTitle String
    | UpdateNewArticleContent String
    | AddNewArticle
    | ToggleFavorite Article
    | ToggleShowFavorites

-- Update
update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        UpdateNewArticleTitle newTitle ->
            { model | newArticleTitle = newTitle }

        UpdateNewArticleContent newContent ->
            { model | newArticleContent = newContent }

        AddNewArticle ->
            let
                newArticle =
                    { title = model.newArticleTitle
                    , content = model.newArticleContent
                    , isFavorited = False
                    }

                updatedArticles =
                    newArticle :: model.articles
            in
            { model | articles = updatedArticles, newArticleTitle = "", newArticleContent = "" }

        ToggleFavorite article ->
            let
                updatedArticles =
                    List.map
                        (\art ->
                            if art == article then
                                { art | isFavorited = not art.isFavorited }
                            else
                                art
                        )
                        model.articles
            in
            { model | articles = updatedArticles }

        ToggleShowFavorites ->
            { model | showFavorites = not model.showFavorites }

-- View
viewArticle : Article -> Html Msg
viewArticle article =
    div [ class "article" ]
        [ div [ class "article-header" ]
            [ h1 [ class "article-title" ] [ text article.title ]
            , div [ class "favorite-icon" ]
                [ if article.isFavorited then
                    span [ class "favorite", onClick (ToggleFavorite article) ] [ text "❤️" ]
                  else
                    span [ class "favorite", onClick (ToggleFavorite article) ] [ text "🖤" ]
                ]
            ]
        , p [] [ text article.content ]
        ]

view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            [ div [ class "form-container" ]
                [ Html.input [ placeholder "Título", class "input-field", Html.Attributes.value model.newArticleTitle, Html.Events.onInput UpdateNewArticleTitle ] []
                , Html.input [ placeholder "Conteúdo", class "input-field", Html.Attributes.value model.newArticleContent, Html.Events.onInput UpdateNewArticleContent ] []
                ]
            , div [ class "button-container" ]
                [ Html.button [ class "add-button", onClick AddNewArticle ] [ text "Publicar" ]
                ]
            , div [ class "article-container" ]
                (List.map (\article -> if model.showFavorites && not article.isFavorited then Html.text "" else viewArticle article) model.articles)
            ]
        , div [ class "toggle-favorites-button-container" ]
            [ Html.button [ class "toggle-favorites-button", onClick ToggleShowFavorites ]
                [ text (if model.showFavorites then "Mostrar Todos" else "Meus Favoritos") ]
            ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
