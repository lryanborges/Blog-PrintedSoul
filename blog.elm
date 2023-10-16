module Blog exposing (..)

import Browser
import Html exposing (Html, div, h1, p, span, text, input)
import Html.Attributes exposing (placeholder, class, value, style)
import Html.Events exposing (onClick, onInput)
import List exposing (filter, member)
import Maybe exposing (withDefault)

-- Model
type alias Article =
    { id : Int
    , title : String
    , content : String
    , isFavorited : Bool
    }

type alias Model =
    { articles : List Article
    , newArticleTitle : String
    , newArticleContent : String
    , showFavorites : Bool
    , newArticleError : Maybe String
    , nextId : Int
    }

init : Model
init =
    { articles =
        [ { id = 1, title = "😀😀😀😀", content = "prontos para arrasar no trabalho", isFavorited = False }
        , { id = 2, title = "olá eu sou um título", content = "e eu o conteúdo", isFavorited = False }
        , { id = 3, title = "outro títuloo", content = "e a ideia desse conteúdo é realmente fazer uma quebra de linha", isFavorited = False }
        ]
    , newArticleTitle = ""
    , newArticleContent = ""
    , showFavorites = False
    , newArticleError = Nothing
    , nextId = 4
    }

-- Msg
type Msg
    = NoOp
    | UpdateNewArticleTitle String
    | UpdateNewArticleContent String
    | AddNewArticle
    | ToggleFavorite Article
    | ToggleShowFavorites
    | DeleteArticle Article

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
            if String.isEmpty model.newArticleTitle || String.isEmpty model.newArticleContent then
                { model | newArticleError = Just "Título e conteúdo não podem estar vazios" }
            else
                let
                    newArticle =
                        { id = model.nextId
                        , title = model.newArticleTitle
                        , content = model.newArticleContent
                        , isFavorited = False
                        }

                    updatedArticles =
                        newArticle :: model.articles
                in
                { model | articles = updatedArticles, newArticleTitle = "", newArticleContent = "", newArticleError = Nothing, nextId = model.nextId + 1 }

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

        DeleteArticle article ->
            { model | articles = List.filter (\a -> a.id /= article.id) model.articles }

-- View
viewArticle : Article -> Html Msg
viewArticle article =
    div [ class "article" ]
        [ h1 [ class "article-title" ]
            [ text article.title ]
        , p [] [ text article.content ]
        , div [ class "favorite-icon" ]
            [ if article.isFavorited then
                span [ class "favorite", onClick (ToggleFavorite article) ] [ text "❤️" ]
            else
                span [ class "favorite", onClick (ToggleFavorite article) ] [ text "🖤" ]
            ]
        , div [ class "edit-buttons" ]
            [ span [ class "edit-button", onClick (DeleteArticle article) ] [ text "Excluir" ]
            ]
        ]

view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [ class "header-title" ] [ text "Blog Printed Soul" ]
            ]
        , div [ class "container" ]
            [ div [ class "form-container" ]
                [ Html.input [ placeholder "Título", class "input-field", value model.newArticleTitle, onInput UpdateNewArticleTitle ] []
                , Html.input [ placeholder "Conteúdo", class "input-field", value model.newArticleContent, onInput UpdateNewArticleContent ] []
                , case model.newArticleError of
                    Just errorMsg ->
                        div [ class "error-message" ] [ text errorMsg ]
                    Nothing ->
                        text ""
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
