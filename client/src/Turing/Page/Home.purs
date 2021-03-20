-- | The Turing homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module Turing.Page.Home where

import Prelude

import Component.HigherOrder.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Turing.Api.Endpoint (ArticleParams, Pagination, noArticleParams)
import Turing.Capability.Navigate (class Navigate)
import Turing.Capability.Resource.Article (class ManageArticle, getArticles, getCurrentUserFeed)
import Turing.Capability.Resource.Tag (class ManageTag, getAllTags)
import Turing.Component.HTML.ArticleList (articleList, renderPagination)
import Turing.Component.HTML.Footer (footer)
import Turing.Component.HTML.Header (header)
import Turing.Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Turing.Component.Part.FavoriteButton (favorite, unfavorite)
import Turing.Data.Article (ArticleWithMetadata)
import Turing.Data.PaginatedArray (PaginatedArray)
import Turing.Data.Profile (Profile)
import Turing.Data.Route (Route(..))
import Turing.Env (UserEnv)
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
    = Initialize
    | Receive { currentUser :: Maybe Profile }
    | ShowTab Tab
    | LoadFeed Pagination
    | LoadArticles ArticleParams
    | LoadTags
    | FavoriteArticle Int
    | UnfavoriteArticle Int
    | SelectPage Int MouseEvent

type State =
    { tags :: RemoteData String (Array String)
    , articles :: RemoteData String (PaginatedArray ArticleWithMetadata)
    , tab :: Tab
    , page :: Int
    , currentUser :: Maybe Profile
    }

data Tab
    = Feed
    | Global
    | Tag String

derive instance eqTab :: Eq Tab

tabIsTag :: Tab -> Boolean
tabIsTag (Tag _) = true
tabIsTag _ = false

component
    :: forall q o m r
     . MonadAff m
    => MonadAsk { userEnv :: UserEnv | r } m
    => Navigate m
    => ManageTag m
    => ManageArticle m
    => H.Component HH.HTML q {} o m
component = Connect.component $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            , initialize = Just Initialize
            }
    }
    where
    initialState { currentUser } =
        { tags: NotAsked
        , articles: NotAsked
        , tab: Global
        , currentUser
        , page: 1
        }

    handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
    handleAction = case _ of
        Initialize -> do
            void $ H.fork $ handleAction LoadTags
            state <- H.get
            case state.currentUser of
                Nothing ->
                    void $ H.fork $ handleAction $ LoadArticles noArticleParams
                profile -> do
                    void $ H.fork $ handleAction $ LoadFeed { limit: Just 20, offset: Nothing }
                    H.modify_ _ { tab = Feed }

        Receive { currentUser } ->
            H.modify_ _ { currentUser = currentUser }

        LoadTags -> do
            H.modify_ _ { tags = Loading}
            tags <- getAllTags
            H.modify_ _ { tags = fromMaybe tags }

        LoadFeed params -> do
            st <- H.modify _ { articles = Loading }
            articles <- getCurrentUserFeed params
            H.modify_ _ { articles = fromMaybe articles }

        LoadArticles params -> do
            H.modify_ _ { articles = Loading }
            articles <- getArticles params
            H.modify_ _ { articles = fromMaybe articles }

        ShowTab thisTab -> do
            st <- H.get
            when (thisTab /= st.tab) do
                H.modify_ _ { tab = thisTab }
                void $ H.fork $ handleAction case thisTab of
                    Feed ->
                        LoadFeed { limit: Just 20, offset: Nothing }
                    Global ->
                        LoadArticles (noArticleParams { limit = Just 20 })
                    Tag tag ->
                        LoadArticles (noArticleParams { tag = Just tag, limit = Just 20 })

        FavoriteArticle index ->
            favorite (_article index)

        UnfavoriteArticle index ->
            unfavorite (_article index)

        SelectPage index event -> do
            H.liftEffect $ preventDefault $ toEvent event
            st <- H.modify _ { page = index }
            let offset = Just (index * 20)
            void $ H.fork $ handleAction case st.tab of
                Feed ->
                    LoadFeed { limit: Just 20, offset }
                Global ->
                    LoadArticles (noArticleParams { limit = Just 20, offset = offset })
                Tag tag ->
                    LoadArticles (noArticleParams { tag = Just tag, limit = Just 20, offset = offset })

    render :: forall slots. State -> H.ComponentHTML Action slots m
    render state@{ tags, articles, currentUser } =
        HH.div_
            [ header currentUser Home
            , HH.div
                [ css "main" ]
                [ HH.div
                    [ css "title" ]
                    [ HH.text "Turing" ]
                , HH.div
                    [ css "buttons" ]
                    [ HH.a
                        [ css "button"
                        , safeHref Settings
                        ]
                        [ HH.text "Settings" ]
                    , HH.a
                        [ css "button" ]
                        [ HH.text "Credits" ]
                    , HH.a
                        [ css "button" ]
                        [ HH.text "Documentation" ]
                    ]
                ]
            ]

_article :: Int -> Traversal' State ArticleWithMetadata
_article i =
    prop (SProxy :: SProxy "articles")
        <<< _Success
        <<< prop (SProxy :: SProxy "body")
        <<< ix i
