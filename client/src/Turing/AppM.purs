-- | A custom application monad that provides concrete implementations for capabilities like
-- | logging, navigation, and resource management. This is our production monad -- it interprets
-- | our capabilities as they are meant to run on our production site.
-- |
-- | However, since capabilities like logging are implemented as type classes, we can also provide
-- | one or more test monads that provide different interpretations.
-- |
-- | For example, this monad will actually hit the server with API requests when we manage a
-- | resource, but our test monad might just return mock JSON or error responses.
-- |
-- | See the various `Turing.Capability.*` modules for deeper explanations of each capability, and
-- | the accompanying guide for a thorough introduction to this style of application architecture.
-- |
-- | https://thomashoneyman.com/guides/real-world-halogen
module Turing.AppM where

import Prelude

import Turing.Api.Endpoint (Endpoint(..), noArticleParams)
import Turing.Api.Request (RequestMethod(..))
import Turing.Api.Request as Request
import Turing.Api.Utils (authenticate, decode, decodeWithUser, mkAuthRequest, mkRequest)
import Turing.Capability.LogMessages (class LogMessages)
import Turing.Capability.Navigate (class Navigate, navigate)
import Turing.Capability.Now (class Now)
import Turing.Capability.Resource.Article (class ManageArticle)
import Turing.Capability.Resource.Comment (class ManageComment)
import Turing.Capability.Resource.Tag (class ManageTag)
import Turing.Capability.Resource.Spec (class ManageSpec)
import Turing.Capability.Resource.User (class ManageUser, class ManageUser2)
import Turing.Data.Article as Article
import Turing.Data.Comment as Comment
import Turing.Data.Log as Log
import Turing.Data.Profile as Profile
import Turing.Data.Route as Route
import Turing.Env (Env, LogLevel(..))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)

import Effect.Console as Console
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem)
import Turing.Capability.Firebase
import Control.Promise (toAff)
import Effect.Aff
import Data.Time.Duration
import Data.Either (Either(..), hush)

-- | In the capability modules (`Turing.Capability.*`), we wrote some abstract, high-level
-- | interfaces for business logic that tends to be highly effectful, like resource management and
-- | logging. We wrote interfaces (just the types, no actual implementation) so that we could write
-- | the same code once and swap in different implementations as we see fit.
-- |
-- | This carries two main benefits. First, it helps abstract away the implementation so we can
-- | focus on logic: businesses care about reading, writing, and deleting resources, not whether
-- | that's done on the file system, over RPC, a REST API, or something else. Second, it lets us
-- | write code once and swap in different implementations under the hood. That means we can run
-- | the same section of code in production using one implementation and as part of a test suite
-- | using another. For example, our production code might use a REST API, but our test code might
-- | just provide mock JSON responses.
-- |

-- | This module implements a monad that can run all the abstract capabilities we've defined. This
-- | is our production monad. We'll implement the monad first, and then we'll provide concrete
-- | instances for each of our abstract capabilities.
-- |
-- | Our application monad is going to combine the abilities of the `Aff` (asynchronous effects)
-- | and `Reader` (read-only environment) monads, and then we'll add several more abilities by
-- | writing instances for our various capabilities.
-- |
-- | The `Reader` monad allows us to access some data (`Env`, in our case, as defined in `Env`) using
-- | the `ask` function, without having passed the data as an argument. It will help us avoid
-- | tediously threading commonly-used information throughout the application.
-- |
-- | The `Aff` monad allows us to run asynchronous effects. When you're in `Aff`, you can write
-- | code that makes API requests, writes files, and so on. In addition, using the `liftEffect`
-- | function, you can use any function that relies on the `Effect` monad in `Aff`. For example,
-- | you can log messages to the console within `Aff` using (liftEffect <<< Console.log). It's
-- | powerful stuff.
-- |
-- | `AppM` combines the `Aff` and `Reader` monads under a new type, which we can now use to write
-- | instances for our capabilities. We're able to combine these monads because `ReaderT` is a
-- | monad transformer. Monad transformers are too large a topic to delve into here; for now, it's
-- | enough to know that they let you combine the abilities of two or more monads.
newtype AppM a = AppM (ReaderT Env Aff a)

-- | Fantastic! We can now implement our custom application monad with our `AppM` type. However,
-- | as described in detail in the `Main` module, Halogen has no idea how to run an application in
-- | our custom monad. It only knows how to run things in `Aff`. For that reason, we need to be able
-- | to transform our custom app monad back into `Aff` when we run the app.
-- |
-- | We can get back to `Aff` by taking two steps. First, we'll unwrap the `AppM` type so that we
-- | can work with `ReaderT` directly. Next, we'll use `runReaderT` along with the environment we
-- | want to supply throughout the application to eliminate `Reader` altogether and get left with
-- | only `Aff`.
-- |
-- | See `Main` for more details as to why this is necessary.
runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

-- | We can get a monad out of our `AppM` type essentially for free by deferring to the underlying
-- | `ReaderT` instances. PureScript allows any newtype to re-use the type class instances of the
-- | type it wraps with the `derive newtype instance` syntax. It's as if the newtype didn't exist
-- | and the function was being applied to the type underneath directly.
-- |
-- | To be a monad, a type must implement the `Functor`, `Apply`, `Applicative`, and `Bind` type
-- | classes. In addition, because we used `Aff` as the base of our custom monad, we can also
-- | derive `MonadEffect` and `MonadAff`, two type classes that let us use any functions that run
-- | in `Effect` or in `Aff`. Having access to these two type classes lets us perform pretty much
-- | any effect we see fit, from API requests to local storage access.
-- |
-- | With the compiler by your side, you don't need to know how to implement a monad from scratch.
-- | You can derive everything you need! We can now focus just on the instances that matter to us:
-- | our app environment and our capabilities.
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

-- | The first instance we'll implement is a little funky. We can't write instances for type
-- | synonyms, and we defined our environment (`Env`) as a type synonym for convenience. To get
-- | around this, we can use `TypeEquals` to assert that types `a` and `b` are in fact the same.
-- |
-- | In our case, we'll write a `MonadAsk` (an alternate name for `Reader`) instance for the type
-- | `e`, and assert it is our `Env` type. This is how we can write a type class instance for a
-- | type synonym, which is otherwise disallowed.
-- |
-- | With this instance, any monad `m` with the `MonadAsk Env m` constraint can read from the
-- | environment we defined. This is done with the `ask` function. For example:
-- |
-- | ```purescript
-- | toggleLogLevel :: forall m. MonadAsk Env m => m LogLevel
-- | toggleLogLevel = do
-- |    env <- ask
-- |    if env.logLevel == Dev then Prod else Dev
-- | ```
instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
    ask = AppM $ asks from

-- | `ParAppM` allows our `AppM` to be parallelizable in conjunction with `Aff`.
newtype ParAppM a
    = ParAppM (ReaderT Env ParAff a)

derive newtype instance functorParAppM :: Functor ParAppM
derive newtype instance applyParAppM :: Apply ParAppM
derive newtype instance applicativeParAppM :: Applicative ParAppM

instance parallelAppM :: Parallel ParAppM AppM where
    parallel (AppM readerT) = ParAppM (parallel readerT)
    sequential (ParAppM readerT) = AppM (sequential readerT)

-- | We're finally ready to write concrete implementations for each of our abstract capabilities.
-- | For an in-depth description of each capability, please refer to the relevant `Capability.*`
-- | module for the capability that interests you.

-- | First up: the `Now` capability, which allows us to retrieve the current time. In our
-- | production monad, we'll rely on existing functions that run in the `Effect` (synchronous
-- | effects) monad and use the `liftEffect` function so that they run in `AppM` instead. This is
-- | made possible because our `AppM` type implements the `MonadEffect` type class.
-- |
-- | ```purescript
-- | liftEffect :: forall a m. MonadEffect m => Effect a -> m a
-- | ```
-- |
-- | In our test monad, we won't perform effects -- we'll just return a hard-coded time so that we
-- | can ensure tests are reproducible.
instance nowAppM :: Now AppM where
    now = liftEffect Now.now
    nowDate = liftEffect Now.nowDate
    nowTime = liftEffect Now.nowTime
    nowDateTime = liftEffect Now.nowDateTime

-- | Next up: logging. Ideally we'd use a logging service, but for the time being, we'll just log
-- | to the console. We'll rely on our global environment to decide whether to log all messages
-- | (`Dev`) or just important messages (`Prod`).
instance logMessagesAppM :: LogMessages AppM where
    logMessage log = do
        env <- ask
        liftEffect case env.logLevel, Log.reason log of
            Prod, Log.Debug -> pure unit
            _, _ -> Console.log $ Log.message log

-- | Our app uses hash-based routing, so to navigate from place to place, we'll just set the hash.
-- | Note how our navigation capability uses our routing data type rather than let you set any
-- | arbitrary hash. Logging out is a little more involved, because we need to clean up things like
-- | the auth token. Navigating home will take care of emptying the reference to the current user.
instance navigateAppM :: Navigate AppM where
    navigate =
        liftEffect <<< setHash <<< print Route.routeCodec

    logout = do
        { currentUser, userBus } <- asks _.userEnv
        liftEffect do
            Ref.write Nothing currentUser
            Request.removeToken
        liftAff do
            Bus.write Nothing userBus
        navigate Route.Home

-- | Our first resource class describes what operations we have available to manage users. Logging
-- | in and registration require manipulating a token, but we've designed the `Token` type so its
-- | contents can't be read by any function outside the `Api.Request` module. For that reason,
-- | the `login` and `register` implementations are directly imported. The others use our nicer
-- | `mkRequest` and `mkAuthRequest` helpers.
instance manageUserAppM :: ManageUser AppM where
    loginUser =
        authenticate Request.login

    registerUser =
        authenticate Request.register

    getCurrentUser = do
        mbJson <- mkAuthRequest { endpoint: User, method: Get }
        map (map _.user)
            $ decode (CAR.object "User" { user: Profile.profileWithEmailCodec }) mbJson

    getAuthor username = do
        mbJson <- mkRequest { endpoint: Profiles username, method: Get }
        map (map _.profile)
            $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

    updateUser fields =
        void $ mkAuthRequest
            { endpoint: User
            , method: Put (Just (Codec.encode Profile.profileWithEmailPasswordCodec fields))
            }

    followUser username = do
        mbJson <- mkAuthRequest { endpoint: Follow username, method: Post Nothing }
        map (map _.profile)
            $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

    unfollowUser username = do
        mbJson <- mkAuthRequest { endpoint: Follow username, method: Delete }
        map (map _.profile)
            $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

-- | Our operations for managing tags
instance manageTagAppM :: ManageTag AppM where
    getAllTags = do
        mbJson <- mkRequest { endpoint: Tags, method: Get }
        map (map _.tags) $ decode (CAR.object "Tags" { tags: CA.array CA.string }) mbJson

-- | Our operations for managing comments
instance manageCommentAppM :: ManageComment AppM where
    getComments slug = do
        mbJson <- mkRequest { endpoint: Comments slug, method: Get }
        map (map _.comments)
            $ decodeWithUser (\u -> CAR.object "Comments" { comments: CA.array (Comment.codec u) }) mbJson

    createComment slug body =
        let method = Post $ Just $ Codec.encode (CAR.object "CommentBody" { body: CA.string }) { body }
         in void $ mkAuthRequest { endpoint: Comments slug, method }

    deleteComment slug id =
        void $ mkAuthRequest { endpoint: Comment slug id, method: Delete }

-- | Our operations for managing articles
instance manageArticleAppM :: ManageArticle AppM where
    getArticle slug = do
        mbJson <- mkRequest { endpoint: Article slug, method: Get }
        map (map _.article)
            $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

    getArticles fields =
        mkRequest { endpoint: Articles fields, method: Get }
            >>= decodeWithUser Article.articlesWithMetadataCodec

    createArticle article = do
        let
            codec = CAR.object "Article" { article: Article.articleCodec }
            method = Post $ Just $ Codec.encode codec { article }

        mbJson <- mkAuthRequest { endpoint: Articles noArticleParams, method }
        map (map _.article)
            $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

    updateArticle slug article = do
        let
            codec = CAR.object "Article" { article: Article.articleCodec }
            method = Put $ Just $ Codec.encode codec { article }

        mbJson <- mkAuthRequest { endpoint: Article slug, method }
        map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

    deleteArticle slug =
        void $ mkAuthRequest { endpoint: Article slug, method: Delete }

    favoriteArticle slug = do
        mbJson <- mkAuthRequest { endpoint: Favorite slug, method: Post Nothing }
        map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

    unfavoriteArticle slug = do
        mbJson <- mkAuthRequest { endpoint: Favorite slug, method: Delete }
        map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

    getCurrentUserFeed params =
        mkAuthRequest { endpoint: Feed params, method: Get }
            >>= decodeWithUser Article.articlesWithMetadataCodec

-- | Our operations for managing articles
instance manageSpecAppM :: ManageSpec AppM where
    createSpec spec = liftEffect $ launchAff_ do
        userCredential <- signInAnonymously =<< liftEffect auth
        liftEffect $ Console.log (show userCredential)

instance manageUser2AppM :: ManageUser2 AppM where
    getCurrentUser2 = do
        userCredential <- liftAff $ signInAnonymously =<< liftEffect auth
        pure $ hush $ userCredential
