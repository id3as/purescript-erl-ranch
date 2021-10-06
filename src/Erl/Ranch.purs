module Erl.Ranch
  ( startListener
  , startListenerPassive
  , stopListener
  , Options
  , TransportConfig(..)
  , OptionToMaybe
  , ListenerRef
  , HandlerResult(..)
  , HandlerFn
  , PassiveHandlerFn
  , GenericHandlerFn
  --, defaultHandler
  , start_link
  , class OptionsToErl
  , optionsToErl
  , makeTerms
  , class OptionToErl
  , optionToErl
  , ExcludedOptions
  , excludeOptions
  , defaultOptions
  ) where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn3, mkEffectFn3)
import Erl.Atom (Atom, atom)
import Erl.Atom.Symbol (toAtom)
import Erl.Atom.Symbol as AtomSymbol
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Kernel.Inet (ActiveSocket, CanGenerateMessages, CannotGenerateMessages, SocketActive, SocketDeliver)
import Erl.Kernel.Inet as Inet
import Erl.Kernel.Tcp (SocketPacket, TcpMessage)
import Erl.Kernel.Tcp as Tcp
import Erl.Otp.Types.Stdlib (ChildType)
import Erl.Process (class ReceivesMessage)
import Erl.Process.Raw (Pid)
import Erl.Ranch.Transport (fromModule, Socket)
import Erl.Ssl as Ssl
import Erl.Types (class ToErl, IntOrInfinity, NonNegInt, PosInt, Timeout, toErl)
import Erl.Untagged.Union (class IsSupportedMessage)
import Foreign (Foreign, unsafeToForeign)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (Proxy(..))

data ListenerRef :: forall k. k -> Type
data ListenerRef ref

type Options
  = ( connection_type :: Maybe ChildType
    , handshake_timeout :: Maybe Timeout
    , max_connections :: Maybe IntOrInfinity
    , num_acceptors :: Maybe PosInt
    , num_conns_sups :: Maybe PosInt
    , shutdown :: Maybe Timeout
    )

defaultOptions :: Record Options
defaultOptions =
  { connection_type: Nothing
  , handshake_timeout: Nothing
  , max_connections: Nothing
  , num_acceptors: Nothing
  , num_conns_sups: Nothing
  , shutdown: Nothing
  }

data TransportConfig
  = RanchTcp (Record Tcp.ListenOptions)
  | RanchSsl (Record Ssl.ListenOptions)

-- todo - we can do better with errors than this
data HandlerResult
  = HandlerOk Pid
  | HandlerError Foreign

type HandlerFn :: forall k. k -> Type
type HandlerFn ref
  = ListenerRef ref ->
    ( forall m msg.
      MonadEffect m =>
      ReceivesMessage m msg =>
      IsSupportedMessage TcpMessage msg =>
      Unit -> m (Socket CanGenerateMessages ActiveSocket)
    ) ->
    Effect HandlerResult

type PassiveHandlerFn :: forall k. k -> Type
type PassiveHandlerFn ref
  = ListenerRef ref -> (Unit -> Effect (Socket CannotGenerateMessages ActiveSocket)) -> Effect HandlerResult

data GenericHandlerFn :: forall k. k -> Type
data GenericHandlerFn ref
  = Active (HandlerFn ref)
  | Passive (PassiveHandlerFn ref)

type ListenerConfiguration ref options
  = { ref :: ref
    , options :: options
    , transport :: TransportConfig
    , handler :: HandlerFn ref
    }

type PassiveListenerConfiguration ref options
  = { ref :: ref
    , options :: options
    , transport :: TransportConfig
    , handler :: PassiveHandlerFn ref
    }

type ExcludedOptions r
  = ( active :: Maybe SocketActive
    , deliver :: Maybe SocketDeliver
    , header :: Maybe NonNegInt
    , reuseaddr :: Maybe Boolean
    , tclass :: Maybe NonNegInt
    , ttl :: Maybe NonNegInt
    , recvtos :: Maybe Boolean
    , recvtclass :: Maybe Boolean
    , recvttl :: Maybe Boolean
    , packet :: Maybe SocketPacket
    , packet_size :: Maybe NonNegInt
    , show_econnreset :: Maybe Boolean
    | r
    )

excludeOptions ::
  forall r.
  Record (ExcludedOptions r) -> Record (ExcludedOptions r)
excludeOptions r =
  r
    { active = Nothing
    , deliver = Nothing
    , header = Nothing
    , reuseaddr = Nothing
    , tclass = Nothing
    , ttl = Nothing
    , recvtos = Nothing
    , recvtclass = Nothing
    , recvttl = Nothing
    , packet = Nothing
    , packet_size = Nothing
    , show_econnreset = Nothing
    }

data OptionToMaybe
  = OptionToMaybe

instance convertOption_OptionToMaybe :: ConvertOption OptionToMaybe sym a (Maybe a) where
  convertOption _ _ val = Just val

------------------------------------------------------------------------------
-- startListener
-- todo - errors - better than foreign
startListener ::
  forall ref options.
  ConvertOptionsWithDefaults OptionToMaybe (Record Options) options (Record Options) =>
  ListenerConfiguration ref options -> Effect (Either Foreign (ListenerRef ref))
startListener { ref, transport, options, handler } = do
  let
    Tuple mod finalOptions = startListenerOpts transport options
  startListenerImpl Left Right (unsafeToForeign ref) mod finalOptions (Active handler)

startListenerPassive ::
  forall ref options.
  ConvertOptionsWithDefaults OptionToMaybe (Record Options) options (Record Options) =>
  PassiveListenerConfiguration ref options -> Effect (Either Foreign (ListenerRef ref))
startListenerPassive { ref, transport, options, handler } = do
  let
    Tuple mod finalOptions = startListenerOpts transport options
  startListenerImpl Left Right (unsafeToForeign ref) mod finalOptions (Passive handler)

startListenerOpts ::
  forall options.
  ConvertOptionsWithDefaults OptionToMaybe (Record Options) options (Record Options) =>
  TransportConfig -> options -> Tuple Atom Foreign
startListenerOpts transport options = do
  let
    socketOptions = case transport of
      RanchTcp tcpOpts -> do
        Inet.optionsToErl $ excludeOptions $ tcpOpts
      RanchSsl sslOpts -> do
        Inet.optionsToErl $ excludeOptions $ sslOpts

    userOptions = convertOptionsWithDefaults OptionToMaybe defaultOptions options
    withSocketOptions = Record.insert (Proxy :: _ "socket_opts") socketOptions userOptions

    finalOptions = optionsToErl $ withSocketOptions

    mod = case transport of
      RanchTcp _ -> atom "ranch_tcp"
      RanchSsl _ -> atom "ranch_ssl"
  Tuple mod finalOptions

foreign import stopListener :: Atom -> Effect Unit

foreign import startListenerImpl ::
  forall ref.
  (Foreign -> (Either Foreign (ListenerRef ref))) ->
  ((ListenerRef ref) -> (Either Foreign (ListenerRef ref))) ->
  Foreign ->
  Atom ->
  Foreign ->
  GenericHandlerFn ref ->
  Effect (Either Foreign (ListenerRef ref))

start_link ::
  forall ref.
  EffectFn3 (ListenerRef ref) Atom (GenericHandlerFn ref) (Tuple2 Atom Foreign)
start_link = mkEffectFn3 startLink
  where
  startLink ref transportModule handler = do
    handlerRes <- case handler of
      Active handler' -> handler' ref handshake
      Passive handler' -> handler' ref handshakePassive
    pure
      $ case handlerRes of
          HandlerOk pid -> tuple2 (atom "ok") (unsafeToForeign pid)
          HandlerError reason -> tuple2 (atom "error") (unsafeToForeign reason)
    where
    handshake ::
      forall m msg.
      MonadEffect m =>
      ReceivesMessage m msg =>
      IsSupportedMessage TcpMessage msg =>
      Unit -> m (Socket CanGenerateMessages ActiveSocket)
    handshake _ = do
      socket <- liftEffect $ handshakeImpl ref
      let
        transport = fromModule transportModule socket
      maybe (liftEffect $ throw "Erl.Ranch: unexpected transport") pure transport
    handshakePassive _ = do
      socket <- handshakeImpl ref
      let
        transport = fromModule transportModule socket
      maybe (throw "Erl.Ranch: unexpected transport") pure transport

{-
defaultHandler ::
  forall ref socketMessages.
  (ListenerRef ref -> Socket socketMessages ActiveSocket -> Effect Unit) -> (HandlerFn ref socketMessages)
defaultHandler handlerFn = \ref handshake -> do
  let
    defaultInit = do
      transport <- liftEffect $ handshake unit
      handlerFn ref transport
  spawnDefaultHandlerImpl defaultInit
-}
foreign import handshakeImpl :: forall ref. ListenerRef ref -> Effect Foreign

foreign import spawnDefaultHandlerImpl :: forall m. m Unit -> Effect HandlerResult

optionsToErl ::
  forall r rl.
  RL.RowToList r rl =>
  OptionsToErl r rl =>
  Record r -> Foreign
optionsToErl = unsafeToForeign <<< makeTerms (Proxy :: _ rl)

class OptionToErl :: Symbol -> Type -> Constraint
class OptionToErl sym option where
  optionToErl :: Map Atom Foreign -> AtomSymbol.Atom sym -> option -> Map Atom Foreign

instance optionToErl_List :: (IsSymbol name, ToErl a) => OptionToErl name (List a) where
  optionToErl acc name val = Map.insert (toAtom name) (unsafeToForeign (toErl <$> val)) acc
else instance optionToErl_Record ::
  ( IsSymbol name
  , RL.RowToList r rl
  , OptionsToErl r rl
  ) =>
  OptionToErl name (Record r) where
  optionToErl acc name val = Map.insert (toAtom name) (optionsToErl val) acc
else instance optionToErl_Other :: (IsSymbol name, ToErl a) => OptionToErl name a where
  optionToErl acc name val = Map.insert (toAtom name) (toErl val) acc

class OptionsToErl :: Row Type -> RL.RowList Type -> Constraint
class OptionsToErl r rl where
  makeTerms :: Proxy rl -> Record r -> Map Atom Foreign

instance optionsToErl_nil :: OptionsToErl r RL.Nil where
  makeTerms _ _r = Map.empty

instance optionsToErl_consMaybe ::
  ( IsSymbol sym
  , Row.Cons sym (Maybe a) t1 r
  , OptionsToErl r tail
  , OptionToErl sym a
  ) =>
  OptionsToErl r (RL.Cons sym (Maybe a) tail) where
  makeTerms _ r = do
    let tail = makeTerms (Proxy :: _ tail) r
    maybe tail (optionToErl tail (AtomSymbol.atom :: AtomSymbol.Atom sym)) $ Record.get (Proxy :: _ sym) r
else instance optionsToErl_cons ::
  ( IsSymbol sym
  , Row.Cons sym a t1 r
  , OptionsToErl r tail
  , OptionToErl sym a
  ) =>
  OptionsToErl r (RL.Cons sym a tail) where
  makeTerms _ r = do
    let tail = makeTerms (Proxy :: _ tail) r
    optionToErl tail (AtomSymbol.atom :: AtomSymbol.Atom sym) $ Record.get (Proxy :: _ sym) r
