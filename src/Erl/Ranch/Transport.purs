module Erl.Ranch.Transport
  ( fromModule
  , Socket
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Erl.Atom (Atom, atom)
import Erl.Kernel.Inet (ActiveSocket, close, recv, send)
import Erl.Kernel.Inet as Inet
import Erl.Kernel.Tcp (TcpSocket)
import Erl.Ssl (SslSocket)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

data Socket socketType
  = Tcp (TcpSocket socketType)
  | Ssl (SslSocket socketType)

instance Inet.Socket Socket where
  send = case _ of 
    Tcp s -> send s
    Ssl s -> send s
  recv = case _ of 
    Tcp s -> recv s
    Ssl s -> recv s
  close = case _ of 
    Tcp s -> close s
    Ssl s -> close s

fromModule :: Atom -> Foreign -> Maybe (Socket ActiveSocket)
fromModule moduleAtom socket =
  if moduleAtom == atom "ranch_tcp" then
    Just $ Tcp (unsafeCoerce socket)
  else if moduleAtom == atom "ranch_ssl" then
    Just $ Ssl (unsafeCoerce socket)
  else
    Nothing
