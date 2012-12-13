module Main where
--import Control.Exception
--import Control.Monad.Reader
import qualified Data.Map as Map
import System.Environment
import System.IO
import Text.Printf
import Prelude hiding (catch)

--import Irc as Irc

import Network.XMPP

import Protocol

bot :: Bot
bot = Bot {
   name = "testbot",
   server = "alopex.li",
   resource = "bot",
   password = "testbot",

   status = "Alive",
   nick = "TestBot",

   chats = ["bottest@conference.alopex.li"],
   info = Map.empty,
   memos = Map.empty,
   dataPath = "/home/icefox/my.src/glennbot3/testbot",
   someNum = 1
}
   

main :: IO ()
main = do
   Protocol.start bot
{-   conn <- openStream  (server bot)
   getStreamStart conn
   runXMPP conn $ do
      startAuth (name bot) (server bot) (password bot) (resource bot)
      sendPresence (Just ("", [status bot])) Nothing
      run

run :: XMPP ()
run = do
   msg <- waitForStanza (isChat `conj` hasBody)
   let sender = maybe "" id (getAttr "from" msg)
   let len = length $ maybe "" id (getMessageBody msg)
   sendMessage sender ("Your message was " ++ (show len) ++ " characters long.")
   run
-}
   

{-

forever :: a -> b
forever x y = do
   s <- x y
   forever x s
listenAndPrint :: Irc.Bot -> IO Irc.Bot
listenAndPrint bot = do
   m <- Irc.listen (Irc.server bot)
   Irc.handle m bot

main :: IO ()
main = do
   args <- getArgs
   let nick = args !! 0
   let serv = args !! 1
   let port = (fromIntegral (read (args !! 2)))
   let chan = args !! 3
   s <- Irc.connect serv port nick
   Irc.logOn s
   s <- Irc.join s chan
   Irc.say s chan "Foo!"
   let b = Bot {handlers = Irc.extendedHandlers, server=s}
   forever listenAndPrint b
-}
