module Protocol where
import Data.Char
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Time.Format
import Data.Time.LocalTime
import System.Directory
import System.FilePath
import System.Info
import System.IO
import System.Locale

import Network.XMPP
import Network.XMPP.MUC as MUC

-- For debugging
import System.IO.Unsafe

type InfoSet = Map.Map String [String]

data Bot = Bot {
   name :: String,
   server :: String,
   resource :: String,
   password :: String,

   status :: String,
   nick :: String,

   chats :: [String],
   info :: InfoSet,
   memos :: InfoSet,
   dataPath :: String,
   someNum :: Int
}

botVersion :: String
botVersion = "3.0"

emptyInfo :: InfoSet
emptyInfo = Map.empty

infoPath :: Bot -> String
infoPath bot = joinPath [(dataPath bot), "info"]

memoPath :: Bot -> String
memoPath bot = joinPath [(dataPath bot), "memo"]

logPath :: Bot -> String
logPath bot = joinPath [(dataPath bot), "log"]

today :: IO String
today = do
   t <- getZonedTime
   return $ formatTime defaultTimeLocale "%Y-%m-%d" t

now :: IO String
now = do
   t <- getZonedTime
   return $ formatTime defaultTimeLocale "%Y-%m-%d %T" t

logFile :: Bot -> String -> IO String
logFile bot chat = do
   t <- today
   return $ joinPath [(dataPath bot), "log", chat ++ "-" ++ t ++ ".log"]

-- Only does info right now, but should do other things later...
saveBot :: Bot -> IO ()
saveBot bot = do
   let infoStr = show (info bot)
   let memoStr = show (memos bot)
   writeFile (infoPath bot) infoStr
   writeFile (memoPath bot) memoStr
   

loadBot :: Bot -> IO Bot
loadBot bot = do
   infoStr <- readFile (infoPath bot)
   memoStr <- readFile (memoPath bot)
   return bot {info = read infoStr, memos = read memoStr}

alterBot :: (Bot -> Bot) -> Bot -> IO Bot
alterBot f bot = do
   let newbot = f bot
   saveBot newbot
   return newbot

getInfo :: String -> Bot -> [String]
getInfo key bot =
   case Map.lookup key (info bot) of
      Nothing -> []
      Just lst -> lst

removeInfo :: String -> Bot -> IO Bot
removeInfo key bot =
   let f b = b {info = (Map.delete key (info b))} in
   alterBot f bot

removeInfoItem :: String -> String -> Bot -> IO Bot
removeInfoItem key val bot =
   let f bot = 
         let nfo = getInfo key bot in
         case nfo of
            [] -> bot
            lst -> let newlist = delete val lst
                       newinfo = Map.insert key newlist (info bot) in
                   bot {info = newinfo}
   in alterBot f bot

addInfo :: Bot -> String -> String -> IO Bot
addInfo bot key item =
   let f bot =
         let fn = maybe (Just [item]) (\i -> Just (item : i))
             i = Map.alter fn key (info bot) in
         bot {info = i}
   in alterBot f bot

log :: Bot -> String -> String -> String -> IO ()
log bot chat who text = do
   lf <- logFile bot chat
   let lp = logPath bot
   n <- now
   System.Directory.createDirectoryIfMissing True lp
   let s = n ++ " <" ++ who ++ "> " ++ text
   appendFile lf s

{-
 - Could be an easy enhancement, or a huge pain.
data Chat = Chat {
   name :: String,
   nick :: String,
   log :: [String],
   pass :: Maybe String
}
-}

joinChat :: Bot -> String -> XMPP Bot
joinChat bot chat =
   if (chat `elem` (chats bot)) then do
      return bot
      else do
         MUC.joinGroupchat (nick bot) chat Nothing
         return bot {chats = chat:(chats bot)}

      

leaveChat :: Bot -> String -> XMPP Bot
leaveChat bot chat =
   if (chat `elem` (chats bot)) then do
      MUC.leaveGroupchat chat
      return bot {chats = delete chat (chats bot)}
      else return bot 


joinChats :: Bot -> XMPP ()
joinChats bot = do
   let rooms = chats bot
   mapM_ (\room -> MUC.joinGroupchat (nick bot) room Nothing) rooms


-- Init function
start :: Bot -> IO ()
start bot = do
   conn <- openStream (server bot)
   getStreamStart conn
   runXMPP conn $ do
      startAuth (name bot) (server bot) (password bot) (resource bot)
      sendPresence (Just ("", [status bot])) Nothing
      addHandlers bot
      joinChats bot

unsafePutStrLn :: String -> ()
unsafePutStrLn str =
   unsafePerformIO $ putStrLn str

isDelayed :: StanzaPredicate
isDelayed xml =
   let xep091 =
        let x = xmlPath ["x"] xml in
        case x of
           Nothing -> False
           Just xelt -> 
            (getAttr "xmlns" xelt) == (Just "jabber:x:delay")
       -- XXX: This may or may not work
       xep203 = 
        let x = xmlPath ["delay"] xml in
        x /= Nothing
   in
     xep091 || xep203



addHandlers :: Bot -> XMPP ()
addHandlers bot = do
   let osString = System.Info.os ++ " " ++ System.Info.arch
   handleVersion "Glennbot" botVersion osString
   botref <- liftIO $ newIORef bot
   addHandler (isChat `conj` hasBody) (chatHandler botref) True
   --addHandler (isChat `conj` hasBody) (chatHandler bot) True
   addHandler ((not . isDelayed) `conj` isGroupchatMessage `conj` hasBody) (groupchatHandler botref) True

data Reply =
   Reply String
 | PrivReply String String


chatReply :: String -> Reply -> XMPP ()
chatReply from reply =
   case reply of
      Reply s -> sendMessage from s
      PrivReply to s -> sendMessage to s

groupchatReply :: String -> String -> Reply -> XMPP ()
groupchatReply chat from reply =
   case reply of
      Reply s -> sendGroupchatMessage chat s
      PrivReply to s -> sendGroupchatPrivateMessage to chat s

-- Both chatHandler and groupchatHandler do similar things:
-- Read bot out of the ref, grab appropriate data from the XML stanza,
-- make sure the bot isn't talking to itself, and if not call the 'process'
-- function on the method.  'process' returns a new bot which gets tucked
-- back into the ref and saved to disk, and a list of replies which get 
-- sent off with the appropriate function.
-- The only reason they're not the same function is that single-user
-- messages and groupchats have _slightly_ different interfaces, and are
-- handled _slightly_ differently (groupchats are logged, for instance,
-- and the handling of private messages is different for each)
chatHandler :: IORef Bot -> StanzaHandler
chatHandler botref xml = do
  bot <- liftIO $ readIORef botref
  let sender = getBareJid $ maybe "" id (getAttr "from" xml)
  let message = maybe "" id (getMessageBody xml) 
  if sender /= ((name bot) ++ "@" ++ (server bot)) then do
     let (newbot,replies) = process bot sender message
     liftIO $ writeIORef botref newbot
     liftIO $ saveBot newbot
     mapM_ (chatReply sender) replies
     else return ()

groupchatHandler :: IORef Bot -> StanzaHandler 
groupchatHandler botref xml = do
  bot <- liftIO $ readIORef botref
  let jid = maybe "" id (getAttr "from" xml)
  let chat = getBareJid jid
  let sender = getResource jid
  let message = maybe "" id (getMessageBody xml) 
  liftIO $ Protocol.log bot chat sender message
  if sender /= (nick bot) then do
     let (newbot,replies) = process bot sender message
     liftIO $ writeIORef botref newbot
     liftIO $ saveBot newbot
     mapM_ (groupchatReply chat sender) replies
     else return ()

downcase :: String -> String
downcase = map toLower

lex :: String -> [String]
lex str =
   let loop str accm = 
        case str of
           "" -> reverse accm
           hd:tl ->
            if isSpace hd then loop tl accm else
               if (isSymbol hd) || (isPunctuation hd) then 
                  loop tl ([hd]:accm)
                  else let s = takeWhile isAlphaNum str in
                     loop (dropWhile isAlphaNum str) (s:accm)
   in loop (downcase str) []

-- Bot, from, full message
-- ...Okay.  We have something resembling a real parser, we're going
-- to need to have something resembling a real command set.
-- ...or something.  Fuck, I dunno.
type BotCommand = Bot -> String -> String -> (Bot, [Reply])

data Command =
   Memo (String,String)
 | Echo
 | Join String
 | Leave String
 | Kill String

data CommandMessage = Cm {
   cFrom :: String,
   cCommand :: Command,
   cMessage :: String
}
   
   

-- Where things actually happen.
-- Okay.  So we read a command, figure out what to do, then do it, then send
-- back the results.
-- Sending back results is trivial.
process :: Bot -> String -> String -> (Bot,[Reply])
process bot from message =
   let m = Protocol.lex message
       command = searchCommand m
       --specific = (take 2 m) == [(nick bot), ":"]
   in case command of
       Nothing -> (bot,[])
       Just command -> command bot from message

-- Returns true if every element in l1 exists in l2 in the same order.
looseMatch :: [String] -> [String] -> Bool
looseMatch [] [] = True
looseMatch [] _  = True
looseMatch _ []  = False
looseMatch (hd1:l1) (hd2:l2) =
   if hd1 == hd2 then looseMatch l1 l2
      else looseMatch (hd1:l1) l2


echoDataC :: BotCommand
echoDataC bot from message =
   (bot, [Reply (nick bot), Reply message])

--memoC :: BotCommand
--memoC bot from message =
   

commands :: [([String], BotCommand)]
commands = [(["echo", "!"], echoDataC)]

-- This matches a command to a function
searchCommand :: [String] -> Maybe BotCommand
searchCommand message =
   let loop cmds = if null cmds then Nothing
         else let (c,func) = head cmds
                  t = tail cmds
              in if looseMatch c message then Just func
                 else loop t
   in loop commands 
