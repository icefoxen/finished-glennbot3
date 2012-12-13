module IrcParse where

import Data.List
import Data.Char
import Data.Maybe
import System.IO
import Text.Printf

-- MESSAGE TYPE, PARSER AND UNPARSER

-- See RFC 1459 section 2.3 for message syntax
newtype Message = M (Maybe Prefix, Messagebody)

data Prefix =
   PServername String
 | PHostmask Hostmask

newtype Hostmask = H (String, String, String)

-- See RFC 1459 section 4 for message types.
-- Most client messages are included, some might be ignored, some might be
-- incomplete or limited.
-- I blame the protocol for being _ridiculous_.
data Messagebody =
   Pass String
 | Nick String
 | User String String String String
 | Quit String

 | Join String -- [(String, String)]
 | Part String String
 -- Does not support limits.
 | Cmode String [CModemask] Hostmask
 | Umode String [UModemask]
 | Topic String String
 | Names [String]
 | List [String]
 | Invite String String
 | Kick String String String

 | Privmsg String String
 | Ctcp String String
 | Notice String String
 | Who String
 | Whois String

-- | Kill String String
 | Ping String
 | Pong String String
 | Error String
 
 | Other String

type CModemask = String
type UModemask = String

instance Show Message where show = showMessage
instance Show Hostmask where show = showHostmask
instance Show Prefix where show = showPrefix
instance Show Messagebody where show = showMessagebody

showMessage :: Message -> String
showMessage (M (Nothing,messagebody)) = (show messagebody)
showMessage (M (Just prefix,messagebody)) =
      ":" ++ (show prefix) ++ " " ++ (show messagebody)

showPrefix :: Prefix -> String
showPrefix (PServername s)  = s
showPrefix (PHostmask h)  = show h

showHostmask :: Hostmask -> String
showHostmask (H (a,b,c)) = printf "%s!%s@%s" a b c

ifNotBlank :: String -> String
ifNotBlank s =
   if s == ""
      then ""
      else " " ++ s


commafy :: [String] -> String
commafy s =
   let loop item accm = 
         case item of  
            hd:tl -> loop tl $ hd ++ "," ++ accm
            [] -> accm
   in
   loop s []

trim :: String -> String
trim =
   takeWhile (not . Data.Char.isSpace) . dropWhile Data.Char.isSpace


showMessagebody :: Messagebody -> String
showMessagebody (Pass s) = "PASS " ++ s
showMessagebody (Nick name) = "NICK " ++ name 
showMessagebody (User user host server real) = 
   "USER " ++ (unwords [user, host, server, real])
showMessagebody (Quit s) = "QUIT :" ++ s
showMessagebody (Join things) =
   --let (joins,keys) = unzip things in
   --let join = commafy joins in
   --let key = commafy keys in
   "JOIN " ++ things -- ++ (ifNotBlank key)
   
showMessagebody (Part channel message) = unwords ["PART", channel, message]
showMessagebody (Cmode channel cmodemask hostmask) =
   "MODE " ++ channel ++ (ifNotBlank $ show cmodemask) ++ (ifNotBlank $ show hostmask)
showMessagebody (Umode nick umodemask) = 
   "MODE " ++ nick ++ (ifNotBlank $ show umodemask)
showMessagebody (Topic channel topic) = 
   let topic' = if topic == "" then "" else ":" ++ topic in
   "TOPIC " ++ channel ++ topic'
showMessagebody (Names s) = 
   "NAMES " ++ (commafy s)
showMessagebody (List channels) = 
   "LIST" ++ (ifNotBlank $ commafy channels)
showMessagebody (Invite channel user) = 
   "INVITE " ++ channel ++ " " ++ user
showMessagebody (Kick channel user message) = 
   let message' = if message == "" then "" else ":" ++ message in
   "KICK " ++ channel ++ " " ++ user ++ (ifNotBlank message')
showMessagebody (Privmsg who message) = 
   "PRIVMSG " ++ who ++ " :" ++ message
showMessagebody (Ctcp who what) = 
   "PRIVMSG " ++ who ++ " :" ++ what
showMessagebody (Notice who message) =
   "NOTICE " ++ who ++ " :" ++ message
showMessagebody (Who s) =
   "WHO " ++ s
showMessagebody (Whois s) = 
   "WHOIS " ++ s
--showMessagebody (Kill s1 s2) = 
--   "KILL " ++ s1 ++ " :" ++ s2
showMessagebody (Ping s) = 
   "PING " ++ s
showMessagebody (Pong s1 s2) = 
   "PONG " ++ s1 ++ " " ++ s2
showMessagebody (Error s) = 
   "ERROR :" ++ s
showMessagebody (Other s) =
   "OTHER :" ++ s


parseHostmask :: String -> Hostmask
parseHostmask s =
   let nick = takeWhile (/= '!') s in
   let name = takeWhile (/= '@') . drop 1 . dropWhile (/= '!') $ s in
   let host = drop 1 . dropWhile (/= '@') $ s in
   H (nick, name, host)
   

parsePrefix :: String -> Maybe Prefix
parsePrefix s =
   if (s !! 0) == ':' then
      let bit = takeWhile (/= ' ') . drop 1 $ s in
      if '@' `elem` bit then Just $ PHostmask $ parseHostmask bit
      else Just $ PServername bit
   else Nothing

ditchPrefix :: String -> String
ditchPrefix s =
   if (s !! 0) == ':'
      then drop 1 . dropWhile (/= ' ') $ s
      else s

parseArgs :: String -> [String]
parseArgs s =
   let s' = ditchPrefix s in
   let args = drop 1 . dropWhile (/= ' ') $ s' in
   if ':' `elem` args then
      let (a,rest) = break (== ':') args in
         (words a) ++ [(drop 1 rest)]
   else words args

padWith :: [String] -> [String] -> [String]
padWith input padding =
   if (length input) >= (length padding) then input
   else input ++ (drop (length input) padding)

-- XXX: More error checking for required arguments.
parseMessagebody :: String -> Messagebody
parseMessagebody s =
   let args = parseArgs s in
   let body = takeWhile (/= ' ') . ditchPrefix $ s in
   case body of
      "PASS" -> Pass (args !! 0)
      "NICK" -> Nick (args !! 0)
      "USER" -> let a = padWith args ["","","",""] in
                   User (a !! 0) (a !! 1) (a !! 2) (':' : (a !! 3))
      "QUIT" -> let a = padWith args [""] in Quit (a !! 0)
      -- XXX: FIX
      "JOIN" -> Join (args !! 0) -- [body] -- args
      "PART" -> let a = padWith args ["", ""] in Part (a !! 0) (a !! 1)
      "MODE" -> Other $ unwords $ "MODE " : args  -- XXX: FIX
      "TOPIC" -> let a = padWith args ["",""] in Topic (a !! 0) (a !! 1)
      "NAMES" -> Names args
      "LIST" -> List args
      "INVITE" -> let a = padWith args ["",""] in Invite (a !! 0) (a !! 1)
      "KICK" -> let a = padWith args ["","",""] in
                Kick (a !! 0) (a !! 1) (a !! 2)

      -- XXX: Handle CTCP
      "PRIVMSG" -> let a = padWith args ["",""] in Privmsg (a !! 0) (a !! 1)
      "NOTICE" -> let a = padWith args ["",""] in Notice (a !! 0) (a !! 1)
      "WHO" -> let a = padWith args [""] in Who (a !! 0)
      "WHOIS" -> let a = padWith args [""] in Whois (a !! 0)

      "PING" -> let a = padWith args [""] in Ping (a !! 0)
      "PONG" -> let a = padWith args ["",""] in Pong (a !! 0) (a !! 1)
      "ERROR" -> let a = padWith args [""] in Error (a !! 0)
      _ -> Other s
   
parseMessage :: String -> Message
parseMessage s =
   let h = parsePrefix s in
   let m = parseMessagebody s in
   M (h,m)

whoFrom :: Message -> Maybe String
whoFrom (M(Just(PHostmask (H(who,_,_))), _)) = Just who
whoFrom _ = Nothing

whoToReplyTo :: Message -> Maybe String
whoFrom (M(Just(PHostmask (H(who,_,_))), Privmsg(to, _)) =
   if (head to) == '#' then Just to
   else Just who
whoFrom _ = Nothing
