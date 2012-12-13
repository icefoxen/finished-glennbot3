module Irc where
import FastIRC.Messages as I
import Message

ircToMessage :: I.Message -> Message
ircToMessage ircm =
   let c = (I.msgCommand ircm)
       from = (I.msgOrigin ircm) in
   case c of
      PrivMsgCmd targetset arg =

messageToIrc :: Message -> I.Message
