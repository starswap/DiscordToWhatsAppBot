{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ( (.:), withObject, FromJSON(parseJSON) )
import Network.HTTP.Simple
import System.Environment (getEnv)
import Data.ByteString.Char8 (pack)
import LoadEnv ( loadEnvFrom )
import Data.Time
import Data.Time.ISO8601
import Data.Maybe
import System.IO  


type TelephoneNum = String

data TwilioSendRequest = TwilioSendRequest {
    towhatsapp :: TelephoneNum
  , fromwhatsapp  :: TelephoneNum
  , body :: String
  , authKey :: String
} deriving Show

newtype TwilioSendResponse = TwilioSendResponse {
  responseBody :: String
} deriving Show

newtype Author = Author {
  username :: String
} deriving Show

data DiscordMessage = DiscordMessage {
  content :: String,
  author :: Author,
  timestamp :: UTCTime
} deriving Show

instance FromJSON Author where
    parseJSON = withObject "Author" $ \v -> Author
        <$> v .: "username"

instance FromJSON DiscordMessage where
    parseJSON = withObject "DiscordMessage" $ \v ->do
      content <- v .: "content"
      author <- v .: "author"
      timestamp <- v .: "timestamp"
      let time = timestamp >>= parseISO8601
      return (DiscordMessage content author (timestampFromMaybe time))

instance FromJSON TwilioSendResponse where
    parseJSON = withObject "TwilioSendResponse" $ \v -> TwilioSendResponse
        <$> v .: "body"
  

timestampFromMaybe :: Maybe UTCTime -> UTCTime
timestampFromMaybe = fromMaybe (UTCTime (ModifiedJulianDay 0) 0 )

getChannelMessages ::  String -> String -> IO (Response [DiscordMessage])
getChannelMessages channelId botToken = withHeaders >>= httpJSON 
  where monadRequest = parseRequest ("GET https://discord.com/api/v9/channels/"++channelId++"/messages")
        withHeaders = fmap (setRequestHeaders headers) monadRequest 
        headers = [("Authorization",  pack ("Bot " ++ botToken)),
                  ("Content-Type","application/json"),
                  ("User-Agent","DiscordBot (https://discord.com/developers/applications/1023255778980802720/bot, 1)")]

sendMessageToWhatsapp:: TwilioSendRequest -> String -> IO (Response TwilioSendResponse)
sendMessageToWhatsapp (TwilioSendRequest towhatsapp fromwhatsapp body authKey) twilioAuthString = withBody >>= httpJSON 
  where monadRequest = parseRequest ("POST https://api.twilio.com/2010-04-01/Accounts/" ++ authKey ++ "/Messages.json")
        withHeaders = fmap (setRequestHeaders headers) monadRequest
        withBody = fmap (setRequestBodyURLEncoded bodyCode) withHeaders 
        headers = [("Authorization",pack ("Basic " ++ twilioAuthString))]
        bodyCode = [("Body", pack body),
                  ("From", pack ("whatsapp:+" ++ fromwhatsapp) ),
                  ("To",  pack ("whatsapp:+" ++ towhatsapp))]
              
main :: IO ()
main = do
  loadEnvFrom ".env"
  token <- getEnv "BOT_TOKEN"
  twilioAuthString <- getEnv "TWILIO_AUTH_STRING"
  twilioUserName <- getEnv "TWILIO_UNAME"
  tomNumber <- getEnv "TOM_NUMBER"
  twilioNumber <- getEnv "TWILIO_NUMBER"
  channelNumber <- getEnv "CHANNEL_NUMBER"
    
  fileContents <- readFile "lastMessageTimeStamp.txt"

  let lastMessageSentTimeStamp = timestampFromMaybe (parseISO8601 fileContents)
  print lastMessageSentTimeStamp

  channelMessagesResponse <- getChannelMessages channelNumber token
  let messages = filter (\(DiscordMessage _ _ timestamp) -> timestamp > lastMessageSentTimeStamp) (getResponseBody channelMessagesResponse)
  let newLastMessageTimeStamp = maximum $ map (\(DiscordMessage _ _ timestamp) -> timestamp) messages

  mapM_ print messages
  mapM_ (\(DiscordMessage content (Author username) _) -> sendMessageToWhatsapp (TwilioSendRequest tomNumber twilioNumber (username ++ " says " ++ content) twilioUserName) twilioAuthString) messages
  putStrLn "Sent Messages to Whatsapp!"

  writeFile "lastMessageTimeStamp.txt" (formatISO8601 newLastMessageTimeStamp)

