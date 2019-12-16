{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Simple
import System.Environment (getArgs)
import System.IO

import Web.Bugzilla
import Web.Bugzilla.Search

main :: IO ()
main = dispatch Nothing Nothing Nothing =<< getArgs

type RestPath = T.Text

dispatch :: Maybe T.Text -> Maybe BugzillaServer -> Maybe RestPath -> [String] -> IO ()
dispatch Nothing s r ("--token" : token : as)   = dispatch (Just $ T.pack token) s r as
dispatch l Nothing r ("--server" : server : as) = dispatch l (Just $ T.pack server) r as
dispatch l s Nothing ("--rest-path" : restPath : as) = dispatch l s (Just $ T.pack restPath) as
dispatch l s r ["--assigned-to", user]          = withBzToken l s r $ doAssignedTo (T.pack user)
dispatch l s r ["--assigned-to-brief", user]    = withBzToken l s r $ doAssignedToBrief (T.pack user)
dispatch l s r ["--requests", user]             = withBzToken l s r $ doRequests (T.pack user)
dispatch l s r ["--history", bug, n]            = withBzToken l s r $ doHistory (read bug) (read n)
dispatch _ _ _ ["--cheat"]                      = goCheat
dispatch _ _ _ _                                = usage

usage :: IO ()
usage = hPutStrLn stderr "Connection options:"
     >> hPutStrLn stderr "  --server [domain name] - REQUIRED. The Bugzilla server to access."
     -- >> hPutStrLn stderr "  --login [user email]   - The user to log in with."
     >> hPutStrLn stderr "  --token [bz token]     - Token to access Bugzilla."
     >> hPutStrLn stderr "  --rest-path [path]     - Path to server's REST access point."
     >> hPutStrLn stderr ""
     >> hPutStrLn stderr "Bugzilla queries:"
     >> hPutStrLn stderr "  --assigned-to [user email] - List bugs assigned to the user."
     >> hPutStrLn stderr "  --assigned-to-brief [user email] - Briefly list bugs assigned to the user."
     >> hPutStrLn stderr "  --requests [user email]    - List requests for the user."
     >> hPutStrLn stderr "  --history [bug number] [n] - List the most recent 'n' changes to the bug."

-- withBz :: Maybe UserEmail -> Maybe BugzillaServer -> (BugzillaSession -> IO ()) -> IO ()
-- withBz mLogin mServer f = do
--   let server = case mServer of
--                  Just s  -> s
--                  Nothing -> error "Please specify a server with '--server'"
--   withBugzillaContext server $ \ctx ->
--     case mLogin of
--       Just login -> do hPutStrLn stderr "Enter password: "
--                        password <- T.pack <$> withEcho False getLine
--                        mSession <- loginSession ctx login password
--                        case mSession of
--                          Just session -> do hPutStrLn stderr "Login successful."
--                                             f session
--                          Nothing      -> do hPutStrLn stderr "Login failed. Falling back to anonymous session."
--                                             f $ anonymousSession ctx
--       Nothing -> f $ anonymousSession ctx
                     
withBzToken :: Maybe T.Text -> Maybe BugzillaServer -> Maybe RestPath -> (BugzillaSession -> IO ()) -> IO ()
withBzToken mTokText mServer mRestPath f = do
  let server = case mServer of
                 Just s  -> s
                 Nothing -> error "Please specify a server with '--server'"
      restPath = case mRestPath of
                   Just r  -> T.splitOn (T.pack "/") r
                   Nothing -> []
  withBugzillaContext server restPath $ \ctx -> do
    case mTokText of
      Just tokText -> f $ LoginSession ctx $ BugzillaToken tokText
      Nothing      -> f $ anonymousSession ctx

goCheat :: IO ()
goCheat = do
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- liftIO $ newManager settings
    initReq <- parseRequest "https://SERVER.NAME.HERE/PATH/COMPONENTS/HERE/rest/bug?token=XXX-YYYYYYYYYY&include_fields=id&f1=assigned_to&o1=equals&v1=me%40mycompany.com"
    -- let req
    --         = setRequestManager manager
    --         $ initReq
    -- response <- Network.HTTP.Simple.httpLbs req
    putStrLn $ "request: " ++ (show initReq)
    response <- Network.HTTP.Conduit.httpLbs initReq manager

    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    --L8.putStrLn $ getResponseBody response


doAssignedTo :: UserEmail -> BugzillaSession -> IO ()
doAssignedTo user session = do
    let search = AssignedToField .==. user
    bugs <- searchBugsWithLimit session 10 0 search
    mapM_ showBug bugs
  where
    showBug (Bug {..}) = putStrLn $ show bugId ++ ": " ++ show bugSummary
                                 ++ " [" ++ T.unpack bugStatus ++ ": " ++ T.unpack bugResolution ++ "] Updated: "
                                 ++ show bugLastChangeTime

doAssignedToBrief :: UserEmail -> BugzillaSession -> IO ()
doAssignedToBrief user session = do
    let search = AssignedToField .==. user
    bugs <- searchBugsWithLimit' session 10 0 search
    mapM_ print bugs

doRequests :: UserEmail -> BugzillaSession -> IO ()
doRequests user session = do
    let needinfoSearch = FlagRequesteeField .==. user .&&. FlagsField `contains` "needinfo"
    needinfoBugs <- searchBugs session needinfoSearch
    mapM_ showNeedinfo needinfoBugs

    let reviewSearch = FlagRequesteeField .==. user .&&.
                       (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
    reviewBugs <- map bugId <$> searchBugs session reviewSearch
    forM_ reviewBugs $ \rBugId -> do
      attachments <- getAttachments session rBugId
      mapM_ showReview $ filter (any hasReviewFlag . attachmentFlags) attachments
      mapM_ showFeedback $ filter (any hasFeedbackFlag . attachmentFlags) attachments

  where
    showNeedinfo (Bug {..}) = do
      let flags = filter hasNeedinfoFlag bugFlags
      forM_ flags $ \flag ->
        putStrLn $ "[NEEDINFO] " ++ show bugId ++ ": " ++ show bugSummary
                ++ " (" ++ show (flagSetter flag) ++ " " ++ show (flagCreationDate flag) ++ ")"

    showReview (Attachment {..}) =
      putStrLn $ "[REVIEW] " ++ show attachmentBugId ++ ": " ++ show attachmentSummary
              ++ " (" ++ show attachmentCreator ++ " " ++ show attachmentCreationTime ++ ")"

    showFeedback (Attachment {..}) =
      putStrLn $ "[FEEDBACK] " ++ show attachmentBugId ++ ": " ++ show attachmentSummary
              ++ " (" ++ show attachmentCreator ++ " " ++ show attachmentCreationTime ++ ")"

    hasNeedinfoFlag f = flagRequestee f == Just user && flagName f == "needinfo"
    hasReviewFlag f   = flagRequestee f == Just user && flagName f == "review"
    hasFeedbackFlag f = flagRequestee f == Just user && flagName f == "feedback"

doHistory :: BugId -> Int -> BugzillaSession -> IO ()
doHistory bug count session = do
    comments <- getComments session bug
    history <- getHistory session bug
    recentEventsRev <- takeRecent count (reverse comments)
                                        (reverse $ historyEvents history)
    mapM_ putStrLn (reverse recentEventsRev)
  where
    takeRecent :: Int -> [Comment] -> [HistoryEvent] -> IO [String]
    takeRecent 0 _ _ = return []
    takeRecent n (c:cs) (e:es)
      | commentCreationTime c `diffUTCTime` historyEventTime e >= 0 = (:) <$> showComment c
                                                                          <*> takeRecent (n - 1) cs (e:es)
      | otherwise                                                   = (:) <$> showEvent e
                                                                          <*> takeRecent (n - 1) (c:cs) es
    takeRecent n cs@(_:_) [] = mapM showComment $ take n cs
    takeRecent n [] es@(_:_) = mapM showEvent $ take n es
    takeRecent _ [] []       = return []

    -- FIXME: showComment and showEvent will call getUser for the same
    -- user over and over again. You should never do this in a real application.
    showComment (Comment {..}) = do
      user <- getUser session commentCreator
      let commentUserRealName = fromMaybe commentCreator $ userRealName <$> user
      let commentUserEmail = fromMaybe commentCreator $ userEmail =<< user
      return $ "(Comment " ++ show commentCount ++ ") " ++ T.unpack commentUserRealName
            ++ " <" ++ T.unpack commentUserEmail ++ "> " ++ show commentCreationTime
            ++ "\n" ++ (unlines . map ("  " ++) . lines . T.unpack $ commentText)

    showEvent (HistoryEvent {..}) = do
      user <- getUser session historyEventUser
      let eventUserRealName = fromMaybe historyEventUser $ userRealName <$> user
      let eventUserEmail = fromMaybe historyEventUser $ userEmail =<< user
      return $ "(Event " ++ show historyEventId ++ ") " ++ T.unpack eventUserRealName
            ++ " <" ++ T.unpack eventUserEmail ++ ">\n"
            ++ concatMap showChange historyEventChanges

    showChange (TextFieldChange f (Modification r a aid)) = showChange' f r a aid
    showChange (ListFieldChange f (Modification r a aid)) = showChange' f r a aid
    showChange (IntFieldChange f (Modification r a aid))  = showChange' f r a aid
    showChange (TimeFieldChange f (Modification r a aid)) = showChange' f r a aid
    showChange (BoolFieldChange f (Modification r a aid)) = showChange' f r a aid

    showChange' f r a aid = "  " ++ showField f ++ ": "
                         ++ showMod r ++ " -> " ++ showMod a
                         ++ showAid aid ++ "\n"

    showField = T.unpack . fieldName

    showMod :: Show a => Maybe a -> String
    showMod (Just v) = show v
    showMod Nothing  = "___"

    showAid :: Maybe AttachmentId -> String
    showAid (Just aid) = " (Attachment " ++ show aid ++ ")"
    showAid Nothing    = ""

withEcho :: Bool -> IO a -> IO a
withEcho echo action =
    bracket (hGetEcho stdin)
            (hSetEcho stdin)
            (const $ hSetEcho stdin echo >> action)
