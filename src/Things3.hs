{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Moving.Tasks.Things where

import GHC.Generics
import Data.Kind
import Data.Aeson
import qualified Data.List as L
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import System.Process
import Network.URI.Encode

data Via = ViaURL | ViaCLI

data ThingsCmd (k :: Via) (ret :: Type) where
  Add ::
    { title    :: Text
    , when     :: Maybe When
    , list     :: Maybe Text
    -- , notes    :: Maybe Text
    -- , tags     :: Maybe [Text]
    -- , deadline :: Maybe When
    } -> ThingsCmd ViaURL ()
  Update ::
    { id :: Text
    , new_title :: Maybe Text
    , when :: Maybe When
    , list :: Maybe Text
    } -> ThingsCmd ViaURL ()
  List ::
    { list_target :: WhenOrInbox
    } -> ThingsCmd ViaCLI [ThingsItem]

data When
  = Today
  | Anytime
  | NextWeek
  | NextMonth
  | NextSemester
  | Someday
  -- | Evening
  -- | Tomorrow
  -- | Date Date
  deriving (Show, Eq, Generic, Enum, Bounded)
    
data Date
  = DateString Text
  | DateTimeString Text
  deriving (Show, Eq, Generic)

type AuthToken = Text

thingsCmdToUrl :: ThingsCmd 'ViaURL r -> AuthToken -> Text
thingsCmdToUrl cmd token =
  "things:///" <> case cmd of
    Add{..} -> "add?" <>
      T.concat
        (L.intersperse "&" $
          [ "title=" <> encodeText title ] ++
          [ "when=" <> when'' <>
            "&tags=" <> encodeTags tags
            | Just when' <- [when] 
            , let (when'', tags) = urlEncodeWhen when'
          ] ++
          [ "list="  <> encodeText list'  | Just list' <- [list] ]
          -- [ "notes=" <> encodeText notes' | Just notes' <- [notes] ]
        )
    Update{..} -> "update?" <>
      T.concat
        (L.intersperse "&" $
          [ "auth-token=" <> token ] ++
          [ "id=" <> id ] ++
          [ "title=" <> encodeText title' | Just title' <- [new_title] ] ++
          [ "when="  <> when'' <>
            -- ROMES:TODO: this will overwrite all tags, but I don't care too much about tags
            "&tags=" <> encodeTags tags
            | Just when'  <- [when]
            , let (when'', tags) = urlEncodeWhen when'
          ] ++
          [ "list="  <> encodeText list'  | Just list'  <- [list] ]
        )
  where
    encodeTags = T.concat . L.intersperse "," . map encodeText

thingsCmd :: ThingsCmd k r -> AuthToken -> IO r
thingsCmd cmd tok = do
  let callOpen url = callProcess "open" ["-gu", unpack url]
  case cmd of
    Add{}    -> callOpen (thingsCmdToUrl cmd tok)
    Update{} -> do
      putStrLn $ "Calling things url: " ++ show (thingsCmdToUrl cmd tok)
      callOpen (thingsCmdToUrl cmd tok)
    List{list_target} -> do
      let (when, tags) = cliEncodeWhenOrInbox list_target
          tagArgs = concat [ ["-t", unpack tag] | tag <- tags]
      str <- readProcess "things-cli" (tagArgs ++ ["--json", unpack when]) ""
      case eitherDecodeStrictText @[ThingsItem] (pack str) of
        Left e -> error (show e ++ " parsing " ++ str)
        Right ts -> return ts

--------------------------------------------------------------------------------
-- Aux
--------------------------------------------------------------------------------

-- Comment out because its better to simply enumerate all boxes and synchronize
-- each one instead of synchroninzing with "All".
-- "All" will get more data than necessary.
-- newtype AllThingsItems = AllThingsItems { unAllThingsItem :: [ThingsItem] } deriving Show
-- instance FromJSON AllThingsItems where
--   -- If you list *all* to-dos, you get a slightly different structure
--   parseJSON = withObject "AllThingsItems" $ \v -> do


data ThingsItem =
  ThingsToDo
  { uuid :: Text
  , title :: Text
  , notes :: Text
  , start :: WhenOrInbox
  }
  | ThingsOtherItem{ title :: Text, type' :: Text }
  deriving (Show, Generic)

instance FromJSON ThingsItem where
  parseJSON = withObject "ThingsItem" $ \v -> do
    ty <- v .: "type"
    case ty of
      "to-do" ->
        ThingsToDo
          <$> v .: "uuid"
          <*> v .: "title"
          <*> v .: "notes"
          <*> v .: "start"
      _ -> ThingsOtherItem
            <$> v .: "title"
            <*> pure ty

data WhenOrInbox
  = WOII
  | WOIW When
  deriving (Show, Eq, Generic)

instance FromJSON WhenOrInbox where
  parseJSON = withText "When" $ pure . go . T.toLower
    where
      go = \case
        "inbox" -> WOII
        "today" -> WOIW Today
        "anytime" -> WOIW Anytime
        "nextweek" -> WOIW NextWeek
        "nextmonth" -> WOIW NextMonth
        "nextsemester" -> WOIW NextSemester
        "someday" -> WOIW Someday
        x -> error $ "Unimplemented parser for When: " ++ show x

instance ToJSON WhenOrInbox where
  toJSON = \case
    WOII -> "Inbox"
    WOIW Today    -> "Today"
    WOIW Anytime  -> "Anytime"
    WOIW NextWeek -> "NextWeek"
    WOIW NextMonth -> "NextMonth"
    WOIW NextSemester -> "NextSemester"
    WOIW Someday  -> "Someday"

cliEncodeWhenOrInbox :: WhenOrInbox -> (Text, [Text])
cliEncodeWhenOrInbox = \case
    WOII -> ("inbox", [])
    WOIW w -> urlEncodeWhen w

urlEncodeWhen :: When -> (Text, [Text])
urlEncodeWhen = \case
  Today -> ("today", [])
  Anytime -> ("anytime", [])
  NextWeek -> ("someday", ["review semanal"])
  NextMonth -> ("someday", ["review mensal"])
  NextSemester -> ("someday", ["review semestral"])
  Someday -> ("someday", [])
