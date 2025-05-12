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
module Things3 where

import GHC.Generics
import Data.Kind
import Data.Aeson
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI.Encode
-- import System.Process

data ThingsCmd (auth_token :: Type) (ret :: Type) where
  Add ::
    { title    :: Text
    , when     :: Maybe When
    , list     :: Maybe Text
    , notes    :: Maybe Text
    , tags     :: Maybe [Text]
    , checklist_items :: Maybe [Text]
    -- , deadline :: Maybe When
    } -> ThingsCmd a ()
  Update ::
    { update_id :: Text
    , new_title :: Maybe Text
    , new_when :: Maybe When
    , new_list :: Maybe Text
    } -> ThingsCmd AuthToken ()

data When
  = Today
  | Anytime
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

thingsCmdToUrl :: ThingsCmd auth_token r -> auth_token -> Text
thingsCmdToUrl cmd token =
  "things:///" <> case cmd of
    Add{..} -> "add?" <>
      T.concat
        (L.intersperse "&" $
          [ "title=" <> encodeText title ] ++
          [ "when=" <> urlEncodeWhen when' | Just when' <- [when] ] ++
          [ "tags=" <> encodeTags tags'  | Just tags' <- [tags] ] ++
          [ "list="  <> encodeText list'  | Just list' <- [list] ] ++
          [ "checklist-items="  <> encodeText (T.unlines list')  | Just list' <- [checklist_items] ] ++
          [ "notes=" <> encodeText notes' | Just notes' <- [notes] ]
        )
    Update{..} -> "update?" <>
      T.concat
        (L.intersperse "&" $
          [ "auth-token=" <> token ] ++
          [ "id=" <> update_id ] ++
          [ "title=" <> encodeText title' | Just title' <- [new_title] ] ++
          [ "when="  <> when'' -- <>
            -- ROMES:TODO: this will overwrite all tags, but I don't care too much about tags
            -- "&tags=" <> encodeTags tags
            | Just when'  <- [new_when]
            , let when'' = urlEncodeWhen when'
          ] ++
          [ "list="  <> encodeText list'  | Just list'  <- [new_list] ]
        )
  where
    encodeTags = T.concat . L.intersperse "," . map encodeText

-- thingsCmd :: ThingsCmd k auth_token r -> auth_token -> IO r
-- thingsCmd cmd tok = do
--   let callOpen url = callProcess "open" ["-gu", unpack url]
--   case cmd of
--     Add{}    -> callOpen (thingsCmdToUrl cmd tok)
--     Update{} -> do
--       putStrLn $ "Calling things url: " ++ show (thingsCmdToUrl cmd tok)
--       callOpen (thingsCmdToUrl cmd tok)
--     List{list_target} -> do
--       let (when, tags) = cliEncodeWhenOrInbox list_target
--           tagArgs = concat [ ["-t", unpack tag] | tag <- tags]
--       str <- readProcess "things-cli" (tagArgs ++ ["--json", unpack when]) ""
--       case eitherDecodeStrictText @[ThingsItem] (pack str) of
--         Left e -> error (show e ++ " parsing " ++ str)
--         Right ts -> return ts

--------------------------------------------------------------------------------
-- Aux
--------------------------------------------------------------------------------

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
        "someday" -> WOIW Someday
        x -> error $ "Unimplemented parser for When: " ++ show x

instance ToJSON WhenOrInbox where
  toJSON = \case
    WOII -> "Inbox"
    WOIW Today    -> "Today"
    WOIW Anytime  -> "Anytime"
    WOIW Someday  -> "Someday"

urlEncodeWhen :: When -> Text
urlEncodeWhen = \case
  Today -> "today"
  Anytime -> "anytime"
  Someday -> "someday"
