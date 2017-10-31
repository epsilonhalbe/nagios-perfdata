-- This file is part of nagios-perfdata.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE OverloadedStrings #-}

module Data.Nagios.Perfdata.Template
  ( ItemMap
  , getItems
  , parseDataType
  , parseHostname
  , parseTimestamp
  , parseHostState
  , parseMetrics
  , perfdataFromDefaultTemplate,
  ) where

import           Data.Nagios.Perfdata.Error
import           Data.Nagios.Perfdata.Metric

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.ByteString.Char8            (readInteger)
import qualified Data.ByteString.Char8            as B8
import           Data.Char                        (isAsciiUpper)
import           Data.Int
import qualified Data.Map                         as M
import           Data.Word
import           Prelude                          hiding (takeWhile)

data Item = Item {
    label   :: B.ByteString,
    content :: B.ByteString
} deriving (Show)

-- | Matches the '::' separating items in check result output.
separator :: Parser [Word8]
separator = count 2 (char8 ':') <?> "separator"

-- | Matches the key in check result output.
ident :: Parser ByteString
ident = takeWhile isAsciiUpper <?> "item identifier"

-- | Matches the value in check result output.
val :: Parser ByteString
val = takeTill isTabOrEol <?> "item value"
  where
    isTabOrEol c = c == '\t' || c == '\n'

-- | Matches a key::value pair in check result output.
item :: Parser Item
item = Item `fmap` ident <* separator <*> val <* skipWhile isTab <?> "perfdata item"
  where
    isTab = (== '\t')

-- | Matches a line of key::value pairs (i.e., the result of one check).
line :: Parser [Item]
line = many item <?> "perfdata line"

-- | Map from key to value for items in a check result.
type ItemMap = M.Map ByteString ByteString

-- | Insert items from a list into a map for easy access by key.
mapItems :: [Item] -> ItemMap
mapItems = foldl (\m i -> M.insert (label i) (content i) m) M.empty

-- | Parse the output from a Nagios check.
parseLine :: ByteString -> Result [Item]
parseLine = parse line

-- | We have no more data to give the parser at this point, so we
-- either fail or succeed here and return a ParserError or an ItemMap
-- respectively.
extractItems :: Result [Item] -> Either ParserError ItemMap
extractItems (Done _ is)       = pure $ mapItems is
extractItems (Fail _ ctxs err) = fail $ fmtParseError ctxs err
extractItems (Partial f)       = extractItems (f "")

getItems :: ByteString -> Either ParserError ItemMap
getItems = extractItems . parseLine

-- | Called if the check output is from a service check. Returns the
-- service-specific component of the perfdata.
parseServiceData :: ItemMap -> Either ParserError ServicePerfdata
parseServiceData m = do
    desc   <- lookupOrFail "SERVICEDESC"  m
    sState <- lookupOrFail "SERVICESTATE" m
    case parseOnly parseReturnState sState of
      Right st -> pure $ ServicePerfdata desc st
      _ -> fail ("invalid service state " ++ B8.unpack sState)

-- | Whether this perfdata item is for a host check or a service check
-- (or Nothing on failure to determine).
parseDataType :: ItemMap -> Either ParserError HostOrService
parseDataType m = do
    s <- lookupOrFail "DATATYPE" m
    case s of
        "HOSTPERFDATA" -> pure Host
        "SERVICEPERFDATA" -> Service <$> parseServiceData m
        _                 -> fail "Invalid datatype"

parseHostname :: ItemMap -> Either ParserError ByteString
parseHostname = lookupOrFail "HOSTNAME"

parseTimestamp :: ItemMap -> Either ParserError Int64
parseTimestamp m = do
    t <- lookupOrFail "TIMET" m
    n <- maybe (fail "Invalid timestamp") (pure . fst) $ readInteger t
    pure $ fromInteger (n * nanosecondFactor)
  where
    nanosecondFactor = 1000000000

parseHostState :: ItemMap -> Either ParserError ByteString
parseHostState = lookupOrFail "HOSTSTATE"

-- | Given an item map extracted from a check result, parse and return
-- the performance metrics (or store an error and return Nothing).
parseMetrics :: HostOrService -> ItemMap -> Either ParserError [Metric]
parseMetrics typ m =
  let typ' = case typ of Host      -> "HOST"
                         Service _ -> "SERVICE"
  in maybe (fail (typ' ++ "PERFDATA not found")) parseMetricString $
        M.lookup (B8.pack $ typ' ++ "PERFDATA") m

-- | Given an item map extracted from a check result, parse and return
-- a Perfdata object.
extractPerfdata :: ItemMap -> Either ParserError Perfdata
extractPerfdata m = do
    typ   <- parseDataType m
    name  <- parseHostname m
    t     <- parseTimestamp m
    state <- parseHostState m
    ms    <- parseMetrics typ m
    return $ Perfdata typ t (B8.unpack name) (Just state) ms

-- | Extract perfdata from a Nagios perfdata item formatted according to
-- the default template[0]. This is the format that is used in the
-- perfdata spool files and consumed by pnp4nagios.
--
-- [0] Default templates defined in the Nagios source (xdata/xpddefault.h).
-- Service perfdata:
--   "[SERVICEPERFDATA]\t$TIMET$\t$HOSTNAME$\t$SERVICEDESC$\t$SERVICEEXECUTIONTIME$\t$SERVICELATENCY$\t$SERVICEOUTPUT$\t$SERVICEPERFDATA$"
-- Host perfdata:
--   "[HOSTPERFDATA]\t$TIMET$\t$HOSTNAME$\t$HOSTEXECUTIONTIME$\t$HOSTOUTPUT$\t$HOSTPERFDATA$"
perfdataFromDefaultTemplate :: ByteString -> Either ParserError Perfdata
perfdataFromDefaultTemplate s =
    getItems s >>= extractPerfdata

lookupOrFail :: Monad m => ByteString -> ItemMap -> m ByteString
lookupOrFail str m = maybe (fail $ B8.unpack str ++ "not found") pure $ M.lookup str m
