-- This file is part of nagios-perfdata.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Nagios.Perfdata.Metric(
    Perfdata(..),
    MetricList,
    Metric(..),
    critValue,
    maxValue,
    minValue,
    warnValue,
    MetricValue,
    HostOrService(..),
    ServicePerfdata(..),
    perfdataHostState,
    perfdataHostname,
    perfdataTimestamp,
    perfdataType,
    parseReturnCode,
    parseReturnState,
    parseMetricString,
    isUnknownMetricValue,
    UOM(..),
    parseUOM,
    ReturnState(..),
    Threshold,
    perfdataServiceDescription,
    metricValueDefault,
    isMetricBase,
    convertMetricToBase,
    convertPerfdataToBase,
    ) where

import           Data.Nagios.Perfdata.Error

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as S
import           Data.Int
import           Control.Lens
import           Safe                             (toEnumMay)

import           Prelude                          hiding (takeWhile)

-- | Value of a performance metric. We may lose some data converting
-- to doubles here; this may change in the future.
type MetricValue = Maybe Double

-- | Value of a min/max/warn/crit threshold, subject to the same
-- constraints as MetricValue.
type Threshold = Maybe Double

-- | List of metrics by metric name.
type MetricList = [(String, Metric)]

data Prefix = Base | Milli | Micro | Kilo | Mega | Giga | Tera
    deriving (Eq)

instance Show Prefix where
    show Base  = ""
    show Milli = "m"
    show Micro = "u"
    show Kilo  = "K"
    show Mega  = "M"
    show Giga  = "G"
    show Tera  = "T"

-- | Nagios unit of measurement. NullUnit is an empty string in the
-- check result; UnknownUOM indicates a failure to parse.
data UOM = Second | Millisecond | Microsecond | Percent | Byte | Kilobyte
         | Megabyte | Gigabyte | Terabyte | Counter | NullUnit | UnknownUOM
    deriving (Eq)

toPrefix :: UOM -> Prefix
{-# INLINE toPrefix #-}
toPrefix Second      = Base
toPrefix Millisecond = Milli
toPrefix Microsecond = Micro
toPrefix Percent     = Base
toPrefix Byte        = Base
toPrefix Kilobyte    = Kilo
toPrefix Megabyte    = Mega
toPrefix Gigabyte    = Giga
toPrefix Terabyte    = Tera
toPrefix Counter     = Base
toPrefix NullUnit    = Base
toPrefix UnknownUOM  = Base

toBase :: UOM -> UOM
{-# INLINE toBase #-}
toBase Second      = Second
toBase Millisecond = Second
toBase Microsecond = Second
toBase Percent     = Percent
toBase Byte        = Byte
toBase Kilobyte    = Byte
toBase Megabyte    = Byte
toBase Gigabyte    = Byte
toBase Terabyte    = Byte
toBase Counter     = Counter
toBase NullUnit    = NullUnit
toBase UnknownUOM  = UnknownUOM

instance Show UOM where
    show Second      = "s"
    show Percent     = "%"
    show Byte        = "B"
    show Counter     = "c"
    show NullUnit    = ""
    show UnknownUOM  = "?"
    show uom         = show (toPrefix uom) ++ show (toBase uom)

-- | Encapsulates the data in a Nagios performance metric. A service can
-- have several of these.
data Metric = Metric {
    _metricValue :: MetricValue,
    _metricUOM   :: UOM,
    _warnValue   :: Threshold,
    _critValue   :: Threshold,
    _minValue    :: Threshold,
    _maxValue    :: Threshold
} deriving (Show, Eq)

$(makeLenses ''Metric)
--
-- | The part of the check result that's specific to service checks,
-- and doesn't appear in host checks.
data ServicePerfdata = ServicePerfdata {
    serviceDescription :: S.ByteString,
    serviceState       :: !ReturnState
    } deriving (Show, Eq)

-- | The check type, either Service with associated ServiceData or Host.
data HostOrService = Service ServicePerfdata
                   | Host
                   deriving (Show, Eq)

data ReturnState = OKState
                 | WarningState
                 | CriticalState
                 | UnknownState
                 deriving (Show,Eq, Enum, Bounded)

-- | Encapsulates all the data in a check result that's relevant to
-- metrics (we throw away things like the state type of HARD/SOFT).
data Perfdata = Perfdata {
    _perfdataType      :: !HostOrService,
    _perfdataTimestamp :: !Int64,
    _perfdataHostname  :: !String,
    _perfdataHostState :: Maybe S.ByteString,
    _perfdataMetrics   :: MetricList
    } deriving (Show, Eq)

$(makeLenses ''Perfdata)

metricValueDefault :: Double -> Metric -> Double
metricValueDefault d = fromMaybe d . _metricValue

isUnknownMetricValue :: Metric -> Bool
isUnknownMetricValue = isNothing . _metricValue


prefixToScale :: Prefix -> Double
{-# INLINE prefixToScale #-}
prefixToScale Micro =               0.000001
prefixToScale Milli =               0.001
prefixToScale Base  =             1.0
prefixToScale Kilo  =          1000.0
prefixToScale Mega  =       1000000.0
prefixToScale Giga  =    1000000000.0
prefixToScale Tera  = 1000000000000.0

toScale :: UOM -> Double -> Double
toScale = (*) . prefixToScale . toPrefix

convertUnitToBase :: (MetricValue, UOM) -> (MetricValue, UOM)
convertUnitToBase (x@(Just _), uom)  = (toScale uom <$> x, toBase uom)
convertUnitToBase x = x

convertMetricToBase :: Metric -> Metric
convertMetricToBase m = m & metricValue .~ v
                          & metricUOM   .~ uom
    where (v, uom) = convertUnitToBase (m^.metricValue , m^.metricUOM)

isMetricBase :: Metric -> Bool
isMetricBase Metric{..} = _metricUOM == toBase _metricUOM

convertPerfdataToBase :: Perfdata -> Perfdata
convertPerfdataToBase = perfdataMetrics.mapped._2 %~ convertMetricToBase

parseReturnCode :: Int -> Maybe ReturnState
parseReturnCode = toEnumMay

parseReturnState :: Parser ReturnState
parseReturnState = choice
                 [ string "OK"       *> pure OKState
                 , string "WARNING"  *> pure WarningState
                 , string "CRITICAL" *> pure CriticalState
                 , string "UNKNOWN"  *> pure UnknownState
                 , fail "Expected one of [OK, WARNING, CRITICAL, UNKNOWN]"
                 ]

perfdataServiceDescription :: Perfdata -> S.ByteString
perfdataServiceDescription datum = case _perfdataType datum of
    Host -> "host"
    Service serviceData -> serviceDescription serviceData

parseUOM :: Parser UOM
parseUOM = choice [ string "s"  *> pure Second
                  , string "ms" *> pure Millisecond
                  , string "us" *> pure Microsecond
                  , string "%"  *> pure Percent
                  , string "B"  *> pure Byte
                  , string "KB" *> pure Kilobyte
                  , string "MB" *> pure Megabyte
                  , string "GB" *> pure Gigabyte
                  , string "TB" *> pure Terabyte
                  , string "c"  *> pure Counter
                  , pure . maybe NullUnit (const UnknownUOM) =<< peekChar
                  ]

metricName :: Parser String
metricName = option quote (char quote) *>
             many (satisfy nameChar)   <*
             option quote (char quote)
  where
    quote = '\''
    nameChar = notInClass "'="

value :: Parser MetricValue
value = option Nothing $ fmap Just double

threshold :: Parser Threshold
threshold = char8 ';' *> thresholdValue
            <|> thresholdValue
  where
    thresholdValue = option Nothing (fmap Just double)

metric :: Parser (String, Metric)
metric = do
    name <- metricName
    void $ char8 '='
    m    <- Metric `fmap` value <*>
                          parseUOM <*>
                          threshold <*>
                          threshold <*>
                          threshold <*>
                          threshold
    pure (name, m)

metricLine :: Parser MetricList
metricLine = many (metric <* (skipMany (char8 ';') <* skipSpace))

-- | Parse the component of the check output which contains the
-- performance metrics (HOSTPERFDATA or SERVICEPERFDATA).
parseMetricString :: S.ByteString -> Either ParserError MetricList
parseMetricString = completeParse . parse metricLine
  where
    completeParse r = case r of
        Done _ m -> Right m
        Fail _ ctxs err -> Left $ fmtParseError ctxs err
        Partial parseRest -> completeParse (parseRest "")
