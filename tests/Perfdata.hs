{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow               ((&&&))
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString  (parseOnly)
import           Data.ByteString.Char8       (ByteString, unpack)
import           Data.Either
import           Data.Either.Utils
import           Data.Nagios.Perfdata
import           Data.Nagios.Perfdata.Metric
import           Test.Hspec
import           Test.HUnit

defaultTemplateData :: ByteString
defaultTemplateData =
    "DATATYPE::SERVICEPERFDATA\tTIMET::1388445486\tHOSTNAME::node4.example.com\tSERVICEDESC::diskio\tSERVICEPERFDATA::overall_read_count=32294c;;;;; overall_write_count=6497849c;;;;; overall_read_bytes=468035584c;;;;; overall_write_bytes=40444282880c;;;;; dm-1_read_count=1343c;;;;; dm-1_write_count=14379c;;;;; dm-1_read_bytes=5500928c;;;;; dm-1_write_bytes=58896384c;;;;; vda1_read_count=235c;;;;; vda1_write_count=102c;;;;; vda1_read_bytes=937984c;;;;; vda1_write_bytes=141312c;;;;; dm-0_read_count=17308c;;;;; dm-0_write_count=4922832c;;;;; dm-0_read_bytes=227918848c;;;;; dm-0_write_bytes=20163174400c;;;;; vda2_read_count=13408c;;;;; vda2_write_count=1560536c;;;;; vda2_read_bytes=233677824c;;;;; vda2_write_bytes=20222070784c;;;;;\tSERVICECHECKCOMMAND::check_diskio\tHOSTSTATE::UP\tHOSTSTATETYPE::HARD\tSERVICESTATE::OK\tSERVICESTATETYPE::HARD"

cpuTemplateData :: ByteString
cpuTemplateData =
    "DATATYPE::SERVICEPERFDATA\tTIMET::1388445486\tHOSTNAME::some.example.com\tSERVICEDESC::cpu\tSERVICEPERFDATA::CpuUtilisation=0.00%;;;0;100; CpuUser=1092132c;;;;;\tSERVICESTATE::OK\tHOSTSTATE::OK"

ntpTemplateData :: ByteString
ntpTemplateData =
    "DATATYPE::SERVICEPERFDATA\tTIMET::1404443722\tHOSTNAME::example1\tSERVICEDESC::ntp\tSERVICEPERFDATA::offset=0.001416s;60.000000;120.000000;\tSERVICECHECKCOMMAND::check_ntp_by_nrpe\tHOSTSTATE::UP\tHOSTSTATETYPE::HARD\tSERVICESTATE::OK\tSERVICESTATETYPE::HARD"

defaultModGearmanResult :: ByteString
defaultModGearmanResult =
    "\"host_name=kvm33.syd1.anchor.net.au\ncore_start_time=1405044854.0\nstart_time=1405044867.223223\nfinish_time=1405044867.347834\nreturn_code=0\nexited_ok=1\nservice_description=procs\noutput=PROCS OK: 796 processes |procs=796;;\\n\n\n\n\n\""

suite :: Spec
suite = do
    describe "perfdataFromDefaultTemplate" $ do
        it "extracts perfdata from Nagios perfdata template"  $
            perfdataFromDefaultTemplate defaultTemplateData `shouldSatisfy` isRight
        it "extracts timestamp correctly" $
            (_perfdataTimestamp . fromRight $ perfdataFromDefaultTemplate defaultTemplateData) @?= 1388445486000000000
        it "handles empty threshold fields correctly" $ do
            let datum = perfdataFromDefaultTemplate cpuTemplateData
            datum `shouldSatisfy` isRight
            liftIO . print $ datum
            let (_:m:_) = _perfdataMetrics . fromRight $ datum
            _metricName m @?= "CpuUser"
        it "handles full threshold fields correctly" $ do
            let datum = perfdataFromDefaultTemplate ntpTemplateData
            datum `shouldSatisfy` isRight
            {-liftIO . print $ datum-}
            let (metric:_) = _perfdataMetrics . fromRight $ datum
            _metricName metric @?= "offset"
        it "parses known values correctly" $ do
            let datum = perfdataFromDefaultTemplate ntpTemplateData
            datum `shouldSatisfy` isRight
            {-liftIO . print $ datum-}
            let (m:_) = _perfdataMetrics . fromRight $ datum
            metricValueDefault (-1.0) m @?= 0.001416
            isUnknownMetricValue m @?= False
    describe "perfdataFromModGearmanResult" $
        it "extracts perfdata from Nagios check result"  $ do
            let datum = perfdataFromGearmanResult defaultModGearmanResult
            datum `shouldSatisfy` isRight
            {-liftIO . print $ datum-}
            let (m:_) = _perfdataMetrics . fromRight $ datum
            _metricName m @?= "procs"
    describe "UOM conversions+parsing" $ do
        it "converts all strings to UOMs correctly" $
            traverse (parseOnly parseUOM) testStrings @?= Right testUOMs
        it "converts all UOMs to strings correctly" $ do
            let len = length testUOMs - 2
            map show (take len testUOMs) @?= map unpack (take len testStrings)
            show UnknownUOM @?= "?"
        it "correctly verifies when metrics use a base SI unit" $ do
            isMetricBase (simpleMetric (Just 42.00) Second     ) @?= True
            isMetricBase (simpleMetric (Just 12.34) Millisecond) @?= False
            isMetricBase (simpleMetric (Just 52.13) Byte       ) @?= True
            isMetricBase (simpleMetric (Just 52.13) Counter    ) @?= True
            isMetricBase (simpleMetric (Just 52.13) Gigabyte   ) @?= False
        it "correctly converts metrics to use base SI units" $ do
            let metrics = [simpleMetric Nothing Second, simpleMetric (Just 123) Second, simpleMetric (Just 0.5) Kilobyte]
            let converted = map convertMetricToBase metrics
            let expectedValues = [Nothing, Just 123, Just 500]
            let expectedUOMs = [Second, Second, Byte]
            let (values, uoms) = unzip $ map  (_metricValue &&& _metricUOM) converted
            values @?= expectedValues
            uoms   @?= expectedUOMs

  where
    simpleMetric mValue uom = Metric "" mValue uom Nothing Nothing Nothing Nothing
    testStrings = [   "s",        "ms",        "us",       "",     "%",  "B",     "KB",     "MB",     "GB",     "TB",     "c",        "x", "maryhadalittlelamb"]
    testUOMs    = [Second, Millisecond, Microsecond, NullUnit, Percent, Byte, Kilobyte, Megabyte, Gigabyte, Terabyte, Counter, UnknownUOM,           UnknownUOM]

main :: IO ()
main = hspec suite

