module Presets where

--presetCommand
presetCommand:: String ->[String]
presetCommand "!bond"   = ["2YY=F","10Y=F", "30Y=F"]
presetCommand "!market" =[ "^DJI", "^GSPC", "^RUT", "^IXIC", "^VIX"]
presetCommand "!grain"  =[ "ZC=F", "ZS=F", "ZO=F", "ZR=F"]
presetCommand "!metal"  =["GC=F", "SI=F", "PL=F", "HG=F", "PA=F", "ALI=F", "HRC=F"]
presetCommand "!fx"     =["GBPUSD=X", "JPY=X", "EURUSD=X", "USDCNY", "CAD=X", "MXN=X", "GC=F", "BTC-USD"]
presetCommand "!oil"    =["CL=F", "BZ=F", "NG=F", "RB=F"]
presetCommand "!crypto" =["BTC-USD","ETH-USD","DOGE-USD"]
presetCommand "!asia"   =["^N225", "^HSI","^STI" , "^AXJO"]
presetCommand "!vol"    =["^VIX", "^VIX9D", "^VIX3M", "^VIX6M"]
presetCommand "!soft"   =["CC=F", "KC=F", "OJ=F" , "SB=F"]
presetCommand "!futures" =[ "ES=F", "NQ=F", "YM=F", "RTY=F"]
presetCommand _         = []

--conversion nicknames
presetConvert::String -> String
presetConvert "ES=F" = "S&P 500"
presetConvert "RTY=F" = "Russel 2000"
presetConvert "YM=F" = "Mini-Dow"
presetConvert "NQ=F" = "Nasdaq 100"
presetConvert "^DJI"  = "Dow"
presetConvert "^GSPC" = "Sp500"
presetConvert "^RUT"  = "Russel"
presetConvert "^IXIC" = "Nasdaq"
presetConvert "^VIX"  = "Vix"
presetConvert "ZC=F"  = "Corn"
presetConvert "ZS=F"  = "Soybean"
presetConvert "ZO=F"  = "Oats"
presetConvert "ZR=F"  = "Rice"
presetConvert "GC=F"  = "Gold"
presetConvert "SI=F"  = "Silver"
presetConvert "PL=F"  = "Platinum"
presetConvert "HG=F"  = "Copper"
presetConvert "PA=F"  = "Palladium"
presetConvert "ALI=F" = "Aluminum"
presetConvert "HRC=F" = "HOT ROLLED STEEL MIDWEST"
presetConvert "GBPUSD=X" = "GBPUSD"
presetConvert "JPY=X" = "JPYUSD"
presetConvert "EURUSD=X" = "EURUSD"
presetConvert "USDCNY" ="USDCNY"
presetConvert "CAD=X" = "CADUSD"
presetConvert "MXN=X" = "MXNUSD"
presetConvert "CL=F"  = "WTI Oil"
presetConvert "BZ=F"  = "Brent Oil"
presetConvert "NG=F"  = "Nat Gas"
presetConvert "RB=F"  = "RBOB Gasoline"
presetConvert "^N225" = "Nikkei"
presetConvert "^HSI"  = "Hang Seng"
presetConvert "^STI"  = "STI Index"
presetConvert "^AXJO" = "ASX 200"
presetConvert "CC=F"  = "Cocao"
presetConvert "KC=F"  = "Coffee"
presetConvert "LBS=F" = "Lumber"
presetConvert "OJ=F"  = "OrangeJ"
presetConvert "SB=F"  = "Sugar"
presetConvert "30Y=F" = "30 Year"
presetConvert "10Y=F" = "10 Year"
presetConvert "2YY=F" = "2 Year"
presetConvert a = a






