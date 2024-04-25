module Presets where

--presetCommand
presetCommand:: String ->[String]
presetCommand ".bond"   = ["^IRX","2YY=F","^FVX","^TNX", "^TYX"]
presetCommand ".market" =[ "^DJI", "^GSPC", "^RUT", "^IXIC", "^VIX"]
presetCommand ".grain"  =[ "ZC=F", "ZS=F", "ZO=F", "ZR=F"]
presetCommand ".metal"  =["GC=F", "SI=F", "PL=F", "HG=F", "PA=F", "ALI=F", "HRC=F"]
presetCommand ".fx"     =["GBPUSD=X", "JPY=X", "EURUSD=X", "CNY=X", "CAD=X", "MXN=X", "GC=F", "BTC-USD", "DX-Y.NYB"]
presetCommand ".currency"=["GBPUSD=X", "JPY=X", "EURUSD=X", "CNY=X", "CAD=X", "MXN=X", "GC=F", "BTC-USD", "DX-Y.NYB"]
presetCommand ".oil"    =["CL=F", "BZ=F", "NG=F", "RB=F"]
presetCommand ".crypto" =["BTC-USD","ETH-USD","DOGE-USD"]
presetCommand ".asia"   =["^N225", "^HSI","^KS11" ,"000001.SS", "^AXJO"]
presetCommand ".vol"    =["^VIX", "^VIX9D", "^VIX3M", "^VIX6M"]
presetCommand ".soft"   =["CC=F", "KC=F", "OJ=F" , "SB=F"]
presetCommand ".futures" =[ "ES=F", "NQ=F", "YM=F", "RTY=F"]
presetCommand _         = []

--conversion nicknames
presetConvert::String -> String
presetConvert "ES=F" = "S&P 500"
presetConvert "RTY=F" = "Russell 2000"
presetConvert "YM=F" = "Mini-Dow"
presetConvert "NQ=F" = "Nasdaq 100"
presetConvert "^DJI"  = "Dow"
presetConvert "^GSPC" = "S&P 500"
presetConvert "^RUT"  = "Russell 2000"
presetConvert "^IXIC" = "Nasdaq"
presetConvert "^VIX"  = "Vix"
presetConvert "ZC=F"  = "Corn"
presetConvert "ZS=F"  = "Soybean"
presetConvert "ZO=F"  = "Oats"
presetConvert "ZR=F"  = "Rice"
presetConvert "GC=F"  = "AU"
presetConvert "SI=F"  = "SI"
presetConvert "PL=F"  = "PL"
presetConvert "HG=F"  = "CU"
presetConvert "PA=F"  = "PA"
presetConvert "ALI=F" = "AL"
presetConvert "HRC=F" = "HOT ROLLED STEEL MIDWEST"
presetConvert "GBPUSD=X" = "GBPUSD"
presetConvert "JPY=X" = "USDJPY"
presetConvert "EURUSD=X" = "EURUSD"
presetConvert "CNY=X" ="USDCNY"
presetConvert "CAD=X" = "USDCAD"
presetConvert "MXN=X" = "USDMXN"
presetConvert "CL=F"  = "WTI Oil"
presetConvert "BZ=F"  = "Brent Oil"
presetConvert "NG=F"  = "Nat Gas"
presetConvert "RB=F"  = "RBOB Gasoline"
presetConvert "^N225" = "Nikkei"
presetConvert "^HSI"  = "Hang Seng"
presetConvert "^STI"  = "STI Index"
presetConvert "DX-Y.NYB" = "DXY"
presetConvert "^KS11" = "Kopsi"
presetConvert "^AXJO" = "ASX 200"
presetConvert "CC=F"  = "Cocao"
presetConvert "KC=F"  = "Coffee"
presetConvert "LBS=F" = "Lumber"
presetConvert "OJ=F"  = "OrangeJ"
presetConvert "SB=F"  = "Sugar"
presetConvert "000001.SS" = "Shanghai"
presetConvert "^TYX" = "30Y "
presetConvert "^TNX" = "10Y "
presetConvert "2YY=F" = "2Y "
presetConvert "^IRX" = "13W "
presetConvert "^FVX" = "5Y "
presetConvert a = a






