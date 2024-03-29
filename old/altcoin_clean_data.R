library('forecast')
library('glmnet')
require('plyr')

clean_data <- function(btc_data, bch_data, eos_data, eth_data, ltc_data, usdt_data, xrp_data, btc_sv_data, bitfinex_data, bitstamp_data, coinbase_data, kraken_data) {
  btc_data = rename(btc_data,c("Price"="btc_price","Open"="btc_open","High"="btc_high", "Low"="btc_low", "Vol."="btc_vol", "Change.."="btc_change"))
  bch_data = rename(bch_data,c("Price"="bch_price","Open"="bch_open","High"="bch_high", "Low"="bch_low", "Vol."="bch_vol", "Change.."="bch_change"))
  eos_data = rename(eos_data,c("Price"="eos_price","Open"="eos_open","High"="eos_high", "Low"="eos_low", "Vol."="eos_vol", "Change.."="eos_change"))
  eth_data = rename(eth_data,c("Price"="eth_price","Open"="eth_open","High"="eth_high", "Low"="eth_low", "Vol."="eth_vol", "Change.."="eth_change"))
  ltc_data = rename(ltc_data,c("Price"="ltc_price","Open"="ltc_open","High"="ltc_high", "Low"="ltc_low", "Vol."="ltc_vol", "Change.."="ltc_change"))
  usdt_data = rename(usdt_data,c("Price"="usdt_price","Open"="usdt_open","High"="usdt_high", "Low"="usdt_low", "Vol."="usdt_vol", "Change.."="usdt_change"))
  xrp_data = rename(xrp_data,c("Price"="xrp_price","Open"="xrp_open","High"="xrp_high", "Low"="xrp_low", "Vol."="xrp_vol", "Change.."="xrp_change"))
  btc_sv_data = rename(btc_sv_data,c("Price"="btcsv_price","Open"="btcsv_open","High"="btcsv_high", "Low"="btcsv_low", "Vol."="btcsv_vol", "Change.."="btcsv_change"))
  
  bitfinex_data = rename(bitfinex_data,c("Open"="bitfinex_open","High"="bitfinex_high", "Low"="bitfinex_low", "Close"= "bitfinex_close", "Volume.From"= "bitfinex_volume_from", "Volume.To"= "bitfinex_volume_To" ))
  bitstamp_data = rename(bitstamp_data,c("Open"="bitstamp_open","High"="bitstamp_high", "Low"="bitstamp_low", "Close"= "bitstamp_close", "Volume.From"= "bitstamp_volume_from", "Volume.To"= "bitstamp_volume_To" ))
  coinbase_data = rename(coinbase_data,c("Open"="coinbase_open","High"="coinbase_high", "Low"="coinbase_low", "Close"= "coinbase_close", "Volume.From"= "coinbase_volume_from", "Volume.To"= "coinbase_volume_To" ))
  kraken_data = rename(kraken_data,c("Open"="kraken_open","High"="kraken_high", "Low"="kraken_low", "Close"= "kraken_close", "Volume.From"= "kraken_volume_from", "Volume.To"= "kraken_volume_To" ))
  bitfinex_data = bitfinex_data[-c(2, 3, 4, 5)]
  bitstamp_data = bitstamp_data[-c(2, 3, 4, 5)]
  coinbase_data = coinbase_data[-c(2, 3, 4, 5)]
  kraken_data = kraken_data[-c(2, 3, 4, 5)]
  
  temp_data <- merge(btc_data, bch_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, eos_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, eth_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, ltc_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, usdt_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, xrp_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, btc_sv_data, by = 'Date', all = TRUE)
  
  temp_data$Date <- as.character(strptime(as.character(temp_data$Date), "%B %d, %Y"))
  temp_data <- merge(temp_data, bitfinex_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, bitstamp_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, coinbase_data, by = 'Date', all = TRUE)
  temp_data <- merge(temp_data, kraken_data, by = 'Date', all = TRUE)
  
  temp_data = temp_data[order(as.Date(temp_data$Date)),]
  new_data = temp_data

  new_data$btc_vol = sub("-", "0", new_data$btc_vol, fixed = TRUE)
  new_data$bch_vol = sub("-", "0", new_data$bch_vol, fixed = TRUE)
  new_data$btcsv_vol = sub("-", "0", new_data$btcsv_vol, fixed = TRUE)
  new_data$eos_vol = sub("-", "0", new_data$eos_vol, fixed = TRUE)
  new_data$eth_vol = sub("-", "0", new_data$eth_vol, fixed = TRUE)
  new_data$ltc_vol = sub("-", "0", new_data$ltc_vol, fixed = TRUE)
  new_data$usdt_vol = sub("-", "0", new_data$usdt_vol, fixed = TRUE)
  new_data$xrp_vol = sub("-", "0", new_data$xrp_vol, fixed = TRUE)
  new_data[,-1] = as.data.frame(lapply(new_data[,-1], function(y) sub(",", "", y, fixed = TRUE)))
  new_data[,-1] = as.data.frame(lapply(new_data[,-1], function(y) sub("%", "", y, fixed = TRUE)))
  new_data[,-1] = as.data.frame(lapply(new_data[,-1], function(y) sub("K", "e3", y, fixed = TRUE)))
  new_data[,-1] = as.data.frame(lapply(new_data[,-1], function(y) sub("M", "e6", y, fixed = TRUE)))
  new_data[,-1] = as.data.frame(lapply(new_data[,-1], function(y) sub("B", "e9", y, fixed = TRUE)))
  new_data[,-1] = as.data.frame(lapply(new_data[,-1], function(y) as.numeric(levels(y))[y]))
  
  new_data[is.na(new_data)] <- 0
  return(new_data)
}

btc_data <- read.csv("btc_data_csv/testing_data_predict/Bitcoin Historical Data - Investing.com.csv") # Jul 18, 2010
bch_data <- read.csv("btc_data_csv/testing_data_predict/Bitcoin Cash Historical Data - Investing.com.csv") # Aug 03, 2017
eos_data <- read.csv("btc_data_csv/testing_data_predict/EOS Historical Data - Investing.com.csv") # Jul 02, 2017
eth_data <- read.csv("btc_data_csv/testing_data_predict/Ethereum Historical Data - Investing.com.csv") # Mar 10, 2016
ltc_data <- read.csv("btc_data_csv/testing_data_predict/Litecoin Historical Data - Investing.com.csv") # Aug 24, 2016
usdt_data <- read.csv("btc_data_csv/testing_data_predict/Tether Historical Data - Investing.com.csv") # Apr 14, 2017
xrp_data <- read.csv("btc_data_csv/testing_data_predict/XRP Historical Data - Investing.com.csv") # Jan 22, 2015
btc_sv_data <- read.csv("btc_data_csv/testing_data_predict/Bitcoin SV Historical Data - Investing.com.csv") # Nov 19, 2018

bitfinex_data <- read.csv("btc_data_csv/testing_data_predict/Bitfinex_BTCUSD_d.csv")[c(-2)]
bitstamp_data <- read.csv("btc_data_csv/testing_data_predict/Bitstamp_BTCUSD_d.csv")[c(-2)]
coinbase_data <- read.csv("btc_data_csv/testing_data_predict/Coinbase_BTCUSD_d.csv")[c(-2)]
kraken_data <- read.csv("btc_data_csv/testing_data_predict/Kraken_BTCUSD_d.csv")[c(-2)]

test_data = clean_data(btc_data, bch_data, eos_data, eth_data, ltc_data, usdt_data, xrp_data, btc_sv_data, bitfinex_data, bitstamp_data, coinbase_data, kraken_data) 
write.csv(test_data, file = "imp_coin_test.csv")

btc_data <- read.csv("btc_data_csv/investing.com//Bitcoin Historical Data - Investing.com.csv") # Jul 18, 2010
bch_data <- read.csv("btc_data_csv/investing.com/Bitcoin Cash Historical Data - Investing.com.csv") # Aug 03, 2017
eos_data <- read.csv("btc_data_csv/investing.com/EOS Historical Data - Investing.com.csv") # Jul 02, 2017
eth_data <- read.csv("btc_data_csv/investing.com/Ethereum Historical Data - Investing.com.csv") # Mar 10, 2016
ltc_data <- read.csv("btc_data_csv/investing.com/Litecoin Historical Data - Investing.com.csv") # Aug 24, 2016
usdt_data <- read.csv("btc_data_csv/investing.com/Tether Historical Data - Investing.com.csv") # Apr 14, 2017
xrp_data <- read.csv("btc_data_csv/investing.com/XRP Historical Data - Investing.com.csv") # Jan 22, 2015
btc_sv_data <- read.csv("btc_data_csv/investing.com/Bitcoin SV Historical Data - Investing.com.csv") # Nov 19, 2018

bitfinex_data <- read.csv("btc_data_csv/exchange//Bitfinex_BTCUSD_d.csv")[c(-2)]
bitstamp_data <- read.csv("btc_data_csv/exchange/Bitstamp_BTCUSD_d.csv")[c(-2)]
coinbase_data <- read.csv("btc_data_csv/exchange/Coinbase_BTCUSD_d.csv")[c(-2)]
kraken_data <- read.csv("btc_data_csv/exchange/Kraken_BTCUSD_d.csv")[c(-2)]

data = clean_data(btc_data, bch_data, eos_data, eth_data, ltc_data, usdt_data, xrp_data, btc_sv_data, bitfinex_data, bitstamp_data, coinbase_data, kraken_data) 
write.csv(data, file = "imp_coin.csv")

btc_data <- read.csv("btc_data_csv/bitfinex_data/BTC_USD Bitfinex Historical Data.csv") # Jul 18, 2010
bch_data <- read.csv("btc_data_csv/bitfinex_data/BCH_USD Bitfinex Historical Data.csv") # Aug 03, 2017
eos_data <- read.csv("btc_data_csv/bitfinex_data/EOS_USD Bitfinex Historical Data.csv") # Jul 02, 2017
eth_data <- read.csv("btc_data_csv/bitfinex_data/ETH_USD Bitfinex Historical Data.csv") # Mar 10, 2016
ltc_data <- read.csv("btc_data_csv/bitfinex_data/LTC_USD Bitfinex Historical Data.csv") # Aug 24, 2016
xrp_data <- read.csv("btc_data_csv/bitfinex_data/XRP_USD Bitfinex Historical Data.csv") # Jan 22, 2015
btc_sv_data <- read.csv("btc_data_csv/bitfinex_data/BCHSV_USD Bitfinex Historical Data.csv") # Nov 19, 2018

data = clean_data(btc_data, bch_data, eos_data, eth_data, ltc_data, NULL, xrp_data, btc_sv_data, NULL, NULL, NULL, NULL)
write.csv(data, file = "bitfinex_coin.csv")
