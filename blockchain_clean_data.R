require('plyr')

clean_data <- function(market_price, avg_block_size, estimated_transaction_volume_usd, estimated_transaction_volume, output_volume, n_transactions, miners_revenuem, difficulty, market_cap, trade_volume, cost_per_transaction, transaction_fees){
  market_price = rename(market_price,c("V1"="date","V2"="market_price"))
  avg_block_size = rename(avg_block_size,c("V1"="date","V2"="avg_block_size"))
  estimated_transaction_volume_usd = rename(estimated_transaction_volume_usd,c("V1"="date","V2"="estimated_transaction_volume_usd"))
  estimated_transaction_volume = rename(estimated_transaction_volume,c("V1"="date","V2"="estimated_transaction_volume"))
  output_volume = rename(output_volume,c("V1"="date","V2"="output_volume"))
  n_transactions = rename(n_transactions,c("V1"="date","V2"="n_transactions"))
  miners_revenue = rename(miners_revenue,c("V1"="date","V2"="miners_revenue"))
  difficulty = rename(difficulty,c("V1"="date","V2"="difficulty"))
  market_cap = rename(market_cap,c("V1"="date","V2"="market_cap"))
  trade_volume = rename(trade_volume,c("V1"="date","V2"="trade_volume"))
  cost_per_transaction = rename(cost_per_transaction,c("V1"="date","V2"="cost_per_transaction"))
  transaction_fees = rename(transaction_fees,c("V1"="date","V2"="transaction_fees"))

  temp_data=NULL
  temp_data <- merge(market_price, avg_block_size, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, estimated_transaction_volume_usd, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, estimated_transaction_volume, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, output_volume, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, n_transactions, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, miners_revenue, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, difficulty, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, market_cap, by = 'date', all = TRUE)
  # temp_data <- merge(temp_data, trade_volume, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, cost_per_transaction, by = 'date', all = TRUE)
  temp_data <- merge(temp_data, transaction_fees, by = 'date', all = TRUE)
  new_data = temp_data
  return(new_data)
}

market_price <- read.csv("https://api.blockchain.info/charts/market-price?timespan=all&format=csv", header=FALSE)
avg_block_size <- read.csv("https://api.blockchain.info/charts/avg-block-size?timespan=all&format=csv", header=FALSE)
estimated_transaction_volume_usd <- read.csv("https://api.blockchain.info/charts/estimated-transaction-volume-usd?timespan=all&format=csv", header=FALSE)
estimated_transaction_volume <- read.csv("https://api.blockchain.info/charts/estimated-transaction-volume?timespan=all&format=csv", header=FALSE)
output_volume <- read.csv("https://api.blockchain.info/charts/output-volume?timespan=all&format=csv", header=FALSE)
n_transactions <- read.csv("https://api.blockchain.info/charts/n-transactions?timespan=all&format=csv", header=FALSE)
miners_revenue <- read.csv("https://api.blockchain.info/charts/miners-revenue?timespan=all&format=csv", header=FALSE)
difficulty <- read.csv("https://api.blockchain.info/charts/difficulty?timespan=all&format=csv", header=FALSE)
market_cap <- read.csv("https://api.blockchain.info/charts/market-cap?timespan=all&format=csv", header=FALSE)
trade_volume <- read.csv("https://api.blockchain.info/charts/trade-volume?timespan=all&format=csv", header=FALSE)
cost_per_transaction <- read.csv("https://api.blockchain.info/charts/cost-per-transaction?timespan=all&format=csv", header=FALSE)
transaction_fees <- read.csv("https://api.blockchain.info/charts/transaction-fees?timespan=all&format=csv", header=FALSE)

data = clean_data(market_price, avg_block_size, estimated_transaction_volume_usd, estimated_transaction_volume, output_volume, n_transactions, miners_revenuem, difficulty, market_cap, trade_volume, cost_per_transaction, transaction_fees)

write.csv(head(data, 1826), file = "blockchain_data_test.csv")
write.csv(head(data, -1826), file = "blockchain_data_test.csv")
