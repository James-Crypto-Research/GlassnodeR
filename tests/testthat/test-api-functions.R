# Test all exported get_* functions against the live Glassnode API
# Uses a narrow date range to minimize data transfer and API usage

# Skip all tests if no API key is available
skip_if_no_api_key <- function() {
  key <- Sys.getenv("GN_API_KEY")
  if (is.null(key) || nchar(key) == 0) {
    skip("GN_API_KEY not set")
  }
}

# Common test parameters - just 1 day of data
since_date <- "2025-01-01"
until_date <- "2025-01-02"

# Helper to check basic tibble structure
expect_valid_tibble <- function(result, expected_cols = 2) {
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0, info = "Result should have at least one row")
  expect_true("date" %in% names(result), info = "Result should have a 'date' column")
}

# ---- Addresses ----

test_that("get_addresses returns valid data", {
  skip_if_no_api_key()
  result <- get_addresses(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_addresses_profit returns valid data", {
  skip_if_no_api_key()
  result <- get_addresses_profit(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_addresses_loss returns valid data", {
  skip_if_no_api_key()
  result <- get_addresses_loss(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Blockchain ----

test_that("get_block_height returns valid data", {
  skip_if_no_api_key()
  result <- get_block_height(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_block_interval_mean returns valid data", {
  skip_if_no_api_key()
  result <- get_block_interval_mean(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_block_interval_median returns valid data", {
  skip_if_no_api_key()
  result <- get_block_interval_median(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_block_size_mean returns valid data", {
  skip_if_no_api_key()
  result <- get_block_size_mean(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_blocks_count returns valid data", {
  skip_if_no_api_key()
  result <- get_blocks_count(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Market ----

test_that("get_closing_price returns valid data", {
  skip_if_no_api_key()
  result <- get_closing_price(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_btc_dominance returns valid data", {
  skip_if_no_api_key()
  result <- get_btc_dominance(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_marketcap returns valid data", {
  skip_if_no_api_key()
  result <- get_marketcap(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mvrv returns valid data", {
  skip_if_no_api_key()
  result <- get_mvrv(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mvrv_z_score returns valid data", {
  skip_if_no_api_key()
  result <- get_mvrv_z_score(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_realized_price returns valid data", {
  skip_if_no_api_key()
  result <- get_realized_price(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_realized_cap returns valid data", {
  skip_if_no_api_key()
  result <- get_realized_cap(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_delta_cap returns valid data", {
  skip_if_no_api_key()
  result <- get_delta_cap(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_price_drawdown_ath returns valid data", {
  skip_if_no_api_key()
  result <- get_price_drawdown_ath(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lth_mvrv returns valid data", {
  skip_if_no_api_key()
  result <- get_lth_mvrv(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_median_mvrv returns valid data", {
  skip_if_no_api_key()
  result <- get_median_mvrv(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_price_ohlc returns valid data", {
  skip_if_no_api_key()
  result <- get_price_ohlc(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Supply ----

test_that("get_liquid_supply returns valid data", {
  skip_if_no_api_key()
  result <- get_liquid_supply(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_illiquid_supply returns valid data", {
  skip_if_no_api_key()
  result <- get_illiquid_supply(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lth_supply returns valid data", {
  skip_if_no_api_key()
  result <- get_lth_supply(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_sth_supply returns valid data", {
  skip_if_no_api_key()
  result <- get_sth_supply(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_supply_in_profit returns valid data", {
  skip_if_no_api_key()
  result <- get_supply_in_profit(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_supply_in_loss returns valid data", {
  skip_if_no_api_key()
  result <- get_supply_in_loss(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_percent_supply_in_profit returns valid data", {
  skip_if_no_api_key()
  result <- get_percent_supply_in_profit(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_circulable_supply returns valid data", {
  skip_if_no_api_key()
  result <- get_circulable_supply(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_provably_lost_coins returns valid data", {
  skip_if_no_api_key()
  result <- get_provably_lost_coins(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_supply_contracts returns valid data", {
  skip_if_no_api_key()
  result <- get_supply_contracts(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_exchange_balance returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_balance(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Mining ----

test_that("get_revenue_miners returns valid data", {
  skip_if_no_api_key()
  result <- get_revenue_miners(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_hash_rate returns valid data", {
  skip_if_no_api_key()
  result <- get_hash_rate(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mining_difficulty returns valid data", {
  skip_if_no_api_key()
  result <- get_mining_difficulty(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mining_revenue_fees returns valid data", {
  skip_if_no_api_key()
  result <- get_mining_revenue_fees(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Fees ----

test_that("get_fees_mean returns valid data", {
  skip_if_no_api_key()
  result <- get_fees_mean(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_fees_median returns valid data", {
  skip_if_no_api_key()
  result <- get_fees_median(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_fees_total returns valid data", {
  skip_if_no_api_key()
  result <- get_fees_total(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_exchange_fees returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_fees(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_exchange_fee_dominance returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_fee_dominance(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_exchange_fees_mean returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_fees_mean(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_fee_ratio_multiple returns valid data", {
  skip_if_no_api_key()
  result <- get_fee_ratio_multiple(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_gas_price_mean returns valid data", {
  skip_if_no_api_key()
  result <- get_gas_price_mean(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_gas_price_median returns valid data", {
  skip_if_no_api_key()
  result <- get_gas_price_median(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Distribution ----

test_that("get_exchange_net_position_change returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_net_position_change(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_miner_balance returns valid data", {
  skip_if_no_api_key()
  result <- get_miner_balance(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_miner_net_position_change returns valid data", {
  skip_if_no_api_key()
  result <- get_miner_net_position_change(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_gini_coefficient returns valid data", {
  skip_if_no_api_key()
  result <- get_gini_coefficient(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_herfindahl_index returns valid data", {
  skip_if_no_api_key()
  result <- get_herfindahl_index(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_hodl_wave returns valid data", {
  skip_if_no_api_key()
  result <- get_hodl_wave(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_percent_top_balance returns valid data", {
  skip_if_no_api_key()
  result <- get_percent_top_balance(asset = "ETH", since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_percent_exchange_balance returns valid data", {
  skip_if_no_api_key()
  result <- get_percent_exchange_balance(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_all_exchange_balance returns valid data", {
  skip_if_no_api_key()
  result <- get_all_exchange_balance(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Transactions ----

test_that("get_transactions_count returns valid data", {
  skip_if_no_api_key()
  result <- get_transactions_count(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_transactions_rate returns valid data", {
  skip_if_no_api_key()
  result <- get_transactions_rate(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_exchange_inflows returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_inflows(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_exchange_outflows returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_outflows(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_exchange_netflows returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_netflows(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_exchange_deposits_count returns valid data", {
  skip_if_no_api_key()
  result <- get_exchange_deposits_count(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_volume_adjusted_mean returns valid data", {
  skip_if_no_api_key()
  result <- get_volume_adjusted_mean(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_volume_adjusted_median returns valid data", {
  skip_if_no_api_key()
  result <- get_volume_adjusted_median(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_volume_adjusted_total returns valid data", {
  skip_if_no_api_key()
  result <- get_volume_adjusted_total(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_volume_entity_adjusted returns valid data", {
  skip_if_no_api_key()
  result <- get_volume_entity_adjusted(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Lightning Network ----

test_that("get_lightning_network returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_network(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_base_fee returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_base_fee(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_network_capacity returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_network_capacity(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_channel_size_mean returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_channel_size_mean(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_channel_size_median returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_channel_size_median(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_fee_rate returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_fee_rate(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_gini_capacity returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_gini_capacity(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_gini_channels returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_gini_channels(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_node_connectivity returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_node_connectivity(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_channels_count returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_channels_count(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_lightning_nodes_count returns valid data", {
  skip_if_no_api_key()
  result <- get_lightning_nodes_count(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- Mempool ----

test_that("get_mempool_count returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_count(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_size returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_size(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_fees returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_fees(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_value returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_value(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_fees_average_relative returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_fees_average_relative(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_fees_median_relative returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_fees_median_relative(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_value_distribution returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_value_distribution(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_fees_distribution returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_fees_distribution(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_txs_count_distribution returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_txs_count_distribution(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_mempool_txs_size_distribution returns valid data", {
  skip_if_no_api_key()
  result <- get_mempool_txs_size_distribution(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

# ---- DeFi ----

test_that("get_tvl returns valid data", {
  skip_if_no_api_key()
  result <- get_tvl(since = since_date, until = until_date)
  expect_valid_tibble(result)
})

test_that("get_bridge_deposits returns valid data", {
  skip_if_no_api_key()
  result <- get_bridge_deposits(since = since_date, until = until_date)
  expect_valid_tibble(result)
})
