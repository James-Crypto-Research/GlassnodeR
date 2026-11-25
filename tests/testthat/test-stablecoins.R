# Offline unit tests for stablecoin dashboard endpoints

test_that("metadata discovery functions parse responses cleanly", {
  captured <- list()

  local_mocked_bindings(
    call_glassnode_api = function(path, ...) {
      captured$path <<- path
      captured$args <<- list(...)
      if (path == "v1/metadata/assets") {
        data.frame(
          id = c("USDT", "USDC"),
          name = c("Tether", "USD Coin"),
          stringsAsFactors = FALSE
        )
      } else if (path == "v1/metadata/metrics") {
        c("/market/marketcap_stablecoins_usd", "/indicators/velocity", "/transactions/transfers_volume_sum")
      }
    },
    .package = "GlassnodeR"
  )

  stables <- get_stablecoin_assets(api_key = "k")
  expect_s3_class(stables, "tbl_df")
  expect_equal(stables$id, c("USDT", "USDC"))
  expect_equal(captured$path, "v1/metadata/assets")
  expect_equal(captured$args$filter, "asset.semantic_tags.exists(tag,tag=='stablecoin')")

  metrics <- get_metrics(asset = "USDT", api_key = "k")
  expect_s3_class(metrics, "tbl_df")
  expect_equal(names(metrics), c("path", "category", "metric"))
  expect_equal(metrics$category, c("market", "indicators", "transactions"))
  expect_equal(metrics$metric, c("marketcap_stablecoins_usd", "velocity", "transfers_volume_sum"))
})

test_that("stablecoin market and indicator functions return tidy tibbles", {
  seen_paths <- character()

  local_mocked_bindings(
    call_glassnode_api = function(path, ...) {
      seen_paths <<- c(seen_paths, path)
      data.frame(t = c(1735689600, 1735776000), v = c(1.23, 4.56))
    },
    .package = "GlassnodeR"
  )

  mcap <- get_stablecoin_marketcap(since = "2025-01-01", until = "2025-01-02", api_key = "k")
  expect_equal(names(mcap), c("date", "stablecoin_marketcap"))
  expect_s3_class(mcap$date, "Date")

  ssr <- get_ssr(since = "2025-01-01", until = "2025-01-02", api_key = "k")
  expect_equal(names(ssr), c("date", "ssr"))

  ssr_osc <- get_ssr_oscillator(since = "2025-01-01", until = "2025-01-02", api_key = "k")
  expect_equal(names(ssr_osc), c("date", "ssr_oscillator"))

  vel <- get_velocity(asset = "USDC", since = "2025-01-01", until = "2025-01-02", api_key = "k")
  expect_equal(names(vel), c("date", "velocity"))

  expect_equal(
    seen_paths,
    c(
      "v1/metrics/market/marketcap_stablecoins_usd",
      "v1/metrics/indicators/ssr",
      "v1/metrics/indicators/ssr_oscillator",
      "v1/metrics/indicators/velocity"
    )
  )
})

test_that("raw transfer and total exchange flow functions hit expected paths", {
  seen_paths <- character()
  seen_args <- list()

  local_mocked_bindings(
    call_glassnode_api = function(path, ...) {
      seen_paths <<- c(seen_paths, path)
      seen_args[[length(seen_args) + 1L]] <<- list(...)
      data.frame(t = c(1735689600), v = c(500000))
    },
    .package = "GlassnodeR"
  )

  expect_equal(names(get_transfers_count("USDT", api_key = "k")), c("date", "transfers_count"))
  expect_equal(names(get_transfers_volume_sum("USDC", currency = "USD", api_key = "k")), c("date", "transfers_volume_sum"))
  expect_equal(names(get_transfers_volume_mean("USDC", api_key = "k")), c("date", "transfers_volume_mean"))
  expect_equal(names(get_transfers_volume_median("USDC", api_key = "k")), c("date", "transfers_volume_median"))
  expect_equal(names(get_exchange_inflows_sum("USDT", exchange = "binance", api_key = "k")), c("date", "exchange_inflows_sum"))
  expect_equal(names(get_exchange_outflows_sum("USDT", exchange = "binance", api_key = "k")), c("date", "exchange_outflows_sum"))
  expect_equal(names(get_exchange_withdrawals_count("USDT", api_key = "k")), c("date", "exchange_withdrawals_count"))
  expect_equal(names(get_exchange_net_position_change(asset = "USDC", api_key = "k")), c("date", "exchange_net_position_change"))

  expect_equal(
    seen_paths,
    c(
      "v1/metrics/transactions/transfers_count",
      "v1/metrics/transactions/transfers_volume_sum",
      "v1/metrics/transactions/transfers_volume_mean",
      "v1/metrics/transactions/transfers_volume_median",
      "v1/metrics/transactions/transfers_volume_to_exchanges_sum",
      "v1/metrics/transactions/transfers_volume_from_exchanges_sum",
      "v1/metrics/transactions/transfers_from_exchanges_count",
      "v1/metrics/distribution/exchange_net_position_change"
    )
  )
  expect_equal(seen_args[[5]]$e, "binance")
  expect_equal(seen_args[[8]]$a, "USDC")
})

test_that("stablecoin address cohort and bridge functions parse cleanly", {
  local_mocked_bindings(
    call_glassnode_api = function(path, ...) {
      if (grepl("^v1/metrics/bridges/", path)) {
        tibble::tibble(
          t = c(1735689600),
          o = list(list(arbitrum = 100, optimism = 50))
        )
      } else {
        data.frame(t = c(1735689600), v = c(42000))
      }
    },
    .package = "GlassnodeR"
  )

  nz <- get_non_zero_addresses("USDT", api_key = "k")
  expect_equal(names(nz), c("date", "non_zero_addresses"))

  whales <- get_addresses_min_usd("USDC", threshold = "1m", api_key = "k")
  expect_equal(names(whales), c("date", "addresses_min_1m_usd"))
  expect_error(get_addresses_min_usd("USDC", threshold = "invalid", api_key = "k"))

  bw <- get_bridge_withdrawals("USDC", api_key = "k")
  expect_equal(names(bw), c("date", "withdrawals_arbitrum", "withdrawals_optimism"))

  bn <- get_bridge_net_flows("USDC", api_key = "k")
  expect_equal(names(bn), c("date", "net_volume_arbitrum", "net_volume_optimism"))

  bt <- get_bridge_tvl("USDC", api_key = "k")
  expect_equal(names(bt), c("date", "tvl_by_chain_arbitrum", "tvl_by_chain_optimism"))

  br <- get_bridge_tvl_relative("USDC", api_key = "k")
  expect_equal(names(br), c("date", "tvl_relative_arbitrum", "tvl_relative_optimism"))
})
