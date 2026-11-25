# Offline unit tests for core utilities and address helpers

test_that("sanitize_date converts strings, Dates, and POSIXct to UTC unix timestamps", {
  expected <- as.numeric(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"))

  expect_equal(GlassnodeR:::sanitize_date("2025-01-01"), expected)
  expect_equal(GlassnodeR:::sanitize_date(as.Date("2025-01-01")), expected)
  expect_equal(
    GlassnodeR:::sanitize_date(as.POSIXct("2025-01-01 00:00:00", tz = "UTC")),
    expected
  )
  expect_error(
    GlassnodeR:::sanitize_date("not-a-date"),
    "Unable to convert input to a valid date"
  )
})

test_that("make_params sets defaults, drops NULLs, and sanitizes dates", {
  params <- GlassnodeR:::make_params(
    a = "USDC",
    s = "2025-01-01",
    u = NULL,
    api_key = "test-key"
  )

  expect_type(params, "list")
  expect_equal(params$a, "USDC")
  expect_equal(params$i, "24h")
  expect_equal(params$f, "json")
  expect_equal(params$timestamp_format, "unix")
  expect_equal(params$api_key, "test-key")
  expect_false("u" %in% names(params))
  expect_equal(params$s, as.numeric(as.POSIXct("2025-01-01", tz = "UTC")))

  params_1h <- GlassnodeR:::make_params(a = "USDT", i = "1h", api_key = "k")
  expect_equal(params_1h$i, "1h")
})

test_that("get_default_api_key prefers GN_API_KEY and falls back to GLASSNODE_API_KEY", {
  old_gn <- Sys.getenv("GN_API_KEY", unset = NA)
  old_glass <- Sys.getenv("GLASSNODE_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_gn)) Sys.unsetenv("GN_API_KEY") else Sys.setenv(GN_API_KEY = old_gn)
    if (is.na(old_glass)) Sys.unsetenv("GLASSNODE_API_KEY") else Sys.setenv(GLASSNODE_API_KEY = old_glass)
  }, add = TRUE)

  Sys.setenv(GN_API_KEY = "primary-key", GLASSNODE_API_KEY = "fallback-key")
  expect_equal(GlassnodeR:::get_default_api_key(), "primary-key")

  Sys.unsetenv("GN_API_KEY")
  expect_equal(GlassnodeR:::get_default_api_key(), "fallback-key")
})

test_that("redact_url strips api_key values from URLs", {
  expect_equal(
    GlassnodeR:::redact_url("https://api.glassnode.com/v1/metrics/market/price_usd_close?a=BTC&api_key=supersecret&i=24h"),
    "https://api.glassnode.com/v1/metrics/market/price_usd_close?a=BTC&api_key=<REDACTED>&i=24h"
  )
  expect_equal(
    GlassnodeR:::redact_url("https://api.glassnode.com/v1/metrics/market/price_usd_close?api_key=supersecret"),
    "https://api.glassnode.com/v1/metrics/market/price_usd_close?api_key=<REDACTED>"
  )
  expect_equal(
    GlassnodeR:::redact_url("https://api.glassnode.com/v1/metrics/market/price_usd_close?a=BTC"),
    "https://api.glassnode.com/v1/metrics/market/price_usd_close?a=BTC"
  )
})

test_that("call_glassnode_api validates path input", {
  expect_error(GlassnodeR:::call_glassnode_api(), "API path must be a non-empty character string")
  expect_error(GlassnodeR:::call_glassnode_api(""), "API path must be a non-empty character string")
})

test_that("call_glassnode_api sends X-Api-Key header and never leaks api_key in errors", {
  captured_url <- NULL
  captured_headers <- NULL

  fake_response <- function(status_code, body = "[]") {
    structure(
      list(
        url = "https://api.glassnode.com/v1/metrics/market/price_usd_close?a=USDC",
        status_code = status_code,
        headers = structure(list("content-type" = "application/json"), class = c("insensitive", "list")),
        content = charToRaw(body)
      ),
      class = "response"
    )
  }

  local_mocked_bindings(
    GET = function(url, config = list(), ...) {
      captured_url <<- url
      captured_headers <<- config$headers
      fake_response(200L, '[{"t":1735689600,"v":1.0}]')
    },
    .package = "httr"
  )

  res <- GlassnodeR:::call_glassnode_api(
    "v1/metrics/market/price_usd_close",
    a = "USDC",
    api_key = "secret-token-xyz"
  )
  expect_false(grepl("secret-token-xyz", captured_url, fixed = TRUE))
  expect_equal(unname(captured_headers["X-Api-Key"]), "secret-token-xyz")
  expect_s3_class(res, "data.frame")
  expect_equal(res$v, 1.0)

  local_mocked_bindings(
    GET = function(url, config = list(), ...) fake_response(401L, "Unauthorized"),
    .package = "httr"
  )

  err <- expect_error(
    GlassnodeR:::call_glassnode_api(
      "v1/metrics/market/price_usd_close",
      a = "USDC",
      api_key = "secret-token-xyz"
    ),
    "GN_API_KEY"
  )
  expect_false(grepl("secret-token-xyz", conditionMessage(err), fixed = TRUE))
})

test_that("exported individual address functions work with mocked API responses", {
  local_mocked_bindings(
    call_glassnode_api = function(path, ...) {
      data.frame(t = c(1735689600, 1735776000), v = c(100, 150))
    },
    .package = "GlassnodeR"
  )

  w <- get_withdrawing_addresses(asset = "USDC", since = "2025-01-01", until = "2025-01-02", api_key = "k")
  expect_s3_class(w, "tbl_df")
  expect_equal(names(w), c("date", "USDC_with_add"))
  expect_s3_class(w$date, "Date")
  expect_equal(w$USDC_with_add, c(100, 150))

  a <- get_active_addresses(asset = "USDT", since = "2025-01-01", until = "2025-01-02", api_key = "k")
  expect_s3_class(a, "tbl_df")
  expect_equal(names(a), c("date", "USDT_active_add"))
})
