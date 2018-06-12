#' @export
short_put_spread <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    open_options_trades <- function(stock) {
      assign(paste0("open_options_", stock),
             get(paste0(stock, "_options"), envir = as.environment(options_data)) %>%
               dplyr::filter(quotedate %in% first_day$date,
                             type == "put",
                             strike < close,
                             dte >= 5) %>%
               dplyr::mutate(mid = (bid + ask) / 2,
                             spread = ask - bid,
                             m_dte = abs(dte - o_dte),
                             m_delta = abs(delta - p_delta)) %>%
               dplyr::group_by(quotedate) %>%
               dplyr::filter(m_dte == min(m_dte)) %>%
               dplyr::top_n(., n = -2, wt = m_delta) %>%
               dplyr::mutate(side = ifelse(strike == max(strike), "short", "long")) %>%
               dplyr::ungroup(),
             envir = open_trades)
    }

    close_options_trades <- function(stock, open_date, strk, open_price, exp, close_date, spd, side) {
      close_details <- get(paste0(stock, "_options"), envir = as.environment(options_data)) %>%
        dplyr::filter(quotedate == close_date,
                      type == "put",
                      strike == strk,
                      expiration == exp) %>%
        dplyr::mutate(trade_open = open_date,
                      trade_close = close_date,
                      trade_open_price = open_price,
                      trade_close_price = ifelse((bid + ask) / 2 == 0.005, 0, (bid + ask) / 2),
                      spread = spd,
                      spread_perc = spread / trade_open_price,
                      side = side,
                      trade_open_price = ifelse(side == "long", -1 * trade_open_price, trade_open_price),
                      trade_close_price = ifelse(side == "long", -1 * trade_close_price, trade_close_price)) %>%
        dplyr::select(symbol, quotedate, expiration, strike, close, dte, trade_open,
                      trade_close, trade_open_price, trade_close_price, spread, spread_perc, side)
      assign(paste0("option_close_", stock, "_", open_date, "_", strk), close_details,
             envir = close_trades)
    }

    close_option_trades_macro <- function(stock) {
      df <- get(paste0("open_options_", stock), envir = as.environment(open_trades))

      purrr::pmap_df(list(stock = df$symbol,
                          open_date = df$quotedate,
                          strk = df$strike,
                          open_price = df$mid,
                          exp = df$expiration,
                          close_date = df$expiration,
                          spd = df$spread,
                          side = df$side),
                     close_options_trades)
    }

    bind_option_closes <- function(stock) {
      df_pattern <- paste0("option_close_", stock, "_[0-9]{5}")
      df_list <- ls(pattern = df_pattern, envir = close_trades)

      option_analysis_closes <- dplyr::bind_rows(lapply(df_list,
                                                        function(x) get(x, envir = close_trades)))
      assign(paste0("option_closes_", stock), option_analysis_closes, envir = trade_results)
    }

    if (!exists(paste0(stock, "_options"), envir = as.environment(options_data))) {
      load_options_data(stock)
    }

    invisible(purrr::map(stock, open_options_trades))
    invisible(purrr::map(stock, close_option_trades_macro))
    invisible(purrr::map(stock, bind_option_closes))
    format_results(results) # utils.R script
  })}
