#' @export
open_short_put <- function(stock) {
  assign(paste0("open_options_", stock),
         get(paste0(stock, "_options"), envir = as.environment(options_data)) %>%
           dplyr::filter(quotedate %in% first_day$date,
                         type == "put",
                         dte >= 5) %>%
           dplyr::mutate(mid = (bid + ask) / 2,
                         spread = ask - bid,
                         m_dte = abs(dte - o_dte),
                         m_delta = abs(delta - p_delta)) %>%
           dplyr::filter(mid >= .10) %>%
           dplyr::group_by(quotedate) %>%
           dplyr::filter(m_dte == min(m_dte)) %>%
           dplyr::filter(m_delta == min(m_delta)) %>%
           dplyr::filter(dplyr::row_number() == 1) %>%
           dplyr::ungroup(),
         envir = open_trades)
}
