# https://www.cboe.com/micro/vix/vixwhite.pdf

# The VIX Index measures 30-day expected volatility of the S&P 500 Index.
# The components of the VIX Index are near- and next-term put and call options with more than 23 days and
# less than 37 days to expiration.
# These include SPX options with “standard” 3rd Friday expiration dates and “weekly” SPX options that expire every Friday,
# except the 3rd Friday of each month.
# Once each week, the SPX options used to calculate the VIX Index “roll” to new contract maturities.
# For example, on the second Tuesday in October, the VIX Index would be calculated using SPX options expiring 24 days later
# (i.e., “near- term”) and 31 days later (i.e., “next-term”).
# On the following day, the SPX options that expire in 30 calendar days would become the “near-term” options and
# SPX options that expire in 37 calendar days would be the “next-term” options.
# In this hypothetical example, the near-term options are “standard” SPX options with 25 days to expiration,
# the next-term options are P.M.-settled SPX Weeklys with 32 days to expiration;
# and the calculation reflects prices observed at 9:46 a.m. Chicago time.
# For the purpose of calculating time to expiration, “standard” SPX options are deemed to expire at the open of
# trading on SPX settlement day - the third Friday of the month , and “weekly” SPX options are deemed to expire
# at the close of trading (i.e., 3:00 p.m. CT).
# The VIX Index calculation measures time to expiration, T, in calendar days and divides each day into minutes
# in order to replicate the precision that is commonly used by professional option and volatility traders.
# The time to expiration is given by the following expression:
#
# T = { MCurrent day + MSettlement day + MOther days } / Minutes in a year
#


# The risk-free interest rates, R1 and R2, are yields based on U.S. Treasury yield curve rates
# (commonly referred to as “Constant Maturity Treasury” rates or CMTs), to which a cubic spline is applied to
# derive yields on the expiration dates of relevant SPX options. As such, the VIX Index calculation may use
# different risk-free interest rates for near- and next-term options. In this example, assume that R1 = 0.0305%
# for the near-term options and that R2 = 0.0286% for the next-term options.
# Note in this example, T2 uses a value of 900 for MSettlement day, which reflects the 3:00 p.m. expiration time
# of the next-term SPX Weeklys options. Since many of the interim calculations are repetitive,
# only representative samples appear below.
# The complete set of SPX option data and calculations may be found in Appendix 1.

# Step 1: Select the options to be used in the VIX Index calculation
# The selected options are out-of-the-money SPX calls and out-of-the-money SPX puts centered around an at-the-money
# strike price, K0. Only SPX options quoted with non-zero bid prices are used in the VIX Index calculation.
# One important note: as volatility rises and falls, the strike price range of options with non-zero bids tends to
# expand and contract. As a result, the number of options used in the VIX Index calculation may vary from
# month-to-month, day-to-day and possibly, even minute-to-minute.

# For each contract month:

# Determine the forward SPX level, F, by identifying the strike price at which the absolute difference between the
# call and put prices is smallest. The call and put prices in the following table reflect the average of each
# option’s bid / ask quotation. As shown below, the difference between the call and put prices is smallest at
# the 1965 strike for the near- and the 1960 strike for the next-term options.

# Using the 1965 call and put in the near-term, and the 1960 call and put in the next-term contract applied
# to the formula:

# F = Strike Price + eRT x (Call Price - Put Price)
# the forward index prices, F1 and F2, for the near- and next-term options, respectively, are:

# F1 = 1965 + e(0.000305 x 0.0683486) x (21.05 - 23.15) = 1962.89996
# F2 = 1960 + e(0.000286 x 0.0882686) x (27.30 - 24.90) = 1962.40006

# Next, determine K0 - the strike price immediately below the forward index level,
# F - for the near- and next-term options. In this example, K0,1 = 1960 and K0,2 = 1960.
# Select out-of-the-money put options with strike prices < K0. Start with the put strike immediately lower than K0
# and move to successively lower strike prices. Exclude any put option that has a bid price equal to zero (i.e., no bid).
# As shown below, once two puts with consecutive strike prices are found to have zero bid prices, no puts with lower
# strikes are considered for inclusion.
# (Note that the 1350 and 1355 put options are not included despite having non-zero bid prices.)

# Next, select out-of-the-money call options with strike prices > K0.
# Start with the call strike immediately higher than K0 and move to successively higher strike prices,
# excluding call options that have a bid price of zero. As with the puts, once two consecutive call options are found
# to have zero bid prices, no calls with higher strikes are considered.
# (Note that the 2225 call option is not included despite having a non-zero bid price.)

# Finally, select both the put and call with strike price K0. Notice that two options are selected at K0,
# while a single option, either a put or a call, is used for every other strike price.
# The following table contains the options used to calculate the VIX Index in this example.
# The VIX Index uses the average of quoted bid and ask, or mid-quote, prices for each option selected.
# The K0 put and call prices are averaged to produce a single value.
# The price used for the 1960 strike in the near-term is, therefore, (24.25 + 21.30)/2 = 22.775;
# and the price used in the next- term is (27.30 + 24.90)/2 = 26.10.


# Step 2: Calculate volatility for both near-term and next-term options
# Applying the VIX formula (1) to the near-term and next-term options with time to expiration of T1 and T2, respectively, yields:

# The VIX Index is an amalgam of the information reflected in the prices of all of the selected options.
# The contribution of a single option to the VIX value is proportional to ΔK and the price of that option,
# and inversely proportional to the square of the option’s strike price.
# Generally, ΔKi is half the difference between the strike prices on either side of Ki.
# For example, the ΔK for the next-term 1325 Put is 37.5: ΔK1325 Put = (1350 – 1275)/2.
# At the upper and lower edges of any given strip of options, ΔKi is simply
# the difference between Ki and the adjacent strike price. In this example,
# the 1370 Put is the lowest strike in the strip of near- term options and 1375 is the adjacent strike.
# Therefore, ΔK1370 Put = 5 (i.e., 1375 – 1370).

# The contribution of the near-term 1370 Put is given by:

# A similar calculation is performed for each option.
# The resulting values for the near-term options are then summed and multiplied by 2/T1.
# Likewise, the resulting values for the next-term options are summed and multiplied by 2/T2.
# The table below summarizes the results for each strip of options.

# Step 3
# Calculate the 30-day weighted average of σ21 and σ22.
# Then take the square root of that value and multiply by 100 to get the VIX value.

# The inclusion of SPX Weeklys in the VIX Index calculation means that the near-term options will always
# have more than 23 days to expiration and the next-term options always have less than 37 days to expiration,
# so the resulting VIX value will always reflect an interpolation of σ21 and σ22 ;
# i.e., each individual weight is less than or equal to 1 and the sum of the weights equals 1.

# Returning to the example...
# NT1 = number of minutes to settlement of the near-term options (35,924)
# NT2 = number of minutes to settlement of the next-term options (46,394)
# N30 = number of minutes in 30 days (30 x 1,440 = 43,200)
# N365 = number of minutes in a 365-day year (365 x 1,440 = 525,600)

symbol <- "SPY"
min_day <- 1440
min_month <- min_day * 30
min_year <- 525600

option_data <- readRDS(paste0(here::here(), "/data/options/", symbol, ".RDS"))
rates <- risk_free_rate()

option_data <- option_data %>%
  dplyr::mutate(mid = (ask + bid) / 2) %>%
  dplyr::filter(dte >= 23)

puts <- option_data %>%
  dplyr::filter(type == "put",
                strike <= close) %>%
  dplyr::group_by(quotedate, dte) %>%
  dplyr::arrange(desc(strike)) %>%
  dplyr::mutate(lead_1 = ifelse(is.na(lead(bid, 1)), 0, lead(bid, 1)),
                lead_2 = ifelse(is.na(lead(bid, 2)), 0, lead(bid, 2))) %>%
  dplyr::filter(bid > 0.00 |
                  lead_1 > 0.00 |
                  lead_2 > 0.00) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(quotedate) %>%
  dplyr::mutate(near_dte = min(dte)) %>%
  dplyr::mutate(near_dte_diff = dte - near_dte) %>%
  dplyr::filter(dte == near_dte |
                  near_dte_diff > 6) %>%
  dplyr::mutate(dte_rank = dense_rank(near_dte_diff)) %>%
  dplyr::filter(dte == near_dte | dte_rank == 2) %>%
  dplyr::ungroup() #%>%
  #dplyr::filter(quotedate == "2018-03-28")

calls <- option_data %>%
  dplyr::filter(type == "call",
                strike >= close) %>%
  dplyr::group_by(quotedate, dte) %>%
  dplyr::arrange(strike) %>%
  dplyr::mutate(lead_1 = ifelse(is.na(lead(bid, 1)), 0, lead(bid, 1)),
                lead_2 = ifelse(is.na(lead(bid, 2)), 0, lead(bid, 2))) %>%
  dplyr::filter(bid > 0.00 |
                  lead_1 > 0.00 |
                  lead_2 > 0.00) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(quotedate) %>%
  dplyr::mutate(near_dte = min(dte)) %>%
  dplyr::mutate(near_dte_diff = dte - near_dte) %>%
  dplyr::filter(dte == near_dte |
                  near_dte_diff > 6) %>%
  dplyr::mutate(dte_rank = dense_rank(near_dte_diff)) %>%
  dplyr::filter(dte == near_dte | dte_rank == 2) %>%
  dplyr::ungroup() #%>%
  #dplyr::filter(quotedate == "2018-03-28")





