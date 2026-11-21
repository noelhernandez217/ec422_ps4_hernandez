library(tidyverse)
library(fpp3)
library(BVAR)

# Part 2 – A Business Forecasting Horse Race: ARIMA vs. ETS

# 2.1a
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# 2.1b - Plotting 

liquor |> autoplot(sales) + 
  labs(title = "Liquour Sales")

liquor |> gg_season(log(sales)) + 
  labs(title = "Seasonal log(sales) of Liquour")

# In a few sentences: describe the trend and the seasonal pattern. 
# Which months are high and which are low? 
# Do the seasonal swings grow with the level of the series, and what does that 
# suggest about using logs?
  
#  2.2. Fit Both Models 

# 2.2a. 

liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

# 2.2b 

liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 6)

# p = 1, P = 2, q = 2, Q = 1, TOTAL = 6

# 2.3. The Horse Race

# 2.3a
# DONT RERUN THIS A LOT -> Takes up to ~10 minutes
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

# saving data
liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")

# 2.3b 

liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

# 2.4. Are the Differences Significant?

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

# arima vs ets
forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

# arima vs snaive
forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

# Part 3 – Forecast Combination

# 3a

liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

# Part 4 – A Medium-Scale VAR vs. a BVAR

# 4.1a
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

# 4.1b

growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

# 4.1c 

N <- nrow(y) # 240
origins <- 120:(N - 1)
actual <- y[121:N, ]

glue("Origins: {length(origins)}")
glue("Actual: {length(actual)}")

# extra code to help me answer question: 

all_dates <- filter(qd, date > as.Date("1989-12-01"))
all_dates
first_date <- all_dates$date[1]
last_date <- all_dates$date[nrow(all_dates)]

glue("First date: {first_date}")
glue("Last date: {last_date}")

# 4.2. OLS VARs 

var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

# Compute the RMSE of each model for each variable

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

# 4.3. A BVAR

### 4.3a 

bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

# 4.3b

set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)


# 4.3c

e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

e_infl_var4 <- actual[, "infl"] - fc_var4[, 5]
e_infl_bvar <- actual[, "infl"] - fc_bvar_03[, 5]

forecast::dm.test(e_infl_var4, e_infl_bvar, h = 1, power = 2)



