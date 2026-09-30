
      
      
      
if (!require(pacman)) install.packages("pacman")
install.packages("modelsummary")
install.packages("forecast")

# Load packages
pacman::p_load(
  tidyverse,
  tidyr,
  lubridate,
  readr,
  zoo,
  ggplot2, 
  dplyr, 
  dynlm,
  lmtest,
  sandwich,
  vars, 
  modelsummary, 
  tseries, 
  car, 
  readxl, 
  urca,
  broom, 
  forecast
)


setwd("~/Desktop/Bocconi secondo semestre")  
                                     
#----------------------------------------------------------------------------------------

# Load CPI data (horizontal)
cpi <- read_csv("France_GDP_Data/dataset_2026-05-12T14_02_32.665156924Z_DEFAULT_INTEGRATION_IMF.STA_CPI_5.0.0.csv") %>%
  filter(str_starts(SERIES_CODE, "FRA")) %>%        #keep only France 
  dplyr::select(-matches("-M\\d{2}$"),              #keep only quarterly observations (remove monthly)
         -matches("^\\d{4}$")) %>%                  #remove yearly
  filter(SERIES_CODE == "FRA.CPI._T.IX.Q")          #keep CPI (total items index)

#----------------------------------------------------------------------------------------
# Load GDP data (horizontal)
gdp <- read_csv("France_GDP_Data/dataset_2026-05-12T14_01_16.394756773Z_DEFAULT_INTEGRATION_IMF.STA_QNEA_7.0.0.csv") %>%
  filter(str_starts(SERIES_CODE, "FRA")) %>%  #keep only France 
  filter(SERIES_CODE == "FRA.B1GQ.V.SA.XDC.Q")   #keep quarterly, seasonally-adjusted, volume GDP (domestic currency)

#----------------------------------------------------------------------------------------
# Load IP Index data (horizontal)
ip <- read_csv("France_GDP_Data/dataset_2026-05-13T12_25_25.527885999Z_DEFAULT_INTEGRATION_IMF.STA_PI_2.0.0.csv") %>%
  filter(str_starts(SERIES_CODE, "FRA")) %>%  #keep only France 
  filter(SERIES_CODE == "FRA.IND.SA_YOY_PCH_PT.Q")   #keep quarterly, seasonally-adjusted, volume GDP (domestic currency)

  #drop unneeded columns
ip <- ip %>%
  dplyr::select(-DATASET, -SCALE, -OBS_MEASURE, -COUNTRY, -PRODUCTION_INDEX, -TYPE_OF_TRANSFORMATION, -FREQUENCY)
#----------------------------------------------------------------------------------------
# Load ESI data and aggregate monthly data to quarterly levels (ALREADY VERTICAL)
esi_q <- read_excel("main_indicators_nace2.xlsx", sheet = "MONTHLY") %>%
  dplyr::select(date = 1, ESI = `EU.ESI`) %>%
  mutate(
    date = as.Date(as.yearmon(date, "%b-%y")),
    quarter = as.yearqtr(date)
  ) %>%
  group_by(quarter) %>%
  summarise(
    ESI_q = mean(ESI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    su = 100 * (log(ESI_q) - log(lag(ESI_q))) #su = quarterly growth rate of the EA ESI
  )

#fix date to match format of IMF data
esi_q <- esi_q %>%
  mutate(
    date = format(as.yearqtr(quarter), "%Y-Q%q")
  ) %>%
  dplyr::select(-quarter)


#----------------------------------------------------------------------------------------
# Load Eurostoxx data and aggregate monthly data to quarterly levels (ALREADY VERTICAL)
sr <- read_csv("France_GDP_Data/ECB_Data_Portal_20260513144131.csv") %>%
  rename(
    date = DATE,
    eurostoxx = `EURO STOXX 50 Equity Index - Historical close, average of observations through period (FM.M.U2.EUR.DS.EI.DJES50I.HSTA)`
  ) %>%
  mutate(
    date = as.Date(date),
    quarter = as.yearqtr(date)
  ) %>%
  group_by(quarter) %>%
  summarise(
    eurostoxx_q = mean(eurostoxx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sr = 100 * (log(eurostoxx_q) - log(lag(eurostoxx_q)))
  )

#fix date to match format of IMF data
sr <- sr %>%
  mutate(
    date = format(as.yearqtr(quarter), "%Y-Q%q")
  ) %>%
  dplyr::select(-quarter)

#----------------------------------------------------------------------------------------
# Load IR data, ALREADY VERTICAL (3-month interbank rate)
fred_ir <- read_csv("France_GDP_Data/IR3TIB01FRM156N.csv") %>%
  rename(
    fred_date = observation_date,
    interbank_3m = IR3TIB01FRM156N
  ) %>%
  mutate(
    fred_date = as.Date(fred_date),
    quarter = ceiling(as.numeric(format(fred_date, "%m")) / 3),
    date = paste0(format(fred_date, "%Y"), "-Q", quarter)
  ) %>%
  group_by(date) %>%
  summarise(
    interbank_3m = mean(interbank_3m, na.rm = TRUE),
    .groups = "drop"
  )


#----------------------------------------------------------------------------------------
# Append datasets vertically
all_wide <- bind_rows(cpi, gdp, ip)
all_long <- all_wide %>%
  # Drop columns
  dplyr::select(-DATASET, -OBS_MEASURE) %>%
  # Reshape to long
  pivot_longer(
    cols = -SERIES_CODE,   # keep identifier column
    names_to = "date",
    values_to = "value"
  ) %>%
pivot_wider(
  names_from = SERIES_CODE,
  values_from = value
)

# Merge IR, stock returns, and ESI into file:
all_long <- all_long %>%
  left_join(fred_ir, by = "date") %>%
  left_join(sr, by = "date") %>%
  left_join(esi_q, by = "date") 

#COMPUTE VARIABLES:
#compute YoY inflation rate: 
all_long <- all_long %>%
  arrange(date) %>%
  mutate(
    inflation = 100 * (FRA.CPI._T.IX.Q / lag(FRA.CPI._T.IX.Q, 4) - 1)
  )

#compute YoY GDP growth: 
all_long <- all_long %>%
  arrange(date) %>%
  mutate(
    gdp_growth = 100 * (FRA.B1GQ.V.SA.XDC.Q / lag(FRA.B1GQ.V.SA.XDC.Q, 4) - 1)
  )

# Drop unused variables
all_long <- all_long %>%
  dplyr::select(-eurostoxx_q, -ESI_q, -FRA.CPI._T.IX.Q) 


# Remove rows with any missing values
all_long <- na.omit(all_long)



#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#Assignment II
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
############################################################
# GDP forecasting exercise
############################################################

# =========================
# 1. Prepare data
# =========================

df <- all_long %>%
  rename(
    ip_growth = FRA.IND.SA_YOY_PCH_PT.Q
  ) %>%
  mutate(
    date_q = as.yearqtr(date, format = "%Y-Q%q")
  ) %>%
  arrange(date_q) %>%
  dplyr::select(date_q, gdp_growth, inflation, interbank_3m, ip_growth, sr, su) %>%
  na.omit()

summary(df)
str(df)

# Add lagged variables once, using the full ordered sample.
# This avoids broken lags at the train/test boundary.
df_adl <- df %>%
  arrange(date_q) %>%
  mutate(
    gdp_l1 = lag(gdp_growth, 1),
    gdp_l2 = lag(gdp_growth, 2),
    ip_l1  = lag(ip_growth, 1),
    ip_l2  = lag(ip_growth, 2),
    su_l1  = lag(su, 1),
    su_l2  = lag(su, 2),
    inf_l1 = lag(inflation, 1),
    inf_l2 = lag(inflation, 2)
  )

# =========================
# 2. Descriptive analysis
# =========================

desc_stats <- df %>%
  dplyr::select(-date_q) %>%
  summarise(across(
    everything(),
    list(
      mean = mean,
      sd   = sd,
      min  = min,
      max  = max
    ),
    .names = "{.col}_{.fn}"
  ))

print(desc_stats)

cor_matrix <- cor(df %>% dplyr::select(-date_q))
print(round(cor_matrix, 3))

df_long <- df %>%
  pivot_longer(-date_q, names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = date_q, y = value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Descriptive plots of macroeconomic variables",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal()

# =========================
# 3. Split sample
# =========================

forecast_horizon <- 40
n <- nrow(df)

train <- df[1:(n - forecast_horizon), ]
test  <- df[(n - forecast_horizon + 1):n, ]

train_adl <- df_adl[1:(n - forecast_horizon), ] %>%
  na.omit()

test_adl <- df_adl[(n - forecast_horizon + 1):n, ]

# =========================
# 4. Baseline and ADL models
# =========================

model_1 <- lm(gdp_growth ~ ip_growth + su + inflation + sr + interbank_3m, data = train)
model_2 <- lm(gdp_growth ~ ip_growth + su + sr + interbank_3m, data = train)
model_3 <- lm(gdp_growth ~ ip_growth + su + inflation, data = train)
model_4 <- lm(gdp_growth ~ ip_growth + inflation + interbank_3m, data = train)
model_5 <- lm(gdp_growth ~ ip_growth + su, data = train)

model_6 <- lm(
  gdp_growth ~ gdp_l1 + ip_growth + su + inflation,
  data = train_adl
)

model_7 <- lm(
  gdp_growth ~ gdp_l1 + gdp_l2 + ip_growth + su + inflation,
  data = train_adl
)

model_8 <- lm(
  gdp_growth ~ gdp_l1 + gdp_l2 +
    ip_growth + ip_l1 +
    su + su_l1 +
    inflation + inf_l1,
  data = train_adl
)

model_9 <- lm(
  gdp_growth ~ gdp_l1 + gdp_l2 +
    ip_growth + ip_l1 + ip_l2 +
    su + su_l1 + su_l2 +
    inflation + inf_l1 + inf_l2,
  data = train_adl
)

models <- list(
  Model_1_full         = model_1,
  Model_2_no_inflation = model_2,
  Model_3_no_financial = model_3,
  Model_4_policy_macro = model_4,
  Model_5_simple       = model_5,
  Model_6_ADL1         = model_6,
  Model_7_ADL2_y_lags  = model_7,
  Model_8_ADL1_x_lags  = model_8,
  Model_9_ADL2_full    = model_9
)

lapply(models, summary)
lapply(models, function(m) coeftest(m, vcov = NeweyWest(m)))

ic_table <- data.frame(
  model  = names(models),
  AIC    = sapply(models, AIC),
  BIC    = sapply(models, BIC),
  adj_R2 = sapply(models, function(m) summary(m)$adj.r.squared)
)

print(ic_table)

preferred_name <- ic_table$model[which.min(ic_table$BIC)]
preferred_model <- models[[preferred_name]]
preferred_formula <- formula(preferred_model)

cat("Preferred model by BIC:", preferred_name, "\n")
summary(preferred_model)

# =========================
# 5. Diagnostic checks
# =========================

print(bgtest(preferred_model, order = 4))
print(bptest(preferred_model))
print(jarque.bera.test(residuals(preferred_model)))
print(vif(preferred_model))

sctest <- lmtest::resettest(preferred_model)
print(sctest)

# Residual diagnostic plots
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))
plot(preferred_model)
par(mfrow = c(1, 1))
# =========================
# 6. Accuracy functions
# =========================

rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
mafe <- function(e) mean(abs(e), na.rm = TRUE)

# =========================
# 7. Static forecasts
# =========================

static_fc <- predict(preferred_model, newdata = test_adl)

forecast_eval <- test_adl %>%
  mutate(
    forecast_static_preferred = as.numeric(static_fc),
    error_static_preferred = gdp_growth - forecast_static_preferred
  )

static_rmse <- rmse(forecast_eval$error_static_preferred)
static_mafe <- mafe(forecast_eval$error_static_preferred)

cat("Static preferred model RMSE:", static_rmse, "\n")
cat("Static preferred model MAFE:", static_mafe, "\n")

forecast_plot_df <- forecast_eval %>%
  dplyr::select(date_q, gdp_growth, forecast_static_preferred) %>%
  rename(
    Actual = gdp_growth,
    `Static forecast` = forecast_static_preferred
  ) %>%
  pivot_longer(
    cols = c(Actual, `Static forecast`),
    names_to = "Series",
    values_to = "Value"
  )

p_static <- ggplot(forecast_plot_df, aes(x = date_q, y = Value, linetype = Series)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Static 1-step-ahead Forecasts",
    subtitle = paste0(
      "RMSE = ", round(static_rmse, 3),
      ", MAFE = ", round(static_mafe, 3)
    ),
    x = "Date",
    y = "GDP growth (YoY %)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_static)

# =========================
# 8. Recursive preferred-model forecasts
# =========================

recursive_forecast_lm <- function(data, formula, initial_window) {
  n <- nrow(data)
  fc <- rep(NA_real_, n)
  
  for (i in (initial_window + 1):n) {
    train_i <- data[1:(i - 1), ] %>%
      na.omit()
    
    test_i <- data[i, , drop = FALSE]
    
    model_i <- lm(formula, data = train_i)
    fc[i] <- as.numeric(predict(model_i, newdata = test_i))
  }
  
  fc
}

df_adl$fc_recursive_preferred <- recursive_forecast_lm(
  data = df_adl,
  formula = preferred_formula,
  initial_window = n - forecast_horizon
)

recursive_eval <- df_adl[(n - forecast_horizon + 1):n, ] %>%
  mutate(
    error_recursive_preferred = gdp_growth - fc_recursive_preferred
  )

recursive_rmse <- rmse(recursive_eval$error_recursive_preferred)
recursive_mafe <- mafe(recursive_eval$error_recursive_preferred)

cat("Recursive preferred model RMSE:", recursive_rmse, "\n")
cat("Recursive preferred model MAFE:", recursive_mafe, "\n")

# =========================
# 9. Static vs recursive plot
# =========================

forecast_compare_df <- recursive_eval %>%
  dplyr::select(date_q, gdp_growth, fc_recursive_preferred) %>%
  left_join(
    forecast_eval %>%
      dplyr::select(date_q, forecast_static_preferred),
    by = "date_q"
  ) %>%
  rename(
    Actual = gdp_growth,
    `Static forecast` = forecast_static_preferred,
    `Recursive forecast` = fc_recursive_preferred
  ) %>%
  pivot_longer(
    cols = c(Actual, `Static forecast`, `Recursive forecast`),
    names_to = "Series",
    values_to = "Value"
  )

p_compare <- ggplot(forecast_compare_df, aes(x = date_q, y = Value, linetype = Series)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Static and Recursive 1-step-ahead Forecasts",
    subtitle = paste0(
      "Recursive RMSE = ", round(recursive_rmse, 3),
      ", MAFE = ", round(recursive_mafe, 3)
    ),
    x = "Date",
    y = "GDP growth (YoY %)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_compare)

# =========================
# 10. AR(2) benchmark
# =========================

recursive_forecast_ar2 <- function(data, yvar, initial_window) {
  n <- nrow(data)
  fc <- rep(NA_real_, n)
  
  for (i in (initial_window + 1):n) {
    y_train <- data[[yvar]][1:(i - 1)]
    y_train <- na.omit(y_train)
    
    ar_model <- Arima(y_train, order = c(2, 0, 0), include.mean = TRUE)
    fc[i] <- as.numeric(forecast(ar_model, h = 1)$mean[1])
  }
  
  fc
}

df_adl$fc_ar2 <- recursive_forecast_ar2(
  data = df_adl,
  yvar = "gdp_growth",
  initial_window = n - forecast_horizon
)

ar2_eval <- df_adl[(n - forecast_horizon + 1):n, ] %>%
  mutate(error_ar2 = gdp_growth - fc_ar2)

ar2_rmse <- rmse(ar2_eval$error_ar2)
ar2_mafe <- mafe(ar2_eval$error_ar2)

cat("AR(2) RMSE:", ar2_rmse, "\n")
cat("AR(2) MAFE:", ar2_mafe, "\n")

# =========================
# 11. VAR(2) benchmark
# =========================

recursive_forecast_var2 <- function(data, vars_used, target_var, initial_window) {
  n <- nrow(data)
  fc <- rep(NA_real_, n)
  
  for (i in (initial_window + 1):n) {
    train_i <- data[1:(i - 1), vars_used] %>%
      na.omit()
    
    var_model <- VAR(train_i, p = 2, type = "const")
    pred <- predict(var_model, n.ahead = 1)
    
    fc[i] <- pred$fcst[[target_var]][1, "fcst"]
  }
  
  fc
}

var_vars <- c("gdp_growth", "inflation", "interbank_3m")

df_adl$fc_var2 <- recursive_forecast_var2(
  data = df_adl,
  vars_used = var_vars,
  target_var = "gdp_growth",
  initial_window = n - forecast_horizon
)

var2_eval <- df_adl[(n - forecast_horizon + 1):n, ] %>%
  mutate(error_var2 = gdp_growth - fc_var2)

var2_rmse <- rmse(var2_eval$error_var2)
var2_mafe <- mafe(var2_eval$error_var2)

cat("VAR(2) RMSE:", var2_rmse, "\n")
cat("VAR(2) MAFE:", var2_mafe, "\n")

# =========================
# 12. Forecast comparison table
# =========================

comparison_table <- data.frame(
  model = c(
    "Preferred regression - static",
    "Preferred regression - recursive",
    "AR(2)",
    "VAR(2)"
  ),
  RMSE = c(static_rmse, recursive_rmse, ar2_rmse, var2_rmse),
  MAFE = c(static_mafe, recursive_mafe, ar2_mafe, var2_mafe)
) %>%
  mutate(
    RMSE = round(RMSE, 3),
    MAFE = round(MAFE, 3)
  )

print(comparison_table)

# =========================
# 13. Plot all recursive forecasts
# =========================

plot_df <- df_adl[(n - forecast_horizon + 1):n, ] %>%
  dplyr::select(date_q, gdp_growth, fc_recursive_preferred, fc_ar2, fc_var2) %>%
  rename(
    Actual = gdp_growth,
    `Preferred recursive` = fc_recursive_preferred,
    `AR(2)` = fc_ar2,
    `VAR(2)` = fc_var2
  ) %>%
  pivot_longer(
    cols = -date_q,
    names_to = "Series",
    values_to = "Value"
  )

p_all <- ggplot(plot_df, aes(x = date_q, y = Value, linetype = Series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "One-step-ahead GDP growth forecasts",
    subtitle = "Preferred model vs AR(2) and VAR(2)",
    x = "Date",
    y = "YoY real GDP growth"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_all)

# =========================
# 14. Export results
# =========================

write_csv(comparison_table, "forecast_comparison_results.csv")
write_csv(forecast_eval, "static_forecasts.csv")
write_csv(df_adl, "all_recursive_forecasts.csv")

############################################################
# End of main script* 
############################################################


# =========================================================
# ROBUSTNESS CHECK: COVID dummy specification
# =========================================================
df_adl_covid <- df_adl %>%
  mutate(
    covid = ifelse(
      date_q >= as.yearqtr("2020 Q1") &
        date_q <= as.yearqtr("2021 Q1"),
      1, 0
    )
  ) %>%
  na.omit()

model_8_full <- lm(
  gdp_growth ~ gdp_l1 + gdp_l2 +
    ip_growth + ip_l1 +
    su + su_l1 +
    inflation + inf_l1,
  data = df_adl_covid
)

model_8_covid <- lm(
  gdp_growth ~ gdp_l1 + gdp_l2 +
    ip_growth + ip_l1 +
    su + su_l1 +
    inflation + inf_l1 +
    covid,
  data = df_adl_covid
)

AIC(model_8_full, model_8_covid)
BIC(model_8_full, model_8_covid)
summary(model_8_covid)


############################################################
# End of script
############################################################


#checks 
nobs(preferred_model)
summary(preferred_model)
residuals(preferred_model)
range(residuals(preferred_model))

preferred_name
formula(preferred_model)

df_adl[c(73, 74, 77), ]

