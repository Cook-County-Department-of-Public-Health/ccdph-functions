# Wastewater Trend Classification Helpers Script
# On source() this will:
#   -Preprocess wastewater data from IDPH, implements flow and PMMOV normalization for following targets: Sars-Cov2, Flu A/B, RSV
#   -Load function to implement GAM to quantify viral trends using derivative of fitted spline
# Created 5/3/23 by rishi.kowalski@cookcountyhealth.org

#load relevant packages
require(keyring)
require(tidyverse)
require(mgcv)
require(plotly)
#require(gratia), played around w/ this package

## Wastewater Preprocessing ##
#read in data, data comes in from /nwss flu/ from initial work with nwss ili - adapted for other diseases 4/27/23
ww_raw <- read_csv(paste0(key_get("rk-data-fpath"), "nwss/05_01_2023_NWSS_IDPH.csv")) %>% janitor::clean_names()

#adapted kelley code to clean ww data
ww_clean <- ww_raw %>%
  filter(county_names == 17031 & sample_location == "wwtp" & sample_collect_date >= as.Date("2022-10-12")) %>%  
  #drop_na(inf_a_copies_l_wastewater) %>% #remove as rsv testing begins 2/23 creates many nas - account for in gam
  mutate(
    wwtp_name = tolower(wwtp_name), #clean names
    display_name = case_when(
      grepl("kirie", wwtp_name) ~ "Kirie, Mid Northwest Suburbs",
      grepl("egan", wwtp_name) ~ "Egan, Far Northwest Suburbs",
      grepl("hanover", wwtp_name) ~ "Hanover Park, Far Northwest Suburbs",
      grepl("brien", wwtp_name) ~ "O'Brien, Northeast Suburbs and Chicago",
      grepl(" sws", wwtp_name) ~ "Stickney, West Suburbs and Chicago (South Site)",
      grepl(" ws", wwtp_name) ~ "Stickney, West Suburbs and Chicago (North Site)",
      grepl("lemont", wwtp_name) ~ "Lemont, Far Southwest Suburbs",
      grepl("calumet", wwtp_name) ~ "Calumet, South Suburbs and Chicago"),
    short_name = case_when(
      grepl("kirie", wwtp_name) ~ "kirie",
      grepl("egan", wwtp_name) ~ "egan",
      grepl("hanover", wwtp_name) ~ "hanover",
      grepl("brien", wwtp_name) ~ "obrien",
      grepl(" sws", wwtp_name) ~ "stickney south",
      grepl(" ws", wwtp_name) ~ "stickney north",
      grepl("lemont", wwtp_name) ~ "lemont",
      grepl("calumet", wwtp_name) ~ "calumet"))

#adjust dates for which lemont sampled day after other sites (total should be 8/day) - any way to do this in dpyr call?
lemont_adjust_dates <- ww_clean %>%
  group_by(sample_collect_date) %>% 
  mutate(
    id_sites_date = ifelse(sum(short_name != "lemont") == 7, 1, 0)) %>% #id dates for which 7 sites sampled w/o lemont
  distinct(sample_collect_date, id_sites_date) %>% 
  filter(id_sites_date == 1) %>% 
  pull(sample_collect_date) + 1 #take next date 

#apply lemont date adjustement, calculate reference values for pmmov, flow by site and normalize viral counts
#only pull dates for which all 8  sites report
ww_cook_bysite <- ww_clean %>%
  group_by(short_name, sample_collect_date) %>% #one sample per day per site
  arrange(sample_collect_time) %>%
  slice(1) %>% 
  ungroup() %>%
  mutate(#update lemont dates
    sample_collect_date = if_else(sample_collect_date %in% lemont_adjust_dates & short_name == "lemont", sample_collect_date - 1, sample_collect_date)) %>%
  group_by(sample_collect_date) %>%
  mutate(num_sites = length(sample_collect_date)) %>%
  ungroup() %>%
  filter(num_sites == 8) %>% #pull dates for which all 8 sites report, sum copies, normalize by pmmov and reference concentration (= avg over all sites)
  group_by(short_name) %>%
  mutate(
    pmmov_ref_conc_site = mean(pmmov_copies_l_wastewater), #site level reference concentrations of pmmov
    flow_rte_site_avg = mean(flow_rate)) %>% #site avg flow rate
  ungroup() %>%
  group_by(short_name, sample_collect_date) %>%
  summarise(
    inf_a_copies_l_wastewater = first(inf_a_copies_l_wastewater), #save for data frame
    inf_b_copies_l_wastewater = first(inf_b_copies_l_wastewater),
    sars_cov2_copies_l_wastewater = first(pcr_target_avg_conc),
    rsv_copies_l_wastewater = first(rsv_copies_l_wastewater),
    pmmov_copies_l_wastewater = first(pmmov_copies_l_wastewater),
    pmmov_ref_conc_site = first(pmmov_ref_conc_site),
    flow_rte_site_avg = first(flow_rte_site_avg),
    flu_a_flow_pmmov_norm_site = inf_a_copies_l_wastewater * flow_rte_site_avg * (pmmov_ref_conc_site / pmmov_copies_l_wastewater),
    flu_b_flow_pmmov_norm_site = inf_b_copies_l_wastewater * flow_rte_site_avg * (pmmov_ref_conc_site / pmmov_copies_l_wastewater),
    sars_cov2_flow_pmmov_norm_site = sars_cov2_copies_l_wastewater * flow_rte_site_avg * (pmmov_ref_conc_site / pmmov_copies_l_wastewater),
    rsv_flow_pmmov_norm_site = rsv_copies_l_wastewater * flow_rte_site_avg * (pmmov_ref_conc_site / pmmov_copies_l_wastewater))


## GAM Implementation ##
# function to implement gam given vector of viral target, sample dates
# spline basis/knots chosen according to min RSME and constraint
# returns data frame of sample dates, predicted values, derivative and derivative cis
# if specified, returns model output for testing and above data frame in list
implementGAMSite <- function(tgt, date, conf.level = .95, model_return = F) {
  set.seed(1)
  
  #get viral target
  arg_name <- deparse(substitute(tgt))
  
  #sub out df$varname if non-dplyr input, (?<=\$)((.+)(\_)(a|b|cov2)(\_)|rsv) captures too much and couldn't figure out
  arg_name <- gsub("^.+\\$", "", arg_name) 
  
  #capture viral target, account for extra term in inf a/b, cov2 variable names
  viral_tgt <- regmatches(arg_name, regexpr("(^(rsv\\_)|((.+)(\\_)(a|b|cov2)(\\_)))", arg_name))
  
  #assign variable names for dataframe out later
  df_names_out <- c(paste0(arg_name, "_pred"), paste0(viral_tgt, c("fst_deriv"), c("", "_ul", "_ll")))
  
  #fit models, cubic spline basis w/ shrinkage
  #choose model w/ lowest rmse subject to constraint: at most one knot/basis fx per 5 sample-dates (don't specify knots here so I think knots = basis fx)
  n_samples <- length(unique(date)) - sum(is.na(tgt)) #non na targets 
  k_max <- floor(n_samples / 5)
  
  #throw error if fewer than 3 non-intercept basis functions for constraint
  if(k_max < 4) {
    stop("Not enough data to fit 3 non-intercept basis functions with current constraint - please consider alternative method.")
  }
  
  #outlier detection: remove samples 5 sds above sample median
  tgt_med <- median(tgt, na.rm = T)
  tgt_sd <- sd(tgt, na.rm = T)
  tgt_outlier <- which(tgt < tgt_med + (5 * tgt_sd))
  tgt_model <- tgt[tgt_outlier]
  date_copy <- date #don't love this but need to preserve 'date' var going into model for testing later
  date <- date[tgt_outlier]
  
  if(length(tgt_outlier) > 0) {
    print(paste(length(tgt) - length(tgt_outlier), "Sample(s) Removed from Model Due to Outlier Values"))
  }
  
  #fit models
  models <- lapply(seq(4, k_max), function(x) gam(tgt_model ~ s(as.double(date), bs = "cs", k = x), family = quasipoisson(link = "log")))
  mod_rmse <- sapply(models, function(y) sd(y$residuals))
  
  mod <- models[[which.min(mod_rmse)]]
  
  #estimate derivative
  date1 <- min(date_copy)
  date2 <- max(date_copy)
  sm <- mod$smooth  #get_smooths_by_id(model, id)[[1L]]
  sm_term <- sm[[1]][["term"]]
  newDF <- data.frame(date = seq.Date(as.Date(date1), as.Date(date2), by = 1))
  newDF_adj <- newDF + 1
  
  pred_out <- predict(mod, newDF, type = "response")
  X0 <- predict(mod, newDF, type = "lpmatrix") #values of linear predictors
  X1 <- predict(mod, newDF_adj, type = "lpmatrix")
  Xp <- X1 - X0
  
  want <- grep(sm_term, colnames(Xp), fixed = T) #only matters for multiple splines
  Xi <- Xp * 0
  Xi[, want] <- Xp[, want]
  deriv1 <- drop(Xi %*% coef(mod)) #multiply linear predictor values by coefs for derivative of entire spline, would need to repeat if multiple splines
  
  #se for entire spline -> calculate confint
  se_deriv1 <- rowSums(Xi %*% mod$Vp * Xi)^.5
  norm_adj <- qnorm((1 - conf.level)/2, lower.tail = F)
  lower <- deriv1 - norm_adj*se_deriv1
  upper <- deriv1 + norm_adj*se_deriv1
  
  #results in dataframe, subset to sample dates
  df_tot <- data.frame(sample_collect_date = newDF$date, pred_out, deriv1, upper, lower)
  names(df_tot) <- c("sample_collect_date", df_names_out)
  df_sub <- subset(df_tot, sample_collect_date %in% date_copy) #
  
  #return list w/ model object if specified, else just dataframe
  if (model_return) {
    out_list <- list(model = mod, data_frame = df_sub)
    return(out_list)
  } else {
    return(df_sub) 
  }
}


## References for Later ##
#https://stats.stackexchange.com/questions/517375/splines-relationship-of-knots-degree-and-degrees-of-freedom
#https://stats.stackexchange.com/questions/531893/what-is-the-relationship-between-knots-and-cubic-spline-basis-functions-in-gams
#https://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=1141&context=usdeptcommercepub