#######################################################################
#######################################################################
####### EMPIRICAL ANALYSIS FOR THE COFFEE INSURANCE APPLICATION #######
#######################################################################
#######################################################################


### Load main packages
library(readr)
library(readxl)
library(tidyverse)

### Load additional packages
library(sf)
library(geobr)
library(RColorBrewer)
library(plm)
library(rqpd)
library(lqmm)
library(stargazer)
library(lmtest)
library(gplots)
library(visreg)
library(PerformanceAnalytics)
library(ceRtainty)

### Clean workspace
rm(list=ls())

### Set working directory
setwd("/Users/luisschmidt/Desktop/Doutorado/TESE/Dados/R")

### Load data
load("r-data/article-one-v2/df_2020.rda")

### Transform the data frame into a panel df
df_p <- pdata.frame(df_2020, index = c("municipality_name","year"), drop.index = FALSE)
df_p$muni_cod <- as.character(df_p$municipality_code)

######################################################################
######## YIELD-INDEX RELANTIONSHIP MODELS -- POOLED, FE, RQPD ########
######################################################################
# ------------------------------------------------------------------------------------------
# Pooled Model
# ------------------------------------------------------------------------------------------
pooled_spi_r1 <- plm(detrended_yield ~ r1_spi,
                     data = df_p,
                     model = 'pooling',
                     index = c("municipality_code", "year"))
pooled_spi_r2 <- plm(detrended_yield ~ r2_spi,
                     data = df_p,
                     model = 'pooling',
                     index = c("municipality_code", "year"))
pooled_spi_r1r2 <- plm(detrended_yield ~ r1r2_spi,
                     data = df_p,
                     model = 'pooling',
                     index = c("municipality_code", "year"))
stargazer(pooled_spi_r1, pooled_spi_r2, pooled_spi_r1r2, type = 'text')
#summary(pooled_spi_r2)
#S <- summary(pooled_spi)
#S$coef

mean(df_p$detrended_yield)

# ------------------------------------------------------------------------------------------
# Fixed Effects Model
# ------------------------------------------------------------------------------------------
fe_spi_r1 <- plm(detrended_yield ~ r1_spi,
                 data = df_p,
                 model = 'within',
                 #effect = 'twoways',
                 index = c("municipality_code", "year"))
fe_spi_r2 <- plm(detrended_yield ~ r2_spi,
                 data = df_p,
                 model = 'within',
                 #effect = 'twoways',
                 index = c("municipality_code", "year"))
fe_spi_r1r2 <- plm(detrended_yield ~ r1r2_spi,
                 data = df_p,
                 model = 'within',
                 #effect = 'twoways',
                 index = c("municipality_code", "year"))
stargazer(fe_spi_r1, fe_spi_r2, fe_spi_r1r2, type = 'text')
summary(fe_spi_r2)
within_intercept(fe_spi_r2, vcov = NULL, return.model = FALSE)
#S <- summary(fe_spi_r2)
#S$coef
#fixef <- as.data.frame(fixef(fe_spi_r2, type = "level"))
#summary(fixef(fe_spi_r2, type = "level"))
#summary(fixef(fe_spi_r2, type = "dmean"))
#within_intercept(fe_spi_r2, vcov = function(x) vcovHC(x, method="arellano", type="HC0"), return.model = FALSE)

# PFTEST BETWEEN POOLOED AND FIXED EFFECTS MODEL
pFtest(fe_spi_r2, pooled_spi_r2)
  
### Correlation matrix
#df <- select_if(df_p, is.numeric) %>% select("detrended_yield", contains("_spi"))
#r <- cor(df, use = "complete.obs")
#r <- round(r, 1)
#ggcorrplot(r, hc.order = TRUE, type = "lower", lab = "TRUE")

# ------------------------------------------------------------------------------------------
# Quantile Regression for Panel Data
# ------------------------------------------------------------------------------------------
rqpd_fe_r1 <- rqpd(detrended_yield ~ r1_spi | muni_cod,
                   panel(#taus=c(0.1, 0.25, 0.5, 0.75, 0.9),
                     #tauw=rep(1/5, 5),
                     taus = 0.3,
                     tauw = 1,
                     method = "pfe"),
                   data = df_p)
rqpd_fe_r2 <- rqpd(detrended_yield ~ r2_spi | muni_cod,
                   panel(#taus=c(0.10, 0.20, 0.30, 0.4, 0.5, 0.60, 0.70, 0.8, 0.9),
                     #tauw=rep(1/9, 9),
                     taus = 0.3,
                     tauw = 1,
                     method = "pfe"),
                   data = df_p)
rqpd_fe_r1r2 <- rqpd(detrended_yield ~ r1r2_spi | muni_cod,
                   panel(#taus=c(0.1, 0.25, 0.5, 0.75, 0.9),
                     #tauw=rep(1/5, 5),
                     taus = 0.3,
                     tauw = 1,
                     method = "pfe"),
                   data = df_p)
summary.rqpd(rqpd_fe_r2)

#S <- summary.rqpd(rqpd_fe_r2)
#S$coef
#residuals <- resid(rqpd_fe)
#mean(residuals)
#summary(rqpd_fe$coefficients)


###############################################################
######## CONTRACT SPECIFICATIONS AND HISTORICAL PAYOUT ########
###############################################################
rm(list = ls())
load("r-data/article-one-v2/df_2020.rda")
yield_exp <- aggregate(detrended_yield ~ municipality_code, df_2020, mean) %>%
  rename(yield_expec = detrended_yield)

premium_load = 0.0  # Define premium load (e.g. 0.1 for 10%)
subsidy = 0.0       # Define premium subsidy (e.g. 0.1 for 10%)
strike = 0          # Define strike (trigger) level
limit = -4          # Define limit (exit) level
price = 1           # Define price in R$ per 60 kg coffee bag
coverage = 1        # Define policy coverage (e.g. 0.6 for 60%)


df_actuarial <- df_2020 %>%
  left_join(yield_exp) %>%
  select(municipality_code, municipality_name:detrended_yield, yield_expec, r1_spi:r1r2_spei) %>%
  mutate(strike = strike) %>%
  mutate(limit = limit) %>%
  mutate(price = price) %>%
  mutate(liability = yield_expec*coverage*price) %>%
  mutate(tick = case_when(r2_spi > strike ~ 0,
                              r2_spi <= limit ~ 1,
                              limit < r2_spi & r2_spi <= strike ~ (strike - r2_spi)/(strike - limit)
                              )) %>%
  mutate(payout = liability*tick) %>%
  mutate(loss_ratio = payout/liability)


############################################################
######## PREMIUM RATE AND PREMIUM VALUE CALCULATION ########
############################################################
premium <- df_actuarial %>%
  group_by(municipality_code) %>%
  summarise_at(
    vars(loss_ratio),
    list(premium_rate = mean)
    ) %>%
  mutate(premium_rate_loaded = premium_rate*(1+premium_load)) %>%
  ungroup()
df_actuarial <- df_actuarial %>%
  left_join(premium) %>%
  mutate(premium_value = premium_rate*liability) %>%
  mutate(premium_value_loaded = premium_rate_loaded*liability)


####################################################################
######## FINAL WEALTH WITH AND WITHOUT INSURANCE + FINAL DF ########
####################################################################
df_actuarial <- df_actuarial %>%
  mutate(revenue = detrended_yield*price) %>%
  mutate(wealth_insurance = revenue - premium_value_loaded*(1-subsidy) + payout) %>%
  mutate(wealth_noinsurance = revenue) %>%
  select(municipality_code:year, geom, detrended_yield, yield_expec, r2_spi, r2_spei, strike:wealth_noinsurance)
save(df_actuarial, file = "r-data/article-one-v2/df_actuarial.rda")

mean(df_actuarial$liability) # Calculate the average liability for Table 9
mean(df_actuarial$payout) # Calculate the average payout for Table 9
#round(mean(df_actuarial$loss_ratio)*100,2)
round(mean(df_actuarial$premium_rate)*100,2) # Calculate the average premium rate for Table 9
round(mean(df_actuarial$premium_rate_loaded)*100,2) # Calculate the average premium rate for Table 9, scenario 2
round(mean(df_actuarial$premium_rate_loaded)*(1-subsidy)*100,2) # Calculate the average premium rate for Table 9, scenario 2
#mean(df_actuarial$premium_value)
#mean(df_actuarial$premium_value_loaded)
mean(df_actuarial$premium_value_loaded)/mean(df_actuarial$payout) # Calculate Clarke's m for Table 11

#conditional <- df_actuarial %>% subset(r2_spi < 0)
#mean(conditional$payout)
#mean(df_actuarial$payout)
#mean(conditional$payout)/mean(df_actuarial$payout)

# Tidy the environment
rm(df_2020, premium, yield_exp)


#########################################################
######## HEDGING EFFECTIVENESS AND RISK ANALYSIS ########
#########################################################
rm(list = ls())
load("r-data/article-one-v2/df_actuarial.rda")
# ------------------------------------------------------------------------------------------
# Mean-semivariance approach
# ------------------------------------------------------------------------------------------
mean <- df_actuarial %>%
  #group_by(municipality_code) %>%
  summarise(mean_insurance = mean(wealth_insurance),
            mean_noinsurance = mean(wealth_noinsurance)) %>%
  mutate(diff = mean_insurance - mean_noinsurance) %>%
  ungroup()

semivariance <- df_actuarial %>%
  #group_by(municipality_name) %>%
  summarise(sv_insurance = SemiVariance(wealth_insurance),
            sv_noinsurance = SemiVariance(wealth_noinsurance)) %>%
  mutate(diff = sv_insurance - sv_noinsurance) %>%
  ungroup()

ggplot(data = df_actuarial, aes(x = year), group = municipality_name) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance")) +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance")) + 
  facet_wrap(vars(municipality_name)) +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = "", y = "Final wealth in 60 kg bags/ha",
       title = "Final wealth in selected municipalities in South/Southwestern Minas Gerais with and without insurance adoption",
       #caption = "Research results",
       size = 20
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2018))

# ------------------------------------------------------------------------------------------
# SERF approach
# ------------------------------------------------------------------------------------------
ce <- df_actuarial %>%
  select(wealth_insurance, wealth_noinsurance) %>%
  ceRtainty::certainty(
    ival = .5,
    fval = 4,
    utility = "Power")
ce$CE_plot()

# Define dataframe to ggplot the wealths against the rrac
equivalents <- ce$CE_values %>%
  mutate(count = c(1:630)) %>%
  mutate(rac = seq(4, 0.5, by = -3.5/629)) %>%
  select(rac, wealth_insurance, wealth_noinsurance)

G_ce_c1 <-
  ggplot(data = equivalents, aes(x = rac)) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance"), linewidth=1.5, linetype = "solid") +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance"), linewidth=1.5, linetype = "dashed") +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
             ) +
  labs(x = "", y = "Wealth (60 kg bags/ha)",
       #title = "Title",
       #caption = "Research results",
       #size = 20
       ) +
  theme(text = element_text(colour="black",
                            size=22,
                            #family = "LM Roman 10",
                            #face="bold"
                            )) +
  theme(legend.position = "none") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2, 2.5, 3, 3.5, 4)) +
  ylim(31, 33.5)
ggsave("r-graphs/article-one-v2/G_ce_c1.png", G_ce_c1)


# The Risk premium values respect to wealth with insurance adoption
ce_values <- ce$CE_values  # CE table
ce_rac <- ce$RAC           # RAC vector

rp <- ceRtainty::premium(tbase = "wealth_insurance",
                         ce_data = ce$CE_values,
                         rac = ce$RAC,
                         utility = "Power")
#rp$PremiumRisk
#rp$PremiumRiskPer100
rp$RP_plot()

###############################################################################
###############################################################################
########################### ADDITIONAL CALCULATIONS ###########################
###############################################################################
###############################################################################

# ===== Contract 1: higher frequency, loaded premium, premium with subsidy =====
rm(list = ls())
load("r-data/article-one-v2/df_2020.rda")
yield_exp <- aggregate(detrended_yield ~ municipality_code, df_2020, mean) %>%
  rename(yield_expec = detrended_yield)

premium_load = 0.4  # Define premium load (e.g. 0.1 for 10%)
subsidy = 0.4       # Define premium subsidy (e.g. 0.1 for 10%)
strike = 0          # Define strike (trigger) level
limit = -4          # Define limit (exit) level
price = 1           # Define price in R$ per 60 kg coffee bag
coverage = 1      # Define policy coverage (e.g. 0.6 for 60%)

df_actuarial_b <- df_2020 %>%
  left_join(yield_exp) %>%
  select(municipality_code, municipality_name:detrended_yield, yield_expec, r1_spi:r1r2_spei) %>%
  mutate(strike = strike) %>%
  mutate(limit = limit) %>%
  mutate(price = price) %>%
  mutate(liability = yield_expec*coverage*price) %>%
  mutate(tick = case_when(r2_spi > strike ~ 0,
                              r2_spi <= limit ~ 1,
                              limit < r2_spi & r2_spi <= strike ~ (strike - r2_spi)/(strike - limit))) %>%
  mutate(payout = liability*tick) %>%
  mutate(loss_ratio = payout/liability)

premium <- df_actuarial_b %>%
  group_by(municipality_code) %>%
  summarise_at(
    vars(loss_ratio),
    list(premium_rate = mean)
  ) %>%
  mutate(premium_rate_loaded = premium_rate*(1+premium_load)) %>%
  ungroup()

df_actuarial_b <- df_actuarial_b %>%
  left_join(premium) %>%
  mutate(premium_value = premium_rate*liability) %>%
  mutate(premium_value_loaded = premium_rate_loaded*liability)

df_actuarial_b <- df_actuarial_b %>%
  mutate(revenue = detrended_yield*price) %>%
  mutate(wealth_insurance = revenue - premium_value_loaded*(1-subsidy) + payout) %>%
  mutate(wealth_noinsurance = revenue) %>%
  select(municipality_code:year, geom, detrended_yield, yield_expec, r2_spi, r2_spei, strike:wealth_noinsurance)
save(df_actuarial_b, file = "r-data/article-one-v2/df_actuarial_b.rda")

mean(df_actuarial_b$liability) # Calculate the average liability for Table 9
mean(df_actuarial_b$payout) # Calculate the average payout for Table 9
#round(mean(df_actuarial_b$loss_ratio)*100,2)
round(mean(df_actuarial_b$premium_rate)*100,2) # Calculate the average premium rate for Table 9
round(mean(df_actuarial_b$premium_rate_loaded)*100,2) # Calculate the average premium rate for Table 9, scenario 2
round(mean(df_actuarial_b$premium_rate_loaded)*(1-subsidy)*100,2) # Calculate the average premium rate for Table 9, scenario 2
mean(df_actuarial_b$premium_value)
mean(df_actuarial_b$premium_value_loaded)
mean(df_actuarial_b$premium_value)/mean(df_actuarial_b$payout)
mean(df_actuarial_b$premium_value_loaded)/mean(df_actuarial_b$payout) # Calculate Clarke's m for Table 11
mean(df_actuarial_b$premium_value_loaded)*(1-subsidy)/mean(df_actuarial_b$payout)

#conditional_b <- df_actuarial_b %>% subset(r2_spi < 0)
#mean(conditional_b$payout)
#mean(df_actuarial_b$payout)
#mean(conditional_b$payout)/mean(df_actuarial_b$payout)

# Tidy the environment
rm(df_2020, premium, yield_exp)

mean_b <- df_actuarial_b %>%
  #group_by(municipality_code) %>%
  summarise(mean_insurance = mean(wealth_insurance),
            mean_noinsurance = mean(wealth_noinsurance)) %>%
  mutate(diff = mean_insurance - mean_noinsurance) %>%
  ungroup()

semivariance_b <- df_actuarial_b %>%
  #group_by(municipality_name) %>%
  summarise(sv_insurance = SemiVariance(wealth_insurance),
            sv_noinsurance = SemiVariance(wealth_noinsurance)) %>%
  mutate(diff = sv_insurance - sv_noinsurance) %>%
  ungroup()

G_wealth_b <-
  ggplot(data = df_actuarial_b, aes(x = year), group = municipality_name) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance"), linewidth = 0.5, linetype = "solid") +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance"), linewidth = 0.5, linetype = "dashed") +
  scale_color_manual(values=c("midnightblue", "darkred")) +
  facet_wrap(vars(municipality_name)) +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = "", y = "", #Final wealth in 60 kg bags/ha
       #title = "Final wealth in selected municipalities in South/Southwestern Minas Gerais with and without insurance adoption",
       #caption = "Research results",
       size = 20
  ) +
  theme(text = element_text(colour="black",
                            size=13,
                            #family = "LM Roman 10",
                            #face="bold"
  )) +
  theme(legend.position = "none") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2018))
ggsave("r-graphs/article-one-v2/G_wealth_b.png", G_wealth_b)

ce <- df_actuarial_b %>%
  select(wealth_insurance, wealth_noinsurance) %>%
  ceRtainty::certainty(
    ival = .5,
    fval = 4,
    utility = "Power")
ce$CE_plot()

equivalents <- ce$CE_values %>%
  mutate(count = c(1:630)) %>%
  mutate(rac = seq(4, 0.5, by = -3.5/629)) %>%
  select(rac, wealth_insurance, wealth_noinsurance)

G_ce_c1_b <-
  ggplot(data = equivalents, aes(x = rac)) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance"), linewidth=1.5, linetype = "solid") +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance"), linewidth=1.5, linetype = "dashed") +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = " ", y =  "",
       #title = "Title",
       #caption = "Research results",
       #size = 20
  ) +
  theme(text = element_text(colour="black",
                            size=22,
                            #family = "LM Roman 10",
                            #face="bold"
  )) +
  theme(legend.position = "none") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2, 2.5, 3, 3.5, 4)) +
  ylim(31, 33.5)
ggsave("r-graphs/article-one-v2/G_ce_c1_b.png", G_ce_c1_b)

# The Risk premium values respect to wealth with insurance adoption
ce_values <- ce$CE_values  # CE table
ce_rac <- ce$RAC           # RAC vector

rp <- ceRtainty::premium(tbase = "wealth_insurance",
                         ce_data = ce$CE_values,
                         rac = ce$RAC,
                         utility = "Power")
#rp$PremiumRisk
#rp$PremiumRiskPer100
rp$RP_plot()

# ===== Contract 2: lower frequency, fair premium, no premium subsidy =====
rm(list = ls())
load("r-data/article-one-v2/df_2020.rda")
yield_exp <- aggregate(detrended_yield ~ municipality_code, df_2020, mean) %>%
  rename(yield_expec = detrended_yield)

premium_load = 0.0  # Define premium load (e.g. 0.1 for 10%)
subsidy = 0.0       # Define premium subsidy (e.g. 0.1 for 10%)
strike = -1.5          # Define strike (trigger) level
limit = -2          # Define limit (exit) level
price = 1           # Define price in R$ per 60 kg coffee bag
coverage = 1      # Define policy coverage (e.g. 0.6 for 60%)

df_actuarial_c2 <- df_2020 %>%
  left_join(yield_exp) %>%
  select(municipality_code, municipality_name:detrended_yield, yield_expec, r1_spi:r1r2_spei) %>%
  mutate(strike = strike) %>%
  mutate(limit = limit) %>%
  mutate(price = price) %>%
  mutate(liability = yield_expec*coverage*price) %>%
  mutate(tick = case_when(r2_spi > strike ~ 0,
                              r2_spi <= limit ~ 1,
                              limit < r2_spi & r2_spi <= strike ~ (strike - r2_spi)/(strike - limit))) %>%
  mutate(payout = liability*tick) %>%
  mutate(loss_ratio = payout/liability)

premium <- df_actuarial_c2 %>%
  group_by(municipality_code) %>%
  summarise_at(
    vars(loss_ratio),
    list(premium_rate = mean)
  ) %>%
  mutate(premium_rate_loaded = premium_rate*(1+premium_load)) %>%
  ungroup()

df_actuarial_c2 <- df_actuarial_c2 %>%
  left_join(premium) %>%
  mutate(premium_value = premium_rate*liability) %>%
  mutate(premium_value_loaded = premium_rate_loaded*liability)

df_actuarial_c2 <- df_actuarial_c2 %>%
  mutate(revenue = detrended_yield*price) %>%
  mutate(wealth_insurance = revenue - premium_value_loaded*(1-subsidy) + payout) %>%
  mutate(wealth_noinsurance = revenue) %>%
  select(municipality_code:year, geom, detrended_yield, yield_expec, r2_spi, r2_spei, strike:wealth_noinsurance)
save(df_actuarial_c2, file = "r-data/article-one-v2/df_actuarial_c2.rda")

mean(df_actuarial_c2$liability)
mean(df_actuarial_c2$payout)
#round(mean(df_actuarial_c2$loss_ratio)*100,2)
round(mean(df_actuarial_c2$premium_rate)*100,2)
round(mean(df_actuarial_c2$premium_rate_loaded)*100,2)
round(mean(df_actuarial_c2$premium_rate_loaded)*(1-subsidy)*100,2)
#mean(df_actuarial_c2$premium_value)
mean(df_actuarial_c2$premium_value_loaded)*(1-subsidy)
mean(df_actuarial_c2$premium_value_loaded)/mean(df_actuarial_c2$payout)
mean(df_actuarial_c2$premium_value_loaded)*(1-subsidy)/mean(df_actuarial_c2$payout)

# Tidy the environment
rm(df_2020, premium, yield_exp)

mean_c2 <- df_actuarial_c2 %>%
  #group_by(municipality_code) %>%
  summarise(mean_insurance = mean(wealth_insurance),
            mean_noinsurance = mean(wealth_noinsurance)) %>%
  mutate(diff = mean_insurance - mean_noinsurance) %>%
  ungroup()

semivariance_c2 <- df_actuarial_c2 %>%
  #group_by(municipality_name) %>%
  summarise(sv_insurance = SemiVariance(wealth_insurance),
            sv_noinsurance = SemiVariance(wealth_noinsurance)) %>%
  mutate(diff = sv_insurance - sv_noinsurance) %>%
  ungroup()

ggplot(data = df_actuarial_c2, aes(x = year), group = municipality_name) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance")) +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance")) + 
  facet_wrap(vars(municipality_name)) +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = "", y = "Final wealth in 60 kg bags/ha",
       title = "Final wealth in selected municipalities in South/Southwestern Minas Gerais with and without insurance adoption",
       #caption = "Research results",
       size = 20
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2018))

ce <- df_actuarial_c2 %>%
  select(wealth_insurance, wealth_noinsurance) %>%
  ceRtainty::certainty(
    ival = .5,
    fval = 4,
    utility = "Power")
ce$CE_plot()

equivalents <- ce$CE_values %>%
  mutate(count = c(1:630)) %>%
  mutate(rac = seq(4, 0.5, by = -3.5/629)) %>%
  select(rac, wealth_insurance, wealth_noinsurance)

G_ce_c2 <-
  ggplot(data = equivalents, aes(x = rac)) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance"), linewidth=1.5, linetype = "solid") +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance"), linewidth=1.5, linetype = "dashed") +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = " ", y = "Wealth (60 kg bags/ha)",
       #title = "Title",
       #caption = "Research results",
       #size = 20
  ) +
  theme(text = element_text(colour="black",
                            size=22,
                            #family = "LM Roman 10",
                            #face="bold"
  )) +
  theme(legend.position = "none") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2, 2.5, 3, 3.5, 4)) +
  ylim(31, 33.5)
ggsave("r-graphs/article-one-v2/G_ce_c2.png", G_ce_c2)

# The Risk premium values respect to wealth with insurance adoption
ce_values <- ce$CE_values  # CE table
ce_rac <- ce$RAC           # RAC vector

rp <- ceRtainty::premium(tbase = "wealth_insurance",
                         ce_data = ce$CE_values,
                         rac = ce$RAC,
                         utility = "Power")
#rp$PremiumRisk
#rp$PremiumRiskPer100
rp$RP_plot()



# ===== Contract 2: lower frequency, loaded premium, premium with subsidy =====
rm(list = ls())
load("r-data/article-one-v2/df_2020.rda")
yield_exp <- aggregate(detrended_yield ~ municipality_code, df_2020, mean) %>%
  rename(yield_expec = detrended_yield)

premium_load = 0.4  # Define premium load (e.g. 0.1 for 10%)
subsidy = 0.4       # Define premium subsidy (e.g. 0.1 for 10%)
strike = -1.5          # Define strike (trigger) level
limit = -2          # Define limit (exit) level
price = 1           # Define price in R$ per 60 kg coffee bag
coverage = 1      # Define policy coverage (e.g. 0.6 for 60%)

df_actuarial_c2_b <- df_2020 %>%
  left_join(yield_exp) %>%
  select(municipality_code, municipality_name:detrended_yield, yield_expec, r1_spi:r1r2_spei) %>%
  mutate(strike = strike) %>%
  mutate(limit = limit) %>%
  mutate(price = price) %>%
  mutate(liability = yield_expec*coverage*price) %>%
  mutate(tick = case_when(r2_spi > strike ~ 0,
                              r2_spi <= limit ~ 1,
                              limit < r2_spi & r2_spi <= strike ~ (strike - r2_spi)/(strike - limit))) %>%
  mutate(payout = liability*tick) %>%
  mutate(loss_ratio = payout/liability)

premium <- df_actuarial_c2_b %>%
  group_by(municipality_code) %>%
  summarise_at(
    vars(loss_ratio),
    list(premium_rate = mean)
  ) %>%
  mutate(premium_rate_loaded = premium_rate*(1+premium_load)) %>%
  ungroup()

df_actuarial_c2_b <- df_actuarial_c2_b %>%
  left_join(premium) %>%
  mutate(premium_value = premium_rate*liability) %>%
  mutate(premium_value_loaded = premium_rate_loaded*liability)

df_actuarial_c2_b <- df_actuarial_c2_b %>%
  mutate(revenue = detrended_yield*price) %>%
  mutate(wealth_insurance = revenue - premium_value_loaded*(1-subsidy) + payout) %>%
  mutate(wealth_noinsurance = revenue) %>%
  select(municipality_code:year, geom, detrended_yield, yield_expec, r2_spi, r2_spei, strike:wealth_noinsurance)
save(df_actuarial_c2_b, file = "r-data/article-one-v2/df_actuarial_c2_b.rda")

mean(df_actuarial_c2_b$liability)
mean(df_actuarial_c2_b$payout)
#round(mean(df_actuarial_c2_b$loss_ratio)*100,2)
round(mean(df_actuarial_c2_b$premium_rate)*100,2)
round(mean(df_actuarial_c2_b$premium_rate_loaded)*100,2)
round(mean(df_actuarial_c2_b$premium_rate_loaded)*(1-subsidy)*100,2)
#mean(df_actuarial_c2_b$premium_value)
#mean(df_actuarial_c2_b$premium_value_loaded)*(1-subsidy)
mean(df_actuarial_c2_b$premium_value_loaded)/mean(df_actuarial_c2_b$payout)
mean(df_actuarial_c2_b$premium_value_loaded)*(1-subsidy)/mean(df_actuarial_c2_b$payout)


# Tidy the environment
rm(df_2020, premium, yield_exp)

mean_c2_b <- df_actuarial_c2_b %>%
  #group_by(municipality_code) %>%
  summarise(mean_insurance = mean(wealth_insurance),
            mean_noinsurance = mean(wealth_noinsurance)) %>%
  mutate(diff = mean_insurance - mean_noinsurance) %>%
  ungroup()

semivariance_c2_b <- df_actuarial_c2_b %>%
  #group_by(municipality_name) %>%
  summarise(sv_insurance = SemiVariance(wealth_insurance),
            sv_noinsurance = SemiVariance(wealth_noinsurance)) %>%
  mutate(diff = sv_insurance - sv_noinsurance) %>%
  ungroup()

ggplot(data = df_actuarial_c2_b, aes(x = year), group = municipality_name) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance")) +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance")) + 
  facet_wrap(vars(municipality_name)) +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = "", y = "Final wealth in 60 kg bags/ha",
       title = "Final wealth in selected municipalities in South/Southwestern Minas Gerais with and without insurance adoption",
       #caption = "Research results",
       size = 20
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2018))

ce <- df_actuarial_c2_b %>%
  select(wealth_insurance, wealth_noinsurance) %>%
  ceRtainty::certainty(
    ival = .5,
    fval = 4,
    utility = "Power")
ce$CE_plot()

equivalents <- ce$CE_values %>%
  mutate(count = c(1:630)) %>%
  mutate(rac = seq(4, 0.5, by = -3.5/629)) %>%
  select(rac, wealth_insurance, wealth_noinsurance)

G_ce_c2_b <-
  ggplot(data = equivalents, aes(x = rac)) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance"), linewidth=1.5, linetype = "solid") +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance"), linewidth=1.5, linetype = "dashed") +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = " ", y = " ",
       #title = "Title",
       #caption = "Research results",
       #size = 20
  ) +
  theme(text = element_text(colour="black",
                            size=20,
                            #family = "LM Roman 10",
                            #face="bold"
  )) +
  theme(legend.position = "none") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2, 2.5, 3, 3.5, 4)) +
  ylim(31, 33.5)
ggsave("r-graphs/article-one-v2/G_ce_c2_b.png", G_ce_c2_b)

# The Risk premium values respect to wealth with insurance adoption
ce_values <- ce$CE_values  # CE table
ce_rac <- ce$RAC           # RAC vector

rp <- ceRtainty::premium(tbase = "wealth_insurance",
                         ce_data = ce$CE_values,
                         rac = ce$RAC,
                         utility = "Power")
#rp$PremiumRisk
#rp$PremiumRiskPer100
rp$RP_plot()


# ===== Contract 3: all-or-nothing, fair premium, no premium subsidy =====
rm(list = ls())
load("r-data/article-one-v2/df_2020.rda")
yield_exp <- aggregate(detrended_yield ~ municipality_code, df_2020, mean) %>%
  rename(yield_expec = detrended_yield)

premium_load = 0.0  # Define premium load (e.g. 0.1 for 10%)
subsidy = 0.0       # Define premium subsidy (e.g. 0.1 for 10%)
strike = -2          # Define strike (trigger) level
limit = -2         # Define limit (exit) level
price = 1           # Define price in R$ per 60 kg coffee bag
coverage = 1      # Define policy coverage (e.g. 0.6 for 60%)

df_actuarial_c3 <- df_2020 %>%
  left_join(yield_exp) %>%
  select(municipality_code, municipality_name:detrended_yield, yield_expec, r1_spi:r1r2_spei) %>%
  mutate(strike = strike) %>%
  mutate(limit = limit) %>%
  mutate(price = price) %>%
  mutate(liability = yield_expec*coverage*price) %>%
  mutate(tick = case_when(r2_spi > strike ~ 0,
                          r2_spi <= strike ~ 1)) %>%
  mutate(payout = liability*tick) %>%
  mutate(loss_ratio = payout/liability)

premium <- df_actuarial_c3 %>%
  group_by(municipality_code) %>%
  summarise_at(
    vars(loss_ratio),
    list(premium_rate = mean)
  ) %>%
  mutate(premium_rate_loaded = premium_rate*(1+premium_load)) %>%
  ungroup()

df_actuarial_c3 <- df_actuarial_c3 %>%
  left_join(premium) %>%
  mutate(premium_value = premium_rate*liability) %>%
  mutate(premium_value_loaded = premium_rate_loaded*liability)

df_actuarial_c3 <- df_actuarial_c3 %>%
  mutate(revenue = detrended_yield*price) %>%
  mutate(wealth_insurance = revenue - premium_value_loaded*(1-subsidy) + payout) %>%
  mutate(wealth_noinsurance = revenue) %>%
  select(municipality_code:year, geom, detrended_yield, yield_expec, r2_spi, r2_spei, strike:wealth_noinsurance)
save(df_actuarial_c3, file = "r-data/article-one-v2/df_actuarial_c3.rda")

mean(df_actuarial_c3$liability)
mean(df_actuarial_c3$payout)
round(mean(df_actuarial_c3$loss_ratio)*100,2)
round(mean(df_actuarial_c3$premium_rate)*100,2)
round(mean(df_actuarial_c3$premium_rate_loaded)*100,2)
round(mean(df_actuarial_c3$premium_rate_loaded)*(1-subsidy)*100,2)
mean(df_actuarial_c3$premium_value)
mean(df_actuarial_c3$premium_value_loaded)*(1-subsidy)
mean(df_actuarial_c3$premium_value_loaded)/mean(df_actuarial_c3$payout)
mean(df_actuarial_c3$premium_value_loaded)*(1-subsidy)/mean(df_actuarial_c3$payout)


# Tidy the environment
rm(df_2020, premium, yield_exp)

mean_c3 <- df_actuarial_c3 %>%
  #group_by(municipality_code) %>%
  summarise(mean_insurance = mean(wealth_insurance),
            mean_noinsurance = mean(wealth_noinsurance)) %>%
  mutate(diff = mean_insurance - mean_noinsurance) %>%
  ungroup()

semivariance_c3 <- df_actuarial_c3 %>%
  #group_by(municipality_name) %>%
  summarise(sv_insurance = SemiVariance(wealth_insurance),
            sv_noinsurance = SemiVariance(wealth_noinsurance)) %>%
  mutate(diff = sv_insurance - sv_noinsurance) %>%
  ungroup()

ggplot(data = df_actuarial_c3, aes(x = year), group = municipality_name) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance")) +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance")) + 
  facet_wrap(vars(municipality_name)) +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = "", y = "Final wealth in 60 kg bags/ha",
       title = "Final wealth in selected municipalities in South/Southwestern Minas Gerais with and without insurance adoption",
       #caption = "Research results",
       size = 20
  ) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2018))

ce <- df_actuarial_c3 %>%
  select(wealth_insurance, wealth_noinsurance) %>%
  ceRtainty::certainty(
    ival = .5,
    fval = 4,
    utility = "Power")
ce$CE_plot()

equivalents <- ce$CE_values %>%
  mutate(count = c(1:630)) %>%
  mutate(rac = seq(4, 0.5, by = -3.5/629)) %>%
  select(rac, wealth_insurance, wealth_noinsurance)

G_ce_c3 <-
  ggplot(data = equivalents, aes(x = rac)) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance"), linewidth=1.5, linetype = "solid") +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance"), linewidth=1.5, linetype = "dashed") +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = "Relative risk aversion coefficient", y = "Wealth (60 kg bags/ha)",
       #title = "Title",
       #caption = "Research results",
       #size = 20
  ) +
  theme(text = element_text(colour="black",
                            size=22,
                            #family = "LM Roman 10",
                            #face="bold"
  )) +
  theme(legend.position = "none") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2, 2.5, 3, 3.5, 4)) +
  ylim(31, 33.5)
ggsave("r-graphs/article-one-v2/G_ce_c3.png", G_ce_c3)

# The Risk premium values respect to wealth with insurance adoption
ce_values <- ce$CE_values  # CE table
ce_rac <- ce$RAC           # RAC vector

rp <- ceRtainty::premium(tbase = "wealth_insurance",
                         ce_data = ce$CE_values,
                         rac = ce$RAC,
                         utility = "Power")
#rp$PremiumRisk
#rp$PremiumRiskPer100
rp$RP_plot()



# ===== Contract 3: all-or-nothing, loaded premium, premium with subsidy =====
rm(list = ls())
load("r-data/article-one-v2/df_2020.rda")
yield_exp <- aggregate(detrended_yield ~ municipality_code, df_2020, mean) %>%
  rename(yield_expec = detrended_yield)

premium_load = 0.4  # Define premium load (e.g. 0.1 for 10%)
subsidy = 0.4       # Define premium subsidy (e.g. 0.1 for 10%)
strike = -2          # Define strike (trigger) level
limit = -2         # Define limit (exit) level
price = 1           # Define price in R$ per 60 kg coffee bag
coverage = 1      # Define policy coverage (e.g. 0.6 for 60%)

df_actuarial_c3_b <- df_2020 %>%
  left_join(yield_exp) %>%
  select(municipality_code, municipality_name:detrended_yield, yield_expec, r1_spi:r1r2_spei) %>%
  mutate(strike = strike) %>%
  mutate(limit = limit) %>%
  mutate(price = price) %>%
  mutate(liability = yield_expec*coverage*price) %>%
  mutate(tick = case_when(r2_spi > strike ~ 0,
                          r2_spi <= strike ~ 1)) %>%
  mutate(payout = liability*tick) %>%
  mutate(loss_ratio = payout/liability)

premium <- df_actuarial_c3_b %>%
  group_by(municipality_code) %>%
  summarise_at(
    vars(loss_ratio),
    list(premium_rate = mean)
  ) %>%
  mutate(premium_rate_loaded = premium_rate*(1+premium_load)) %>%
  ungroup()

df_actuarial_c3_b <- df_actuarial_c3_b %>%
  left_join(premium) %>%
  mutate(premium_value = premium_rate*liability) %>%
  mutate(premium_value_loaded = premium_rate_loaded*liability)

df_actuarial_c3_b <- df_actuarial_c3_b %>%
  mutate(revenue = detrended_yield*price) %>%
  mutate(wealth_insurance = revenue - premium_value_loaded*(1-subsidy) + payout) %>%
  mutate(wealth_noinsurance = revenue) %>%
  select(municipality_code:year, geom, detrended_yield, yield_expec, r2_spi, r2_spei, strike:wealth_noinsurance)
save(df_actuarial_c3_b, file = "r-data/article-one-v2/df_actuarial_c3_b.rda")

mean(df_actuarial_c3_b$liability)
mean(df_actuarial_c3_b$payout)
round(mean(df_actuarial_c3_b$loss_ratio)*100,2)
round(mean(df_actuarial_c3_b$premium_rate)*100,2)
round(mean(df_actuarial_c3_b$premium_rate_loaded)*100,2)
round(mean(df_actuarial_c3_b$premium_rate_loaded)*(1-subsidy)*100,2)
mean(df_actuarial_c3_b$premium_value)
mean(df_actuarial_c3_b$premium_value_loaded)*(1-subsidy)
mean(df_actuarial_c3_b$premium_value_loaded)/mean(df_actuarial_c3_b$payout)
mean(df_actuarial_c3_b$premium_value_loaded)*(1-subsidy)/mean(df_actuarial_c3_b$payout)


# Tidy the environment
rm(df_2020, premium, yield_exp)

mean_c3_b <- df_actuarial_c3_b %>%
  #group_by(municipality_code) %>%
  summarise(mean_insurance = mean(wealth_insurance),
            mean_noinsurance = mean(wealth_noinsurance)) %>%
  mutate(diff = mean_insurance - mean_noinsurance) %>%
  ungroup()

semivariance_c3_b <- df_actuarial_c3_b %>%
  #group_by(municipality_name) %>%
  summarise(sv_insurance = SemiVariance(wealth_insurance),
            sv_noinsurance = SemiVariance(wealth_noinsurance)) %>%
  mutate(diff = sv_insurance - sv_noinsurance) %>%
  ungroup()

G_wealth_c3_b <-
  ggplot(data = df_actuarial_c3_b, aes(x = year), group = municipality_name) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance"), linewidth = 0.5, linetype = "solid") +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance"), linewidth = 0.5, linetype = "dashed") +
  scale_color_manual(values=c("midnightblue", "darkred")) +
  facet_wrap(vars(municipality_name)) +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = "", y = "", #Final wealth in 60 kg bags/ha
       #title = "Final wealth in selected municipalities in South/Southwestern Minas Gerais with and without insurance adoption",
       #caption = "Research results",
       size = 20
  ) +
  theme(text = element_text(colour="black",
                            size=13,
                            #family = "LM Roman 10",
                            #face="bold"
  )) +
  theme(legend.position = "none") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(2006, 2010, 2014, 2018))
ggsave("r-graphs/article-one-v2/G_wealth_c3_b.png", G_wealth_c3_b)

ce <- df_actuarial_c3_b %>%
  select(wealth_insurance, wealth_noinsurance) %>%
  ceRtainty::certainty(
    ival = .5,
    fval = 4,
    utility = "Power")
ce$CE_plot()

equivalents <- ce$CE_values %>%
  mutate(count = c(1:630)) %>%
  mutate(rac = seq(4, 0.5, by = -3.5/629)) %>%
  select(rac, wealth_insurance, wealth_noinsurance)

G_ce_c3_b <-
  ggplot(data = equivalents, aes(x = rac)) +
  geom_line(aes(y = wealth_insurance, colour = "wealth_insurance"), linewidth=1.5, linetype = "solid") +
  geom_line(aes(y = wealth_noinsurance, colour = "wealth_noinsurance"), linewidth=1.5, linetype = "dashed") +
  theme_grey(base_size = 11,
             #base_family = "",
             base_line_size = 11/22,
             base_rect_size = 11/22
  ) +
  labs(x = "Relative risk aversion coefficient", y = "",
       #title = "Title",
       #caption = "Research results",
       #size = 20
  ) +
  theme(text = element_text(colour="black",
                            size=22,
                            #family = "LM Roman 10",
                            #face="bold"
  )) +
  theme(legend.position = "none") +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(breaks = c(0.5, 1.0, 1.5, 2, 2.5, 3, 3.5, 4)) +
  ylim(31, 33.5)
ggsave("r-graphs/article-one-v2/G_ce_c3_b.png", G_ce_c3_b)

# The Risk premium values respect to wealth with insurance adoption
ce_values <- ce$CE_values  # CE table
ce_rac <- ce$RAC           # RAC vector

rp <- ceRtainty::premium(tbase = "wealth_insurance",
                         ce_data = ce$CE_values,
                         rac = ce$RAC,
                         utility = "Power")
#rp$PremiumRisk
#rp$PremiumRiskPer100
rp$RP_plot()


### THE FOLLOWING IS NOT USED IN THE PAPER ###
# ===== SPEI: Models with SPEI as the index regressor =====
# Pooled Model
pooled_spei_r1 <- plm(detrended_yield ~ r1_spei,
                     data = df_p,
                     model = 'pooling',
                     index = c("municipality_code", "year"))
pooled_spei_r2 <- plm(detrended_yield ~ r2_spei,
                     data = df_p,
                     model = 'pooling',
                     index = c("municipality_code", "year"))
pooled_spei_r1r2 <- plm(detrended_yield ~ r1r2_spei,
                       data = df_p,
                       model = 'pooling',
                       index = c("municipality_code", "year"))
stargazer(pooled_spei_r1, pooled_spei_r2, pooled_spei_r1r2, type = 'text')

# Fixed Effects Model
fe_spei_r1 <- plm(detrended_yield ~ r1_spei,
                 data = df_p,
                 model = 'within',
                 #effect = 'twoways',
                 index = c("municipality_code", "year"))
fe_spei_r2 <- plm(detrended_yield ~ r2_spei,
                 data = df_p,
                 model = 'within',
                 #effect = 'twoways',
                 index = c("municipality_code", "year"))
fe_spei_r1r2 <- plm(detrended_yield ~ r1r2_spei,
                   data = df_p,
                   model = 'within',
                   #effect = 'twoways',
                   index = c("municipality_code", "year"))
stargazer(fe_spei_r1, fe_spei_r2, fe_spei_r1r2, type = 'text')
summary(fe_spei_r2)
within_intercept(fe_spei_r1, vcov = NULL, return.model = FALSE)
within_intercept(fe_spei_r2, vcov = NULL, return.model = FALSE)
within_intercept(fe_spei_r1r2, vcov = NULL, return.model = FALSE)

# Quantile Regression for Panel Data
rqpd_fe_r1 <- rqpd(detrended_yield ~ r1_spei | muni_cod,
                   panel(#taus=c(0.1, 0.25, 0.5, 0.75, 0.9),
                     #tauw=rep(1/5, 5),
                     taus = 0.3,
                     tauw = 1,
                     method = "pfe"),
                   data = df_p)
rqpd_fe_r2 <- rqpd(detrended_yield ~ r2_spei | muni_cod,
                   panel(#taus=c(0.1, 0.20, 0.30, 0.4, 0.5, 0.60, 0.70, 0.8, 0.9),
                     #tauw=rep(1/9, 9),
                     taus = 0.3,
                     tauw = 1,
                     method = "pfe"),
                   data = df_p)
rqpd_fe_r1r2 <- rqpd(detrended_yield ~ r1r2_spei | muni_cod,
                     panel(#taus=c(0.1, 0.25, 0.5, 0.75, 0.9),
                       #tauw=rep(1/5, 5),
                       taus = 0.3,
                       tauw = 1,
                       method = "pfe"),
                     data = df_p)
summary.rqpd(rqpd_fe_r1)
summary.rqpd(rqpd_fe_r2)
summary.rqpd(rqpd_fe_r1r2)

# Correlation between SPI and SPEI indexes
cor(df_2020$r2_spi, df_2020$r2_spei,
    method = "pearson",
    #method = "kendall",
    #method = "spearman"
    )


