# 1. SETUP ----------------------------------------------------------------

library(tidyverse)   # process dataframe
library(lme4)        # lme model
library(lmerTest)    # lme model test
library(MuMIn)       # model selection
library(effects)     # present effects
library(AICcmodavg)  # calculate AIC
library(patchwork)   # arrange plot

# FUNCTION
## scaleing function
s. <- function(x) {(x - mean(x))/sd(x)} 

##centring function
#c. <- function (x) {(x - mean(x))} 

#function for data exploration from Zuur et al., 2010
source("D:/DEM/6. NIPRA/EoMP II/ref eomp/Zuur et al., 2010/HighstatLib.r") 

# dir_model
dir_model <- "./model"

# 2. LOAD DATA ------------------------------------------------------------
data_raw <- read_rds("./data/EoMPII_data.rds")

# process data
## remove NA values
data <- data_raw %>% filter(is.na(life.expectancy) == F,
                            is.na(under5.mortality) == F,
                            is.na(gdp.per.capita) == F,
                            is.na(population.density) == F,
                            is.na(urban.population) == F,
                            is.na(basic.drinking.service) == F,
                            is.na(basic.sanitation.service) == F,
                            is.na(access.electricity) == F,
                            is.na(incidence.tuberculosis) == F,
                            is.na(institutional.index) == F,
                            is.na(commitment.total) == F,
                            is.na(commitment.bilaterals) == F,
                            is.na(commitment.multilaterals) == F,
                            is.na(disbursement.total) == F,
                            is.na(disbursement.bilaterals) == F,
                            is.na(disbursement.multilaterals) == F,
                            is.na(aid.volatility.commitment) == F,
                            is.na(aid.volatility.disbursement) == F)

## check year range of data
data_range <-  data %>% group_by(country) %>% summarize(year.min = min(year),
                                                        year.max = max(year),
                                                        year.range = paste0(min(year), "-", max(year)))

## only keep data with year.range >= 15
list_country <- data_range %>% filter(year.max - year.min > 15)

data <- data %>% filter(country %in% list_country$country)

## create new field: gpd.growth (%)
data <- data %>% group_by(country) %>% mutate(gdp.growth = gdp.per.capita/min(gdp.per.capita)*100 - 100)
data <- ungroup(data)

# 3. EXPLORE DATA ---------------------------------------------------------

## 3.0. Explore data -------------
### description
table1(~life.expectancy + under5.mortality, data = data)
table1(~commitment.total + disbursement.total + aid.volatility.commitment + aid.volatility.disbursement, data = data)
table1(~institutional.index + vae + pve + gee + rqe + rle + cce, data = data)
table1(~gdp.growth + population.density + urban.population + basic.drinking.service + basic.sanitation.service + access.electricity + incidence.tuberculosis, data = data)
### collinearity (use threshold 3)
#### aid.commitment
Z <- data %>% select(gdp.growth, population.density:incidence.tuberculosis, 
                     institutional.index,commitment.total, aid.volatility.commitment)

corvif(Z) 
# collinearity access.electricity, basic.drinking.service, urban.population
## remove access.electricity
Z <- data %>% select(gdp.growth, population.density:incidence.tuberculosis,
                     institutional.index,commitment.total, aid.volatility.commitment,
                     -access.electricity)

corvif(Z) 

## remove access.electricity, basic.drinking.service
Z <- data %>% select(gdp.growth, population.density:incidence.tuberculosis, 
                     institutional.index,commitment.total, aid.volatility.commitment,
                     -access.electricity, -basic.drinking.service)

corvif(Z) 

## 3.1. life.expectancy ---------------------------------------------------------

# life.expectancy vs gpd.growth
ggplot(data = data, aes(x = gdp.growth, y = life.expectancy, color = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none")
## different gpd.per.capita slope for each country

# life.expectancy vs aid
ggplot(data = data, aes(x = commitment.total, y = life.expectancy, color = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none")
## different commitment.total slope for each country

# life.expectancy vs aid.volatility vs institutional.index
ggplot(data = data, aes(x = aid.volatility.commitment, y = life.expectancy, color = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none")

ggplot(data = data, aes(x = institutional.index, y = life.expectancy, color = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none") 

## ??? different aid.volatility, institutional.index slope for each country 
ggplot(data = data, aes(x = log(gdp.per.capita), y = population.density, color = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none")

## 3.2. under5.mortality ---------------------------------------------------------

# under5.mortality vs gdp.growth
ggplot(data = data, aes(x = gdp.growth, y = under5.mortality, color = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none")

# under5.mortality vs aid
ggplot(data = data, aes(x = commitment.total, y = under5.mortality, color = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none")

## different gdp.per.capita slope for each country

# under5.mortality vs aid.volatility vs institutional.index
ggplot(data = data, aes(x = aid.volatility.commitment, y = under5.mortality, color = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme(legend.position = "none")

ggplot(data = data, aes(x = institutional.index, y = under5.mortality, color = country)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = "none")


# 4. ANALYZE DATA ---------------------------------------------------------

# response variable: life.expectancy/under5.mortality

# explanatory variables of interest (fixed effects): 
## gdp.growth
## aid: commitment/disbursement
## aid.volatility: commitment/disbursement
## institutional.index

# control variables (fixed effects): 
## population.density, urban.population, 
## basic.sanitation.service, incidence.tuberculosis

# random effects
## country
## year

# scale variables to facilitate model covergence + change year to factor
data <- data %>% mutate(s_gdp.per.capita = s.(gdp.per.capita),
                        s_gdp.growth = s.(gdp.growth),
                        s_population.density = s.(population.density),
                        s_urban.population = s.(urban.population),
                        s_basic.drinking.service = s.(basic.drinking.service),
                        s_basic.sanitation.service = s.(basic.sanitation.service),
                        s_access.electricity = s.(access.electricity),
                        s_incidence.tuberculosis = s.(incidence.tuberculosis),
                        s_institutional.index = s.(institutional.index),
                        s_commitment.total = s.(commitment.total),
                        s_commitment.bilaterals = s.(commitment.bilaterals),
                        s_commitment.multilaterals = s.(commitment.multilaterals),
                        s_disbursement.total = s.(disbursement.total),
                        s_disbursement.bilaterals = s.(disbursement.bilaterals),
                        s_disbursement.multilaterals = s.(disbursement.multilaterals),
                        s_aid.volatility.commitment = s.(aid.volatility.commitment),
                        s_aid.volatility.disbursement = s.(aid.volatility.disbursement),
                        fyear = factor(year))

write_rds(data, file.path("./data/EoMPII_data-for-analysis.rds"))

## 4.1. Life expectancy ----------------------------------------------------
### 4.1.1. Baseline: the determinants of life expectancy ---------------------------

#### 4.1.1.1. Fit model ---------------------------------------------------------------
m.le <- lmer(life.expectancy ~ s_gdp.growth +
                   s_population.density + s_urban.population + 
                   s_basic.sanitation.service + s_incidence.tuberculosis +
                   (1 | country) + 
                   (1 | fyear), data = data, REML = T)
summary(m.le)

#### 4.1.1.2. Plot effect ---------------------------------------------------------------
###### gdp.growth ----------------------------------
# get prediction 
pred_gdp <- as.data.frame(Effect(c('s_gdp.growth'), 
                                     m.le, 
                                     xlevels = list(s_gdp.growth = data$s_gdp.growth)))
# plot
p_gdp <- ggplot() + 
  geom_line(data = pred_gdp, aes(x = s_gdp.growth, 
                                     y = fit)) +
  geom_ribbon(data = pred_gdp, aes(x = s_gdp.growth,
                                       ymin = lower,
                                       ymax = upper),
              alpha = 0.3) +
  theme_classic()
p_gdp

# plot for each country
data_pred_gdp <- tibble(life.expectancy = NA,
                            fyear = NA,
                            s_gdp.growth = data$s_gdp.growth,
                            country = data$country,
                            s_commitment.total = 0,
                            s_aid.volatility.commitment = 0,
                            s_institutional.index = 0,
                            s_population.density = 0,
                            s_urban.population = 0,
                            s_basic.sanitation.service = 0,
                            s_access.electricity = 0,
                            s_incidence.tuberculosis = 0,
                            index = "pred") 

data$index <- "est"
data_pred_gdp <- bind_rows(data, data_pred_gdp)

# get prediction from the model
data_pred_gdp$pred <- predict(m.le, newdata=data_pred_gdp, allow.new.levels=T)

##extract pred data only
data_pred_gdp <- data_pred_gdp %>% filter(index == "pred")

# plot
ggplot(data = data_pred_gdp, 
       aes(x = s_gdp.growth, 
           y = pred)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

###### population.density ----------------------------------
# get prediction 
pred_pop.den <- as.data.frame(Effect(c('s_population.density'), 
                                     m.le, 
                                     xlevels = list(s_population.density = data$s_population.density)))
# plot
p_pop.den <- ggplot() + 
  geom_line(data = pred_pop.den, aes(x = s_population.density, 
                                     y = fit)) +
  geom_ribbon(data = pred_pop.den, aes(x = s_population.density,
                                       ymin = lower,
                                       ymax = upper),
              alpha = 0.3) +
  theme_classic()
p_pop.den

# plot for each country
data_pred_pop.den <- tibble(life.expectancy = NA,
                            fyear = NA,
                            s_gdp.growth = 0,
                            country = data$country,
                            s_commitment.total = 0,
                            s_aid.volatility.commitment = 0,
                            s_institutional.index = 0,
                            s_population.density = data$s_population.density,
                            s_urban.population = 0,
                            s_basic.sanitation.service = 0,
                            s_access.electricity = 0,
                            s_incidence.tuberculosis = 0,
                            index = "pred") 

data$index <- "est"
data_pred_pop.den <- bind_rows(data, data_pred_pop.den)

# get prediction from the model
data_pred_pop.den$pred <- predict(m.le, newdata=data_pred_pop.den, allow.new.levels=T)

##extract pred data only
data_pred_pop.den <- data_pred_pop.den %>% filter(index == "pred")

# plot
ggplot(data = data_pred_pop.den, 
       aes(x = s_population.density, 
           y = pred)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

###### incidence.tuberculosis ----------------------------------
# get prediction 
pred_tb <- as.data.frame(Effect(c('s_incidence.tuberculosis'), 
                                m.le, 
                                xlevels = list(s_incidence.tuberculosis = data$s_incidence.tuberculosis)))
# plot
p_tb <- ggplot() + 
  geom_line(data = pred_tb, aes(x = s_incidence.tuberculosis, 
                                y = fit)) +
  geom_ribbon(data = pred_tb, aes(x = s_incidence.tuberculosis,
                                  ymin = lower,
                                  ymax = upper),
              alpha = 0.3) +
  theme_classic()
p_tb

# plot for each country
data_pred_tb <- tibble(life.expectancy = NA,
                       fyear = NA,
                       s_gdp.growth = 0,
                       country = data$country,
                       s_commitment.total = 0,
                       s_aid.volatility.commitment = 0,
                       s_institutional.index = 0,
                       s_population.density = 0,
                       s_urban.population = 0,
                       s_basic.sanitation.service = 0,
                       s_access.electricity = 0,
                       s_incidence.tuberculosis = data$s_incidence.tuberculosis,
                       index = "pred") 

data$index <- "est"
data_pred_tb <- bind_rows(data, data_pred_tb)

# get prediction from the model
data_pred_tb$pred <- predict(m.le, newdata=data_pred_tb, allow.new.levels=T)

##extract pred data only
data_pred_tb <- data_pred_tb %>% filter(index == "pred")

# plot
ggplot(data = data_pred_tb, 
       aes(x = s_incidence.tuberculosis, 
           y = pred)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

###### merge plot ----------------------------------
(p_gdp | p_pop.den | p_tb)

### 4.1.2. Commitment ---------------------------------------------------------

#### 4.1.2.1. Fit model ------------------------------------------------------
m.le.cmt <- lmer(life.expectancy ~ s_gdp.growth + s_commitment.total + 
                   s_aid.volatility.commitment*s_institutional.index +
                   s_population.density + s_urban.population + 
                   s_basic.sanitation.service + s_incidence.tuberculosis +
                   (1 | country) + 
                   (1 | fyear), data = data, REML = T)

summary(m.le.cmt)
#### 4.1.2.2. Plot effect ------------------------------------------------------
##### commitment.total
# average effect
pred_cmt <- as.data.frame(Effect(c('s_commitment.total'), 
                                          m.le.cmt, 
                                          xlevels = list(s_commitment.total = data$s_commitment.total)))

# plot
ggplot() + 
  geom_line(data = pred_cmt, aes(x = s_commitment.total, 
                                          y = fit)) +
  geom_ribbon(data = pred_cmt, aes(x = s_commitment.total,
                                            ymin = lower,
                                            ymax = upper),
              alpha = 0.3) +
  theme_classic()

# effect by country
data_pred_cmt <- tibble(life.expectancy = NA,
                       fyear = NA,
                       s_gdp.growth = 0,
                       country = data$country,
                       s_commitment.total = data$s_commitment.total,
                       s_aid.volatility.commitment = 0,
                       s_institutional.index = 0,
                       s_population.density = 0,
                       s_urban.population = 0,
                       s_basic.sanitation.service = 0,
                       s_access.electricity = 0,
                       s_incidence.tuberculosis = 0,
                       index = "pred") 

data$index <- "est"
data_pred_cmt <- bind_rows(data, data_pred_cmt)

# get prediction from the model
data_pred_cmt$pred <- predict(m.le.cmt, newdata=data_pred_cmt, allow.new.levels=T)

##extract pred data only
data_pred_cmt <- data_pred_cmt %>% filter(index == "pred")

# plot
ggplot(data = data_pred_cmt, 
       aes(x = s_commitment.total, 
           y = pred)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

##### aid.volatility.commitment and institutional.index -----------------------

###### average effect ----------
# setting
## set institutional.index in 3 level: min, mean, max - low, middle, high
df_institutional <- tibble(s_institutional.index = c(min(data$s_institutional.index),
                                                  0,
                                                  max(data$s_institutional.index)),
                           institutional.level = factor(c("low", "middle", "high"), levels = c("low", "middle", "high")) )
df_institutional <- df_institutional %>% mutate(s_institutional.index = round(s_institutional.index, 2))

# get prediction 
pred_aid.vol_inst <- as.data.frame(Effect(c('s_aid.volatility.commitment', 's_institutional.index'), 
                                          m.le.cmt, 
                                 xlevels = list(s_aid.volatility.commitment = data$s_aid.volatility.commitment,
                                                s_institutional.index = df_institutional$s_institutional.index)))
pred_aid.vol_inst <- left_join(pred_aid.vol_inst, df_institutional)

# plot
ggplot() + 
  geom_line(data = pred_aid.vol_inst, aes(x = s_aid.volatility.commitment, 
                                    y = fit,
                                    color = institutional.level)) +
  geom_ribbon(data = pred_aid.vol_inst, aes(x = s_aid.volatility.commitment,
                                      ymin = lower,
                                      ymax = upper,
                                      fill = institutional.level),
              alpha = 0.3) +
  theme_classic()

###### by country effect (with country random effect) ----------
# ref: https://tmalsburg.github.io/predict-vs-simulate.html

# create pred data
## set range aid.vol and inst - min, mean, max
range_aid.vol <- c(min(data$s_aid.volatility.commitment),
                   0,
                   max(data$s_aid.volatility.commitment)) %>% round(2)
range_inst <- c(min(data$s_institutional.index),
                0,
                max(data$s_institutional.index)) %>% round(2)

## each country (out of 39), there are 9 combination of aid.vol and inst
data_pred_aid.vol_inst <- tibble(life.expectancy = NA,
                                 fyear = NA,
                                 s_gdp.growth = 0,
                                 country = rep(data$country, 3),
                                 s_commitment.total = 0,
                                 s_aid.volatility.commitment = rep(data$s_aid.volatility.commitment, 3),
                                 s_institutional.index = rep(range_inst,
                                                             each = nrow(data)),
                                 s_population.density = 0,
                                 s_urban.population = 0,
                                 s_basic.sanitation.service = 0,
                                 s_access.electricity = 0,
                                 s_incidence.tuberculosis = 0,
                                 index = "pred"
) %>% 
  left_join(df_institutional)

data$index <- "est"
data_pred_aid.vol_inst <- bind_rows(data, data_pred_aid.vol_inst)

# get prediction from the model
data_pred_aid.vol_inst$pred <- predict(m.le.cmt, newdata=data_pred_aid.vol_inst, allow.new.levels=T)

##extract pred data only
data_pred_aid.vol_inst <- data_pred_aid.vol_inst %>% filter(index == "pred")

# plot
ggplot(data = data_pred_aid.vol_inst, 
       aes(x = s_aid.volatility.commitment, 
           y = pred, 
           color = institutional.level)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

ggplot(data = data_pred_aid.vol_inst, aes(x = s_aid.volatility.commitment, y = pred, color = country)) + 
  geom_line() +
  theme_classic() + 
  theme(legend.position = "none") + 
  facet_wrap(~ institutional.level)

### 4.1.3 Disbursement -------------------------------------------------------



## 4.2. Under-5 mortality --------------------------------------------------
### 4.2.1. Baseline: the determinants of under-5 mortality ------------------
#### 4.2.1.1 Fit model -------------------------------------------------------
m.u5 <- lmer(life.expectancy ~ s_gdp.growth +
               s_population.density + s_urban.population + 
               s_basic.sanitation.service + s_incidence.tuberculosis +
               (1 | country) + 
               (1 | fyear), data = data, REML = T)
summary(m.u5)
#### 4.2.1.2. Plot effect ---------------------------------------------------------------
###### gdp.growth ----------------------------------
# get prediction 
pred_gdp <- as.data.frame(Effect(c('s_gdp.growth'), 
                                 m.u5, 
                                 xlevels = list(s_gdp.growth = data$s_gdp.growth)))
# plot
p_gdp <- ggplot() + 
  geom_line(data = pred_gdp, aes(x = s_gdp.growth, 
                                 y = fit)) +
  geom_ribbon(data = pred_gdp, aes(x = s_gdp.growth,
                                   ymin = lower,
                                   ymax = upper),
              alpha = 0.3) +
  theme_classic()
p_gdp

# plot for each country
data_pred_gdp <- tibble(under5.mortality = NA,
                        fyear = NA,
                        s_gdp.growth = data$s_gdp.growth,
                        country = data$country,
                        s_commitment.total = 0,
                        s_aid.volatility.commitment = 0,
                        s_institutional.index = 0,
                        s_population.density = 0,
                        s_urban.population = 0,
                        s_basic.sanitation.service = 0,
                        s_access.electricity = 0,
                        s_incidence.tuberculosis = 0,
                        index = "pred") 

data$index <- "est"
data_pred_gdp <- bind_rows(data, data_pred_gdp)

# get prediction from the model
data_pred_gdp$pred <- predict(m.u5, newdata=data_pred_gdp, allow.new.levels=T)

##extract pred data only
data_pred_gdp <- data_pred_gdp %>% filter(index == "pred")

# plot
ggplot(data = data_pred_gdp, 
       aes(x = s_gdp.growth, 
           y = pred)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

###### population.density ----------------------------------
# get prediction 
pred_pop.den <- as.data.frame(Effect(c('s_population.density'), 
                                     m.u5, 
                                     xlevels = list(s_population.density = data$s_population.density)))
# plot
p_pop.den <- ggplot() + 
  geom_line(data = pred_pop.den, aes(x = s_population.density, 
                                     y = fit)) +
  geom_ribbon(data = pred_pop.den, aes(x = s_population.density,
                                       ymin = lower,
                                       ymax = upper),
              alpha = 0.3) +
  theme_classic()
p_pop.den

# plot for each country
data_pred_pop.den <- tibble(under5.mortality = NA,
                            fyear = NA,
                            s_gdp.growth = 0,
                            country = data$country,
                            s_commitment.total = 0,
                            s_aid.volatility.commitment = 0,
                            s_institutional.index = 0,
                            s_population.density = data$s_population.density,
                            s_urban.population = 0,
                            s_basic.sanitation.service = 0,
                            s_access.electricity = 0,
                            s_incidence.tuberculosis = 0,
                            index = "pred") 

data$index <- "est"
data_pred_pop.den <- bind_rows(data, data_pred_pop.den)

# get prediction from the model
data_pred_pop.den$pred <- predict(m.u5, newdata=data_pred_pop.den, allow.new.levels=T)

##extract pred data only
data_pred_pop.den <- data_pred_pop.den %>% filter(index == "pred")

# plot
ggplot(data = data_pred_pop.den, 
       aes(x = s_population.density, 
           y = pred)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

###### incidence.tuberculosis ----------------------------------
# get prediction 
pred_tb <- as.data.frame(Effect(c('s_incidence.tuberculosis'), 
                                m.u5, 
                                xlevels = list(s_incidence.tuberculosis = data$s_incidence.tuberculosis)))
# plot
p_tb <- ggplot() + 
  geom_line(data = pred_tb, aes(x = s_incidence.tuberculosis, 
                                y = fit)) +
  geom_ribbon(data = pred_tb, aes(x = s_incidence.tuberculosis,
                                  ymin = lower,
                                  ymax = upper),
              alpha = 0.3) +
  theme_classic()
p_tb

# plot for each country
data_pred_tb <- tibble(under5.mortality = NA,
                       fyear = NA,
                       s_gdp.growth = 0,
                       country = data$country,
                       s_commitment.total = 0,
                       s_aid.volatility.commitment = 0,
                       s_institutional.index = 0,
                       s_population.density = 0,
                       s_urban.population = 0,
                       s_basic.sanitation.service = 0,
                       s_access.electricity = 0,
                       s_incidence.tuberculosis = data$s_incidence.tuberculosis,
                       index = "pred") 

data$index <- "est"
data_pred_tb <- bind_rows(data, data_pred_tb)

# get prediction from the model
data_pred_tb$pred <- predict(m.u5, newdata=data_pred_tb, allow.new.levels=T)

##extract pred data only
data_pred_tb <- data_pred_tb %>% filter(index == "pred")

# plot
ggplot(data = data_pred_tb, 
       aes(x = s_incidence.tuberculosis, 
           y = pred)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

###### merge plot ----------------------------------
(p_gdp | p_pop.den | p_tb)

### 4.2.2. Commitment ---------------------------------------------------------

#### 4.2.2.1. Fit model ------------------------------------------------------
m.u5.cmt <- lmer(under5.mortality ~ s_gdp.growth + s_commitment.total + 
                   s_aid.volatility.commitment*s_institutional.index +
                   s_population.density + s_urban.population + 
                   s_basic.sanitation.service + s_incidence.tuberculosis +
                   (1 | country) + 
                   (1 | fyear), data = data, REML = T)

summary(m.u5.cmt)
#### 4.2.2.2. Plot effect ------------------------------------------------------
##### commitment.total
# average effect
pred_cmt <- as.data.frame(Effect(c('s_commitment.total'), 
                                 m.u5.cmt, 
                                 xlevels = list(s_commitment.total = data$s_commitment.total)))

# plot
ggplot() + 
  geom_line(data = pred_cmt, aes(x = s_commitment.total, 
                                 y = fit)) +
  geom_ribbon(data = pred_cmt, aes(x = s_commitment.total,
                                   ymin = lower,
                                   ymax = upper),
              alpha = 0.3) +
  theme_classic()

# effect by country
data_pred_cmt <- tibble(under5.mortality = NA,
                        fyear = NA,
                        s_gdp.growth = 0,
                        country = data$country,
                        s_commitment.total = data$s_commitment.total,
                        s_aid.volatility.commitment = 0,
                        s_institutional.index = 0,
                        s_population.density = 0,
                        s_urban.population = 0,
                        s_basic.sanitation.service = 0,
                        s_access.electricity = 0,
                        s_incidence.tuberculosis = 0,
                        index = "pred") 

data$index <- "est"
data_pred_cmt <- bind_rows(data, data_pred_cmt)

# get prediction from the model
data_pred_cmt$pred <- predict(m.u5.cmt, newdata=data_pred_cmt, allow.new.levels=T)

##extract pred data only
data_pred_cmt <- data_pred_cmt %>% filter(index == "pred")

# plot
ggplot(data = data_pred_cmt, 
       aes(x = s_commitment.total, 
           y = pred)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

##### aid.volatility.commitment and institutional.index -----------------------

###### average effect ----------
# setting
## set institutional.index in 3 level: min, mean, max - low, middle, high
df_institutional <- tibble(s_institutional.index = c(min(data$s_institutional.index),
                                                     0,
                                                     max(data$s_institutional.index)),
                           institutional.level = factor(c("low", "middle", "high"), levels = c("low", "middle", "high")) )
df_institutional <- df_institutional %>% mutate(s_institutional.index = round(s_institutional.index, 2))

# get prediction 
pred_aid.vol_inst <- as.data.frame(Effect(c('s_aid.volatility.commitment', 's_institutional.index'), 
                                          m.u5.cmt, 
                                          xlevels = list(s_aid.volatility.commitment = data$s_aid.volatility.commitment,
                                                         s_institutional.index = df_institutional$s_institutional.index)))
pred_aid.vol_inst <- left_join(pred_aid.vol_inst, df_institutional)

# plot
ggplot() + 
  geom_line(data = pred_aid.vol_inst, aes(x = s_aid.volatility.commitment, 
                                          y = fit,
                                          color = institutional.level)) +
  geom_ribbon(data = pred_aid.vol_inst, aes(x = s_aid.volatility.commitment,
                                            ymin = lower,
                                            ymax = upper,
                                            fill = institutional.level),
              alpha = 0.3) +
  theme_classic()

###### by country effect (with country random effect) ----------
# ref: https://tmalsburg.github.io/predict-vs-simulate.html

# create pred data
## set range aid.vol and inst - min, mean, max
range_aid.vol <- c(min(data$s_aid.volatility.commitment),
                   0,
                   max(data$s_aid.volatility.commitment)) %>% round(2)
range_inst <- c(min(data$s_institutional.index),
                0,
                max(data$s_institutional.index)) %>% round(2)

## each country (out of 39), there are 9 combination of aid.vol and inst
data_pred_aid.vol_inst <- tibble(under5.mortality = NA,
                                 fyear = NA,
                                 s_gdp.growth = 0,
                                 country = rep(data$country, 3),
                                 s_commitment.total = 0,
                                 s_aid.volatility.commitment = rep(data$s_aid.volatility.commitment, 3),
                                 s_institutional.index = rep(range_inst,
                                                             each = nrow(data)),
                                 s_population.density = 0,
                                 s_urban.population = 0,
                                 s_basic.sanitation.service = 0,
                                 s_access.electricity = 0,
                                 s_incidence.tuberculosis = 0,
                                 index = "pred"
) %>% 
  left_join(df_institutional)

data$index <- "est"
data_pred_aid.vol_inst <- bind_rows(data, data_pred_aid.vol_inst)

# get prediction from the model
data_pred_aid.vol_inst$pred <- predict(m.u5.cmt, newdata=data_pred_aid.vol_inst, allow.new.levels=T)

##extract pred data only
data_pred_aid.vol_inst <- data_pred_aid.vol_inst %>% filter(index == "pred")

# plot
ggplot(data = data_pred_aid.vol_inst, 
       aes(x = s_aid.volatility.commitment, 
           y = pred, 
           color = institutional.level)) + 
  geom_line() +
  theme_classic() + 
  facet_wrap(~ country)

ggplot(data = data_pred_aid.vol_inst, aes(x = s_aid.volatility.commitment, y = pred, color = country)) + 
  geom_line() +
  theme_classic() + 
  theme(legend.position = "none") + 
  facet_wrap(~ institutional.level)

### 4.2.3 Disbursement -------------------------------------------------------


