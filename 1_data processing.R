
# 1. SETUP ----------------------------------------------------------------

library(tidyverse)   #process dataframe
library(lubridate)   #process date time
library(haven)       #read dta stata file
library(psych)       #pca analysis
library(INLA)        #autogressive analysis


# 2. LOAD DATA ------------------------------------------------------------
wb <- read_csv("./data/WB_World Development Indicators.csv")
who_u5m <- read_csv("./data/WHO_under 5 mortality.csv")
who_tb <- read_csv("./data/WHO_TB new cases and relapse.csv")
wgi <- read_dta("./data/wgidataset.dta")
oecd_cmt <- read_csv("./data/OECD_TABLE3A_commitment.csv")
oecd_dbm <- read_csv("./data/OECD_TABLE2A_disbursement.csv")


# 3. PROCESS DATA ---------------------------------------------------------

## 3.1. WB -----------------------------------------------------------------

# remove last 5 rows with NA values
wb <- wb[1:480,]

# get indicator and country list
indicator_list <- unique(wb$`Series Name`)

# create year column
wb <- wb %>% pivot_longer(cols = 5:66, names_to = "year")
wb <- wb %>% mutate(year = as.numeric(substr(year, 1, 4)))
## change value to numeric
wb <- wb %>% mutate(value = as.numeric(if_else(value == "..", NA, value)))
wb <- wb %>% mutate(value = round(value, 2))

# create series column (remove Series Code)
wb <- wb %>% select(!`Series Code`) %>% pivot_wider(names_from = `Series Name`, values_from = value)


# create short field names
wb <- wb %>% mutate(country = `Country Name`,
                    country.code = `Country Code`,
                    infant.mortality = `Mortality rate, infant (per 1,000 live births)`,
                    life.expectancy = `Life expectancy at birth, total (years)`,
                    gdp.per.capita = `GDP per capita (constant 2015 US$)`,
                    population.density = `Population density (people per sq. km of land area)`,
                    urban.population = `Urban population (% of total population)`,
                    basic.drinking.service = `People using at least basic drinking water services (% of population)`,
                    basic.sanitation.service = `People using at least basic sanitation services (% of population)`,
                    access.electricity = `Access to electricity (% of population)`,
                    incidence.tuberculosis = `Incidence of tuberculosis (per 100,000 people)`,
                    community.health.worker = `Community health workers (per 1,000 people)`)

wb <- wb %>% select(country, country.code, year, infant.mortality:community.health.worker)


# get country list Sub-Saharan Africa
country_list <- unique(select(wb, country, country.code))

## 3.2. WHO ----------------------------------------------------------------

# who_u5m
who_u5m <- who_u5m %>% 
  filter(Dim1ValueCode == "BTSX", #get data both sex
         SpatialDimValueCode %in% country_list$country.code) %>% #filter Sub-Saharan Africa 
  mutate(country = Location,
         country.code = SpatialDimValueCode,
         year = Period,
         under5.mortality = FactValueNumeric,
         under5.mortality.low = FactValueNumericLow,
         under5.mortality.high = FactValueNumericHigh) %>%
  select(country:under5.mortality.high)
  

# who_tb
who_tb <- who_tb %>% 
  filter(SpatialDimValueCode %in% country_list$country.code) %>% #filter Sub-Saharan Africa 
  mutate(country = Location,
         country.code = SpatialDimValueCode,
         year = Period,
         tuberculosis.new.relapse = FactValueNumeric,
         tuberculosis.new.relapse.low = FactValueNumericLow,
         tuberculosis.new.relapse.high = FactValueNumericHigh) %>%
  select(country:tuberculosis.new.relapse.high)

# compile who data
who <- left_join(who_u5m, who_tb, by = c("country", "country.code", "year"))

## change country name according to wb data
who <- who %>% mutate(country = if_else(country == "Democratic Republic of the Congo", "Congo, Dem. Rep.",
                                      if_else(country == "Congo", "Congo, Rep.",   
                                              if_else(country == "Côte d’Ivoire", "Cote d'Ivoire", 
                                                      if_else(country == "United Republic of Tanzania", "Tanzania", country)))))


## 3.3. WGI ---------------------------------------------------------------------
wgi <- wgi %>% mutate(country.code = code,
                      country = countryname) %>%
  select(country, country.code, year, vae, pve, gee, rqe, rle, cce)

# integrate wgi to 1 indicator
## new wgi data without NA values
wgi_1 <- wgi%>%filter(is.na(vae) == F & is.na(pve) == F & is.na(gee) == F & is.na(rqe) == F & is.na(rle) == F & is.na(cce) == F)
PCA_results <- principal(select(wgi_1, vae:cce), nfactors = 1)
### proportion variance explained by the index: 85% (see PC1 in PCA_results)
wgi_1$institutional.index <- as.vector(PCA_results$scores)

wgi_1 <- wgi_1 %>% select(country, country.code, year, institutional.index)

## merge institutional.index to wgi
wgi <- left_join(wgi, wgi_1 , by = c("country", "country.code", "year"))

## 3.4. OECD ---------------------------------------------------------------

### 3.4.1. Commitment data --------------------------------------------------
# Commitment data
## Note data: 
### Total Commitments 
### Constant prices (using reference year 2021)
### Unit: Million US dollars

## filter Aid type: Total Commitments, Amount type: Constant Prices 
## also filter Recipient - remove "Djibouti" , "Mayotte", "Saint Helena" (not Sub-Saharan in WB list)
oecd_cmt <- oecd_cmt %>% filter(`Aid type` == "Total Commitments",
                                `Amount type` == "Constant Prices",
                                !Recipient %in% c("Djibouti" , "Mayotte", "Saint Helena"))

## change country name according to wb data
oecd_cmt <- oecd_cmt %>% mutate(Recipient = if_else(Recipient == "Democratic Republic of the Congo", "Congo, Dem. Rep.",
                                        if_else(Recipient == "Congo", "Congo, Rep.",   
                                                if_else(Recipient == "Côte d'Ivoire", "Cote d'Ivoire", 
                                                        if_else(Recipient == "United Republic of Tanzania", "Tanzania", 
                                                                if_else(Recipient == "Gambia", "Gambia, The", Recipient))))))

## Combine DAC and non-DAC -> bilateral and keep multilateral & official total
oecd_cmt <- oecd_cmt %>% mutate(Donor_new = if_else(Donor == "DAC Countries, Total" | Donor == "Non-DAC Countries, Total", "commitment.bilaterals",
                                                    if_else(Donor == "Multilaterals, Total", "commitment.multilaterals", "commitment.total")) )
## Group by new three categories
oecd_cmt <- oecd_cmt %>% group_by(Recipient, Donor_new, Year) %>% summarize(Value = sum(Value, na.rm = T))

## Pivot wider to have three columns of three categories
oecd_cmt <- oecd_cmt %>% pivot_wider(names_from = Donor_new, values_from = Value)

## Rename fields
oecd_cmt <- oecd_cmt %>% rename(country = Recipient,
                                year = Year)

### 3.4.2. Disbursement data ------------------------------------------------
# Disbursement data
## Note data: 
### Memo: ODA Total, Gross disbursements
### Constant prices (using reference year 2021)
### Unit: Million US dollars

## filter Aid type: Memo: ODA Total, Gross disbursements , Amount type: Constant Prices 
## also filter Recipient - remove "Djibouti" , "Mayotte", "Saint Helena" (not Sub-Saharan in WB list)
oecd_dbm <- oecd_dbm %>% filter(`Aid type` == "Memo: ODA Total, Gross disbursements",
                                `Amount type` == "Constant Prices",
                                !Recipient %in% c("Djibouti" , "Mayotte", "Saint Helena"))

## change country name according to wb data
oecd_dbm <- oecd_dbm %>% mutate(Recipient = if_else(Recipient == "Democratic Republic of the Congo", "Congo, Dem. Rep.",
                                                    if_else(Recipient == "Congo", "Congo, Rep.",   
                                                            if_else(Recipient == "Côte d'Ivoire", "Cote d'Ivoire", 
                                                                    if_else(Recipient == "United Republic of Tanzania", "Tanzania", 
                                                                            if_else(Recipient == "Gambia", "Gambia, The", Recipient))))))

## Combine DAC and non-DAC -> bilateral and keep multilateral & official total
oecd_dbm <- oecd_dbm %>% mutate(Donor_new = if_else(Donor == "DAC Countries, Total" | Donor == "Non-DAC Countries, Total", "disbursement.bilaterals",
                                                    if_else(Donor == "Multilaterals, Total", "disbursement.multilaterals", "disbursement.total")) )
## Group by new three categories
oecd_dbm <- oecd_dbm %>% group_by(Recipient, Donor_new, Year) %>% summarize(Value = sum(Value, na.rm = T))

## Pivot wider to have three columns of three categories
oecd_dbm <- oecd_dbm %>% pivot_wider(names_from = Donor_new, values_from = Value)

## Rename fields
oecd_dbm <- oecd_dbm %>% rename(country = Recipient,
                                year = Year)


### 3.4.3 Compile OECD data-------------------------------------------------------------------
oecd <- full_join(oecd_cmt,
                  oecd_dbm, by = c("country", "year"))

### 3.4.4 Aid volatility -------------------------------------------------------------------

# note: do autoregressive model for each country
# only calculate aid volatility from 1980 - to be comparable to Boateng et al., 2021
# distribution: gamma (not normal) - as commitment.total and disbursement.total are positive

# aid commitment 
## create country list
oecd_country <- sort(unique(oecd$country)) 

## calculate aid volatility
aid.volatility.cmt <- tibble()
for (i in 1:length(oecd_country)) {
  # print progress
  print(paste0("processing country ", oecd_country[i]))
  
  # autogressive order 1 model
  ar1 <- inla(commitment.total ~ f(year, model = "ar1"), 
              data = filter(oecd, year >= 1980, country == oecd_country[i]),
              family = "gamma")
  
  # extract year random effect (aid.volatility)
  re <- ar1$summary.random$year[,1:2] %>% rename(year = ID, aid.volatility.commitment = mean)
  re$country <- oecd_country[i]
  
  # compile aid.volatility 
  aid.volatility.cmt <- bind_rows(aid.volatility.cmt, re)
}

## scale aid volatility by country to have the same mean 0, sd 1 (x - mean(x)/sd(x))
aid.volatility.cmt$aid.volatility.commitment.scaled <- with(aid.volatility.cmt, ave(aid.volatility.commitment, country, FUN = scale))

## plot aid volatility commitment
ggplot(data = aid.volatility.cmt, 
       aes(x = year, y = aid.volatility.commitment)) + 
  geom_line() + xlim(1980, 2021) +
  facet_wrap(~ country)               

ggplot(data = aid.volatility.cmt, 
       aes(x = year, y = aid.volatility.commitment.scaled)) + 
  geom_line() + xlim(1980, 2021) +
  facet_wrap(~ country)

# aid disbursment 
## create country list
#oecd_country <- sort(unique(oecd$country)) 

## calculate aid volatility
aid.volatility.dbm <- tibble()
for (i in 1:length(oecd_country)) {
  # print progress
  print(paste0("processing country ", oecd_country[i]))
  
  # autogressive order 1 model
  ar1 <- inla(disbursement.total ~ f(year, model = "ar1"), 
              data = filter(oecd, year >= 1980, country == oecd_country[i]),
              family = "gamma")
  
  # extract year random effect (aid.volatility)
  re <- ar1$summary.random$year[,1:2] %>% rename(year = ID, aid.volatility.disbursement = mean)
  re$country <- oecd_country[i]
  
  # compile aid.volatility 
  aid.volatility.dbm <- bind_rows(aid.volatility.dbm, re)
}

## scale aid volatility by country to have the same mean 0, sd 1 (x - mean(x)/sd(x))
aid.volatility.dbm$aid.volatility.disbursement.scaled <- with(aid.volatility.dbm, ave(aid.volatility.disbursement, country, FUN = scale))

## plot aid volatility commitment
ggplot(data = aid.volatility.dbm, 
       aes(x = year, y = aid.volatility.disbursement)) + 
  geom_line() + xlim(1980, 2021) +
  facet_wrap(~ country)

ggplot(data = aid.volatility.dbm, 
       aes(x = year, y = aid.volatility.disbursement.scaled)) + 
  geom_line() + xlim(1980, 2021) +
  facet_wrap(~ country)

# compile aid volatility data
oecd <- left_join(oecd, aid.volatility.cmt)
oecd <- left_join(oecd, aid.volatility.dbm)

ggplot() + 
  geom_line(data = oecd %>% filter(year >= 1980), 
            aes(x = year, y = aid.volatility.commitment.scaled), color = "red") + 
  geom_line(data = oecd %>% filter(year >= 1980), 
            aes(x = year, y = aid.volatility.disbursement.scaled), color = "blue") +
  xlim(1980, 2021) +
  facet_wrap(~ country)

# note: disbursement trend is different from Boateng et al., 2021
# but the problem seems to be of Boateng's paper (unknown)
# as no raw Aid type data shows the trend as in Boateng
# also the range in Boateng's paper (few hundred is strage)

## 3.5. Merge data ---------------------------------------------------------
wb <- left_join(wb, who, by = c("country", "country.code", "year"))
wb <- left_join(wb, wgi, by = c("country", "country.code", "year"))  
wb <- left_join(wb, oecd, by = c("country", "year"))

write_rds(wb, "./data/EoMPII_data.rds")





