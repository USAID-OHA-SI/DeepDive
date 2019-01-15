library(tidyverse)

df_site <- read_rds("~/ICPI/Data/MER_Structured_Dataset_Site_IM_FY17-18_20181115_v1_2_Nigeria.rds")

tx_sites <- df_site %>% 
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
         !is.na(fy2018apr),
         fy2018apr !=0) %>% 
  select(orgunituid, sitename, snu1, psnu, fundingagency, mechanismid, primepartner, tx_curr_fy18apr = fy2018apr) %>% 
  write_csv("Output/lowyield_tx.csv", na = "")

pmtct_sites <- df_site %>% 
  filter(indicator %in% c("PMTCT_STAT", "PMTCT_STAT_POS"),
         standardizeddisaggregate == "Total Numerator",
         !is.na(fy2018apr),
         fy2018apr !=0) %>% 
  select(orgunituid, sitename, snu1, psnu, fundingagency, mechanismid, primepartner, indicator, fy18apr = fy2018apr)

pmtct_sites <- pmtct_sites %>% 
  spread(indicator, fy18apr, fill = 0) %>% 
  rename(pmtct_stat_fy18apr = PMTCT_STAT, 
         pmtct_stat_pos_fy18apr = PMTCT_STAT_POS) %>%
  mutate(pmtct_positivity_fy18apr = round(pmtct_stat_pos_fy18apr /pmtct_stat_fy18apr, 3)) %>% 
  write_csv("Output/lowyield_pmtct.csv", na = "")



#how many sites are reporting TX_CURR (exlusive of reporting 0)?
tx_sites %>%
  distinct(sitename) %>% 
  nrow()

#how many sites are reporting <10 TX_CURR?
tx_sites %>% 
  filter(tx_curr_fy18apr < 10) %>% 
  nrow()

#how many USAID sites are reporting TX_CURR (exlusive of reporting 0)?
tx_sites %>%
  filter(fundingagency == "USAID") %>% 
  distinct(sitename) %>% 
  nrow()

#how many USAID sites are reporting <10 TX_CURR?
tx_sites %>% 
  filter(fundingagency == "USAID",
         tx_curr_fy18apr < 10) %>% 
  nrow()




#PMTCT

pmtct_sites <- df_site %>% 
  filter(indicator %in% c("PMTCT_STAT", "PMTCT_STAT_POS"),
         standardizeddisaggregate == "Total Numerator",
         !is.na(fy2018apr),
         fy2018apr !=0) %>% 
  select(orgunituid, sitename, snu1, psnu, fundingagency, mechanismid, primepartner, indicator, fy18apr = fy2018apr)

pmtct_sites <- pmtct_sites %>% 
  spread(indicator, fy18apr, fill = 0) %>% 
  rename(pmtct_stat_fy18apr = PMTCT_STAT, 
         pmtct_stat_pos_fy18apr = PMTCT_STAT_POS) %>%
  mutate(pmtct_positivity_fy18apr = round(pmtct_stat_pos_fy18apr /pmtct_stat_fy18apr, 3))


#how many sites are reporting PMTCT_STAT (exlusive of reporting 0)?
pmtct_sites %>% 
  nrow()

#how many sites are reporting a PMTCT positivity of 0?
pmtct_sites %>% 
  filter(pmtct_stat_pos_fy18apr == 0)

#how many sites are reporting PMTCT_STAT (exlusive of reporting 0)?
pmtct_sites %>% 
  filter(fundingagency == "USAID") %>% 
  nrow()

#how many sites are reporting a PMTCT positivity of 0?
pmtct_sites %>% 
  filter(fundingagency == "USAID") %>% 
  filter(pmtct_stat_pos_fy18apr == 0) %>% 
  nrow()
