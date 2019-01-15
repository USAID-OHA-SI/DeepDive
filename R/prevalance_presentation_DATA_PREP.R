
#dependencies
library(tidyverse)
library(readxl)
library(scales)


# MSD ---------------------------------------------------------------------

#import
  mer_all <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1.txt") %>% 
    filter(operatingunit == "Nigeria")

#filter & aggregate to state level (level of prevalence)
  mer <- mer_all %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(snu1, indicator) %>% 
    summarize_at(vars(fy2019_targets), sum, na.rm = TRUE) %>% 
    ungroup() 

  
#reshape
  mer <- mer %>% 
    spread(indicator, fy2019_targets) %>% 
    rename(state = snu1)

# 90-90-90
  
  cascade <- mer_all %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    mutate(indicator = ifelse(standardizeddisaggregate == "Total Denominator", paste0(indicator, "_D"), indicator)) %>% 
    group_by(snu1, indicator) %>% 
    summarize_at(vars(fy2019_targets), sum, na.rm = TRUE) %>% 
    ungroup() 
  
  #reshape
  cascade <- cascade %>% 
    spread(indicator, fy2019_targets) %>% 
    rename(state = snu1)
  
# TX_CURR
  
  tx_curr <- mer_all %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Total Numerator"),
           agecoarse %in% c(NA, "15+"),
           sex %in% c("Female", "Male", NA)) %>% 
    #count(standardizeddisaggregate, sex, agecoarse, wt = fy2018apr)
    mutate(sex = ifelse(is.na(sex), "total", tolower(sex))) %>% 
    group_by(snu1, sex) %>% 
    summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    filter(fy2018apr != 0) %>% 
    rename(TX_CURR = fy2018apr,
           state = snu1)

# PREVALENCE --------------------------------------------------------------

#read in prevalence data
  prev <- read_xlsx("Data/Comparison of State HIV Prevalence Oct 19 2018_Sebastian.xlsx")
  
#clean data
  prev <- prev %>% 
    rename(state = State,
           Spectrum2016 = `HIV+ prevalence rate (Spectrum)\r\n2016`,
           Spectrum2018 = `Spectrun estimate\r\nAdults\r\n2018`,
           NAIIS2018 = `NAIIS\r\n(15-64)\r\n2018`) %>% 
    select(state, Spectrum2016, Spectrum2018, NAIIS2018) %>% 
    filter(!is.na(NAIIS2018)) %>% 
    mutate(Spectrum2018 = Spectrum2018/100,
           state = ifelse(state == "FCT Abuja", "FCT", state)) 

#reshape & arrange for graphing
  prev <- prev %>% 
    gather(survey, prevalence, -state) %>% 
    mutate(lab_state = ifelse(survey == "Spectrum2016", state, NA),
           survey = factor(survey,
                           levels = c("Spectrum2016", "Spectrum2018", "NAIIS2018"))) %>% 
    arrange(state, survey) 



# DATA PACK - POPULATION --------------------------------------------------

#import
  dp_path <- "Data/NigeriaCOP18DataPackv2018.02.13 - Version 10 Mar 16 2018_Final.xlsx"
  datapack <- read_xlsx(dp_path, 
                           sheet = "DATIM Indicator Table",
                           skip = 3) %>% 
    select(state = snu1, psnu = snulist, plhiv, pop_num, pop_num_m) %>% 
    mutate(pop_num_f = pop_num - pop_num_m) %>%
    filter(!is.na(state))
  
  datapack <- datapack %>% 
    group_by(state) %>% 
    summarize_at(vars(pop_num, pop_num_m, pop_num_f, plhiv), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    rename(plhiv_old = plhiv)


# COMBINE -----------------------------------------------------------------

  burden <- prev %>% 
    filter(survey == "NAIIS2018") %>%
    select(-lab_state, -survey) %>% 
    left_join(datapack) %>% 
    select(-pop_num_m, -pop_num_f) %>% 
    mutate(plhiv_est = pop_num * prevalence) %>% 
    left_join(mer) %>% 
    mutate(unmet_burden = plhiv_est - TX_CURR) %>% 
    select(state, pop_num, plhiv_old, prevalence_naiis = prevalence, plhiv_est, TX_CURR, unmet_burden, everything())


# NAIIS -------------------------------------------------------------------
  
  # naiis <- read_csv("Data/NAIIS_raw_all.csv")
  # 
  # naiis <- naiis %>% 
  #   select(-contains("ci"), -n) %>% 
  #   filter(group != "Viral suppression",
  #          !type %in% c("15-24 years", "15-49 years")) %>% 
  #   mutate(type = case_when(type == "0-14 years"  ~ "Prev <15",
  #                           type == "15-64 years" ~ "Prev 15+",
  #                           TRUE                  ~ type),
  #          state = ifelse(state == "FCT Abuja", "FCT", state)) %>% 
  #   gather(sex, val, female, male, total)
  # 
  # naiis <- naiis %>% 
  #   filter(type != "Prev <15") %>% 
  #   select(-group) %>% 
  #   mutate(val = val/100) %>% 
  #   spread(type, val)
  
  naiis <- read_xlsx("Data/Updated USAID NAIIS Result Scenarios 14.01.19.xlsx",
                     sheet = "REACHING_ECT_TX_COVERAGE RATES") %>% 
    select(state, `Prev 15+_total`:`Virally Suppressed_total`) %>% 
    filter(!state %in% c("SNU1", "zTotal", NA)) %>% 
    gather(type, val, -state)  %>% 
    separate(type, c("type", "sex"), sep = "_") %>% 
    mutate(val = as.numeric(val)) %>% 
    spread(type, val) %>% 
    mutate(`Prev 15+` = `Prev 15+` /100)
  
# EXPORT ------------------------------------------------------------------

write_rds(prev, "Output/prev_surveys.rds")
write_rds(burden, "Output/nga_buden.rds")
write_rds(datapack, "Output/datapack.rds")
write_rds(cascade, "Output/cascade.rds")
write_rds(tx_curr, "Output/tx_curr.rds")
write_rds(naiis, "Output/naiis.rds")

  