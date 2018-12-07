##  Nigeria FY18 Treatment Cascade
##  A.Chafetz
##  Purpose: look at the treatment cascade across the year
##  Date: 2018-12-07

#dependencies
  library(tidyverse)
  library(scales)
  library(knitr)
  library(kableExtra)

  
#import data
  df_mer <- read_rds("MER_Structured_Dataset_OU_IM_FY17-18_20181115_v1_1.rds")
  
#filter
  cascade <- df_mer %>% 
    filter(operatingunit == "Nigeria", 
           indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_RET", "TX_PVLS"),
           standardizeddisaggregate  %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
           !is.na(agecoarse)) %>% 
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
           fundingagency = ifelse(fundingagency != "USAID", "Other", fundingagency))

#summarize
  cascade_age <- cascade %>% 
    group_by(fundingagency, indicator, agecoarse) %>% 
    summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(agecoarse != "Unknown Age")


#overall age
  cascade_agetot <- cascade_age %>%
    mutate(agecoarse = "Total") %>% 
    group_by(fundingagency, indicator, agecoarse) %>% 
    summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup()

#combine age & age total
  cascade_agecombo <- bind_rows(cascade_age, cascade_agetot)
  
#country total
  cascade_ctry <- cascade_agecombo %>% 
    mutate(fundingagency = "Nigeria") %>% 
    group_by(fundingagency, indicator, agecoarse) %>% 
    summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup()

#combine
  cascade_full <- bind_rows(cascade_agecombo, cascade_ctry)
  
#spread
  cascade_full <- cascade_full %>%
    spread(indicator, fy2018apr) 
  
 
#Nigeria cascade
  cascade_full %>% 
    filter(fundingagency == "Nigeria") %>% 
    mutate(Linkage = percent(TX_NEW / HTS_TST_POS, accuracy = 1),
           Positivity = percent(HTS_TST_POS / HTS_TST, accuracy = .1),
           VL = percent(TX_PVLS / TX_PVLS_D, accuracy = 1),
           Retention = percent(TX_RET / TX_RET_D, accuracy = 1)) %>% 
    select(` ` = fundingagency, `Age Group` = agecoarse, HTS_TST, HTS_TST_POS, Positivity, TX_NEW, 
           Linkage, TX_CURR, TX_RET, TX_RET_D, Retention, TX_PVLS, TX_PVLS_D, VL) %>% 
    kable(format.args = list(big.mark = ",", zero.print = FALSE), align = "r", caption = "Nigeria FY18 Cascade") %>% 
    #add_header_above(c(" " = 1, "FY18 Q1" = 3, "Q2" = 3, "Q3" = 3, "Q4" = 3)) %>% 
    kable_styling()
  
#USAID cascade
  cascade_full %>% 
    filter(fundingagency == "USAID") %>% 
    mutate(Linkage = percent(TX_NEW / HTS_TST_POS, accuracy = 1),
           Positivity = percent(HTS_TST_POS / HTS_TST, accuracy = .1),
           VL = percent(TX_PVLS / TX_PVLS_D, accuracy = 1),
           Retention = percent(TX_RET / TX_RET_D, accuracy = 1)) %>% 
    select(` ` = fundingagency, `Age Group` = agecoarse, HTS_TST, HTS_TST_POS, Positivity, TX_NEW, 
           Linkage, TX_CURR, TX_RET, TX_RET_D, Retention, TX_PVLS, TX_PVLS_D, VL) %>% 
    kable(format.args = list(big.mark = ",", zero.print = FALSE), align = "r", caption = "USAID/Nigeria FY18 Cascade") %>% 
    #add_header_above(c(" " = 1, "FY18 Q1" = 3, "Q2" = 3, "Q3" = 3, "Q4" = 3)) %>% 
    kable_styling()
    
    
    
    
  
  
