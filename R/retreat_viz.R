##   NGA Review
##   A.Chafetz, USAID
##   Purpose: 
##   Date: 2018-12-03

##   Data Source: MSD Q4i OUxIM, Nigeria


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(scales)
library(knitr)
library(kableExtra)

# IMPORT ------------------------------------------------------------------

df_nga <- read_rds("~/ICPI/Data/MER_Structured_Dataset_OU_IM_FY17-18_20181115_v1_1.rds") %>% 
  filter(operatingunit == "Nigeria")

df_nga_psnu <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_1.rds") %>% 
  filter(operatingunit == "Nigeria")

# USAID PARTNERS - TX_NEW -------------------------------------------------


mech_lst <- df_nga %>% 
  filter(operatingunit == "Nigeria",
         fundingagency == "USAID",
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(mechanismid) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  filter(fy2018apr != 0) %>% 
  arrange(desc(fy2018apr)) %>% 
  distinct(mechanismid) %>% 
  pull()

names <- tribble(~mechanismid, ~name,
        "14505", "SIDHAS",
        "18441", "MSH",
        "14664", "Heartland")


df_nga %>% 
  filter(operatingunit == "Nigeria",
         fundingagency == "USAID",
         indicator == "TX_NEW",
         standardizeddisaggregate == "Total Numerator",
         mechanismid %in% mech_lst) %>% 
  group_by(mechanismid) %>% 
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  gather(pd, val, -mechanismid, na.rm = TRUE) %>%
  arrange(mechanismid, pd) %>%
  filter(val != 0) %>% 
  mutate(pd = str_remove(pd, "20") %>% toupper(.),
         mechanismid = factor(mechanismid, levels = mech_lst)) %>% 
  ggplot(aes(pd, val, group = mechanismid, color = mechanismid)) +
  geom_path() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_discrete(labels = c("FY17Q1", rep("", 6), "FY18Q4")) + 
  labs(title = "TX_CURR", x = "", y = "") +
  facet_grid(. ~ mechanismid) +
  theme(legend.position = "none")


df_nga %>% 
  filter(operatingunit == "Nigeria",
         fundingagency == "USAID",
         indicator %in% c("TX_NEW", "HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator",
         mechanismid %in% mech_lst) %>% 
  group_by(mechanismid, indicator) %>% 
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  gather(pd, val, -mechanismid, -indicator, na.rm = TRUE) %>%
  arrange(mechanismid, indicator, pd) %>%
  filter(val != 0) %>% 
  mutate(pd = str_remove(pd, "20") %>% toupper(.),
         mechanismid = factor(mechanismid, levels = mech_lst)) %>% 
  unite(grp, c("mechanismid", "indicator"), remove = FALSE) %>% 
  ggplot(aes(pd, val, group = grp, color = indicator)) +
  geom_path() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_discrete(labels = c("FY17Q1", rep("", 6), "FY18Q4")) + 
  labs(x = "", y = "") +
  facet_grid(. ~ mechanismid) +
  theme(legend.position = "none")



# TX_CURR -----------------------------------------------------------------

df_nga %>% 
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
         fundingagency != "Dedup") %>% 
  group_by(fundingagency) %>% 
  summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(pd, val, -fundingagency) %>% 
  mutate(pd = str_remove(pd, "20") %>% toupper(.),
         lab = ifelse(pd == "FY18Q4", fundingagency, NA)) %>% 
  ggplot(aes(pd, val, group = fundingagency, color = fundingagency)) +
  geom_path() +
  geom_point() +
  geom_text(aes(label = lab), vjust = -.9, na.rm = TRUE) +
  scale_y_continuous(label = comma) +
  labs(title = "Nigeria Current on Treatment", subtitle = "FY17-18", x = "", y = "") +
  theme(legend.position = "none") 


# LINKAGE -----------------------------------------------------------------

df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_PVLS"),
        standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
        fundingagency == "USAID") %>% 
  mutate(indicator = ifelse(standardizeddisaggregate == "Total Denominator", paste0(indicator, "_D"),indicator)) %>% 
  group_by(mechanismid, indicator) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  filter(fy2018apr != 0) %>% 
  spread(indicator, fy2018apr) %>% 
  mutate(Positvity = percent(HTS_TST_POS/HTS_TST, accuracy = .1),
         Linkage = percent(TX_NEW/HTS_TST_POS, accuracy = 1),
         VL = percent(TX_PVLS / TX_PVLS_D, accuracy = 1),
         HTS_TST = comma(HTS_TST),
         HTS_TST_POS = comma(HTS_TST_POS),
         TX_NEW = comma(TX_NEW)) %>% 
  left_join(., names, by = "mechanismid") %>% 
  select(name, HTS_TST, HTS_TST_POS, Positvity, TX_NEW, Linkage, VL) 


# HEARTLAND LINKAGE BY STATE ----------------------------------------------

hrt <- df_nga_psnu %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
         standardizeddisaggregate == "Total Numerator",
         mechanismid == "14664") %>% 
  group_by(mechanismid, snu1, psnu, indicator) %>% 
  summarize_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, fy2018apr) %>% 
  mutate(Linkage = TX_NEW / HTS_TST_POS) %>% 
  arrange(desc(Linkage), desc(HTS_TST_POS)) %>% 
  mutate(partner = "Hearland") %>% 
  select(mechanismid, partner, everything())

  
write_csv(hrt, "C:/Users/achafetz/Downloads/hearland_linkage.csv", na = "")
