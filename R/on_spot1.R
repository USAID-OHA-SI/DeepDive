library(tidyverse)
library(ICPIutilities)
library(RearWindow)
library(scales)
library(gridExtra)


df_nga <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_1_Nigeria.rds") %>% 
  rename_official() %>% 
  rw_gen_nn_target()


#add palette
color <- rw_addpalette()
#establish thresholds
threshold <- rw_addthresholds(df_nga)

# df_nga %>% 
#   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW")) %>% 
#   write_csv("C:/Users/achafetz/Documents/Nigeria TDY/cascade.csv", na = "")


# df_nga %>% 
#   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW"),
#          standardizeddisaggregate == "Total Numerator",
#          fundingagency != "Dedup") %>% 
#   group_by(fundingagency, indicator) %>% 
#   summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
#   ungroup() %>% 
#   spread(indicator, fy2018apr) %>% 
#   mutate(`Linkage (%)` = round(TX_NEW / HTS_TST_POS, 2) * 100)
  
  
#Cascade

cascade <- function(df, agency = NULL) {
  
  df_cascade <- df_nga %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
           standardizeddisaggregate == "Total Numerator") %>%
    select(-fy2019_targets) %>% 
    group_by(fundingagency, indicator) %>% 
    summarise_at(vars(matches("target|apr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    gather(ind, val, -fundingagency, -indicator) %>% 
    mutate(pd = paste0("FY", str_sub(ind, 5L, 6L)),
           type = ifelse(str_detect(ind, "targets"), "TARGET", "RESULT")) %>% 
    select(-ind) %>% 
    spread(type, val) %>% 
    mutate(indicator = factor(indicator, levels = c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")))
  
  if(!is.null(agency)) df_cascade <- filter(df_cascade, fundingagency == agency)
  
  v_hts <- df_cascade %>% 
    filter(indicator == "HTS_TST",
           pd == "FY18") %>% 
    ggplot(aes(indicator, RESULT)) +
    geom_col() +
    geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
    scale_y_continuous(label = comma) +
    labs(x = "", y = "", title = "") +
    rw_plot_theme()
  
  v_postx <- df_cascade %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
           pd == "FY18") %>% 
    ggplot(aes(indicator, RESULT)) +
    geom_col() +
    geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
    scale_y_continuous(label = comma) +
    labs(x = "", y = "", title = "") +
    rw_plot_theme()
  
  plot <- grid.arrange(v_hts, v_postx, ncol = 2)
  
  agency <- str_remove(agency, "HHS/")
  savename <- case_when(!is.null(agency)   ~ paste0(agency, "_cascade.png"),
                        !is.null(priority) ~ paste0(priority, "_cascade.png"),
                        TRUE               ~ "cascade.png")
  ggsave(savename, 
         plot = plot,
         path = "Viz",
         w = 9, h = 3.75, units = "in",
         dpi = 300)
}

cascade()
cascade(df_nga, "USAID")
cascade(df_nga, "HHS/CDC")
cascade(df_nga, "DOD")




df_cascade <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
         standardizeddisaggregate == "Total Numerator") %>%
  select(-fy2019_targets) %>% 
  group_by(indicator) %>% 
  summarise_at(vars(matches("target|apr")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(ind, val, -indicator) %>% 
  mutate(pd = paste0("FY", str_sub(ind, 5L, 6L)),
         type = ifelse(str_detect(ind, "targets"), "TARGET", "RESULT")) %>% 
  select(-ind) %>% 
  spread(type, val) %>% 
  mutate(indicator = factor(indicator, levels = c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")))

v_hts <- df_cascade %>% 
  filter(indicator == "HTS_TST",
         pd == "FY18") %>% 
  ggplot(aes(indicator, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "", title = "") +
  rw_plot_theme()

v_postx <- df_cascade %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
         pd == "FY18") %>% 
  ggplot(aes(indicator, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "", title = "") +
  rw_plot_theme()


ggsave("cascade.png", 
       plot = plot,
       path = "Viz",
       w = 9, h = 3.75, units = "in",
       dpi = 300)

#age sex
df_cascade <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus")) %>%
  select(-fy2019_targets) %>% 
  mutate(sex = ifelse(agecoarse == "<15", "Children", sex)) %>% 
  filter(!is.na(sex)) %>% 
  group_by(indicator, sex) %>% 
  summarise_at(vars(matches("target|apr")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(ind, val, -sex, -indicator) %>% 
  mutate(pd = paste0("FY", str_sub(ind, 5L, 6L)),
         type = ifelse(str_detect(ind, "targets"), "TARGET", "RESULT")) %>% 
  select(-ind) %>% 
  spread(type, val) %>% 
  mutate(indicator = factor(indicator, levels = c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")))



v_hts <- df_cascade %>% 
  filter(indicator == "HTS_TST",
         pd == "FY18",
         sex == "Female") %>% 
  ggplot(aes(indicator, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "", title = "") +
  rw_plot_theme()

v_postx <- df_cascade %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
         pd == "FY18",
         sex == "Female") %>% 
  ggplot(aes(indicator, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "", title = "") +
  rw_plot_theme()

plot <- grid.arrange(v_hts, v_postx, ncol = 2)

savename <- case_when(!is.null(agency)   ~ paste0(agency, "_cascade.png"),
                      !is.null(priority) ~ paste0(priority, "_cascade.png"),
                      TRUE               ~ "cascade.png")
ggsave("female_cascade.png", 
       plot = plot,
       path = "Viz",
       w = 9, h = 3.75, units = "in",
       dpi = 300)


v_hts <- df_cascade %>% 
  filter(indicator == "HTS_TST",
         pd == "FY18",
         sex == "Male") %>% 
  ggplot(aes(indicator, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "", title = "") +
  rw_plot_theme()

v_postx <- df_cascade %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
         pd == "FY18",
         sex == "Male") %>% 
  ggplot(aes(indicator, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "", title = "") +
  rw_plot_theme()

plot <- grid.arrange(v_hts, v_postx, ncol = 2)

ggsave("male_cascade.png", 
       plot = plot,
       path = "Viz",
       w = 9, h = 3.75, units = "in",
       dpi = 300)


v_hts <- df_cascade %>% 
  filter(indicator == "HTS_TST",
         pd == "FY18",
         sex == "Children") %>% 
  ggplot(aes(indicator, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "", title = "") +
  rw_plot_theme()

v_postx <- df_cascade %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
         pd == "FY18",
         sex == "Children") %>% 
  ggplot(aes(indicator, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "", title = "") +
  rw_plot_theme()

plot <- grid.arrange(v_hts, v_postx, ncol = 2)

ggsave("children_cascade.png", 
       plot = plot,
       path = "Viz",
       w = 9, h = 3.75, units = "in",
       dpi = 300)


df_cascade <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
         standardizeddisaggregate == "Total Numerator") %>%
  select(-fy2019_targets) %>% 
  group_by(indicator) %>% 
  summarise_at(vars(matches("target|apr")), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  gather(ind, val, -indicator) %>% 
  mutate(pd = paste0("FY", str_sub(ind, 5L, 6L)),
         type = ifelse(str_detect(ind, "targets"), "TARGET", "RESULT")) %>% 
  select(-ind) %>% 
  spread(type, val) %>% 
  mutate(indicator = factor(indicator, levels = c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")))


df_cascade %>% 
  ggplot(aes(pd, RESULT)) +
  geom_col() +
  geom_errorbar(aes(ymax=TARGET, ymin=TARGET), width=0.75, size =1, color= color["amazon"]) +
  scale_y_continuous(label = comma) +
  facet_wrap(indicator ~ ., scales = "free_y") +
  labs(x = "", y = "") +
  rw_plot_theme()

ggsave("cascade_change.png", 
       path = "Viz",
       w = 9, h = 3.75, units = "in",
       dpi = 300)


viz_hts <- function(sex = NULL){
  
  df_hts_sex <- df_nga %>% 
    filter(indicator %in% c("HTS_TST","HTS_TST_POS"),
           standardizeddisaggregate %in% 
             c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result")) %>% 
    mutate(sex = ifelse(agecoarse == "<15", "Children", sex)) %>% 
    group_by(indicator, modality, sex) %>% 
    summarize_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(fy2018apr != 0) %>% 
    spread(indicator, fy2018apr) %>% 
    mutate(`POSITIVITY (%)` = round(HTS_TST_POS/HTS_TST, 2)*100) %>% 
    gather(indicator, val, -modality, -sex)
  
  if(!is.null(sex)) df_hts_sex <- filter(df_hts_sex, sex == sex)
  
  plot <- df_hts_sex %>% 
    ggplot(aes(modality, val)) + 
    geom_col() + 
    facet_grid(. ~ indicator, scales = "free_x") + 
    scale_y_continuous(label = comma) +
    coord_flip() +
    labs(x = "", y = "") +
    rw_plot_theme()

  savename <- paste0("modalities_", sex, ".png")
  
  ggsave(savename,
         plot = plot,
         path = "Viz",
         w = 9, h = 3.75, units = "in",
         dpi = 300)
}

viz_hts()
viz_hts("Female")
viz_hts("Male")
viz_hts("Children")
  


df_hts_kp <- df_nga %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS"),
         standardizeddisaggregate == "KeyPop/Result") %>% 
  group_by(indicator, otherdisaggregate) %>% 
  summarize_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(fy2018apr != 0) %>% 
  spread(indicator, fy2018apr) %>% 
  mutate(`POSITIVITY (%)` = round(HTS_TST_POS/HTS_TST, 2)*100) %>% 
  gather(indicator, val, -otherdisaggregate)

df_hts_kp %>% 
  ggplot(aes(otherdisaggregate, val)) + 
  geom_col() + 
  facet_grid(. ~ indicator, scales = "free_x") + 
  scale_y_continuous(label = comma) +
  coord_flip() +
  labs(x = "", y = "") +
  rw_plot_theme()

savename <- "kp_hts.png"

ggsave(savename,
       path = "Viz",
       w = 9, h = 3.75, units = "in",
       dpi = 300)


link <- df_nga %>%
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
         standardizeddisaggregate %in% 
           c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result",
             "Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus")) %>% 
  mutate(sex = ifelse(agecoarse == "<15", "Children", sex)) %>%
  filter(!is.na(sex)) %>% 
  group_by(indicator, sex) %>% 
  summarize_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  spread(indicator, fy2018apr) %>% 
  mutate(`Linkage (%)` = round(TX_NEW/ HTS_TST_POS, 2) * 100) %>% 
  gather(indicator, val, -sex) %>% 
  mutate(indicator = factor(indicator, 
                            levels = c("HTS_TST_POS", "TX_NEW", "Linkage (%)")))

link %>% 
  ggplot(aes(sex, val)) +
  geom_col() + 
  facet_grid(. ~ indicator, scales = "free_x") + 
  scale_y_continuous(label = comma) +
  coord_flip() +
  labs(x = "", y = "") +
  rw_plot_theme()
  
  
index <- df_nga %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS"),
         standardizeddisaggregate %in% 
           c("Modality/Age/Sex/Result", "Modality/Age Aggregated/Sex/Result"),
         modality %in% c("Index", "IndexMod")) %>% 
  mutate(sex = ifelse(agecoarse == "<15", "Children", sex)) %>% 
  filter(!is.na(sex)) %>% 
  group_by(indicator, modality, sex) %>% 
  summarize_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(modality = case_when(modality == "Index" ~ "Facility",
                              TRUE ~ "Community"))

index %>% 
  ggplot(aes(sex, fy2018apr)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(label = comma) +
  facet_wrap(indicator ~ modality) +
  labs(x = "", y = "") +
  rw_plot_theme()

ggsave("index.png",
       path = "Viz",
       w = 9, h = 3.75, units = "in",
       dpi = 300)
