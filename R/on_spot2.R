df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency != "Dedup") %>% 
  group_by(indicator, fundingagency) %>% 
  summarise_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(fy2018ach = round(fy2018apr / fy2018_targets, 2) *100) %>% 
  gather(pd, val, starts_with("fy")) %>% 
  spread(indicator, val) %>% 
  mutate(`Positivity (%)` = ifelse(pd != "fy2018ach", round(HTS_TST_POS / HTS_TST, 2) *100, NA),
         pd = case_when(pd == "fy2018apr" ~ "Results",
                        pd == "fy2018ach" ~ "Achievement (%)",
                        TRUE              ~ "Targets"),
         pd = factor(pd, levels = c("Targets", "Results", "Achievement (%)"))) %>% 
  arrange(fundingagency, pd)



df_hts_ach <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency != "Dedup") %>% 
  group_by(indicator, fundingagency) %>% 
  summarise_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(fy2018ach = percent(fy2018apr / fy2018_targets, accuracy = 1))

df_hts_ach %>% 
  ggplot(aes(fundingagency, fy2018apr)) +
  geom_col(fill = color["lice"]) +
  geom_errorbar(aes(ymax=fy2018_targets, ymin=fy2018_targets), width=0.75, size =1, color= color["amazon"]) +
  geom_text(aes(y = fy2018_targets, label = fy2018ach), hjust = -.4) +
  coord_flip() + 
  labs(x = "", y = "") + 
  scale_y_continuous(label = comma) + 
  facet_wrap(. ~ indicator, scales = "free_x") +
  rw_plot_theme()

  
df_trend <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(indicator) %>% 
  summarise_at(vars(starts_with("fy2018q")), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  gather(pd, val, -indicator) %>% 
  spread(indicator, val) %>% 
  mutate(`Positivity(%)` = round(HTS_TST_POS / HTS_TST, 3) *100) %>% 
  gather(indicator, val, -pd) %>% 
  mutate(pd = str_remove(pd, "20") %>% toupper())

df_trend %>% 
  ggplot(aes(pd, val, group = indicator)) +
  geom_line(color = color["ice"]) +
  geom_point(size = 2, color = color["ice"]) +
  scale_y_continuous(labels = comma) +
  facet_wrap(. ~ indicator, scales = "free_y") + 
  expand_limits(x = 0, y = 0) +
  labs(x = "", y = "") +
  rw_plot_theme()

df_yield <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>%
  filter(agecoarse != "<15",
         !agefine %in% c("Unknown Age", "Incompatible Age Entered")) %>% 
  group_by(indicator, agefine, sex) %>% 
  summarize_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, fy2018apr) %>%
  arrange(desc(agefine)) %>% 
  mutate(yield = round(HTS_TST_POS/HTS_TST, 3) *100,
         agefine = as_factor(agefine),
         label = case_when(agefine == "15-19" ~ sex))
  

df_yield %>% 
  ggplot(aes(yield, agefine)) +
  geom_path(color = color["lgray"]) + 
  geom_point(color = ifelse(df_yield$sex == "Female", color["ice"], color["lice"]),
             size = 6) +
  geom_text(aes(label = label), na.rm = TRUE, vjust = -1, 
            color = ifelse(df_yield$sex == "Female", color["ice"], color["txtgray2"])) +
  labs(x= "testing positivity (%)", y = "") +
  rw_plot_theme()



df_modyield <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>%
  filter(agecoarse != "<15",
         !agefine %in% c("Unknown Age", "Incompatible Age Entered")) %>% 
  group_by(indicator, agefine, sex, modality) %>% 
  summarise_at(vars("fy2018apr"), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  spread(indicator, fy2018apr) %>% 
  filter(HTS_TST != 0) %>% 
  mutate(yield = round(HTS_TST_POS/HTS_TST, 2)*100)

df_modyield %>% 
  filter(sex == "Female") %>% 
  ggplot(aes(agefine, yield)) +
  geom_col( na.rm = TRUE) + 
  facet_wrap(. ~ modality) +
  labs(x = "", y = "testing positivity (%)") +
  rw_plot_theme()

df_modyield %>% 
  filter(sex == "Male") %>% 
  ggplot(aes(agefine, yield)) +
  geom_col( na.rm = TRUE) + 
  facet_wrap(. ~ modality) +
  labs(x = "", y = "testing positivity (%)") +
  rw_plot_theme()

df_mods <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
  group_by(indicator, modality) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, fy2018apr) %>% 
  filter(HTS_TST != 0) %>% 
  mutate(yield = HTS_TST_POS/HTS_TST)


df_mods %>% 
  ggplot(aes(reorder(modality, HTS_TST_POS), HTS_TST_POS)) +
  geom_segment(aes(xend=modality, y=0, yend=HTS_TST_POS),
               color = color["ice"],
               size = 2,
               na.rm = TRUE) +
  geom_point(color = color["ice"], size = 6) +
  geom_text(aes(label = comma(HTS_TST_POS)), hjust = -.5) +
  coord_flip() +
  labs(x = "", y = "") +
  rw_plot_theme() +
  theme(axis.text.x = element_blank())

df_mods %>% 
  ggplot(aes(reorder(modality, yield), yield)) +
  geom_segment(aes(xend=modality, y=0, yend=yield),
               color = color["ice"],
               size = 2,
               na.rm = TRUE) +
  geom_point(color = color["ice"], size = 6) +
  geom_text(aes(label = percent(yield)), hjust = -.5) +
  coord_flip() +
  labs(x = "", y = "") +
  rw_plot_theme() +
  theme(axis.text.x = element_blank())
  


df_mods_agency_fac <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         !str_detect(modality, "Mod")) %>% 
  group_by(indicator, modality, fundingagency) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, fy2018apr) %>% 
  filter(HTS_TST != 0) %>% 
  mutate(yield = HTS_TST_POS/HTS_TST,
         agency_color = case_when(fundingagency == "USAID" ~ color["ice"],
                                  fundingagency == "DOD" ~ color["khaki"],
                                  TRUE ~ color["mint"]),
         agency_lab = case_when(str_detect(modality, "Index") ~ fundingagency))

df_mods_agency_fac %>% 
  ggplot(aes(reorder(modality, yield), yield)) +
  geom_point(color = df_mods_agency_fac$agency_color, 
             size = 6) +
  geom_text(aes(y = yield, label = agency_lab), vjust = -.9, color = df_mods_agency_fac$agency_color) +
  coord_flip() +
  scale_y_continuous(label = percent) +
  labs(x = "", y = "") +
  rw_plot_theme()

df_mods_agency_com <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         str_detect(modality, "Mod")) %>% 
  group_by(indicator, modality, fundingagency) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, fy2018apr) %>% 
  filter(HTS_TST != 0) %>% 
  mutate(yield = HTS_TST_POS/HTS_TST,
         agency_color = case_when(fundingagency == "USAID" ~ color["ice"],
                                  fundingagency == "DOD" ~ color["khaki"],
                                  TRUE ~ color["mint"]),
         agency_lab = case_when(str_detect(modality, "Index") ~ fundingagency))

df_mods_agency_com %>% 
  ggplot(aes(reorder(modality, yield), yield)) +
  geom_point(color = df_mods_agency_com$agency_color, 
             size = 6) +
  geom_text(aes(y = yield, label = agency_lab), vjust = -.9, color = df_mods_agency_com$agency_color) +
  coord_flip() +
  scale_y_continuous(label = percent) +
  labs(x = "", y = "") +
  rw_plot_theme()


df_index <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         modality %in% c("Index", "IndexMod")) %>% 
  group_by(indicator, modality, primepartner) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, fy2018apr) %>% 
  filter(HTS_TST != 0) %>% 
  mutate(yield = HTS_TST_POS/HTS_TST,
         facility_color = case_when(modality == "Index"    ~ color["mint"],
                                    modality == "IndexMod" ~ color["ubuntu"]),
         fac_lab = case_when(primepartner == "Center for Integrated Health Programs" ~ "facility",
                             primepartner == "Heartland Alliance for Human Needs and Human Rights" & modality == "IndexMod" ~ "community"))
df_index %>% 
  ggplot(aes(reorder(primepartner, yield), yield)) +
  geom_segment(aes(xend=primepartner, y=0, yend=max(df_index$yield) + .02),
               color = color["lgray"]) +
  geom_point(color = df_index$facility_color, 
             size = 6) +
  geom_text(aes(y = yield, label = fac_lab), vjust = -1, color = color["txtgray2"]) +
  coord_flip() +
  scale_y_continuous(label = percent) +
  labs(x = "", y = "") +
  rw_plot_theme()

df_index_top <- df_nga %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
         standardizeddisaggregate == "Modality/Age/Sex/Result",
         modality %in% c("Index", "IndexMod")) %>% 
  group_by(indicator, snu1) %>% 
  summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, fy2018apr) %>% 
  arrange(desc(HTS_TST)) %>% 
  #mutate(cum_share = cumsum(HTS_TST)/sum(HTS_TST)) 
  mutate(snu1 = fct_lump(snu1, 6, w = HTS_TST)) %>% 
  group_by(snu1) %>%
  summarise_at(vars(HTS_TST, HTS_TST_POS), sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(`Positvity (%)` = round(HTS_TST_POS/HTS_TST, 3)*100,
         snu1 = fct_reorder(snu1, HTS_TST),
         snu1 = fct_relevel(snu1, "Other"))

df_index_top$snu1 <- fct_recode(df_index_top$snu1, `all other states` = "Other")

df_index_top %>% 
  ggplot(aes(snu1, HTS_TST)) +
  geom_col( fill = color["lice"]) +
  geom_text(aes(label = comma(HTS_TST)), hjust = -.3) +
  coord_flip() +
  scale_y_continuous(label = comma) +
  expand_limits(y = max(df_index_top$HTS_TST)*1.15) +
  labs(x = "", y = "") +
  rw_plot_theme() +
  theme(axis.text.x = element_blank())

