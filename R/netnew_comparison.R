
#TX_NET_NEW comparison

df_nga <- df_nga %>% 
  rw_gen_nn_target()

#quarterly trends
df_nga %>%
  filter(indicator %in% c("TX_CURR", "TX_NET_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency != "Dedup") %>%
  group_by(fundingagency, indicator) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() %>%
  gather(pd, val, -fundingagency, -indicator) %>%
  spread(indicator, val) #%>%
  #write_csv("C:/Users/achafetz/Downloads/nga_net_new.csv")

#table of NET_NEW and TX_CURR
df_nga %>% 
  filter(indicator %in% c("TX_CURR", "TX_NET_NEW"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency %in% c("USAID", "HHS/CDC")) %>% 
  group_by(fundingagency, indicator) %>% 
  summarize_at(vars(fy2018apr, fy2018_targets), sum, na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(`Achievement (%)` = round(fy2018apr/fy2018_targets, 3)*100) %>% 
  rename(Agency = fundingagency,
         `FY18 APR` = fy2018apr,
         `FY18 Target` = fy2018_targets,
         ) %>% 
  kable(format.args = list(big.mark = ",", zero.print = FALSE)) %>% 
  kable_styling()
  
  
#Net new graph data
  df_nn <- df_nga %>% 
    filter(indicator %in% c("TX_CURR", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency %in% c("USAID", "HHS/CDC")) %>% 
    group_by(indicator, fundingagency) %>% 
    summarise_at(vars(contains("q"), fy2018_targets), sum, na.rm = TRUE) %>% 
    gather(pd, val, -indicator, -fundingagency) %>% 
    spread(indicator, val) %>% 
    mutate(tx_curr_gr = ifelse(TX_NET_NEW > 0, TX_CURR - TX_NET_NEW, TX_CURR),
           tx_nn_gr = ifelse(TX_NET_NEW > 0, TX_NET_NEW, 0)) %>% 
    select(-TX_CURR, -TX_NET_NEW) %>% 
    gather(indicator, val, -pd, -fundingagency) %>% 
    mutate(pd = str_remove(pd, "20") %>% toupper(.),
           pd = str_replace(pd, "_", " "),
           indicator = case_when(indicator == "tx_nn_gr" ~ "TX_NET_NEW",
                                 TRUE                    ~ "TX_CURR"),
           indicator = factor(indicator, levels = c("TX_NET_NEW","TX_CURR")),
           pd = factor(pd, levels = c("FY17Q1", "FY17Q2", "FY17Q3", "FY17Q4",
                                      "FY18Q1", "FY18Q2", "FY18Q3", "FY18Q4",
                                      "FY18 TARGETS"))) 
#Net new graph  
  df_nn %>% 
    ggplot(aes(pd, fill = indicator)) + 
    geom_bar(aes(weight = val), 
             position = "stack") +
    scale_fill_manual(values=c("#335b8e", "#a2bcdd")) +
    scale_y_continuous(label = comma) +
    labs(title = "Trends in TX_NET_NEW", subtitle = "FY17-18 Results & FY18 Targets", x = "", y = "") +
    facet_grid(fundingagency ~ .) +
    rw_plot_theme()
