library(tidyverse)
library(scales)
library(ggrepel)



# TESTING VISUALIZATIONS --------------------------------------------------

#Munge

  #import data
    df_nga <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_1_Nigeria.rds")
  
  #setup testing data
    testing <- df_nga %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator",
             snu1 != "_Military Nigeria") %>% 
      group_by(snu1, indicator) %>% 
      summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(indicator, fy2018apr) %>% 
      mutate(`POSITIVITY (%)` = round(HTS_TST_POS / HTS_TST, 3)*100) 

#Compare testing, testing pos and positivity
  
  #structure
    testing_bar <- testing %>% 
      arrange(HTS_TST) %>% 
      mutate(snu1 = as_factor(snu1)) %>% 
      gather(indicator, val, -snu1) %>% 
      mutate(bar_color = ifelse(indicator == "POSITIVITY (%)", "#6BB1C9","#6585CF"))
  
  #facet bar
    testing_bar %>% 
      ggplot(aes(snu1, val)) +
      geom_col(fill = testing_bar$bar_color) +
      coord_flip() +
      scale_y_continuous(label = comma) +
      labs(title = "Testing and Positivity across States", x = "", y = "", caption = "Nigeria FY18 APR (MSD)") +
      facet_wrap(. ~ indicator, scales = "free_x") +
      theme_bw() +
      theme(panel.border = element_blank(),
            axis.ticks = element_blank(),
            #text = element_text(size = 12, colour = "#4D4D4D"),
            axis.text = element_text(size = 13, colour = "#4D4D4D"),
            strip.background = element_rect(fill = "white", color = "white"),
            strip.text = element_text(size = 13, color = "#4D4D4D"))
  
    ggsave("Viz/testing.png", dpi = 300, width = 9, height = 7)

#Scatter to compare high volume and yield

  #identify median values for tests and positivity
    (median <- testing %>% 
      summarise_at(vars(HTS_TST, `POSITIVITY (%)`), median, na.rm = TRUE))
  
    hts_med <- median$HTS_TST[1]
    pos_med <- median$`POSITIVITY (%)`[1]
  
  #highlight high volumne and posiviity
    testing_point <- testing %>% 
      mutate(point_color = case_when(`POSITIVITY (%)` > 4 ~ "#6BB1C9", 
                                     HTS_TST > 300000     ~ "#6585CF",
                                     TRUE                 ~ "#808080"),
             point_lab = case_when(`POSITIVITY (%)` > 4 | HTS_TST > 300000 ~ snu1))
      
  #scatter 
    testing_point %>%
      ggplot(aes(HTS_TST, `POSITIVITY (%)`)) +
      geom_hline(yintercept = pos_med, colour = "#D3D3D3") +
      geom_vline(xintercept = hts_med, colour = "#D3D3D3") +
      geom_point(size = 4, 
                 color = testing_point$point_color) +
      geom_text_repel(aes(label = point_lab), size = 4, na.rm = TRUE, color = testing_point$point_color) +
      scale_x_continuous(label = comma) +
      labs(title = "Comparing High Volume vs Positivity States", y = "Positvity (%)",
           caption = "Nigeria FY18 APR (MSD)") +
      theme_bw() +
      theme(text = element_text(size = 13, colour = "#4D4D4D"),
            axis.ticks = element_blank(),
            panel.border = element_blank())
    
    ggsave("Viz/testing_scatter.png", dpi = 300, width = 9, height = 7)
    

#Modalities
    
  #setup testing data
  testing_mod <- df_nga %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           snu1 != "_Military Nigeria") %>% 
    group_by(snu1, indicator, modality) %>% 
    summarise_at(vars(fy2018apr), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(indicator, fy2018apr) %>% 
    filter(HTS_TST != 0) %>% 
    mutate(`POSITIVITY (%)` = round(HTS_TST_POS / HTS_TST, 3)*100) %>% 
    gather(indicator, val, -snu1, -modality)
  
  state_order <- testing_mod %>% 
    filter(indicator == "HTS_TST") %>% 
    group_by(snu1) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    arrange(val) %>% 
    pull(snu1)
  
  mod_order <- testing_mod %>% 
    filter(indicator == "HTS_TST") %>% 
    group_by(modality) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    arrange(val) %>% 
    pull(modality)
  
  testing_mod_full <- testing_mod %>% 
    mutate(snu1 = factor(snu1, levels = state_order)) %>% 
    spread(snu1, val, drop = FALSE, fill = 0) %>% 
    gather(snu1, val, -modality, -indicator) %>% 
    mutate(bar_color = ifelse(indicator == "POSITIVITY (%)", "#6BB1C9","#6585CF"),
           snu1 = factor(snu1, levels = state_order))
  
  
  grph <- function(mod){
    
    df <- testing_mod_full %>% 
      filter(modality == mod) %>% 
      arrange(snu1)
    
    savename <- paste0("mod_", mod, ".png")
    
    #facet bar
    df %>% 
      ggplot(aes(snu1, val)) +
      geom_col(fill = testing_bar$bar_color) +
      coord_flip() +
      scale_y_continuous(label = comma) +
      labs(title = "Testing and Positivity across States", x = "", y = "", caption = "Nigeria FY18 APR (MSD)") +
      facet_wrap(. ~ indicator, scales = "free_x") +
      theme_bw() +
      theme(panel.border = element_blank(),
            axis.ticks = element_blank(),
            #text = element_text(size = 12, colour = "#4D4D4D"),
            axis.text = element_text(size = 13, colour = "#4D4D4D"),
            strip.background = element_rect(fill = "white", color = "white"),
            strip.text = element_text(size = 13, color = "#4D4D4D"))
    
    ggsave(savename, path = "Viz", device = "png", dpi = 300, width = 9, height = 7)
  }
  
  mod_order

  grph("Malnutrition")  
  