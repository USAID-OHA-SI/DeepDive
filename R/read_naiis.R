##  Nigeria FY18 Treatment Cascade
##  A.Chafetz
##  Purpose: extract NAIIS data from Tabula output & combine
##  Date: 2018-12-17

#dependencies

library(tidyverse)

#function
  read_naiis <- function(filepath){
    
    #pull out state name for adding to df
    state_name <- 
      read_csv(filepath, 
               n_max = 1, 
               col_names = FALSE, 
               col_types = cols(.default = "c")) %>% 
      pull(X1)
    
    #import, removing blank columns
    naiis_tbl <- 
      suppressWarnings( 
      read_csv(filepath, col_types = cols(.default = "c"), skip = 1)) %>% 
      select(-starts_with("X"))
    
    #rename the first column since might differ if separator missing
    naiis_tbl <- naiis_tbl %>% 
      rename("type" = !!names(.[1]))
    
    #remove special characters and spaces
    naiis_tbl <- naiis_tbl %>% 
      mutate_all(str_replace_all, "0\\(", "0 \\(") %>%
      mutate_all(str_replace_all, "  ", " ") %>% 
      mutate_all(str_replace_all, ",([[:alnum:]])", ", \\1") %>% #add space if non exists after comma
      mutate_all(str_replace_all, "\\( ([[:alnum:]])", "\\(\\1") %>% #remove space between ( and num
      mutate_all(str_replace_all, "\\(  ([[:alnum:]])", "\\(\\1") %>% #remove space between ( and num
      mutate_all(str_replace_all, "([[:alnum:]])\\(", "\\1 \\(") %>% #add space if non exists after num and (
      mutate_all(str_replace_all, "([[:alnum:]]) \\)", "\\1\\)") %>% #remove space between num and )
      mutate_all(str_remove_all, "†|‡|§|  ,| ,|,|%|\\(|\\)") %>% 
      mutate_at(vars(contains("n")), str_remove, " ")
    
    naiis_tbl <- naiis_tbl %>% 
      mutate(type = str_replace(type, "On Treatment", "On_Treatment"),
             type = str_replace(type, "Virally Suppressed", "Virally_Suppressed"),
             type = str_replace(type, " years", "_years"),
             type = str_replace(type, "Viral suppression", "Viral_suppression"),
             type = str_replace(type, "All PLHIV 15-64_years", "All_PLHIV_15-64_years"),
             type = str_replace(type, "PLHIV on treatment 15-64_years", "PLHIV_on_treatment_15-64_years"),
             type = str_trim(type)) 
    
    #combine all into one string and then seperate out into correct # of columns
    naiis_tbl <- naiis_tbl %>% 
      filter(!type %in% c("Prevalence", NA, "Viral_suppression")) %>% 
      unite(all, sep = " ") %>% 
      separate(all, into = c("type", "female", "female_ci_lower", "female_ci_upper",
                             "male", "male_ci_lower", "male_ci_upper", "total", 
                             "total_ci_lower", "total_ci_upper", "n"), 
               sep = " ")
    
    #add grouping
    if(nrow(naiis_tbl) == 3){
      df_grp <- tibble(group = rep("90-90-90 Achievement", 3))
    } else if (nrow(naiis_tbl) == 6) {
      df_grp <- tibble(group = c(rep("Prevalence", 4), rep("Viral suppression", 2))) 
    } else{
      df_grp <- tibble(group = rep("NA", nrow(naiis_tbl)))
    }
    
    naiis_tbl <- bind_cols(df_grp, naiis_tbl) 
    
    #add state name
    naiis_tbl <- naiis_tbl %>% 
      mutate(state = state_name) %>% 
      select(state, everything())
    
    #convert cols to numeric
    naiis_tbl <- naiis_tbl %>% 
      mutate_at(vars(female, female_ci_lower, female_ci_upper,
                     male, male_ci_lower, male_ci_upper, total, 
                     total_ci_lower, total_ci_upper, n),
                ~ as.double(.))
    
    #remove underscore
    naiis_tbl <- naiis_tbl %>% 
      mutate(type = str_replace_all(type, "_", " "))
    
    return(naiis_tbl)
  }
 
#execute function to import and combine
  df_naiis <- map_dfr(.x = lst_tbl,
          .f = ~ read_naiis(.x))

#export combined table as csv
  write_csv(df_naiis, "NAIIS_raw_all.csv", na = "")






  

