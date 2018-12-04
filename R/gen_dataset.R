
#indicators to filter down to
ind <- c("GEND_GBV", "HTS_TST", "HTS_TST_NEG", "HTS_TST_POS", "OVC_SERV", 
         "PMTCT_ART", "PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_HEI_POS", "PMTCT_EID",
         "PMTCT_EID_Less_Equal_Two_Months","TB_ART", "TB_STAT", 
         "TX_CURR", "TX_NEW", "TX_NET_NEW","TX_PVLS", "TX_RET")

#create dataset
df_nga <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-18_20181115_v1_1_Nigeria.rds") %>% 
  filter(indicator %in% ind) %>% 
  rename_official() %>% 
  rw_gen_nn_target()

#save
fs::dir_create("Data")
write_rds(df_nga, "Data/MSD_PSNU_IM_NGA_filtered_FY18Q4i.rds")
