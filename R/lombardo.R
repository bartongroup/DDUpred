# Prepare Lombardo file for recent data - needs renaming columns and recoding
# moka_ionState7.4 into Moka_status
refurbish_lombardo <- function(in_file, rename_file, out_file) {
  dat <- read_csv(in_file, show_col_types = FALSE, na = c("", "NA", "n/a", "N/A", "None"), guess_max = 10000)
  
  dups <- c("TPSA_NO", "RotBondCount", "MOLWEIGHT", "AROMATIC_RINGS", "HBA_LIPINSKI", "HBD_LIPINSKI")
  
  r <- read_csv(rename_file, show_col_types = FALSE) %>% 
    filter(!(lombardo_variable %in% dups))
  ren <- set_names(r$lombardo_variable, r$current_variable)
  
  dat %>%
    rename(all_of(ren)) %>% 
    select(-all_of(dups)) %>% 
    mutate(Moka_status = Moka_status %>% 
        recode(
          "cationic" = "Base",
          "neutral" = "Neutral",
          "anionic" = "Acid",
          "zwitterionic" = "Zwitterionic"
        )
    ) %>% 
    write_csv(out_file)
}