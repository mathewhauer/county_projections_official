###------FIPSCODES-----
## @knitr projections


SSPs<- read_csv("DATA-PROCESSED/SSP_asrc.csv") %>%
  mutate(GEOID = case_when(
    GEOID=="46113"~ "46102",
    GEOID== "51917" ~ "51019",
    TRUE ~ as.character(GEOID)
  ))
        

# zzz<- SSPs[which(SSPs$GEOID=="51917"),]