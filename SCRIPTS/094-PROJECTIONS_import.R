###------FIPSCODES-----
## @knitr projections


SSPs<- read_csv("DATA-PROCESSED/SSP_asrc.csv")
  # mutate(GEOID = case_when(
  #   GEOID=="46113"~ "46102", # Shannon County (46113)'s name changed to Oglala Lakota (46102)
  #   GEOID== "51917" ~ "51019", # Bedford City (51917) is merged into Bedford County (51019)
  #   GEOID == "02270" ~ "02158", # Wade Hampton (02270) is actually (02158)
  #   TRUE ~ as.character(GEOID)
  # ))
        

# zzz<- SSPs[which(SSPs$GEOID=="51917"),]