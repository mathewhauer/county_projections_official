###------Read the Eval Projections-----
## @knitr readevalproj_fitted


K05_launch <- K05_pop[which(K05_pop$YEAR == launch_year),] %>%
  group_by(STATE, COUNTY, RACE, GEOID, YEAR) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  ungroup()


files <- paste0("PROJECTIONS/EVAL//", list.files(path = "./PROJECTIONS/EVAL/",pattern = ".csv"))
temp <- lapply(files, fread, sep=" ")
z <- rbindlist( temp ) %>%
  # dplyr::rename(YEAR = V3,
  #               SEX = V4,
  #               COUNTYRACE = V5,
  #               TYPE = V6,
  #               AGE = V7,
  #               A = V8,
  #               B = V9,
  #               C = V10,
  #               Var1 = V2) %>%
  mutate(STATE= substr(COUNTYRACE, 1,2),
         COUNTY = substr(COUNTYRACE, 3,5),
         GEOID = paste0(STATE, COUNTY),
         A = if_else(A<0, 0, A),
         B = if_else(B<0, 0, B),
         C = if_else(C<0,0, C),
         RACE = substr(COUNTYRACE, 7,7))
z[is.na(z)] <-0
basesum <-  K05_launch[which( K05_launch$YEAR == launch_year),] %>%
  dplyr::select(STATE, COUNTY, RACE, GEOID, POPULATION)

addsum <- z[which(z$TYPE=="ADD" & z$YEAR == (launch_year+FORLEN)),] %>%
  group_by(STATE, COUNTY, RACE, GEOID, TYPE) %>%
  dplyr::summarise(A = sum(A))

addmult <- left_join(addsum, basesum) %>%
  mutate(COMBINED = if_else(A>= POPULATION, "ADD" ,"Mult")) %>%
  dplyr::select(STATE, COUNTY, RACE, GEOID, COMBINED)



combined<- left_join(z, addmult) %>%
  filter(TYPE == COMBINED) %>%
  mutate(TYPE = "ADDMULT") %>%
  dplyr::select(-COMBINED)

z<- rbind(z, combined) %>%
  dplyr::select(-V1) %>%
  mutate(TYPE = case_when(
  TYPE == "ADD" ~ "CCD",
  TYPE == "Mult" ~ "CCR",
  TYPE == "ADDMULT" ~ "CCD/CCR"
))
z$SEX<-as.character(z$SEX) # converting from integer to character to join with K05_launch2

z<-  left_join(as.data.frame(z), as.data.frame(K05_launch2))
z<- left_join(z, countynames)
z[is.na(z)] <-0
z<- filter(z,
           !GEOID %in% c("02900", "04910", "15900", "35910", "36910", "51910", "51911","51911", "51913", "51914", "51915", "51916", "51918"),
           !YEAR == 2020)

z<- filter(z,
              TYPE == "CCD/CCR")
z2 <- z %>%
  group_by(YEAR, SEX, AGE) %>%
  dplyr::summarise(Atot = sum(A),
                   Ptot = sum(POPULATION))

z<- left_join(z, z2) %>%
  mutate(A = (A/Atot)*Ptot,
         GEOID = case_when(
           GEOID=="46113"~ "46102",
           GEOID== "51917" ~ "51019",
           TRUE ~ as.character(GEOID)))
