
source('./SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./SCRIPTS/001-fipscodes.R')
source('./SCRIPTS/003-proj_basedataload.R')


K05_launch <- K05_pop[which(K05_pop$YEAR == launch_year),] %>%
  group_by(STATE, COUNTY, GEOID, YEAR, RACE, SEX) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  ungroup()


files <- paste0("PROJECTIONS/PROJ//", list.files(path = "./PROJECTIONS/PROJ/",pattern = ".csv"))
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
         A = as.numeric(A),
         B = as.numeric(B),
         C = as.numeric(C),
         A = if_else(A<0, 0, A),
         B = if_else(B<0, 0, B),
         C = if_else(C<0,0, C),
         RACE = substr(COUNTYRACE, 7,7))
z[is.na(z)] <-0
basesum <-  K05_launch[which( K05_launch$YEAR == launch_year),] %>%
  dplyr::select(STATE, COUNTY, GEOID, POPULATION, RACE)

addsum <- z[which(z$TYPE=="ADD" & z$YEAR == (launch_year+FORLEN)),] %>%
  group_by(STATE, COUNTY, GEOID, RACE, TYPE) %>%
  dplyr::summarise(A = sum(A))

addmult <- left_join(addsum, basesum) %>%
  mutate(COMBINED = if_else(A>= POPULATION, "ADD" ,"Mult")) %>%
  dplyr::select(STATE, COUNTY, GEOID, RACE, COMBINED)


basesum2 <-  K05_launch[which( K05_launch$YEAR == launch_year),] %>%
  dplyr::select(STATE, COUNTY, GEOID, POPULATION, RACE) %>%
  group_by(GEOID, RACE) %>%
  dplyr::summarise(poptot = sum(POPULATION))

combined<- left_join(z, addmult) %>%
  filter(TYPE == COMBINED) %>%
  mutate(TYPE = "ADDMULT") %>%
  dplyr::select(-COMBINED)

z2<- rbind(z, combined) %>%
  dplyr::select(-V1)
z2<-  left_join(as.data.frame(z2), as.data.frame(K05_launch2))
z2<- left_join(z2, countynames)
z2[is.na(z2)] <-0
z2<- filter(z2,
           !GEOID %in% c("02900", "04910", "15900", "35910", "36910", "51910", "51911","51911", "51913", "51914", "51915", "51916", "51918"))
z3 <- filter(z2,
             TYPE == "ADDMULT")

totals <- z3 %>%
  group_by(AGE, SEX, YEAR) %>%
  dplyr::summarise(poptot = sum(A)) 
totals2 <- left_join(z3, totals) %>%
  mutate(percentage = (A/poptot))


unzip(zipfile='DATA/SspDb_country_data_2013-06-12.csv.zip', exdir = "DATA")

SSPs <- read_csv("DATA/SspDb_country_data_2013-06-12.csv") %>%
  filter(REGION == "USA",
         grepl("Population",VARIABLE)) %>%
  separate(VARIABLE, c("VARIABLE", "VARIABLE1", "VARIABLE2", "VARIABLE3", "VARIABLE4"), by ="|")

SSPs2 <- SSPs %>%
  dplyr::select(-`1950`:-`2010`, -`2105`:-`2150`) %>%
  mutate(SEX = case_when(
     VARIABLE1 == "Female" ~ 2,
     VARIABLE1 == "Male" ~ 1),
    AGE = case_when(
      VARIABLE2 == "Aged0" ~ 1,
      VARIABLE2 == "Aged5" ~ 2,
      VARIABLE2 == "Aged10" ~ 3,
      VARIABLE2 == "Aged15" ~ 4,
      VARIABLE2 == "Aged20" ~ 5,
      VARIABLE2 == "Aged25" ~ 6,
      VARIABLE2 == "Aged30" ~ 7,
      VARIABLE2 == "Aged35" ~ 8,
      VARIABLE2 == "Aged40" ~ 9,
      VARIABLE2 == "Aged45" ~ 10,
      VARIABLE2 == "Aged50" ~ 11,
      VARIABLE2 == "Aged55" ~ 12,
      VARIABLE2 == "Aged60" ~ 13,
      VARIABLE2 == "Aged65" ~ 14,
      VARIABLE2 == "Aged70" ~ 15,
      VARIABLE2 == "Aged75" ~ 16,
      VARIABLE2 == "Aged80" ~ 17,
      VARIABLE2 == "Aged85" ~ 18,
      VARIABLE2 == "Aged90" ~ 18,
      VARIABLE2 == "Aged95" ~ 18,
      VARIABLE2 == "Aged100" ~ 18),
    SSP = case_when(
      grepl("SSP1", SCENARIO) ~ "SSP1",
      grepl("SSP2", SCENARIO) ~ "SSP2",
      grepl("SSP3", SCENARIO) ~ "SSP3",
      grepl("SSP4", SCENARIO) ~ "SSP4",
      grepl("SSP5", SCENARIO) ~ "SSP5"
    )) %>%
  filter(is.na(VARIABLE4),
         !is.na(VARIABLE2)) %>%
  dplyr::select(-MODEL:-UNIT) %>%
  na.omit %>%
  gather(YEAR, Population, `2015`:`2100`) %>%
  group_by(SEX, AGE, SSP, YEAR) %>%
  dplyr::summarise(Population = sum(Population)) %>%
  ungroup() %>%
  spread(SSP, Population) %>%
  mutate(YEAR = as.integer(YEAR)
         # SEX = as.character(SEX))
)
test <- left_join(totals2, SSPs2) %>%
  mutate(SSP1 = SSP1*percentage*1000000,
         SSP2 = SSP2*percentage*1000000,
         SSP3 = SSP3*percentage*1000000,
         SSP4 = SSP4*percentage*1000000,
         SSP5 = SSP5*percentage*1000000,
         GEOID = case_when(
           GEOID=="46113"~ "46102", # Shannon County (46113)'s name changed to Oglala Lakota (46102)
           GEOID== "51917" ~ "51019", # Bedford City (51917) is merged into Bedford County (51019)
           GEOID == "02270" ~ "02158", # Wade Hampton (02270) is actually (02158)
           TRUE ~ as.character(GEOID))
         ) %>%
  select(YEAR, SEX, STATE, COUNTY, GEOID, RACE, AGE, SSP1:SSP5)

test2 <- test %>%
  group_by(YEAR, STATE) %>%
  dplyr::summarise(SSP1 = sum(SSP1),
                   SSP2 = sum(SSP2),
                   SSP3 = sum(SSP3),
                   SSP4 = sum(SSP4),
                   SSP5 = sum(SSP5)) %>%
  gather(Scenario, Population, SSP1:SSP5)

test3 <- test %>%
  group_by(YEAR, SEX, STATE, COUNTY, GEOID, RACE, AGE) %>%
  dplyr::summarise(SSP1 = sum(SSP1),
                   SSP2 = sum(SSP2),
                   SSP3 = sum(SSP3),
                   SSP4 = sum(SSP4),
                   SSP5 = sum(SSP5))


write_csv(test3, "DATA-PROCESSED/SSP_asrc.csv")
write_csv(test2, "DATA-PROCESSED/ssp_sums.csv")
