###------Uncontrolled Comparison Figure -----
## @knitr uncontrolledcomp_figure

# source('./SCRIPTS/000-Libraries.R')      # loading in the libraries
# source('./SCRIPTS/001-fipscodes.R')
# source('./SCRIPTS/003-proj_basedataload.R')
# 
# 
# K05_launch <- K05_pop[which(K05_pop$YEAR == launch_year),] %>%
#   group_by(STATE, COUNTY, GEOID, YEAR, RACE, SEX) %>%
#   dplyr::summarise(POPULATION = sum(POPULATION)) %>%
#   ungroup()
# 
# 
# files <- paste0("PROJECTIONS/PROJ//", list.files(path = "./PROJECTIONS/PROJ/",pattern = ".csv"))
# temp <- lapply(files, fread, sep=" ")
# z <- rbindlist( temp ) %>%
#   # dplyr::rename(YEAR = V3,
#   #               SEX = V4,
#   #               COUNTYRACE = V5,
#   #               TYPE = V6,
#   #               AGE = V7,
#   #               A = V8,
#   #               B = V9,
#   #               C = V10,
#   #               Var1 = V2) %>%
#   mutate(STATE= substr(COUNTYRACE, 1,2),
#          COUNTY = substr(COUNTYRACE, 3,5),
#          GEOID = paste0(STATE, COUNTY),
#          A = as.numeric(A),
#          B = as.numeric(B),
#          C = as.numeric(C),
#          A = if_else(A<0, 0, A),
#          B = if_else(B<0, 0, B),
#          C = if_else(C<0,0, C),
#          RACE = substr(COUNTYRACE, 7,7))
# z[is.na(z)] <-0
# basesum <-  K05_launch[which( K05_launch$YEAR == launch_year),] %>%
#   dplyr::select(STATE, COUNTY, GEOID, POPULATION, RACE)
# 
# addsum <- z[which(z$TYPE=="ADD" & z$YEAR == (launch_year+FORLEN)),] %>%
#   group_by(STATE, COUNTY, GEOID, RACE, TYPE) %>%
#   dplyr::summarise(A = sum(A))
# 
# addmult <- left_join(addsum, basesum) %>%
#   mutate(COMBINED = if_else(A>= POPULATION, "ADD" ,"Mult")) %>%
#   dplyr::select(STATE, COUNTY, GEOID, RACE, COMBINED)
# 
# 
# basesum2 <-  K05_launch[which( K05_launch$YEAR == launch_year),] %>%
#   dplyr::select(STATE, COUNTY, GEOID, POPULATION, RACE) %>%
#   group_by(GEOID, RACE) %>%
#   dplyr::summarise(poptot = sum(POPULATION))
# 
# combined<- left_join(z, addmult) %>%
#   filter(TYPE == COMBINED) %>%
#   mutate(TYPE = "ADDMULT") %>%
#   dplyr::select(-COMBINED)
# 
# z2<- rbind(z, combined) %>%
#   dplyr::select(-V1)
# z2<-  left_join(as.data.frame(z2), as.data.frame(K05_launch2))
# z2<- left_join(z2, countynames)
# z2[is.na(z2)] <-0
# z2<- filter(z2,
#             !GEOID %in% c("02900", "04910", "15900", "35910", "36910", "51910", "51911","51911", "51913", "51914", "51915", "51916", "51918"))
# z3 <- filter(z2,
#              TYPE == "ADDMULT") %>%
#   distinct()
# 
# 
# totals <- z3 %>%
#   group_by(YEAR) %>%
#   dplyr::summarise(poptot = sum(A)) 
# 
# write_rds(totals, "./DATA-PROCESSED/uncontrolledprojections.RDS")
totals<- read_rds("./DATA-PROCESSED/uncontrolledprojections.RDS")
library(wpp2017)

data("popproj")
data("popproj80l")
data("popproj80u")

data("popproj95l")
data("popproj95u")

remake <- function(df, type){
  df <- filter(df, country_code == "840") %>%
    gather(YEAR, Pop, `2020`:`2100`) %>%
    mutate(YEAR = as.integer(YEAR))
 name <- paste0(type)
 df[[name]] <-df$Pop
 df <- dplyr::select(df, -Pop)
 return(df)
}

popproj <- remake(popproj, "median")
popproj80u <- remake(popproj80u, "u80")
popproj80l <- remake(popproj80l, "l80")
popproj95u <- remake(popproj95u, "u95")
popproj95l <- remake(popproj95l, "l95")

popproj <- left_join(popproj, popproj80l) %>%
  left_join(., popproj80u) %>%
  left_join(., popproj95u) %>%
  left_join(., popproj95l) %>%
  dplyr::select(-`2015`) 

ggplot() +
  # geom_ribbon(data=popproj, aes(x = YEAR, ymin = u80, ymax = l80, group=1, fill = "80th bound"), color = "gray", alpha =0.3) +
  # geom_ribbon(data=popproj, aes(x = YEAR, ymin = u95, ymax = l95, group=1, fill = "95th bound"), color = "gray", alpha =0.3) +
  # geom_line(data=popproj, aes(x = YEAR, y= median, group=1, color = "UN Median", linetype = "UN Median"), size = 1.5) +
  # geom_line(data=totals, aes(x= YEAR, y = poptot/1000, group=1, color = "Uncontrolled", linetype = "Uncontrolled"), linetype =2) +
  geom_line(data=SSPs2, aes(x =YEAR, y = Population, color = Scenario), size = 1) +
  scale_color_brewer(palette="Set2") +
  geom_line(data=totals, aes(x= YEAR, y = poptot/1000000, group=1, color = "Uncontrolled"), color = "black", linetype =2, size = 1.5) +
  # scale_color_manual(name='', values = "Uncontrolled")
  scale_y_continuous(label=comma) +
  theme_bw() +
  labs(x = "Year",
       y = "Population (millions)")