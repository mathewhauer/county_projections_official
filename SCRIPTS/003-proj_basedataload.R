###------DATA LOAD-----
## @knitr projbasedata


# Setting the groupings
GROUPING <- c("STATE", "COUNTY", "YEAR", "AGE", "RACE", "SEX")

# TEST YEAR IS SET TO 2015
test_year = 2015

# LAUNCH YEAR IS THE SAME AS THE TEST YEAR
launch_year = test_year
# THE NUMBER OF AGE GROUPS
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-17
# FORECAST LENGTH. SINCE THE PROJECTION INTERVAL IS 5 YEARS IT IS (STEPS*5)

FORLEN<-(STEPS*5)

years <- 0
years$YEAR <- seq(launch_year+5,launch_year+(STEPS*5), 5)
years$YEAR <- seq(launch_year+1,launch_year+STEPS,1)

##############################################################
# #
# # DOWNLOADING THE CDC POPULATION ESTIMATES FOR 1969-2016.
# #
# # IF RUNNING THIS SCRIPT FOR THE FIRST LINE, Run the download.file line and the gunzip line.
# #
# download.file("https://seer.cancer.gov/popdata/yr1990_2016.19ages/us.1990_2016.19ages.adjusted.txt.gz", "DATA/us.1990_2016.19ages.adjusted.txt.gz")
# # UNZIPPING THE DATA FILE
# gunzip("DATA/us.1990_2016.19ages.adjusted.txt.gz", overwrite = TRUE, remove = TRUE)
# #
###################################################################

# READING THE cdc DATA INTO R. THE DATA ARE IN A SINGLE COLUMN FORMAT AND SO THEY MUST BE BROKEN APART.
K05_pop<- read.table("DATA/us.1990_2016.19ages.adjusted.txt") 
K05_pop$V1 <- as.character(K05_pop$V1) # SETTING THE ENTIRE SINGLE VARIABLE INTO A CHARACTER
K05_pop$YEAR <- as.numeric(substr(K05_pop$V1,1,4)) # SEPARATING THE YEAR AND SETTING IT AS A NUMBER
K05_pop$STATEID <- substr(K05_pop$V1, 5,6) # SEPARATING THE 2 CHARACTER STATE ABBREVIATION
K05_pop$STATE <- substr(K05_pop$V1, 7,8) # SEPARATING THE 2-DIGIT STATE CODE
K05_pop$COUNTY <- substr(K05_pop$V1,9,11) # SEPARATING THE 3-DIGIT COUNTY CODE
K05_pop$REGISTRY <- substr(K05_pop$V1, 12,12) # REGISTRY IS A THROW AWAY VARIABLE REFERING TO ODD GEOGRAPHIES
K05_pop$RACE <- substr(K05_pop$V1, 14,14) # SEPARATING OUT THE RACE CODES.
K05_pop$RACE <- ifelse(K05_pop$RACE == "3", "4", K05_pop$RACE) # CHANGING OTHER CODE FROM 3 TO 4.
K05_pop$ORIGIN <- substr(K05_pop$V1, 15,15) # SEPARATING OUT HISPANIC ORIGIN.
K05_pop$RACE <- ifelse(K05_pop$ORIGIN == "1", "3", K05_pop$RACE) # IF AN ORIGIN IS HISPANIC, CHANGING ITS RACE CODE TO 3.
K05_pop$SEX <- substr(K05_pop$V1, 16,16) # SEPARATING OUT THE SEX DATA

# SEPARATING OUT AGE CATEGORIES. THE CDC DATA CONSISTS OF 19 AGE GROUPS WHERE "00" IS CODED AS 0 YEAR OLDS AND "01" IS CODED AS 1-4 YEAR OLDS.
# I RECODE 00 TO 01 TO CREATE A 0-4 YEAR OLD AGE GROUP.
K05_pop$AGE <- as.numeric(if_else(substr(K05_pop$V1, 17, 18) == "00","01",substr(K05_pop$V1, 17, 18)))
K05_pop$POPULATION <- as.numeric(substr(K05_pop$V1, 19, 30)) # SEPARATING THE ACTUAL POPULATION ESTIMATES.

# THE DATA NEED TO BE AGGREGATED TO THE LEVEL OF ANALYSIS BASED ON THE GROUPING FROM ABOVE. THIS IS TO SUM THE 0 AND 1-4 AGE GROUPS
# INTO THE 0-4 AGE GROUP
K05_pop <- K05_pop %>%
  group_by(.dots = GROUPING) %>%
  dplyr::summarise(POPULATION = sum(POPULATION))

K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY) # SETTING THE 5-DIGIT FIPS CODE
K05_pop$COUNTYRACE <- paste0(K05_pop$GEOID, "_", K05_pop$RACE) # CREATING A UNIQUE VARIABLE THAT IS 0000_1 FOR EACH COUNTY-RACE COMBINATION

# SEPARATING OUT THE LAUNCH POPULATION AND SUMMING TO THE COUNTY TOTAL.
K05_launch <- K05_pop[which(K05_pop$YEAR == launch_year),] %>%
  group_by(STATE, COUNTY, GEOID, YEAR) %>%
  dplyr::summarise(POPULATION = sum(POPULATION)) %>%
  ungroup()

