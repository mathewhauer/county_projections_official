
set.seed(100)

source('./SCRIPTS/000-Libraries.R')      # loading in the libraries
source('./SCRIPTS/001-fipscodes.R')
source('./SCRIPTS/002-basedataload.R')
Klaunch <- K05_pop[which(K05_pop$YEAR==launch_year),]
statelist <- unique(Klaunch$STATE)

# gq_2000.csv IS THE RESULT OF 
gqpop <- read.csv("DATA-PROCESSED/gq_2000.csv") %>%
  mutate(STATE = str_pad(STATE, 2, pad = "0"),
         COUNTY = str_pad(COUNTY, 3, pad="0"),
         GEOID = paste0(STATE, COUNTY),
         AGE = AGEGRP,
         SEX = case_when(
           SEX == "MALE" ~ "1",
           SEX == "FEMALE" ~ "2"),
         RACE = case_when(
           RACE == "BLACK, NH" ~ "2",
           RACE == "OTHER" ~ "3",
           RACE == "WHITE, NH" ~ "1",
           RACE == "HISPANIC" ~ "3",
           RACE == "OTHER, NH" ~ "3")) %>%
  group_by(.dots = GROUPING) %>%
  dplyr::summarise(group_quarters = sum(GQ, na.rm=T))

gqpop$GEOID <- paste0(gqpop$STATE, gqpop$COUNTY)
gqpop$COUNTYRACE <- paste0(gqpop$GEOID, "_", gqpop$RACE)

stateferts <- read_csv("DATA-PROCESSED/state-level-fert-rates_20002015.csv")

samp <- unique(Klaunch$COUNTYRACE)
# samp <- unique(K05_pop$COUNTYRACE[which(K05_pop$STATE == 15)])
# samp <- "13059_1"
x = unlist(list(paste0(samp)))

project = function(x){
  tryCatch({#print(x)
    ###   Prediction of the CCR function
    predccr = function(ccr, sex, x, DF){
       y <- as_data_frame(DF[[as.character(ccr)]][which(DF$COUNTYRACE== x & DF$SEX == sex )])
       num<- seq(1,FORLEN,5)
      # pred<- tryCatch(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),]
      #                 , error=function(e) array(0, c(STEPS)))
              pred<- tryCatch(forecast(arima(y$value, order =arima_order), h= FORLEN)$mean[c(num)]
                              , error=function(e) array(0, c(STEPS)))
      return(pred)

    }

    ###################
    ### DATA PREP
    ##################
    ### Filtering the Census data based on the county/race combination
    K05 <- K05_pop[which(K05_pop$COUNTYRACE == x),] %>%
      group_by(YEAR,  STATE, COUNTY, RACE, SEX, AGE, COUNTYRACE) %>%
      dplyr::summarise(POPULATION = sum(POPULATION)) %>%
      ungroup()
   
    ### Calculating the cohort-change differences (CCDs)
    CCDs<- K05 %>%
      ungroup() %>%
      mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
             GEOID = paste0(STATE, COUNTY),
             POPULATION = as.numeric(POPULATION)) %>%
      spread(AGE, POPULATION)
    if(is.null(CCDs$X01)){CCDs$X01=0}else{CCDs$X01=CCDs$X01}
    if(is.null(CCDs$X02)){CCDs$X02=0}else{CCDs$X02=CCDs$X02}
    if(is.null(CCDs$X03)){CCDs$X03=0}else{CCDs$X03=CCDs$X03}
    if(is.null(CCDs$X04)){CCDs$X04=0}else{CCDs$X04=CCDs$X04}
    if(is.null(CCDs$X05)){CCDs$X05=0}else{CCDs$X05=CCDs$X05}
    if(is.null(CCDs$X06)){CCDs$X06=0}else{CCDs$X06=CCDs$X06}
    if(is.null(CCDs$X07)){CCDs$X07=0}else{CCDs$X07=CCDs$X07}
    if(is.null(CCDs$X08)){CCDs$X08=0}else{CCDs$X08=CCDs$X08}
    if(is.null(CCDs$X09)){CCDs$X09=0}else{CCDs$X09=CCDs$X09}
    if(is.null(CCDs$X10)){CCDs$X10=0}else{CCDs$X10=CCDs$X10}
    if(is.null(CCDs$X11)){CCDs$X11=0}else{CCDs$X11=CCDs$X11}
    if(is.null(CCDs$X12)){CCDs$X12=0}else{CCDs$X12=CCDs$X12}
    if(is.null(CCDs$X13)){CCDs$X13=0}else{CCDs$X13=CCDs$X13}
    if(is.null(CCDs$X14)){CCDs$X14=0}else{CCDs$X14=CCDs$X14}
    if(is.null(CCDs$X15)){CCDs$X15=0}else{CCDs$X15=CCDs$X15}
    if(is.null(CCDs$X16)){CCDs$X16=0}else{CCDs$X16=CCDs$X16}
    if(is.null(CCDs$X17)){CCDs$X17=0}else{CCDs$X17=CCDs$X17}
    if(is.null(CCDs$X18)){CCDs$X18=0}else{CCDs$X18=CCDs$X18}
    CCDs<- CCDs %>%
      arrange(GEOID, SEX, YEAR) %>%
      mutate(ccr1 = X02 - lag(X01, 5),
             ccr2 = X03 - lag(X02, 5),
             ccr3 = X04 - lag(X03, 5),
             ccr4 = X05 - lag(X04, 5),
             ccr5 = X06 - lag(X05, 5),
             ccr6 = X07 - lag(X06, 5),
             ccr7 = X08 - lag(X07, 5),
             ccr8 = X09 - lag(X08, 5),
             ccr9 = X10 - lag(X09, 5),
             ccr10 = X11 - lag(X10, 5),
             ccr11 = X12 - lag(X11, 5),
             ccr12 = X13 - lag(X12, 5),
             ccr13 = X14 - lag(X13, 5),
             ccr14 = X15 - lag(X14, 5),
             ccr15 = X16 - lag(X15, 5),
             ccr16 = X17 - lag(X16, 5),
             ccr17 = X18 - (lag(X17, 5) + lag(X18, 5))) %>%
      filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)
    
    ### Calculating the CCRs
    CCRs<- K05 %>%
      ungroup() %>%
      mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
             GEOID = paste0(STATE, COUNTY),
             POPULATION = as.numeric(POPULATION)) %>%
      spread(AGE, POPULATION)
    if(is.null(CCRs$X01)){CCRs$X01=0}else{CCRs$X01=CCRs$X01}
    if(is.null(CCRs$X02)){CCRs$X02=0}else{CCRs$X02=CCRs$X02}
    if(is.null(CCRs$X03)){CCRs$X03=0}else{CCRs$X03=CCRs$X03}
    if(is.null(CCRs$X04)){CCRs$X04=0}else{CCRs$X04=CCRs$X04}
    if(is.null(CCRs$X05)){CCRs$X05=0}else{CCRs$X05=CCRs$X05}
    if(is.null(CCRs$X06)){CCRs$X06=0}else{CCRs$X06=CCRs$X06}
    if(is.null(CCRs$X07)){CCRs$X07=0}else{CCRs$X07=CCRs$X07}
    if(is.null(CCRs$X08)){CCRs$X08=0}else{CCRs$X08=CCRs$X08}
    if(is.null(CCRs$X09)){CCRs$X09=0}else{CCRs$X09=CCRs$X09}
    if(is.null(CCRs$X10)){CCRs$X10=0}else{CCRs$X10=CCRs$X10}
    if(is.null(CCRs$X11)){CCRs$X11=0}else{CCRs$X11=CCRs$X11}
    if(is.null(CCRs$X12)){CCRs$X12=0}else{CCRs$X12=CCRs$X12}
    if(is.null(CCRs$X13)){CCRs$X13=0}else{CCRs$X13=CCRs$X13}
    if(is.null(CCRs$X14)){CCRs$X14=0}else{CCRs$X14=CCRs$X14}
    if(is.null(CCRs$X15)){CCRs$X15=0}else{CCRs$X15=CCRs$X15}
    if(is.null(CCRs$X16)){CCRs$X16=0}else{CCRs$X16=CCRs$X16}
    if(is.null(CCRs$X17)){CCRs$X17=0}else{CCRs$X17=CCRs$X17}
    if(is.null(CCRs$X18)){CCRs$X18=0}else{CCRs$X18=CCRs$X18}
    CCRs<- CCRs %>%
      arrange(GEOID, SEX, YEAR) %>%
      mutate(ccr1 = X02 / lag(X01, 5),
             ccr2 = X03 / lag(X02, 5),
             ccr3 = X04 / lag(X03, 5),
             ccr4 = X05 / lag(X04, 5),
             ccr5 = X06 / lag(X05, 5),
             ccr6 = X07 / lag(X06, 5),
             ccr7 = X08 / lag(X07, 5),
             ccr8 = X09 / lag(X08, 5),
             ccr9 = X10 / lag(X09, 5),
             ccr10 = X11 / lag(X10, 5),
             ccr11 = X12 / lag(X11, 5),
             ccr12 = X13 / lag(X12, 5),
             ccr13 = X14 / lag(X13, 5),
             ccr14 = X15 / lag(X14, 5),
             ccr15 = X16 / lag(X15, 5),
             ccr16 = X17 / lag(X16, 5),
             ccr17 = X18 / (lag(X17, 5) + lag(X18, 5))) %>%
      filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)

    CCRs[mapply(is.infinite, CCRs)] <- NA
    CCRs[mapply(is.nan, CCRs)] <- NA
    CCRs[is.na(CCRs)] <-0
    CCDs[mapply(is.infinite, CCDs)] <- NA
    CCDs[mapply(is.nan, CCDs)] <- NA
    CCDs[is.na(CCDs)] <-0
    ##################################################
    ### Start of the Additive projections
    ##################################################
    
    ###  Calculating the UCM's of the CCD's for each age/sex group. The confidence interval is set to 80% (1.28*SD) 
    for (i in 1:(SIZE-1)){
      data_tablef <- cbind(predccr(paste0("ccr",i), "2", x, CCDs),0,0)
      data_tablem <- cbind(predccr(paste0("ccr",i), "1", x, CCDs),0,0)
      errf <- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "2")])*1.28
      errm <- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "1")])*1.28
      data_tablef[,2]<- data_tablef[,1]- ifelse(is.na(errf),0, errf)
      data_tablef[,3]<- data_tablef[,1]+ ifelse(is.na(errf),0, errf)
      data_tablem[,2]<- data_tablem[,1]- ifelse(is.na(errm),0, errm)
      data_tablem[,3]<- data_tablem[,1]+ ifelse(is.na(errm),0, errm)
      assign(paste0("BA",i,"f"), data_tablef[1:STEPS,1:3])
      assign(paste0("BA",i,"m"), data_tablem[1:STEPS,1:3])
      rm(data_tablef, data_tablem, errf, errm)
    }
    ### "Stacking" the CCDs into a single vector with a high/medium/low  
    for (i in 1:STEPS){
      namm<-paste0("lx", i, "m")
      namf<-paste0("lx",i,"f")
      assign(namm, rbind(BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
                         , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,], BA17m[i,]))
      assign(namf, rbind(BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA9f[i,], BA10f[i,]
                         , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,], BA17f[i,]))}
    ###   Placing the CCD's into the subdiagonal of a leslie matrix.
    for (i in 1:STEPS){
      data_tablef <- get(paste0("lx",i,"f"))
      weird_dataf <- array(0,c(SIZE,SIZE,ncol(data_tablef)))
      data_tablem <- get(paste0("lx",i,"m"))
      weird_datam <- array(0,c(SIZE,SIZE,ncol(data_tablem)))
      for(j in 1:ncol(data_tablef)){
        weird_dataf[,,j] <- rbind(0,cbind(diag(data_tablef[,j]),0))
        weird_datam[,,j] <- rbind(0,cbind(diag(data_tablem[,j]),0))
        assign(paste0("S",i,"m"), weird_datam)
        assign(paste0("S",i,"f"), weird_dataf)
      }
      rm(data_tablef)
      rm(weird_dataf)
    }
    ### Formatting the base POPULATION data as equal to the total POPULATION minus the group quaters.
    popf <- array(0, c(SIZE))
    for(i in 1:SIZE){    popf[i] <- ifelse(length(K05$POPULATION[which(K05$SEX == "2" & K05$YEAR == launch_year & K05$AGE == i)])==0,
                                           0,
                                           K05$POPULATION[which(K05$SEX == "2" & K05$YEAR == launch_year & K05$AGE == i)])}
    gqf <-  if (length(gqpop$group_quarters[which(gqpop$SEX == "2" & gqpop$COUNTYRACE == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "2" & gqpop$COUNTYRACE == x)]} else {
        0}
    popf <- popf - gqf
    
    popm <- array(0, c(SIZE))
    for(i in 1:SIZE){popm[i] <- ifelse(length(K05$POPULATION[which(K05$SEX == "1" & K05$YEAR == launch_year & K05$AGE == i)])==0,
                                       0,
                                       K05$POPULATION[which(K05$SEX == "1" & K05$YEAR == launch_year & K05$AGE == i)])}
    gqm <- if (length(gqpop$group_quarters[which(gqpop$SEX == "1" & gqpop$COUNTYRACE == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "1" & gqpop$COUNTYRACE == x)]} else {
        0}
    popm <- popm - gqm 
    p0f <-array(0,c(SIZE,SIZE,ncol(lx1f)))
    p0m <-array(0,c(SIZE,SIZE,ncol(lx1f)))
    for (i in 1:ncol(lx1f)){
      p0f[,,i] <-  rbind(0,cbind(diag(popf),0))[1:18,1:18]
      p0f[18,18,i] = popf[18]
      p0m[,,i] <-  rbind(0,cbind(diag(popm),0))[1:18,1:18]
      p0m[18,18,i] = popm[18]
    }  
    ### Calculating the forecasted CWR's from the UCMs. Confidence interval is set at 80% (1.28*SD) 
    n02 <- filter(stateferts, STATE == substr(x, 1,2), RACE == substr(x,7,8)) %>%
      dplyr::select(value)
    n01 <- array(0,c(STEPS,ncol(lx1f)))
    n01[,1] <-n02$value[1:STEPS]
    
    ### PROJECTION ITSELF ###
    
    # Actually projecting with the additive model
    for (i in 1:STEPS){
      data_tablef <- get(paste0("S",i,"f"))
      data_tablem <- get(paste0("S",i,"m"))
      projm<-projf <- array(0,c(SIZE,ncol(lx1f)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lx1f)){  
        projf[,j] <- rowSums(data_tablef[,,j] + popdatf[,,j])
        projm[,j] <- rowSums(data_tablem[,,j] + popdatm[,,j])
        projf[1,j] <- (n01[i,j] * sum(projf[4:10,j]))*.487
        projm[1,j] <- (n01[i,j] * sum(projf[4:10,j]))*.512
        popdatf[,,j] <-rbind(0,cbind(diag(projf[,j]),0))[1:18,1:18]
        popdatm[,,j] <-rbind(0,cbind(diag(projm[,j]),0))[1:18,1:18]
        popdatf[18,18,j] <- projf[18,j]
        popdatm[18,18,j] <- projm[18,j]
        assign(paste0("p",i,"f"), popdatf)
        assign(paste0("p",i,"m"), popdatm)
        assign(paste0("proj",i,"f"),projf)
        assign(paste0("proj",i,"m"), projm)
      }
      rm(data_tablef, data_tablem, projf, projm, popdatf, popdatm)
    }
    ### Collecting the additive projections together.
    projm<-NULL
    projf<-NULL
    for (i in 1:STEPS){
      data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")) + gqm)
      data_tablem$YEAR <- launch_year+ (i*5)
      data_tablem$SEX <- "1"
      data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")) + gqf)
      data_tablef$YEAR <- launch_year+ (i*5)
      data_tablef$SEX <- "2"
      projm <- rbind(projm, data_tablem)
      projf <-rbind(projf, data_tablef)
      namm<- get(paste0("proj",i,"m"))
      rm(data_tablem)
    }
    ### Declaring several variables
    projadd <-rbind(projm, projf) %>%
      dplyr::rename(Scenario = Var2)
    projadd$COUNTYRACE <-x
    projadd$TYPE<- "ADD"
    
    ######################################
    ### PROJECTING THE CCRs
    
    ### Calculating the CCR UCMs for each individual age group
    for (i in 1:(SIZE-1)){
      data_tablef <- cbind(predccr(paste0("ccr",i), "2", x, CCRs),0,0)
      data_tablem <- cbind(predccr(paste0("ccr",i), "1", x, CCRs),0,0)
      errf <- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "2")])*1.28
      errm <- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "1")])*1.28
      data_tablef[,2]<- data_tablef[,1]- ifelse(is.na(errf),0, errf)
      data_tablef[,3]<- data_tablef[,1]+ ifelse(is.na(errf),0, errf)
      data_tablem[,2]<- data_tablem[,1]- ifelse(is.na(errm),0, errm)
      data_tablem[,3]<- data_tablem[,1]+ ifelse(is.na(errm),0, errm)
      assign(paste0("BA",i,"f"), data_tablef[1:STEPS,1:3])
      assign(paste0("BA",i,"m"), data_tablem[1:STEPS,1:3])
      rm(data_tablef, data_tablem, errf, errm)
    }
    ### Stacking the forecasted CCRs into single vectors.
    for (i in 1:STEPS){
      namm<-paste0("lx", i, "m")
      namf<-paste0("lx",i,"f")
      assign(namm, rbind(BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
                         , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,], BA17m[i,]))
      assign(namf, rbind(BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA9f[i,], BA10f[i,]
                         , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,], BA17f[i,]))}
    ### Setting the sub-diagonal of a leslie matrix as equal to the projected CCRs
    for (i in 1:STEPS){
      data_tablef <- get(paste0("lx",i,"f"))
      data_tablem <- get(paste0("lx",i,"m"))
      weird_dataf <- array(0,c(SIZE,SIZE,ncol(data_tablef)))
      weird_datam <- array(0,c(SIZE,SIZE,ncol(data_tablem)))
      for(j in 1:ncol(data_tablef)){  
        weird_dataf[,,j] <- rbind(0,cbind(diag(data_tablef[,j]),0))
        weird_dataf[18,18,j]=data_tablef[17,j]
        weird_datam[,,j] <- rbind(0,cbind(diag(data_tablem[,j]),0))
        weird_datam[18,18,j]=data_tablem[17,j]
        assign(paste0("S",i,"f"), weird_dataf)
        assign(paste0("S",i,"m"), weird_datam)
      }
      rm(data_tablef, data_tablem, weird_dataf, weird_datam)
    }
    ### Formatting the base POPULATION data.
    p0f <-array(0,c(SIZE,1,ncol(lx1f)))
    p0m <-array(0,c(SIZE,1,ncol(lx1f)))
    for (i in 1:ncol(lx1f)){
      p0f[,,i] <-  cbind(popf)
      p0m[,,i] <-  cbind(popm)
      
    }  
    ### PROJECTING THE CCRs
    for (i in 1:STEPS){
      data_tablef <- get(paste0("S",i,"f"))
      data_tablem <- get(paste0("S",i,"m"))
      projm<-projf <- array(0,c(SIZE,1,ncol(lx1f)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lx1f)){  
        projf[,,j] <- data_tablef[,,j] %*% popdatf[,,j]
        projm[,,j] <- data_tablem[,,j] %*%  popdatm[,,j]
        projf[1,,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.487
        projm[1,,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.512
        assign(paste0("p",i,"f"), projf)
        assign(paste0("p",i,"m"), projm)
        assign(paste0("proj",i,"f"), projf)
        assign(paste0("proj",i,"m"), projm)
      }
    }
    ### Collecting the projection results
    projm<-NULL
    projf<-NULL
    for (i in 1:STEPS){
      data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")) + gqm)
      data_tablem$YEAR <- launch_year+ (i*5)
      data_tablem$SEX <- "1"
      data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")) + gqf)
      data_tablef$YEAR <- launch_year+ (i*5)
      data_tablef$SEX <- "2"
            projm <- rbind(projm, data_tablem)
      projf <-rbind(projf, data_tablef)
      namm<- get(paste0("proj",i,"m"))
      rm(data_tablem)
    }
    
    projmult <-rbind(projm, projf) %>%
      dplyr::select(-Var2) %>%
      dplyr::rename(Scenario = Var3)
    projmult$COUNTYRACE <-x
    projmult$TYPE<- "Mult"
   
    # Collecting all projections together
    proj <-rbind(projadd, projmult) #%>%
    
    return(proj)
  }
  , error=function(e){cat(x," ERROR :",conditionMessage(e), "\n")})
}

# for(this.state in stateid){
#   x = unlist(list(unique(K05_pop$COUNTYRACE[which(K05_pop$STATE==this.state)])))
#   KT = rbindlist(pbmclapply(x, project, mc.cores = detectCores()-1))
#   KT2 <- KT %>%
#     mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
#     group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
#     spread(Scenario, Freq)
#   write.table(KT2, paste0("PROJECTIONS/EVAL/COUNTY_20002015_",this.state,".csv"))
# }

pckgs <- c("data.table", "doParallel", "foreach", "tidyverse", "rucm", "forecast")
(start.time <- Sys.time())

foreach(i = 1:length(stateid), 
        .combine = rbind, 
        .errorhandling = "stop", 
        .packages = pckgs) %dopar% {
        
          x = unlist(list(unique(K05_pop$COUNTYRACE[which(K05_pop$STATE==stateid[i])])))
          KT = rbindlist(lapply(x, project))
          
          KT2 <- KT %>%
            mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
            group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
            spread(Scenario, Freq)
          
          write.table(KT2, paste0("PROJECTIONS/EVAL/COUNTY_20002015_",stateid[i],".csv"))
          
        }

