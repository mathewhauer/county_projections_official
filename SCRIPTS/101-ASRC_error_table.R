###------ARSRC Error Table -----
## @knitr asrc_table


eval_ucm_cntytotal <- base_projunfitted %>%
  mutate(A= round(A, 6),
         FLAG1 = if_else(is.na(abs((A-POPULATION))/(A+POPULATION)), 0,abs((A-POPULATION))/(A+POPULATION)),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         ascry = paste0(AGE,SEX,COUNTYRACE,YEAR)) %>%
  group_by(YEAR, TYPE) %>%
  dplyr::summarise(SMAPE = quantile(abs(FLAG1), 0.5),
                   #MALPE = quantile(FLAG1, 0.5),
                   in80percentile = sum(FLAG2)/length(FLAG2),
                   num = length(FLAG2)) %>%
  mutate(SMAPE= percent(SMAPE, accuracy = 0.1),
         in80percentile = percent(in80percentile))

mapeeval<- eval_ucm_cntytotal %>%
  dplyr::select(YEAR, TYPE, SMAPE, num) %>%
  spread(YEAR, SMAPE) %>%
  mutate(EVAL = "Median SAPE")

table <- mapeeval %>%
  dplyr::select(TYPE, num, EVAL, "2005", "2010", "2015")

kable(table, format='pandoc', caption="**Evaluation of Age/Sex/Race/County joint Errors.**", digits = 1)