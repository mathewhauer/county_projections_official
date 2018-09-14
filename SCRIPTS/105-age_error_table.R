###------Age Error Table -----
## @knitr age_error_table



eval_ucm_agetotal <- base_projunfitted %>%
  filter(!TYPE == "BASE") %>%
  group_by(STATE, COUNTY, AGE, YEAR, TYPE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION),
                   A = sum(A),
                   B = sum(B),
                   C = sum(C),
                   num = length(A)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,(A/POPULATION)-1),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         in90percentile = FLAG2/num) %>%
  ungroup() %>%
  group_by(YEAR, TYPE) %>%
  dplyr::summarise(MAPE = quantile(abs(FLAG1), 0.5),
                   MALPE = quantile(FLAG1, 0.5),
                   in80percentile = sum(FLAG2)/length(FLAG2),
                   n = length(FLAG2)) %>%
  # COUNTYnum = length(FLAG2)) %>%
  mutate(MAPE= percent(MAPE, accuracy=0.1),
         MALPE = percent(MALPE, accuracy=0.1),
         in80percentile = percent(in80percentile))
mapeeval<- eval_ucm_agetotal %>%
  dplyr::select(YEAR, TYPE, MAPE, n) %>%
  spread(YEAR, MAPE) %>%
  mutate(EVAL = "Median APE")
malpeeval <- eval_ucm_agetotal %>%
  dplyr::select(YEAR, TYPE, MALPE, n) %>%
  spread(YEAR, MALPE) %>%
  mutate(EVAL = "Median ALPE")
# eval80<- eval_ucm_agetotal %>%
#   dplyr::select(YEAR, TYPE, in80percentile, n) %>%
#   spread(YEAR, in80percentile) %>%
#   mutate(EVAL = "In 80th percentile")

table <- rbind(mapeeval, malpeeval) %>%
  # rbind(., eval80) %>%
  dplyr::select(TYPE, n, EVAL, "2005", "2010", "2015")

kable(table, format='pandoc', caption="\\label{tab:agestotal}**Evaluation of Age Group Errors.**", digits = 4)