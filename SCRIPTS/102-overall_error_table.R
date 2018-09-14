###------Overall Error Table -----
## @knitr overall_table
 

eval_ucm_statetotal <- base_projunfitted %>%
  #mutate(RACE = substr(countyrace, 7,25))
  group_by(YEAR, TYPE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION),
                   A = sum(A),
                   B = sum(B),
                   C = sum(C),
                   num = length(A)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,(A/POPULATION)-1),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         PRED= prettyNum(A, big.mark=",", scientific=FALSE),
         LOW = prettyNum(B, big.mark=",", scientific=FALSE),
         HIGH = prettyNum(C, big.mark=",", scientific=FALSE),
         `% ERROR` = percent(abs(FLAG1), accuracy = 0.1),
         ALPE = percent(FLAG1),
         in80percentile = percent(FLAG2/num, accuracy = 0.1),
         POPULATION = prettyNum(POPULATION, big.mark=",", scientific=FALSE)) %>%
  dplyr::select(-num, -FLAG2, -in80percentile, -A, -B, -C, -FLAG1, -ALPE, -LOW, -HIGH) %>%
  filter(!TYPE == "BASE") %>%
  ungroup()


kable(eval_ucm_statetotal, 
      format='pandoc', 
      caption="\\label{tab:TOTALeval}**Evaluation of overall total errors for the entire United States.**")