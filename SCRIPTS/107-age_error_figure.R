###------Age Error Figure -----
## @knitr age_error_figure


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
  group_by(AGE, TYPE) %>%
  dplyr::summarise(MAPE = quantile(abs(FLAG1), 0.5),
                   MALPE = quantile(FLAG1, 0.5),
                   in80percentile = sum(FLAG2)/length(FLAG2),
                   n = length(FLAG2)) %>%
  ungroup()%>%
  mutate(AGE = case_when(
    AGE == 1 ~ 0,
    AGE == 2 ~ 5,
    AGE == 3 ~ 10,
    AGE == 4 ~ 15,
    AGE == 5 ~ 20,
    AGE == 6 ~ 25,
    AGE == 7 ~ 30,
    AGE == 8 ~ 35,
    AGE == 9 ~ 40,
    AGE == 10 ~ 45,
    AGE == 11 ~ 50,
    AGE == 12 ~ 55,
    AGE == 13 ~ 60,
    AGE == 14 ~ 65,
    AGE == 15 ~ 70,
    AGE == 16 ~ 75,
    AGE == 17 ~ 80,
    AGE == 18 ~ 85
  ))

eval_ucm_agetotal2 <- z %>%
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
  group_by(AGE, TYPE) %>%
  dplyr::summarise(MAPE = quantile(abs(FLAG1), 0.5),
                   MALPE = quantile(FLAG1, 0.5),
                   in80percentile = sum(FLAG2)/length(FLAG2),
                   n = length(FLAG2)) %>%
  ungroup() %>%
  mutate(AGE = case_when(
    AGE == 1 ~ 0,
    AGE == 2 ~ 5,
    AGE == 3 ~ 10,
    AGE == 4 ~ 15,
    AGE == 5 ~ 20,
    AGE == 6 ~ 25,
    AGE == 7 ~ 30,
    AGE == 8 ~ 35,
    AGE == 9 ~ 40,
    AGE == 10 ~ 45,
    AGE == 11 ~ 50,
    AGE == 12 ~ 55,
    AGE == 13 ~ 60,
    AGE == 14 ~ 65,
    AGE == 15 ~ 70,
    AGE == 16 ~ 75,
    AGE == 17 ~ 80,
    AGE == 18 ~ 85
  ),
  TYPE = "RAKED CCD/CCR")

eval_ucm_agetotal <- rbind(eval_ucm_agetotal)

top<-ggplot(data=eval_ucm_agetotal, aes(group = TYPE)) +
  geom_line(aes(x=AGE, y =MALPE, linetype=TYPE, col=TYPE)) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = percent) +
  theme_bw() +
  labs(y = "Median ALPE")

bot<- ggplot(data=eval_ucm_agetotal, aes(group = TYPE)) +
  geom_line(aes(x=AGE, y =MAPE, linetype=TYPE, col=TYPE)) +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = percent) +
  theme_bw() +
  labs(y = "Median APE")

plot_grid(top, bot, ncol =1, labels="auto")