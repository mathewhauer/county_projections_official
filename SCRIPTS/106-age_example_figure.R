###------Age Example Figure -----
## @knitr age_example_figure


collegecampuses<- base_projunfitted[which(base_projunfitted$GEOID %in% c("13059","36109", "42027")),]
collegecampuses <- collegecampuses %>%
  group_by(GEOID, NAME, state, AGE, TYPE, YEAR) %>%
  dplyr::summarise(A = sum(A),
                   POPULATION=sum(POPULATION)) %>%
  ungroup()%>%
  mutate(CNTYNAME = paste0(NAME, ", ",state),
         AGE = AGE*5-5) %>%
  filter(YEAR %in% c(2005,2015)) %>%
  mutate(YEAR = as.factor(YEAR))
suburban<- base_projunfitted[which(base_projunfitted$GEOID %in% c("18057","47187", "13117")),]
suburban <- suburban %>%
  group_by(GEOID, NAME, state, AGE, TYPE, YEAR) %>%
  dplyr::summarise(A = sum(A),
                   POPULATION=sum(POPULATION)) %>%
  ungroup()%>%
  mutate(CNTYNAME = paste0(NAME, ", ",state),
         AGE = AGE*5-5) %>%
  filter(YEAR %in% c(2005,2015)) %>%
  mutate(YEAR = as.factor(YEAR))

retirement<- base_projunfitted[which(base_projunfitted$GEOID %in% c("26089","05005", "37175")),]
retirement <- retirement %>%
  group_by(GEOID, NAME, state, AGE, TYPE, YEAR) %>%
  dplyr::summarise(A = sum(A),
                   POPULATION=sum(POPULATION)) %>%
  ungroup()%>%
  mutate(CNTYNAME = paste0(NAME, ", ",state),
         AGE = AGE*5-5) %>%
  filter(YEAR %in% c(2005,2015)) %>%
  mutate(YEAR = as.factor(YEAR))

largecity <- base_projunfitted[which(base_projunfitted$GEOID %in% c("48201","36047", "06073")),]
largecity <- largecity %>%
  group_by(GEOID, NAME, state, AGE, TYPE, YEAR) %>%
  dplyr::summarise(A = sum(A),
                   POPULATION=sum(POPULATION)) %>%
  ungroup()%>%
  mutate(CNTYNAME = paste0(NAME, ", ",state),
         AGE = AGE*5-5) %>%
  filter(YEAR %in% c(2005,2015)) %>%
  mutate(YEAR = as.factor(YEAR))


figure_age = function(x, title){
  a<-ggplot(x) +
    geom_line(data = x[which(x$TYPE =="CCD/CCR"),], aes(x = AGE, y=A, colour = YEAR, linetype= "dotted")) +
    geom_line(data = x[which(x$TYPE =="CCD/CCR"),], aes(x = AGE, y=POPULATION, colour = YEAR, linetype= "solid")) +
    theme_bw() +
    scale_color_manual(labels = c("2005", "2015"), values = c("red", "blue")) +
    scale_linetype_manual(name = "",labels = c("CCD/CCR", "Observed"), values=c("dashed", "solid"))+
    labs(x = "Age Group",
         y = "Population",
         title = paste0(title)) +
    facet_grid(. ~ CNTYNAME, scales = "fixed")
  return(a)
}
campuses<- figure_age(collegecampuses, "Counties with large college campuses")
suburbs<- figure_age(suburban, "Suburban counties")
retirements<-   figure_age(retirement, "Retirement counties")
largecities<- figure_age(largecity, "Large Cities")

prow <- plot_grid(campuses + theme(legend.position="none"),
                  suburbs + theme(legend.position="none"),
                  retirements + theme(legend.position="none"),
                  largecities + theme(legend.position="none"),
                  labels ="auto", ncol = 1
)
legend <- get_legend(campuses)
plot_grid(prow, legend, rel_widths = c(2.5, 0.5))