###------SSP Explanation Figure -----
## @knitr SSP_figure


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
  group_by(SSP, YEAR) %>%
  dplyr::summarise(Population = sum(Population)) %>%
  ungroup() %>%
  spread(SSP, Population) %>%
  mutate(YEAR = as.integer(YEAR),
         # SEX = as.character(SEX))
  ) %>%
  gather(Scenario, Population, SSP1:SSP5)


fig<-ggplot() + 
  geom_blank() + 
  xlim(0, 10) + 
  ylim(0, 10) +
  annotate("text", x= 2, y = 2, label="SSP1: \n Sustainability") + 
  annotate("text", x= 5, y = 5, label="SSP2:\n Middle of the Road") + 
  annotate("text", x= 8, y =8, label="SSP3:\n Regional Rivalry") + 
  annotate("text", x= 2, y = 8, label="SSP5: \nFossil-fueled \nDevelopment") + 
  annotate("text", x= 8, y = 2, label="SSP4:\n Inequality") + 
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y  = element_line(arrow = arrow()),
        axis.line.x = element_line(arrow = arrow())) +
  labs(x ="Barriers to Adaptation", 
       y = "Barriers to Mitigation") 


SSPfig<- ggplot(data = SSPs2) +
  geom_line( aes(x = YEAR, y = Population, colour = Scenario), size = 1.5) +
  scale_color_brewer(palette="Set2") +
  labs(x = "Year",
       y = "Population (millions)",
       title = "US Population") +
  theme_bw() +
theme(legend.position = c(0.2, 0.75))

plot_grid(fig, SSPfig, ncol=2, labels = "auto")