###------Race Error Figure -----
## @knitr race_error_figure

eval_ucm_agetotal <- base_projunfitted %>%
  filter(!TYPE == "BASE") %>%
  group_by(STATE, COUNTY, RACE, YEAR, TYPE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION),
                   A = sum(A),
                   B = sum(B),
                   C = sum(C),
                   num = length(A)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,(A/POPULATION)-1),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         in90percentile = FLAG2/num) %>%
  ungroup()
figure_race = function(x){
  a<-ggplot() +
    geom_density(data=x[which(x$YEAR == 2015),], aes(x=FLAG1, colour = RACE, fill = RACE), adjust=1.5, lwd=1, alpha=.20) +
    scale_color_manual(name="Race",labels = c("White", "Black", "Other"), values = c("red", "Green", "Blue")) +
    geom_vline(xintercept=0) +
    theme_bw() +
    guides(fill=FALSE) +
    #scale_color_manual(name="TEST",labels = c("White", "Black", "Other"), values = c("red", "blue", "Green")) +
    xlim(-0.5,0.5) + 
    labs(x='Algebraic Percent(error)',
         y='Density') +
    facet_grid(. ~ TYPE, scales = "fixed")
  return(a)
}
figure_race2 = function(x){
  a<-ggplot() +
    geom_density(data=x[which(x$YEAR == 2015),], aes(x=abs(FLAG1), colour = RACE, fill = RACE), adjust=1.5, lwd=1, alpha=.20) +
    scale_color_manual(name="Race",labels = c("White", "Black", "Other"), values = c("red", "Green", "Blue")) +
    geom_vline(xintercept=0) +
    theme_bw() +
    guides(fill=FALSE) +
    #scale_color_manual(name="TEST",labels = c("White", "Black", "Other"), values = c("red", "blue", "Green")) +
    xlim(0,0.5) + 
    labs(x='Absolute Percent(error)',
         y='Density') +
    facet_grid(. ~ TYPE, scales = "fixed")
  return(a)
}
panel1 <- figure_race(eval_ucm_agetotal)
panel2 <- figure_race2(eval_ucm_agetotal)

prow <- plot_grid(panel1 + theme(legend.position="none"),
                  panel2 + theme(legend.position="none"),
                  labels ="auto", ncol = 1
)
legend <- get_legend(panel1)
plot_grid(prow, legend, rel_widths = c(2.5, 0.5))