###------Proj Comparisons Figure -----
## @knitr proj_comparison_figure

test2 <- read_csv("DATA-PROCESSED/ssp_sums.csv")
tx <- read_csv("DATA-PROCESSED/texas_proj.csv")
mn <- read_csv("DATA-PROCESSED/minn_proj.csv")
ca <- read_csv("DATA-PROCESSED/ca_proj.csv")
az <- read_csv("DATA-PROCESSED/az_proj.csv")


label = textGrob(label = "Vintage 2014 Texas Projection \n Texas Demographic Center", x = 0.6, y = 0.1)

tx_fig <- ggplot() +
  geom_line(data=filter(test2, STATE==48), aes(x=YEAR, y = Population, color = Scenario), size = 1.5) +
  scale_color_brewer(palette="Set2") +
  geom_ribbon(data= tx, aes(x=YEAR, ymin=mig0, ymax=mig10), fill = "gray", alpha=0.5) +
  geom_line(data = tx, aes(x = YEAR, y = mig05), linetype = 3, size = 1.5) +
  scale_y_continuous(label=comma, limits = c(0, max(filter(test2, STATE==48)$Population))) +
  theme_bw() +
  # annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(x = "Year",
       y = "Population",
       title = "Texas")

label = textGrob(label = "Vintage 2017 Minnesota Projection \n Minnesota Demographic Center", x = 0.6, y = 0.1)
minn<- ggplot() +
  geom_line(data=filter(test2, STATE==27), aes(x=YEAR, y = Population, color = Scenario), size = 1.5) +
  scale_color_brewer(palette="Set2") +
  geom_line(data = mn, aes(x = YEAR, y = Proj), linetype = 3, size = 1.5) +
  scale_y_continuous(label=comma, limits = c(0, max(filter(test2, STATE==27)$Population))) +
  theme_bw() +
  # annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(x = "Year",
       y = "Population",
       title = "Minnesota")

va <- tribble(
  ~"STATE", ~"YEAR", ~"Proj",
  51, 2020, 8744273,
  51, 2025, 9145616,
  51, 2030, 9546958,
  51, 2035, 9874244,
  51, 2040, 10201530,
  51, 2045, 10528817  
  
)
label = textGrob(label = "Vintage 2017 Virginia Projection \n Weldon Cooper Center", x = 0.6, y = 0.1)
va_fig<- ggplot() +
  geom_line(data=filter(test2, STATE==51), aes(x=YEAR, y = Population, color = Scenario), size = 1.5) +
  scale_color_brewer(palette="Set2") +
  geom_point(data = va, aes(x = YEAR, y = Proj), size = 1.5) +
  scale_y_continuous(label=comma, limits = c(0, max(filter(test2, STATE==51)$Population))) +
  theme_bw() +
  # annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(x = "Year",
       y = "Population",
       title = "Virginia")

ct <- tribble(
  ~"STATE", ~"YEAR", ~"Proj",
  "09", 2015, 3593301,
  "09", 2020, 3604603,
  "09", 2025, 3618763,
  "09", 2030, 3633994,
  "09", 2035, 3645370,
  "09", 2040, 3654015
  
)
label = textGrob(label = "Vintage 2017 Connecticut Projection \n Connecticut State Data Center", x = 0.6, y = 0.1)
ct_fig<- ggplot() +
  geom_line(data=filter(test2, STATE=="09"), aes(x=YEAR, y = Population, color = Scenario), size = 1.5) +
  scale_color_brewer(palette="Set2") +
  geom_line(data = ct, aes(x = YEAR, y = Proj), linetype = 3, size = 1.5) +
  scale_y_continuous(label=comma, limits = c(0, max(filter(test2, STATE=="09")$Population))) +
  theme_bw() +
  # annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(x = "Year",
       y = "Population",
       title = "Connecticut")

label = textGrob(label = "Vintage 2016 California Projection \n California Department of Finance", x = 0.6, y = 0.1)
ca_fig<- ggplot() +
  geom_line(data=filter(test2, STATE=="06"), aes(x=YEAR, y = Population, color = Scenario), size = 1.5) +
  scale_color_brewer(palette="Set2") +
  geom_line(data = ca, aes(x = YEAR, y = Proj), linetype = 3, size = 1.5) +
  scale_y_continuous(label=comma, limits = c(0, max(filter(test2, STATE=="06")$Population))) +
  theme_bw() +
  # annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(x = "Year",
       y = "Population",
       title = "California")

label = textGrob(label = "Vintage 2016 Arizona Projection \n Arizona Office of Economic Opportunity", x = 0.6, y = 0.1)
az_fig<- ggplot() +
  geom_line(data=filter(test2, STATE=="04"), aes(x=YEAR, y = Population, color = Scenario), size = 1.5) +
  scale_color_brewer(palette="Set2") +
  geom_ribbon(data = az, aes(x = YEAR, ymin = projlow, ymax=projhigh), color = "gray", alpha =0.3) +
  geom_line(data = az, aes(x = YEAR, y = Proj), linetype = 3, size = 1.5) +
  scale_y_continuous(label=comma, limits = c(0, max(filter(test2, STATE=="04")$Population))) +
  theme_bw() +
  # annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(x = "Year",
       y = "Population",
       title = "Arizona")

ak <- tribble(
  ~"STATE", ~"YEAR", ~"Proj", ~"low", ~"high",
  "02", 2017, 737080, 737080, 737080,
  "02", 2020, 746582, 733325, 778387,
  "02", 2025, 770392, 722266, 849962,
  "02", 2030, 790777, 705856, 925120,
  "02", 2035, 808367, 685487, 1004419,
  "02", 2040, 823771, 662335, 1088694,
  "02", 2045, 837806, 637336, 1179387
)

label = textGrob(label = "Vintage 2017 Virginia Projection \n Weldon Cooper Center", x = 0.6, y = 0.1)
ak_fig<- ggplot() +
  geom_line(data=filter(test2, STATE=="02"), aes(x=YEAR, y = Population, color = Scenario), size = 1.5) +
  scale_color_brewer(palette="Set2") +
  geom_ribbon(data = ak, aes(x = YEAR, ymin = low, ymax=high), color = "gray", alpha =0.3) +
  geom_point(data = ak, aes(x = YEAR, y = Proj), size = 1.5) +
  scale_y_continuous(label=comma, limits = c(0, max(filter(test2, STATE=="02")$Population))) +
  theme_bw() +
  # annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(x = "Year",
       y = "Population",
       title = "Alaska")

prow<-plot_grid(tx_fig + theme(legend.position="none"), 
                minn + theme(legend.position="none"), 
                va_fig + theme(legend.position="none"),
                ak_fig + theme(legend.position="none"),
                ca_fig + theme(legend.position="none"),
                az_fig + theme(legend.position="none"),
                ncol=2,
                labels = "auto")
legend <- get_legend(tx_fig + theme(legend.position="bottom"))
plot_grid(prow, legend, ncol=1, rel_heights= c(1, 0.05))