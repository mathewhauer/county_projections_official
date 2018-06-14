####-------Lexis Diagram Example-------
## @knitr lexisplot

library(LexisPlotR)
library(cowplot)
figtop<- lexis.grid2(year.start = 2010, year.end = 2020, age.start = 0, age.end = 5, d = 5) + 
  annotate("text", x = as.Date("2014-07-01"), y =3.9, label = "120",fontface =2) +
  annotate("text", x = as.Date("2010-11-01"), y = 0.3, label = "100",fontface =2) +
  # annotate("text", x = as.Date("2012-06-01"), y = 3, label = "120/100 = 1.25", angle = 45,fontface =2) +
  annotate("text", x = as.Date("2015-11-01"), y = 0.3, label = "90",fontface =2) +
  annotate("text", x = as.Date("2019-07-01"), y = 3.9, label = "108", fontface = 'italic') +
  annotate("text", x = as.Date("2017-06-01"), y = 3, label = "(120/100) * 90", angle = 45) +
  labs(title = "Cohort Change Ratios (CCRs)")

figbot<- lexis.grid2(year.start = 2010, year.end = 2020, age.start = 0, age.end = 5, d = 5) + 
  annotate("text", x = as.Date("2014-07-01"), y =3.9, label = "120",fontface =2) +
  annotate("text", x = as.Date("2010-11-01"), y = 0.3, label = "100",fontface =2) +
  # annotate("text", x = as.Date("2012-06-01"), y = 3, label = "120-100 = +25", angle = 45,fontface =2) +
  annotate("text", x = as.Date("2015-11-01"), y = 0.3, label = "90",fontface =2) +
  annotate("text", x = as.Date("2019-07-01"), y = 3.9, label = "110", fontface = 'italic') +
  annotate("text", x = as.Date("2017-06-01"), y = 3, label = "(120-100) + 90", angle = 45) +
  labs(title = "Cohort Change Differences (CCDs)")

plot_grid(figtop, figbot, ncol = 1, labels = "auto")