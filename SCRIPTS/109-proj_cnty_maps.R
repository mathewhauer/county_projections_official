###------Projection County Maps -----
## @knitr proj_cnty_maps

 

states <- spTransform(states(cb=TRUE), CRS("+init=epsg:2163"))
counties2 <- counties(cb = TRUE)
counties <- spTransform(counties2, CRS("+init=epsg:2163")) %>%
  subset(!(STATEFP %in% c("60", "64","66", "68", "69", "70", "74","72", "78")))

countyproj <- SSPs %>%
  # left_join(., statenames) %>%
  dplyr::group_by(YEAR, STATE, GEOID) %>%
  dplyr::summarise(SSP1 = sum(SSP1),
                   SSP2 = sum(SSP2),
                   SSP3 = sum(SSP3),
                   SSP4 = sum(SSP4),
                   SSP5 = sum(SSP5),
                   n = length(unique(GEOID))) %>%
  filter(YEAR %in% c(2020, 2100)) %>%
  gather(Scenario, Population, SSP1:SSP5) %>%
  arrange(GEOID, Scenario, YEAR) %>%
  ungroup() %>%
  group_by(GEOID, Scenario) %>%
  mutate(numgrowth = Population - lag(Population),
         pergrowth = (Population/lag(Population) -1)*100) %>%
  ungroup() %>%
  mutate(Scenario2 = case_when(
           Scenario == "SSP1" ~ "SSP1: Sustainability",
           Scenario == "SSP2" ~ "SSP2: Middle",
           Scenario == "SSP3" ~ "SSP3: Regional rivalry",
           Scenario == "SSP4" ~ "SSP4: Inequality",
           Scenario == "SSP5" ~ "SSP5: Fossil-fueled development"),
         GEOID = case_when(
    GEOID=="46113"~ "46102", # Shannon County (46113)'s name changed to Oglala Lakota (46102)
    GEOID== "51917" ~ "51019", # Bedford City (51917) is merged into Bedford County (51019)
    GEOID == "02270" ~ "02158", # Wade Hampton (02270) is actually (02158)
    TRUE ~ as.character(GEOID)
  ),
  Grouping = case_when(
    numgrowth <= -30000 ~ "Less than -30,000",
    numgrowth <= -15000 ~ "-30,000 to -15,000",
    numgrowth <= -5000 ~ "-15,000 to -5,000",
    numgrowth <= 0 ~"-5,000 to 0",
    numgrowth <= 5000 ~ "0 to 5,000",
    numgrowth <= 15000 ~ "5,000 to 15,000",
    numgrowth <= 90000 ~ "15,000 to 90,000",
    TRUE ~ "90,000 or more"
  )) %>%
  na.omit

countyproj$Grouping <- factor(countyproj$Grouping, levels = c("Less than -30,000", "-30,000 to -15,000", "-15,000 to -5,000",
                                                                 "-5,000 to 0", "0 to 5,000", "5,000 to 15,000",
                                                                 "15,000 to 90,000", "90,000 or more"))
leg<- get_legend(
  ggplot(data = filter(countyproj, STATE == "48")) +
                   geom_bar(aes(x=Population,fill = Grouping)) +
                   scale_fill_brewer("Numeric Change",palette = "Spectral") +
    theme(legend.key.size = unit(0.5, "line"),
          legend.text = element_text(size = 7),
          legend.title = element_text(size=9))
    
  )

# 
# 
# 
# countiesssp1 <- append_data(counties, countyproj[which(countyproj$Scenario == "SSP1"),], key.shp = "GEOID", key.data = "GEOID", ignore.duplicates = TRUE)
# countiesssp2 <- append_data(counties, countyproj[which(countyproj$Scenario == "SSP2"),], key.shp = "GEOID", key.data = "GEOID", ignore.duplicates = TRUE)
# countiesssp3 <- append_data(counties, countyproj[which(countyproj$Scenario == "SSP3"),], key.shp = "GEOID", key.data = "GEOID", ignore.duplicates = TRUE)
# countiesssp4 <- append_data(counties, countyproj[which(countyproj$Scenario == "SSP4"),], key.shp = "GEOID", key.data = "GEOID", ignore.duplicates = TRUE)
# countiesssp5 <- append_data(counties, countyproj[which(countyproj$Scenario == "SSP5"),], key.shp = "GEOID", key.data = "GEOID", ignore.duplicates = TRUE)
# 
# 
# countynummaps <- function(df, scen){
#   US_cont <- df %>%
#   subset(!STATEFP %in% c("02","15"))
# 
#   tm_shape(US_cont) +
#   tm_fill("Grouping",
#               title = "Numeric Change",
#               palette = "Spectral",
#               # breaks= c(-Inf, -90000, -60000,-30000, -15000, -5000, 0, 5000, 15000, 90000, Inf),
# 
#               # midpoint = 0,
#               id = "GEOID",
#               #showNA = TRUE,
#               border.col = "gray50",
#               border.alpha =0,
#               legend.is.portrait=TRUE
#               # style ="jenks",
#               # n = 8
#   ) +
# tm_legend(show = FALSE) +
#   tm_shape(states) +
#   tm_borders(lwd=1, col="black", alpha = 0.5) +
#   tm_layout(legend.position = c("left", "bottom"),
#             # legend.stack = "portrait",
#             # legend.outside =TRUE,
#             legend.text.size = 1.1,
#             frame = FALSE,
#             main.title = paste(scen),
#             main.title.size = 2)
# }
# tmap_save(countynummaps(countiesssp1, "SSP1: Sustainability"), filename="FIGURES/numgrowth_ssp1.pdf")
# tmap_save(countynummaps(countiesssp2, "SSP2: Middle of the road"), filename="FIGURES/numgrowth_ssp2.pdf")
# tmap_save(countynummaps(countiesssp3, "SSP3: Regional rivalry"), filename="FIGURES/numgrowth_ssp3.pdf")
# tmap_save(countynummaps(countiesssp4, "SSP4: Inequality"), filename="FIGURES/numgrowth_ssp4.pdf")
# tmap_save(countynummaps(countiesssp5, "SSP5: Fossil-fueled development"), filename="FIGURES/numgrowth_ssp5.pdf")


numgrowth_ssp1 <- ggdraw() + draw_image(pdf_render_page('FIGURES/numgrowth_ssp1.pdf', page = 1, dpi = 300, numeric = FALSE))
numgrowth_ssp2 <-  ggdraw() + draw_image(pdf_render_page('FIGURES/numgrowth_ssp2.pdf', page = 1, dpi = 300, numeric = FALSE))
numgrowth_ssp3 <-  ggdraw() + draw_image(pdf_render_page('FIGURES/numgrowth_ssp3.pdf', page = 1, dpi = 300, numeric = FALSE))
numgrowth_ssp4 <-  ggdraw() + draw_image(pdf_render_page('FIGURES/numgrowth_ssp4.pdf', page = 1, dpi = 300, numeric = FALSE))
numgrowth_ssp5 <-  ggdraw() + draw_image(pdf_render_page('FIGURES/numgrowth_ssp5.pdf', page = 1, dpi = 300, numeric = FALSE))



top <- plot_grid(numgrowth_ssp5,
                 numgrowth_ssp3,
                  ncol=2, align = 'h')
bot <- plot_grid(numgrowth_ssp1,
                 numgrowth_ssp4,
                 ncol=2, align = 'h')
mid <- plot_grid(leg, numgrowth_ssp2, NULL, ncol=3,rel_widths = c(1,3,1),
                 rel_heights = c(1,5,1))

maps<- plot_grid(top,
                 mid,
                 bot,
                 ncol=1, align = 'h', 
                 # rel_heights = c(2, 1),
                 label_x = 0.3)

fig<- ggplot() + 
  geom_blank() + 
  xlim(0, 10) + 
  ylim(0, 10) +
  theme_classic() + 
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y  = element_line(arrow = arrow()),
        axis.line.x = element_line(arrow = arrow())) +
  labs(x ="Barriers to Adaptation", 
       y = "Barriers to Mitigation") 

countymaps <- ggdraw() +
  draw_plot(fig)+
  draw_plot(maps, scale = 0.91)

ggsave("FIGURES/countymaps.pdf")



 