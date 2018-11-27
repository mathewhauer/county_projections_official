###------County Error Map -----
## @knitr county_error_map



eval_ucm_cntytotal <- base_projunfitted %>%
  filter(!TYPE == "BASE") %>%
  group_by(STATE, COUNTY, GEOID, YEAR, TYPE) %>%
  dplyr::summarise(POPULATION = sum(POPULATION, na.rm=T),
                   A = sum(A, na.rm=T),
                   B = sum(B),
                   C = sum(C),
                   num = length(A)) %>%
  mutate(FLAG1 = if_else(is.na((A/POPULATION)-1), 0,abs((A/POPULATION)-1)),
         FLAG2 = if_else(POPULATION>=B & POPULATION<=C,1,0),
         in90percentile = FLAG2/num) %>%
  ungroup() %>%
  filter(YEAR == 2015,
         TYPE == "CCD/CCR") %>%
  dplyr::select(FLAG1, STATE, GEOID) %>%
  NaRV.omit()

a<- get_acs(geography = "county",
            variables = "B19013_001",
            geometry = TRUE, shift_geo = TRUE)
counties <- append_data(a, eval_ucm_cntytotal, key.shp = "GEOID", key.data = "GEOID", ignore.duplicates = TRUE) 

US_cont <- counties

m_cont<- tm_shape(US_cont) +
  tm_polygons("FLAG1", 
              title = "Absolute Percent Error", 
              palette = "YlOrBr", 
              breaks= c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, Inf),
              auto.palette.mapping = FALSE, 
              id = "GEOID", 
              #showNA = TRUE, 
              border.col = "gray50", 
              border.alpha =0.5,
              legend.is.portrait=FALSE
              #style ="jenks",
              #n = 8
  ) +
  tm_shape(states) +
  tm_borders(lwd=2, col="black", alpha = 0.5) +
  tm_layout(legend.position = c("right", "bottom"), 
            # legend.stack = "portrait",
            legend.text.size = 0.5,
            frame = FALSE)
m_cont