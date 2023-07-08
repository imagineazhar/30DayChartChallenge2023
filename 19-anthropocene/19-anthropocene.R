library(tidyverse)
library(showtext)
library(ggtext)


# ------ Get Data ------ 


df <- read.csv("global-cropland.csv")|>
  janitor::clean_names()

global_df <- df|>
  filter(entity=="World")

global_df$avg_cropland=rowMeans(global_df[,c("cropland_ha","cropland_hyde3_2",
             "cropland_00006620_area_005110_hectares")], na.rm = TRUE)
  
global_cropland <- global_df|>
  filter(year>0)|>
  select("year","avg_cropland")
  

# Define function
format_number <- function(x) {
  sapply(x, function(x) {
    if(is.na(x)) {
      return(NA)
    } else if(x >= 1e9) {
      return(paste0(round(x / 1e9, 2), " B"))
    } else if(x >= 1e6) {
      return(paste0(round(x / 1e6, 2), " M"))
    } else {
      return(as.character(x))
    }
  })
}

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Global cropland is still increasing."
subtitle_text <- "Cropland is land used to grow crops, excluding pasture used for livestock grazing."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: Taylor & Rising (2021); Food and Agriculture Organization of the United Nations; Goldewijk et al. (2017)"

# ------ Plot ------ 

global_cropland|> ggplot(aes(x=year, y=avg_cropland))+
  geom_line(linewidth=1.5,
            color="#1A5D1A")+
  scale_x_continuous(limits = c(100,2020),breaks = seq(100,2020,250))+
  scale_y_continuous(labels=format_number)+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x="",
       y="Cropland (ha)")+
  theme_minimal()+
  theme(
    axis.title.y = element_text(margin = margin(0, 10, 0, 0),
                                size = 11,
                                color = "grey30",
                                family = body_font,
                                face = "plain"),
    # Legend
    legend.position = "off",
    
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(20, 0, 10, 0),
                              size = 40,
                              family = title_font,
                              face = "bold"),
    # Subtitle
    plot.subtitle = element_textbox(family=body_font,
                                    face = "plain",
                                    width = unit(60, "lines"),
                                    size = 20,
                                    color = "grey20",
                                    margin = margin(10,0,20,0)),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=10, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(30, 50, 30, 50)
  )



# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("crop_land.png",dpi=320,
       width = 12, height = 8)
showtext_auto(FALSE)

