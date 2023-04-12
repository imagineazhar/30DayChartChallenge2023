library(tidyverse)
library(showtext)
library(ggtext)
library(bbplot)
library(sf)
library(rnaturalearth)


# ------ Get Data ------ 


world <- ne_countries(scale = "small", returnclass = "sf")

crs <- "+proj=robin"

map_base <- world|> st_transform(crs)

df <- read.csv("forest_area.csv")|>
  janitor::clean_names()|>
  rename(forest_area = "x2020")|>
  filter(!is.na(country_code),
         !is.na(forest_area),
         ! country_code %in% c("AfW", "AFE", "ARB"))

#merge data

forest <- map_base |>
  select(geometry, name, iso_a3)|>
  left_join(df, by= c("iso_a3" = "country_code"))

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Global Forest Coverage"
subtitle_text <- "Percentage of Forest Area, 2020"
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: World Bank "

# ------ Plot ------ 
  ggplot(forest)+
  geom_sf(aes(fill=forest_area))+
  scale_fill_viridis_c(option = "D", direction = -1)+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
  bbc_style()+
      theme(
      axis.title.x  = element_blank(),
      axis.title.y  = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      
      # Legend
      legend.position = "top",
      legend.title = element_blank(),
      legend.key.height= unit(0.5, 'cm'),
      legend.key.width= unit(0.9, 'cm'),
      legend.text = element_text(family = body_font,
                                 size=13,
                                 face = 'plain',
                                 color = "grey10"),
      
      # TITLE
      plot.title.position = "panel",
      plot.title = element_text(size=20,face="bold"),
      
      plot.subtitle = element_text(size=16),
      
      # Caption
      plot.caption = element_text(family=body_font,
                                  face="plain",
                                  size=12, 
                                  hjust = 0.5,
                                  color="grey40",
                                  margin=margin(30,0,0,0)),
      
      plot.background = element_rect(color="white", fill="white"),
      plot.margin = margin(30, 30, 30, 30)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("forest_area.png",dpi=320,
       width = 10, height = 8)
showtext_auto(FALSE)

