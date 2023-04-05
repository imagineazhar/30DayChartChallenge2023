library(tidyverse)
library(showtext)
library(colorspace)
library(ggtext)
library(ggrepel)
library(MetBrewer)

# ------ Get Data ------ 

df <- read.csv("forest-area.csv")|>
  janitor::clean_names()

forest_df <- df|> pivot_wider(names_from = year, 
                                values_from = forest_area_change_rate)|>
  select('entity', '2010', '2020')|>
  mutate(change = `2020` - `2010`)|>
  pivot_longer(
    cols = -c('entity', 'change'),
    names_to = "year",
    values_to = "forest_area"
  )|>
  filter(
    !is.na(change),
    !str_detect(`entity`, "Europe|Aia|(UN)|Latin America|North Africa|Sub-Saharan Africa")
  )
  

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 
title_text <- "The forest area in 62 countries increased between 2010 and 2020."
subtitle_text <- "Forest area net change rate measures the annual net change in forested area, as a percentage of total forest
area. Negative values indicate a net loss of forest, and positive values a net gain."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data:  Food and Agriculture Organization of the United Nations."

# ------ Plot ------ 

forest_df |>
  filter(change>0)|>
  ggplot(aes(year, forest_area, group=entity))+
  geom_line(data = forest_df|>filter(change<0),
            aes(color=change), alpha=0.4, linewidth=0.9)+
  geom_line(aes(color = change * 4, 
                color = after_scale(darken(color, .2, space = "HLS"))), 
            size = 1.3)+
  geom_text_repel(data = forest_df|>filter(change>0, year==2010),
                  aes(label=entity),
                  hjust=1, nudge_x = -.15, size=4.5, family=body_font,
                  direction = "y", force = 0.5, min.segment.length = 0,
                  segment.size=0.5, segment.curvature=-0.15, segment.ncp=3,
                  segment.angle=90, box.padding = .25)+
  geom_text_repel(data = forest_df|>filter(change>0, year==2020),
                  aes(label = glue::glue("{format(abs(change), digits = 3)}% ↑")),
                  hjust=0, nudge_x = .09, size=4.5,family="Overpass Mono",
                  direction = "y", force = 0.5, min.segment.length = 0,
                  segment.size=0.5)+
  geom_point(data = forest_df|>filter(change>0),
             aes(color=change), size=1.5)+
  geom_point(shape = 21, size = 4, stroke = 2, fill = "white", 
             color = "transparent") +
  geom_point(aes(color = change * 4, 
                 color = after_scale(darken(color, .2, space = "HLS"))), 
             shape = 21, stroke = 2, fill = NA, size = 4) +
  geom_point(aes(y = 0), stat = "unique", shape = "-", size = 8,
             color = "grey65") +
  coord_cartesian(clip = "off") +
  scale_x_discrete(expand = c(.3, 0.3), position = "top") +
  scale_y_continuous(expand = c(0, 0), limits = c(-6, 7)) +
  rcartocolor::scale_color_carto_c(palette = "Fall", direction = 1, 
                                   limits = c(-20, 20), guide = "none") +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text)+
  theme_void()+
theme(
  panel.grid.major.x = element_line(color = "grey65", size = 1),
  axis.text.x = element_text(color = "grey50", size = 14, 
                             family = "Overpass ExtraBold", 
                             margin = margin(t = 5, b = 10)),

  # TITLE
  plot.title.position = "panel",
  plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                               size = 26,
                               hjust = 0.5,
                               family = title_font,
                               face = "bold",
                               width = unit(55, "lines")),
  # SUB-TITLE
  plot.subtitle = element_textbox(margin = margin(10, 0, 20, 0),
                               size = 16,
                               hjust = 0.5,
                               family = body_font,
                               color = "grey15",
                               width = unit(55, "lines")),
  
  # Caption
  plot.caption = element_text(family=body_font,
                              face="plain",
                              size=14, 
                              color="grey40",
                              hjust=.5,
                              margin=margin(20,0,0,0)),
  plot.background = element_rect(color="white", fill="white"),
  plot.margin = margin(50, 50, 50, 50),
      )


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("forest_area.png",dpi=320,
       width = 12, height = 14)
showtext_auto(FALSE)

