library(tidyverse)
library(showtext)
library(ggtext)
library(MetBrewer)

# ------ Get Data ------ 

df <- read.csv("ict-adoption.csv")|>
  janitor::clean_names()

ict <- df|> pivot_longer(cols = c("fixed_telephone_subscriptions",
                                  "fixed_broadband_subscriptions",
                                  "mobile_cellular_subscriptions",
                                  "number_of_internet_users"
                                  ),
                         values_to = "users",
                         names_to = "tech")
# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 
title_text <- "Adoption of communication technologies, World."
subtitle_text <- ""
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data:OurWorldInData"

# ------ Plot ------ 

ict |> 
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

