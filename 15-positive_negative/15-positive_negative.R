library(tidyverse)
library(showtext)
library(ggtext)


# ------ Get Data ------ 
genre <- read.csv('genre.csv')

ratings <- read.csv("ratings.csv")


# ------ custom colors ------

my_pal <- rcartocolor::carto_pal(n = 12, name = "Bold")

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- ""
subtitle_text <- ""
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: "

# ------ Plot ------ 


  theme_minimal()+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
    theme(
      axis.title.x  = element_blank(),
      axis.title.y  = element_blank(),
      axis.text.x = element_text(family = body_font, 
                                 face = "bold",
                                 size=16),
      axis.text.y = element_text(family = body_font, 
                                 face = "bold",
                                 size=16),
      
      # Legend
      legend.position = "none",
      
      # TITLE
      plot.title.position = "plot",
      plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                                   size = 32,
                                   family = title_font,
                                   face = "bold",
                                   width = unit(85, "lines")),
      # SUB-TITLE
      plot.subtitle = element_textbox(margin = margin(10, 0, 30, 0),
                                   size = 18,
                                   color = "grey30",
                                   family = body_font,
                                   width = unit(70, "lines")),
      
      # Caption
      plot.caption = element_text(family=body_font,
                                  face="plain",
                                  size=14, 
                                  color="grey40",
                                  hjust=.5,
                                  margin=margin(30,0,0,0)),
      
      plot.background = element_rect(color="white", fill="white"),
      plot.margin = margin(50, 50, 20, 50)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("IMDb.png",dpi=320,
       width = 18, height = 12)
showtext_auto(FALSE)

