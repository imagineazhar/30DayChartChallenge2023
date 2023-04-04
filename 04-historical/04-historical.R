library(tidyverse)
library(showtext)
library(ggtext)
library(MetBrewer)

# ------ Get Data ------ 

df <- read.csv("diet.csv")


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

diet|>  ggplot() +
  coord_cartesian(clip = "off") +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted"),
    axis.line.x = element_line(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.x = element_text(family = body_font, size=12),
    axis.text.y = element_blank(),
  
    
  # Legend
  legend.position = "top",
  legend.title = element_blank(),
  legend.spacing = unit(0.5, 'cm'),
  legend.key.height= unit(0.5, 'cm'),
  legend.key.width= unit(0.7, 'cm'),
  legend.text = element_text(family = body_font,
                             size=13,
                             face = 'plain',
                             color = "grey10"),
  
  # TITLE
  plot.title.position = "plot",
  plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                               size = 30,
                               family = title_font,
                               face = "bold",
                               width = unit(55, "lines")),
  
  # SUB-TITLE
  plot.subtitle = element_text(margin = margin(10, 0, 20, 0),
                            size = 16,
                            family = body_font,
                            color = "grey15"),
  # Caption
  plot.caption = element_text(family=body_font,
                              face="plain",
                              size=14, 
                              color="grey40",
                              hjust=.5,
                              margin=margin(20,0,0,0)),
  
  plot.background = element_rect(color="white", fill="white"),
  plot.margin = margin(30, 30, 20, 40)
)


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave(".png",dpi=320,
       width = 12, height = 10)
showtext_auto(FALSE)

