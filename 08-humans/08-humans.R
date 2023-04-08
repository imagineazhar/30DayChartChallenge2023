library(tidyverse)
library(showtext)
library(ggtext)
library(ggridges)


# ------ Get Data ------ 
# Data: (https://ourworldindata.org/suicide)

df <- read.csv("suicides-rate.csv")|>
  janitor::clean_names()

suicide <- df|>
  filter(nchar(code)<1,
         entity!="Eastern Mediterranean")


suicide$entity <- factor(suicide$entity, levels=c(
  "High-income", "Upper-middle-income", "Lower-middle-income","Low-income",
  "Americas","Europe", "Africa", "South-East Asia",
  "Western Pacific"))



# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Suicide Rate, 2000-2019"
subtitle_text <- "Adjusted annual suicide rate per 100,000 people, accounting for underreporting in various countries."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: WHO, Global Health Observatory"

# ------ Plot ------ 

suicide |> ggplot(aes(x = suicide_rate, y = entity, fill=entity)) +
  geom_density_ridges(alpha=0.8)+
  scale_x_continuous(expand = c(0.01, 0))+
  MetBrewer::scale_fill_met_d("Monet")+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
  theme_ridges()+
  theme(
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(family = body_font, size=16),
    axis.text.y = element_text(family = body_font, size=16),
    
    # Legend
    legend.position = "none",
    # TITLE
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                                 size = 36,
                                 family = title_font,
                                 face = "bold",
                                 width = unit(60, "lines")),
    
    # SUB-TITLE
    plot.subtitle = element_textbox(margin = margin(10, 0, 30, 0),
                                 size = 20,
                                 family = body_font,
                                 color = "grey15",
                                 width = unit(65, "lines")),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=14, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(40, 70, 40, 70)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("suicide.png",dpi=320,
       width = 14, height = 16)
showtext_auto(FALSE)

