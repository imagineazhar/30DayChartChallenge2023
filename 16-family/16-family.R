library(tidyverse)
library(showtext)
library(ggtext)

# ------ Get df ------ 

df=read.csv("one.csv")|>
  janitor::clean_names()

df$entity <- factor(df$entity,                                    
                  levels = df$entity[order(df$share, decreasing = FALSE)])

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "One Person Households in Europe, 2018"
subtitle_text <- "Percentage of one-person households. Estimates combine multiple sources, including cross-country surveys and census data."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: OWID"

# ------ Plot ------ 
df|> ggplot(aes(x=share, y=entity, fill="#7F3C8D"))+
  geom_bar(stat = "identity", alpha=0.8, width = 0.7)+
  ggrepel::geom_text_repel(aes(label = paste0(round(share),"%")), nudge_x = 4) +
  scale_fill_manual(values = "#7F3C8D")+
  theme_void()+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(family = body_font, size=12),
      axis.text.x = element_blank(),
      
      # Legend
      legend.position = "none",
      
      # TITLE
      plot.title.position = "plot",
      plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                                   size = 34,
                                   family = title_font,
                                   face = "bold"),
      # SUB-TITLE
      plot.subtitle = element_textbox(margin = margin(10, 0, 30, 0),
                                   size = 18,
                                   color = "grey30",
                                   family = body_font,
                                   width = unit(45, "lines")),
      
      # Caption
      plot.caption = element_text(family=body_font,
                                  face="plain",
                                  size=14, 
                                  color="grey40",
                                  hjust=.5,
                                  margin=margin(30,0,0,0)),
      
      plot.background = element_rect(color="white", fill="white"),
      plot.margin = margin(30, 100, 30, 100)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("alone.png",dpi=320,
       width = 12, height = 14)
showtext_auto(FALSE)

