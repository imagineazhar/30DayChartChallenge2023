library(tidyverse)
library(showtext)
library(ggtext)
library(ggridges)
library(MetBrewer)

# ------ Get Data ------ 
# Data: (https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data)
df <- readr::read_tsv("earthquakes.tsv")|>
  select(Year,Mo,"Location Name")

earth <- df|> 
  mutate(MonthName=month.name[Mo])|>
  group_by(Year, MonthName)|>
  summarise(n=n())|>
  filter(!is.na(Year))

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Monthly Distribution of Earthquakes, 1950-2023"
subtitle_text <- "Analyzing the number of earthquakes that occurred in each month."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: NOAA"

# ------ Plot ------ 

earth |> ggplot(aes(x = n, y = factor(MonthName, levels = month.name),
                    fill="pink")) +
  geom_density_ridges(alpha=0.8)+
  scale_x_continuous(expand = c(0.01, 0))+
  theme_ridges()+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
  theme(
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(family = body_font, size=14),
    axis.text.y = element_text(family = body_font, size=14),
    
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
    plot.subtitle = element_textbox(margin = margin(10, 0, 20, 0),
                                 size = 24,
                                 family = body_font,
                                 color = "grey15",
                                 width = unit(65, "lines")),
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=16, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(40, 70, 40, 70)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("earthquakes.png",dpi=320,
       width = 14, height = 16)
showtext_auto(FALSE)

