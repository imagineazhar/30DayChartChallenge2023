library(tidyverse)
library(showtext)
library(ggtext)


# ------ Get Data ------ 


df <- read.csv("data.csv")|>
  janitor::clean_names()|>
  filter(item=="Agricultural land")

land_df <- df|>
  select(area,"y1961", "y1962", "y1963", "y1964", "y1965", "y1966", "y1967", "y1968", "y1969", "y1970", "y1971", "y1972", "y1973", "y1974", "y1975", "y1976", "y1977", "y1978", "y1979", "y1980", "y1981", "y1982", "y1983", "y1984", "y1985", "y1986", "y1987", "y1988", "y1989", "y1990", "y1991", "y1992", "y1993", "y1994", "y1995", "y1996", "y1997", "y1998", "y1999", "y2000", "y2001", "y2002" )|>
  pivot_longer(cols = c("y1961", "y1962", "y1963", "y1964", "y1965", "y1966", "y1967", "y1968", "y1969", "y1970", "y1971", "y1972", "y1973", "y1974", "y1975", "y1976", "y1977", "y1978", "y1979", "y1980", "y1981", "y1982", "y1983", "y1984", "y1985", "y1986", "y1987", "y1988", "y1989", "y1990", "y1991", "y1992", "y1993", "y1994", "y1995", "y1996", "y1997", "y1998", "y1999", "y2000", "y2001", "y2002"),
               names_to = "year",
               values_to = "value")|>
  filter(area=="World")

land_df$year<-gsub("y","",as.character(land_df$year))


# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Agricultural land (1000 ha)"
subtitle_text <- ""
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: FAOSTAT"

# ------ Plot ------ 

land_df|> ggplot(aes(x=as.integer(year), y=value, label=value)) +
  geom_area(fill="#70ad47", alpha=0.4)+
  geom_line(color="#70ad47", size=2)+
  scale_y_log10()+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x="",
       y="")+
  theme_minimal()+
  theme(
    strip.text.x = element_text(family = title_font,
                                face = 'bold',
                                size = 12, colour = "grey20"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Legend
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.height= unit(0.5, 'cm'),
    legend.key.width= unit(2.5, 'cm'),
    legend.spacing = unit(1, 'cm'),
    legend.text = element_text(family = body_font,
                               size=15,
                               face = 'plain',
                               color = "grey10"),
    
    
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
                                size=14, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(20,0,0,0)),
    
    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(30, 50, 30, 50)
  )



# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("agri_land.png",dpi=320,
       width = 12, height = 14)
showtext_auto(FALSE)

