library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)
library(MetBrewer)

# ------ Get Data ------ 

df <- read.csv("ict-adoption.csv")|>
  janitor::clean_names()|>
  rename( "Telephone Subscriptions" = "fixed_telephone_subscriptions",
          "Broadband Subscriptions" = "fixed_broadband_subscriptions",
          "Mobile Cellular Subscriptions" = "mobile_cellular_subscriptions",
          "Number of Internet Users" = "number_of_internet_users")

tech_df <- df|> pivot_longer(cols = c("Telephone Subscriptions",
                                  "Broadband Subscriptions",
                                  "Mobile Cellular Subscriptions",
                                  "Mobile Cellular Subscriptions",
                                  "Number of Internet Users"
                                  ),
                         values_to = "users",
                         names_to = "tech")|>
  filter(entity=="World")


# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 
title_text <- "Since 2000, Communication Technologies Adoption Has Skyrocketed Worldwide."
subtitle_text <- "showing the number of users subscribed to different communication technologies."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data:OurWorldInData"

# ------ Plot ------ 

tech_df |> ggplot(aes(x=year, y=users, fill=tech))+
  geom_stream(type = "ridge")+
  scale_x_continuous(limits = c(1960,2020),breaks = seq(1960,2020,10),
                     sec.axis = sec_axis(trans=~.))+
  scale_fill_met_d("VanGogh2", direction = -1)+
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
                                 size = 28,
                                 family = title_font,
                                 face = "bold",
                                 width = unit(80, "lines")),
    
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
    plot.margin = margin(40, 40, 40, 40)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("tech.png",dpi=320,
       width = 16, height = 10)
showtext_auto(FALSE)

