library(tidyverse)
library(showtext)
library(ggtext)
library(MetBrewer)

# ------ Get Data ------ 

df <- readr::read_csv('drinking-water.csv')

# ------ Data Wrangling ------ 

water <- df |>
  filter(Year==2020)|>
  rename("Safely managed" = wat_sm,
         Basic = wat_bas_minus_sm,
         Limited = wat_lim,
         Unimproved = wat_unimp, 
         "No Access (surface water only)" = wat_sur)|>
  pivot_longer(cols = c("Safely managed", "Basic", "Limited", "Unimproved",
                        "No Access (surface water only)"  ),
               values_to = "percentage",
               names_to = "group")|>
  filter(is.na(Code), !is.na(percentage),
         Entity %in% c("High income", "North America and Europe",
                       "Western Asia and Northern Africa",
                       "Upper-middle income",
                       "Latin America and the Caribbean",
                       "World",
                       "Central and Southern Asia",
                       "Lower-middle income",
                       "Sub-Saharan Africa",
                       "Low income"))
water$group <- factor(water$group, levels=c("Safely managed", "Basic",
                                            "Limited", "Unimproved",
                                            "No Access (surface water only)"))
water$Entity <- factor(water$Entity, levels=c(
  "World", "High income", "Upper-middle income", "Lower-middle income","Low income",
  "North America and Europe", "Latin America and the Caribbean",
  "Western Asia and Northern Africa", "Central and Southern Asia",
  "Sub-Saharan Africa"))


# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Only 29% of population have access to safe drinking water in Low-income counturies, 2020"
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: OurWorldinData"

# ------ Plot ------ 
water |> ggplot(aes(y = Entity, x = percentage, fill = group,
                    label= paste0(round(percentage),"%"))) +
  geom_col(width=0.7, position = "stack") +
  geom_text(position = position_stack(vjust = 0.8),
            data = water[water$percentage > 10,], size = 4)+
  scale_fill_met_d("Derain", direction=-1)+
  labs(title = title_text,
       caption = caption_text)+
  theme_minimal()+
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(family = body_font, size=12),
    axis.text.x = element_blank(),
  # Legend
  legend.position = "top",
  legend.title = element_blank(),
  legend.key.height= unit(0.5, 'cm'),
  legend.key.width= unit(0.7, 'cm'),
  legend.text = element_text(family = body_font,
                             size=13,
                             face = 'plain',
                             color = "grey10"),
  
  # TITLE
  plot.title.position = "plot",
  plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                            size = 34,
                            family = title_font,
                            face = "bold",
                            width = unit(54, "lines")),
  # Caption
  plot.caption = element_text(family=body_font,
                              face="plain",
                              size=14, 
                              color="grey40",
                              hjust=.5,
                              margin=margin(20,0,0,0)),
  
  plot.background = element_rect(color="white", fill="white"),
  plot.margin = margin(50, 50, 50, 50)
)


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("drinking-water.png",dpi=320,
       width = 12, height = 9)
showtext_auto(FALSE)

