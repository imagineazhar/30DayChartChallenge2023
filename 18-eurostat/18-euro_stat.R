library(tidyverse)
library(ggforce)
library(ggtext)
library(glue)
library(prismatic)
library(showtext)

# ------ Get Data ------ 
codes <- readxl::read_xlsx('CountryCodes.xlsx')
countries <-  codes|>
  select('alpha-2', 'country')|>
  rename('geo'='alpha-2')

df <- read.csv("life_sat.csv")

# Left join
life <- merge(x=df,y=countries, 
            by="geo", all.x=TRUE)|>
  filter(!is.na(country))|>
  janitor::clean_names()|>
  group_by(country, lev_satis)|>
  ungroup()|>
  group_by(country)|>
  mutate(category_nr = row_number())|>
  ungroup()

f = 0.8  # change to change shape of the "balloon"

df_shapes <- life |>
  rowwise()|>
  mutate(
    # Calculate points on circle for the "balloons", we need 4 x-y pairs for geom_bspline_closed
    x = list(c(0,
               f * obs_value * sin(category_nr * 2 * pi / 9 - pi/4),
               obs_value * sin(category_nr * 2 * pi / 9), # real percentage for main "radius"
               f * obs_value * sin(category_nr * 2 * pi / 9 + pi/5),
               0
    )),
    y = list(c(0,
               f * obs_value * cos(category_nr * 2 * pi / 9 - pi/5),
               obs_value * cos(category_nr * 2 * pi / 9), # real percentage for main "radius"
               f * obs_value * cos(category_nr * 2 * pi / 9 + pi/4),
               0
    ))
  )|>
  ungroup()|>
  pivot_wider(id_cols = c(geo, country), names_from = category_nr,
              values_from = c(x,y))|>
  unnest(x_1:y_3)

# Category colors
pal <- c("#469990","#BAD7E9","#EB455F")

# Pull categories from the dataset
cat <- life|>
  distinct(lev_satis) |>
  pull()

# Join colors with categories
pal_df <- data.frame(c = pal, l = cat)


# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Life Satisfaction in Europe"
subtitle_text <- "Percentage of the population rating their satisfaction as high, medium or low."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: Eurostat"

# ------ Plot ------ 
df_shapes |> ggplot()+
  geom_bspline_closed(aes(x_1, y_1, group = country, fill = pal[1]), alpha = 0.8)+
  geom_bspline_closed(aes(x_2, y_2, group = country, fill = pal[2]), alpha = 0.8)+
  geom_bspline_closed(aes(x_3, y_3, group = country, fill = pal[3]), alpha = 0.8)+
  scale_fill_identity(guide = guide_legend(title = "", nrow = 2, 
                                           override.aes = list(alpha = 0.7, shape = 2)),
                      breaks = pal, labels = pal_df$l) +
  guides(fill = guide_legend(nrow = 1,
                             label.position = "top"))+
  coord_fixed() +
  facet_wrap(vars(country), ncol = 6,
             labeller = labeller(country = label_wrap_gen(width = 15)))+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       x="",
       y="")+
  hrbrthemes::theme_ipsum()+
  theme(
    strip.text.x = element_text(family = title_font,
                                face = 'bold',
                                size = 15, colour = "grey20"),
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
                              family = title_font),
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
ggsave("life_satis.png",dpi=320,
       width = 14, height = 12)
showtext_auto(FALSE)

