library(tidyverse)
library(showtext)
library(ggtext)


# ------ Get Data ------ 

codes <- readxl::read_xlsx('CountryCodes.xlsx')
regions <-  codes|>
  select('alpha-3', 'region')|>
  rename('Code'='alpha-3')

df <- read.csv("urban.csv")|>
  rename(Code=Country.Code,
         urban_pop = "X2021")

# Left join
urban <- merge(x=df,y=regions, 
            by="Code", all.x=TRUE)|>
  janitor::clean_names()|>
  filter(!is.na(region))

## custom colors
my_pal <- rcartocolor::carto_pal(n = 5, name = "Bold")

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "What Percentage Of The Total Population Lives In Urban Areas?"
subtitle_text <- "Each point represents a different country. Countries in a region with the highest and lowest percentages of the urban population are labeled. Only stats for 2021 are shown."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: World Bank"

# ------ Plot ------ 
  ggplot(urban, aes(x = region, y = urban_pop, color = region, fill = region))+
  geom_point(
  position = position_jitter(width = .2, seed = 0),
  size = 4, alpha = .5) +
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 6, stroke = .9, shape = 1, color = "black") +
  geom_text(data = urban |> 
              group_by(region) |>
              slice_min(order_by=urban_pop, n=1),
            aes(label=country_name),
            size=4.5,
            family = body_font,
            nudge_y = -3,
            nudge_x = 0.15)+
  ggrepel::geom_text_repel(data = urban |> 
              group_by(region) |>
              slice_max(order_by=urban_pop, n=1),
              aes(label=country_name),
              hjust=1, nudge_x = -.15, size=4.5, family=body_font,
              direction = "y", force = 0.5, min.segment.length = 6,
              segment.size=0.5, segment.curvature=0.25, segment.ncp=5,
              segment.angle=120, box.padding = .25)+
  geom_boxplot(
    width = .2, fill = "white",
    size = 1.5, outlier.shape = NA,
    position = position_nudge(x = .40)) +
  scale_y_continuous()+
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  theme_minimal()+
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
    theme(
      axis.title.x  = element_blank(),
      axis.title.y  = element_blank(),
      axis.text.x = element_text(family = body_font, 
                                 face = "bold",
                                 size=15),
      axis.text.y = element_text(family = body_font, 
                                 face = "bold",
                                 size=15),
      
      # Legend
      legend.position = "none",
      
      # TITLE
      plot.title.position = "plot",
      plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                                   size = 32,
                                   family = title_font,
                                   face = "bold",
                                   width = unit(75, "lines")),
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
ggsave("urban_pop.png",dpi=320,
       width = 16, height = 11)
showtext_auto(FALSE)

