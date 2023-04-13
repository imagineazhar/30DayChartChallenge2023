library(tidyverse)
library(showtext)
library(ggtext)


# ------ Get Data ------ 
genre <- read.csv('genre.csv')

ratings <- read.csv("ratings.csv")

# Left join
df <- merge(x=genre,y=ratings, 
               by="dataId", all.x=TRUE)|>
  janitor::clean_names()|>
  filter(release_year==2023,
         votes > 1000,
         content_type == "movie",
         !genre %in% c("Sport", "Documentry","Short", "Romance",
                       "Music", "Biography", "Mystery"),
         !duplicated(title))|>
  select(data_id, genre, title, rating)



# ------ custom colors ------

my_pal <- rcartocolor::carto_pal(n = 12, name = "Bold")

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "2023 Movies: How do they rate? A look at IMDb scores with more than 1000 votes."
subtitle_text <- "Each data point represents a movie, with labels indicating the highest and lowest rated movies within their respective genres."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: IMDb"

# ------ Plot ------ 
ggplot(df, aes(x = genre, y = rating, color = genre, fill = genre))+
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 5, alpha = .5) +
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 7, stroke = .9, shape = 1, color = "black") +
  geom_text(data = df |> 
              group_by(genre) |>
              slice_min(order_by=rating, n=1),
            aes(label=title),
            size=4.5,
            family = body_font,
            nudge_y = -0.25,
            nudge_x = 0.15)+
  ggrepel::geom_text_repel(data = df |> 
                             group_by(genre) |>
                             slice_max(order_by=rating, n=1),
                           aes(label=title),
                           hjust=0.5, size=4.5, family=body_font,
                           force = 0.5, nudge_y = 0.25, min.segment.length = 4,
                           )+
  ggdist::stat_gradientinterval(
    width = .3, color = "black",
    position = position_nudge(x = .40))+
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

