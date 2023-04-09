library(tidyverse)
library(showtext)
library(ggtext)
library(ggbump)


# ------ Get Data ------ 

df <- read.csv("data-agri.csv")|>
  janitor::clean_names()|>
  group_by(country_name) |>
  mutate(first_top = min(year[rank <= 10]),
         last_top = max(year[rank <= 10]),
         d_first_top = if_else(year == first_top,
                                1,
                                0)) |>
  filter(!is.na(first_top),
         year >= first_top,
         year <= last_top) |>
  ungroup()

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "Top 10 Countries with the Highest % of Agricultural Land Area"
subtitle_text <- ""
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: World Bank"

# ------ Plot ------ 

ggplot(df, aes(x = year, y = rank, group=country_name)) +
  geom_bump(size = 1, color="grey75") +
  geom_bump(data=df|>filter(country_name=="Lesotho"),
            size = 2,
            color="#34BE82") +
  geom_bump(data=df|>filter(country_name=="West Bank and Gaza"),
            size = 2,
            color="#F4A442") +
  geom_point(data = df |> filter(year %in% c(2000,2020)),
    aes(x = year - .1),
    size = 3, color="grey30") +
  geom_text(data = df|>filter(year == min(year)),
            aes(x = year - 0.3, 
                label = paste0(country_name, " ",as.character(rank),".")),
            size = 4,
            fontface = 2,
            hjust = 1) +
  geom_text(data = df|>filter(year == max(year)),
            aes(x = year + 0.1,
                label = paste0(as.character(rank),".",country_name)),
            size = 4, 
            fontface = 2,
            hjust = 0) +
  geom_text(data = df |> filter(d_first_top == 1,
                                year>2000),
            aes(label = country_name, x = year-.2),
            color = "grey10",
            nudge_y = .23,
            nudge_x = -.05,
            size = 4,
            fontface = 2,
            hjust = 0) +
  geom_point(data = df |> filter(d_first_top == 1,
                                 year>2000),
             aes(x = year - .1),
             size = 3, color="grey30") +
    scale_y_reverse()+
    scale_x_continuous(breaks = round(seq(min(df$year), max(df$year),
                                          by = 4), 1),
                       expand=expand_scale(mult=c(0.3,0.3)))+
    theme_void() +
    labs(title = title_text,
       caption = caption_text)+
    theme(
      axis.title.x  = element_blank(),
      axis.title.y  = element_blank(),
      axis.text.x = element_text(family = body_font, face = "bold",
                               size=14),
      axis.text.y = element_blank(),
      
      # Legend
      legend.position = "none",
      
      # TITLE
      plot.title.position = "plot",
      plot.title = element_textbox(margin = margin(20, 0, 30, 0),
                                   size = 30,
                                   hjust = 0.5,
                                   family = title_font,
                                   face = "bold",
                                   width = unit(60, "lines")),
      
      # Caption
      plot.caption = element_text(family=body_font,
                                  face="plain",
                                  size=14, 
                                  color="grey40",
                                  hjust=.5,
                                  margin=margin(30,0,0,0)),
      
      plot.background = element_rect(color="white", fill="white"),
      plot.margin = margin(50, 50, 50, 50)
  )


  # ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("land.png",dpi=320,
       width = 14, height = 10)
showtext_auto(FALSE)

