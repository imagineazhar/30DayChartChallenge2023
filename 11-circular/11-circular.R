library(tidyverse)
library(showtext)
library(ggtext)
library(usefunc)
library(packcircles)
library(MetBrewer)
# ------ Get Data ------ 

df <- read.csv("space.csv")|>
  janitor::clean_names()

space <- df|>
  filter(year==2022,
         entity!="World",
         !is.na(code))

label <- character(length = nrow(space))
for (i in 1:nrow(space)){
  if (space$entity[i] %in% c("United States", "China", "Russia", "United Kingdom")){
    label[i] = paste0(space$entity[i], "\n\n", format(space$yearly_launches[i],
                                                         big.mark=",", trim=TRUE))
  } else {
    label[i] = space$entity[i]
  }
}
space$label <- label

# ------  pack circles ------ 
packing <- circleProgressiveLayout(space$yearly_launches, sizetype='area')
data <- cbind(space, packing)
space_2 <- circleLayoutVertices(packing, npoints=50)
space_2$n <- rep(space$yearly_launches, each=51)

# ------ Typography ------ 

font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"

# ------ Texts ------ 

title_text <- "United States has launched over 1700 objects into space in 2022."
subtitle_text <- "Objects include satellites, probes, landers, crewed spacecrafts, and space station flight elements launched into Earth orbit or beyond.\n\nThe area of each circle represented the number of space objects launched by each country in 2022."
caption_text <- "Graphic: Muhammad Azhar | #30DayChartChallenge | Data: UNOOSA"

# ------ Plot ------ 
  ggplot()+
  geom_polygon(data = space_2,
               mapping = aes(x, y, group = id, fill=n),
               colour = "white",
               alpha = 1) +
  geom_text(data = data,
            mapping = aes(x, y, size = yearly_launches, 
                          label = paste0(label)),
            family=body_font,
            colour = "white",
            lineheight = 0.5, fontface = "bold") +
  scale_size_continuous(range = c(2,10)) +
  scale_fill_gradientn(colors=met.brewer("Cross", direction = 1)) +
  coord_fixed() +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text)+
    theme_void()+
    theme(
      axis.title.x  = element_blank(),
      axis.title.y  = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      
      # Legend
      legend.position = "none",
      
      # TITLE
      plot.title.position = "plot",
      plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                                   size = 26,
                                   hjust=0,
                                   family = title_font,
                                   face = "bold",
                                   width = unit(30, "lines")),
      # SUB-TITLE
      plot.subtitle = element_textbox(margin = margin(10, 0, 10, 0),
                                   size = 15,
                                   hjust=0,
                                   lineheight = 0.5,
                                   color = "grey30",
                                   family = body_font,
                                   width = unit(30, "lines")),
      
      # Caption
      plot.caption = element_text(family=body_font,
                                  face="plain",
                                  size=12, 
                                  hjust = 0,
                                  color="grey40",
                                  margin=margin(30,0,0,0)),
      
      plot.background = element_rect(color="white", fill="white"),
      plot.margin = margin(30, 85, 30, 50)
  )


# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("space_objects.png",dpi=320,
       width = 8, height = 11.2)
showtext_auto(FALSE)

