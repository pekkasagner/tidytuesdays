#Header
library(tidyverse)
library(tidylog)
library(woodworkR)
library(tidytuesdayR)
library(janitor)
library(geofacet)


#Loading data
(tuesdata <- tidytuesdayR::tt_load("2021-05-11")
)

(broadband <- tuesdata$broadband %>% 
                    janitor::clean_names()
)

#Manipulate data and create grouping 
(broadband_mani <- broadband %>%
          mutate(broadband_availability_per_fcc = na_if(broadband_availability_per_fcc, "-")) %>% 
          mutate(broadband_availability_per_fcc = as.numeric(broadband_availability_per_fcc)) %>% 
          group_by(st) %>% 
          mutate(mean_availability = mean(broadband_availability_per_fcc, na.rm = T)) %>% 
          ungroup() %>%
          mutate(mean_availability = round(mean_availability * 100, 2)) %>% 
          mutate(group = cut(mean_availability, 5)) %>% 
          mutate(group = str_replace_all(group, "\\,", " -")) 
)

#Plot
broadband_mani %>% 
          ggplot(aes(broadband_availability_per_fcc, y = 1, 
                     fill = group, color = group)) +
          geom_jitter() +
          geom_boxplot(alpha = 0.3) +
          coord_flip() +
          facet_geo(~ st) +
          scale_fill_woodworkR() +
          scale_color_woodworkR() +
          theme_woodworkR() +
          theme(axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(hjust = 0),
                plot.subtitle = element_text(hjust = 0),
                plot.caption = element_text(hjust = 0)) +
          scale_x_continuous(labels= scales::percent_format(),
                             breaks = seq(0, 1, by = 0.5)) +
          labs(title = "Sorry, could you please repeat what you said?",
               subtitle = "Percent of people per county with access to fixed 
               terrestrial broadband at speeds of\n25 Mbps/3 Mbps as of the end of 2017",
               caption = "Source: Tidytuesday dataset 2021-05-11, provided by Microsoft by way of The Verge.",
               color = "Mean share per state:",
               fill = "Mean share per state:")

ggsave("2021-05-11.png", 
       width = 60, height = 40, units = "cm", dpi = 500)
