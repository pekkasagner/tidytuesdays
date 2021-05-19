#Header
library(tidyverse)
library(tidylog)
library(woodworkR)
library(tidytuesdayR)
library(ineq)
library(sf)
#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) #states with inset Alaska and Hawaii
library(ggpmisc) # to include tables in plot

#Loading data
(tuesdata <- tidytuesdayR::tt_load("2021-05-18")
)

#Data precleaning
(survey <- tuesdata$survey %>% 
                    filter(currency == "USD") %>% #only looking at salaries in USD
                    drop_na(state) %>% #only if state variable is present
                    filter(annual_salary > 0) %>% #remove negative incomes
                    group_by(state) %>% 
                    filter(between(annual_salary, quantile(annual_salary, 0.01), quantile(annual_salary, 0.99))) %>% #top and bottom coding by state
                    ungroup()
)

#Calculating gini-coefficient by state
          #state-variable is pretty messy, we filter out "clean" data in the next steps
(ineq_by_state <- survey %>% 
          group_by(state) %>% 
          summarise(gini = ineq::ineq(annual_salary, type = "Gini"),
                    count = n()) %>% 
          ungroup()
)

#National average
(ineq_total <- survey %>% 
                    summarise(gini = ineq::ineq(annual_salary, type = "Gini"),
                              count = n()) 
)
#Loading states as sf-object
(states_sf <- get_urbn_map("states", sf = TRUE) %>% 
          rmapshaper::ms_simplify() #simplifying geometry for more efficient plotting
)

#Joining shapefile and data 
(data_merged <- states_sf %>% 
          #I only keep direct matches, 
          #not in the mood for cleaning messings strings
          left_join(ineq_by_state, by = c("state_name" = "state")) %>% 
          filter(state_abbv != "DC") %>% # not a state
          mutate(gini = round(gini,4)) %>% 
          mutate(Rank = rank(gini))
)


#Creating the table of top and bottom 5 states by gini coefficient
(table <- data_merged %>% 
          as_tibble() %>% 
          select(Rank, state_name, gini) %>% 
          filter(Rank > max(Rank) - 5 | Rank < min(Rank) + 5) %>% 
          arrange(Rank) %>% 
          rename("State rank by\ngini coefficient" = Rank, "State" = state_name, "Gini coefficient" = gini) %>% 
          mutate_all(as.character) %>% 
          add_row("State rank by\ngini coefficient" = "...", "State" = "...", "Gini coefficient" = "...", .before = 6)
)

#Creating custom text and background colors for tables
custom_table_background_palette <- c("#2c3227", "#d3d9ce", "#2c3227", "#d3d9ce", "#2c3227",
                          "#ffffff", 
                          "#deccba", "#f7f3ef", "#deccba", "#f7f3ef", "#deccba")
          
custom_table_text_palette <- c("#deccba", "#2c3227", "#deccba", "#2c3227", "#deccba",
                          "black", 
                          "#2c3227", "#2c3227", "#2c3227", "#2c3227", "#2c3227")



#Setting a theme for the table
custom_theme <- ttheme_gtminimal(
          core=list(bg_params = list(fill = custom_table_background_palette),
                    fg_params=list(fontface = 1L, col = custom_table_text_palette)),
          colhead=list(fg_params=list(col = "#2c3227", fontface= 1)))


#map with table
map <- data_merged %>% 
          ggplot() +
          geom_sf(aes(geometry = geometry, fill = gini)) +
          geom_sf_text(aes(label = state_abbv), 
                       size = 3, color = "white") +
          scale_fill_gradient(high = "#deccba", low = "#2c3227") +
          theme_void() +
          theme(plot.margin = margin(0,0,0,2, unit = "in"),
                legend.position = c(-0.08,0.35),
                plot.caption = element_text(hjust = - 0.25),
                plot.title = element_text(hjust = - 0.23),
                plot.subtitle = element_text(hjust = - 0.835),
                plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
                plot.caption.position =  "plot") + #NEW parameter
          ggpmisc::geom_table(x = 1, y = 1, vjust = 0.6, hjust = 2.25, 
                              label = list(table),
                              table.theme =  custom_theme) +
          labs(title = "Inequality among annual salaries in the US",
               subtitle = "Gini coefficients of annual salaries by states. The higher the gini coefficient, the higher the inequality among the salaries.",
               caption = "Source: Tidytuesday dataset 2021-05-18, provided by Ask A Manager. Graph by @pekkato.",
               fill = "Gini coefficient:") +
          guides(fill = guide_colorbar(reverse = TRUE), #reverse color order (higher value on top)
                 size = guide_legend(reverse = TRUE)) #reverse size order (higher diameter on top) 

#Saving the plot
ggsave(map, filename = "2021-05-18.png",
       width = 30, height = 20, units = "cm", dpi = 500)
