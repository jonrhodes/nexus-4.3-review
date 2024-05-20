# ------------------------------------------------------------------------
# Script Name: function_nice_heatmap.R
# Author: Améline Vallet  (ameline.vallet@agroparistech.fr)
# Latest update : 20240411
# Description: This script contains a function for plotting nice heatmaps from the clean database in csv format
# ------------------------


#Useful ressources: http://pcwww.liv.ac.uk/~william/R/crosstab.r and https://stackoverflow.com/questions/66996598/ggplot2-heatmap-with-tile-height-and-width-as-aes

#A function with the clean database as input, as well as the variable of interest to plot against nexus dimensions. 
produce_chpt4_heatmaps <- function(Data, var, levels_y, labels_y) {
  nexus <- Data %>%
    filter(name %in% c("Nexus")) %>%
    select(-name, - Title) %>%
    group_by(CovidenceID) %>%
    nest(data = value) %>%
    mutate(nexus_nice = map_chr(.x = data, .f = function(x) {paste(pull(x), collapse = ' ')})) %>%
    select(-data)

  ref_grid <- expand.grid(nexus_nice = unique(nexus$nexus_nice), 
                          var = levels_y)
  
  #Process the data for plotting
  data_graph <- Data %>%
    filter(name == var) %>%
    rename(var = value) %>%
    select(CovidenceID, var) %>%
    full_join(nexus, by = "CovidenceID") %>%
    filter(!is.na(var)) %>%
    group_by(var, nexus_nice) %>%
    summarize(count = n()) %>%
    mutate(var = factor(var, levels = levels_y)) %>%
    transform(percent = 100 * ave(count, nexus_nice, FUN = prop.table)) %>%
    mutate(percent = round(percent, 0)) %>%
    full_join(ref_grid) %>%
    mutate(percent = ifelse(is.na(percent), 0, percent))
  
  #Create artificial data to graphically represent nexus dimensions in the header of the figure
  my_data <- tibble(nexus_nice = unique(nexus$nexus_nice), var = unique(nexus$nexus_nice)) %>%
    mutate(var = str_split(var, " ")) %>%
    unnest(var) %>%
    add_column(count = NA, percent = NA)  
  
  #Combine the artificial data to the data to plot
  combi <- bind_rows(data_graph, my_data) %>%
    mutate(var = factor(var, levels = c(levels_y, rev(c("Biodiversity", "Climate", "Food", "Health", "Water"))))) %>%
    mutate(var_num = as.numeric(var)) %>%
    mutate(nexus_nice = factor(nexus_nice, levels = sort(unique(nexus$nexus_nice)))) %>%
    mutate(nexus_nice_num = as.numeric(nexus_nice)) %>%
    mutate(height = ifelse(var %in% c("Biodiversity", "Climate", "Food", "Health", "Water"), 1, 2)) %>%
    mutate(var = plyr::mapvalues(var, from = levels_y, to = labels_y)) %>%
    arrange(var, nexus_nice_num)
  
  #Find y location of tiles of different heights
  #Adapted from https://stackoverflow.com/questions/66996598/ggplot2-heatmap-with-tile-height-and-width-as-aes
  breaks <- combi %>% 
    select(-nexus_nice, -count, -nexus_nice_num, -percent) %>%
    ungroup() %>%
    distinct() %>%
    mutate(cumw = cumsum(height),
           pos = .5 * (cumw + lag(cumw, default = 0))) %>%
    select(var, pos)
  
  #Add y location information to the data to plot
  combi <- left_join(combi, breaks, by = 'var') 
  
  #Plot the heatmap
  graph <- ggplot(combi, aes(nexus_nice_num, pos, height = height, fill= percent)) + 
    geom_tile() +
    #scale_fill_gradient(low = "#D9AA80", high = "#791E32", limits = c(1, max(data_graph$percent)), na.value = NA, name = "Percentage of \noccurrences") +
    scale_fill_gradient(low = "#D9AA80", high = "#B65719", name = "Percentage of \noccurrences" , na.value = NA) +
    geom_point(data = filter(combi, var == "Biodiversity"), colour = "#C6D68A", shape = 19, size = 4) +
    geom_point(data = filter(combi, var == "Climate"), colour = "#BAB0C9", shape = 19, size = 4) +
    geom_point(data = filter(combi, var == "Food"), colour = "#B65719", shape = 19, size = 4) +
    geom_point(data = filter(combi, var == "Health"), colour = "#791E32", shape = 19, size = 4) +
    geom_point(data = filter(combi, var == "Water"), colour = "#4A928F", shape = 19, size = 4) +
    scale_y_continuous(breaks = unique(breaks$pos), labels = levels(breaks$var), expand = c(0, 0.1)) +
    scale_x_discrete(breaks = sort(unique(nexus$nexus_nice)), limits = sort(unique(nexus$nexus_nice))) +
    geom_text(data = filter(combi, var %in% labels_y), aes(label = percent), colour = "black", size =3) +
    theme_bw() +
    theme(axis.ticks = element_blank(), axis.line = element_blank(), panel.grid = element_blank(), panel.border = element_blank(), 
          axis.title = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(face = c(rep("plain", length(levels_y)), rep("bold.italic", 5)), colour = c(rep("black", length(levels_y)), "#4A928F", "#791E32", "#B65719", "#BAB0C9", "#C6D68A")),
          legend.position = "bottom")
  
  # #with png logos for nexus elements
  # #https://themockup.blog/posts/2020-10-11-embedding-images-in-ggplot/#ggimage-and-aspect-ratio
  # ggplot(combi_ena, aes(nexus_nice_num, pos, height = height, fill= percent)) + 
  #   geom_tile() +
  #   scale_fill_gradient(low = "#D9AA80", high = "#791E32", limits = c(1, max(data_graph5$percent)), na.value = NA,) +
  #   geom_image(data = filter(combi_ena, EnaBar == "Biodiversity"), aes(image = image)) +
  #   geom_image(data = filter(combi_ena, EnaBar == "Climate"), aes(image = image)) +
  #   geom_image(data = filter(combi_ena, EnaBar == "Food"), aes(image = image)) +
  #   geom_image(data = filter(combi_ena, EnaBar == "Health"), aes(image = image)) +
  #   geom_image(data = filter(combi_ena, EnaBar == "Water"), aes(image = image)) +
  #   scale_y_continuous(breaks = unique(breaks$pos), labels = levels(breaks$EnaBar), expand = c(0, 0.1)) +
  #   #scale_x_continuous(breaks = seq(1:length(unique(combi$nexus_nice))), labels = levels(combi$nexus_nice), expand = c(0, 0.1)) +
  #   geom_text(data = filter(combi_ena, EnaBar %in% c("Policy Design/Implementation", "Equity & Diversity", "Institutional capacity", "Financial & Economic", "Behaviour & Lifestyle", "Technology", "Material & Non-material endowments" )), aes(label = percent)) +
  #   theme_bw() +
  #   theme(axis.ticks = element_blank(), axis.line = element_blank(), panel.grid = element_blank(), panel.border = element_blank(), 
  #         axis.title = element_blank(), axis.text.x = element_blank(),
  #         axis.text.y = element_text(face = c(rep("plain", 7), rep("bold.italic", 5)), colour = c(rep("black", 7), "#4A928F", "#791E32", "#B65719", "#BAB0C9", "#C6D68A")))
  
  return(list(graph = graph, data = data_graph))
}

