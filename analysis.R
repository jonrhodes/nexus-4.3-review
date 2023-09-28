# load packages
library(tidyverse)
library(ggalluvial)
#library(dplyr)
#library(stringr)
library(gplots)
#library(tidyr)
library(pheatmap)
library(stringdist)
library(sf)
library(RColorBrewer)
#library(ggplot2)

# load functions
source("functions.R")

# load review data from covidence
Data <- read_csv("review_283729_20230922170037.csv")

# extract data on paper types, regions, scale, nexus elements, nexus challnenges
# governance types, policy instruments, actors, and cross cutting issues
Data_Select <- Data %>%
              select(CovidenceID = `Covidence #`, Title, PaperType = `What type of paper is this?`, Region = `Select all geographic regions the paper focusses on according to UN standard area codes (https://unstats.un.org/unsd/methodology/m49/)`, Scale = `Select the relevant spatial scales (extent) of the study. Choose all that apply.`, Nexus = `Which nexus elements are considered?`, NChallenge = `Does the study provide evidence for addressing any of the following nexus challenges?`, Gov = `Are any of the following governance approaches proposed or assessed as solutions to the above nexus challenges? Use your judgement to select one of the four governance approaches listed and then use the \"other\" category to list any specific governance approaches referred to in the study (separate multiple governance approaches with \",\")`, Policy = `What type of policy instruments are considered to operationalise the response options proposed or assessed?`, Actors = `Which types of actors are involved in the implementation of the response options proposed or assessed?`, CrossCut = `Which of the following cross cutting issues are considered?`)

# save data for manual error checking - we used this to ckeck for errors and correct in
# covidence where necessary
write_csv(Data_Select, "error_check.csv")

# split multiple responses for the same paper
Data_Select_Split <- Data_Select %>%
              mutate(PaperType = str_split(str_squish(PaperType),"; ")) %>%
              mutate(Region = str_split(str_squish(Region),"; ")) %>%
              mutate(Scale = str_split(str_squish(Scale),"; ")) %>%
              mutate(Nexus = str_split(str_squish(Nexus),"; ")) %>%
              mutate(NChallenge = str_split(str_squish(NChallenge),"; ")) %>%
              mutate(Gov = str_split(str_squish(Gov),"; ")) %>%
              mutate(Policy = str_split(str_squish(Policy),"; ")) %>%
              mutate(Actors = str_split(str_squish(Actors),"; ")) %>%
              mutate(CrossCut = str_split(str_squish(CrossCut),"; "))

# remove any further leading or trailing white space
Data_Select_Split <- Data_Select_Split %>%
              mutate(PaperType = map(PaperType, .f = str_squish)) %>%
              mutate(Region = map(Region, .f = str_squish)) %>%
              mutate(Scale = map(Scale, .f = str_squish)) %>%
              mutate(Nexus = map(Nexus, .f = str_squish)) %>%
              mutate(NChallenge = map(NChallenge, .f = str_squish)) %>%
              mutate(Gov = map(Gov, .f = str_squish)) %>%
              mutate(Policy = map(Policy, .f = str_squish)) %>%
              mutate(Actors = map(Actors, .f = str_squish)) %>%
              mutate(CrossCut = map(CrossCut, .f = str_squish))

# keep "other" category from paper types but remove the term "Other: " from responses
# - change code here to avoid this or to do something else
Data_Select_Split <- Data_Select_Split %>% mutate(PaperType = map(PaperType,
                        .f = function(x) {if (all(is.na(x))) {return(NA)} else
                          {y <- as_tibble(x) %>% rename(PaperType = value) %>%
                          mutate(PaperType = ifelse(str_detect(PaperType, fixed("other",
                                ignore_case = TRUE)), str_remove(PaperType, "Other: "), PaperType));
                          if (all(is.na(y$PaperType))) {return(as.vector(y$PaperType))} else
                          {return(as.vector(filter(y, !is.na(PaperType))$PaperType))}
                        }}))

# recategorise nexus challenges - note that this removes "other" responses - change code here to avoid this or to do something else

# get look up table so as to rename nexus challenges with correct names
LookupNC <- unique(unlist(Data_Select_Split$NChallenge))[unique(unlist(Data_Select_Split$NChallenge)) %>%
          str_detect(fixed("other", ignore_case = TRUE), negate = TRUE) %>% which()]
LookupNC <- LookupNC %>% as_tibble() %>% rename(NChallenge = value) %>% mutate(NChallenge_New = NA) %>%
              mutate(NChallenge_New = case_when(
                str_detect(NChallenge, "Complexity") ~ "Complexity Challenges",
                str_detect(NChallenge, "Values") ~ "Values Challenges",
                str_detect(NChallenge, "Governance") ~ "Governance Challenges",
                str_detect(NChallenge, "Financial") ~ "Financing Challenges",
                str_detect(NChallenge, "Knowledge") ~ "Scaling Challenges"
              ))

# write to csv
write_csv(LookupNC, "nchallenges_lookup.csv")

# rename nexus challenges
Data_Select_Split <- Data_Select_Split %>% mutate(NChallenge = map(NChallenge,
                     .f = function(x) {y <- as_tibble(x) %>% left_join(LookupNC, by = join_by(value == NChallenge)) %>% select(-value);
                       if (all(is.na(y$NChallenge_New))) {return(as.vector(y$NChallenge_New))} else
                       {return(as.vector(filter(y, !is.na(NChallenge_New))$NChallenge_New))}}))

# recategorise governance appraoches - note that this creates new categories based on the "other" responses - change code here to avoid this or to do something else
# note also that this only used governance appraoches listed in Table 4.4 of the chapter - all other govenance appraoches are ignored

# get look up table so as to rename governance types
LookupGov <- unique(unlist(Data_Select_Split$Gov)) %>% as_tibble() %>%
                mutate(value = str_remove(value, fixed("other: ", ignore_case = TRUE))) %>%
                mutate(value = str_squish(value)) %>% mutate(value = str_split(value, ",")) %>%
                mutate(value = map(value, .f = str_squish))
LookupGov <-  unique(unlist(LookupGov$value)) %>% as_tibble() %>%
                rename(Gov = value) %>% mutate(Gov_New = NA) %>% mutate(Gov_New = case_when(
                  str_detect(Gov, fixed("hierarchical", ignore_case = TRUE)) ~ "Hierarchical Governance",
                  str_detect(Gov, fixed("market", ignore_case = TRUE)) ~ "Market Governance",
                  str_detect(Gov, fixed("network", ignore_case = TRUE)) ~ "Network Governance",
                  str_detect(Gov, fixed("good", ignore_case = TRUE)) ~ "Good Governance",
                  str_detect(Gov, fixed("multi-level", ignore_case = TRUE)) ~ "Multi-level Governance",
                  str_detect(Gov, fixed("multilevel", ignore_case = TRUE)) ~ "Multi-level Governance",
                  str_detect(Gov, fixed("community", ignore_case = TRUE)) ~ "Community Governance",
                  str_detect(Gov, fixed("transformative", ignore_case = TRUE)) ~ "Transformative Governance",
                  str_detect(Gov, fixed("polycentric", ignore_case = TRUE)) ~ "Polycentric Governance",
                  str_detect(Gov, fixed("nested", ignore_case = TRUE)) ~ "Hierarchical Governance",
                  str_detect(Gov, fixed("reflexive", ignore_case = TRUE)) ~ "Reflexive Governance",
                  str_detect(Gov, fixed("adaptive", ignore_case = TRUE)) ~ "Adaptive Governance"
                ))

# write to csv
write_csv(LookupGov, "governance_lookup.csv")

# rename governance types
Data_Select_Split <- Data_Select_Split %>% mutate(Gov = map(Gov,
                     .f = function(x) {y <- as_tibble(x) %>%
                        mutate(value = str_remove(value, fixed("other: ", ignore_case = TRUE))) %>%
                        mutate(value = str_squish(value)) %>% mutate(value = str_split(value, ",")) %>%
                        mutate(value = map(value, .f = str_squish)); y <- unlist(y) %>% as_tibble() %>%
                        left_join(LookupGov, by = join_by(value == Gov)) %>% select(-value);
                        if (all(is.na(y$Gov_New))) {return(as.vector(y$Gov_New))} else
                          {return(as.vector(filter(y, !is.na(Gov_New))$Gov_New))}
                     }))

# remove "other" category from policy instruments - change code here to avoid this or to do something else
Data_Select_Split <- Data_Select_Split %>% mutate(Policy = map(Policy,
                        .f = function(x) {if (all(is.na(x))) {return(NA)} else
                          {y <- as_tibble(x) %>% rename(Policy = value) %>%
                          mutate(Policy = ifelse(str_detect(Policy, fixed("other",
                                                                                ignore_case = TRUE)), NA, Policy));
                          if (all(is.na(y$Policy))) {return(as.vector(y$Policy))} else
                          {return(as.vector(filter(y, !is.na(Policy))$Policy))}
                        }}))

# recategorise actors - note that this removes "other" responses - change code here to avoid this or to do something else

# get look up table so as to rename actors with new actor categories
LookupAct <- unique(unlist(Data_Select_Split$Actors))[unique(unlist(Data_Select_Split$Actors)) %>%
          str_detect(fixed("other", ignore_case = TRUE), negate = TRUE) %>% which()]
LookupAct <- LookupAct %>% as_tibble() %>% rename(Actors = value) %>% mutate(Actors_New = NA) %>%
              mutate(Actors_New = case_when(
                str_detect(Actors, "Private") ~ "Private Sector and Business Organisations",
                str_detect(Actors, "Funders") ~ "Financial Institutions",
                str_detect(Actors, "NGOs") ~ "Civil Society and Community-Based Organisations",
                str_detect(Actors, "Multilateral Organisations") ~ "Global/Regional Institutions and Science-Policy Interfaces",
                str_detect(Actors, "Civil Society") ~ "Civil Society and Community-Based Organisations",
                str_detect(Actors, "Media") ~ "Media and the Arts",
                str_detect(Actors, "IPLCs") ~ "IPLCs",
                str_detect(Actors, "Regional Organisations") ~ "Global/Regional Institutions and Science-Policy Interfaces",
                str_detect(Actors, "Governments") ~ "Local/National Governments and Municipalities",
                str_detect(Actors, "Academia") ~ "Knowledge and Educational Communities"
              ))

# write to csv
write_csv(LookupAct, "actors_lookup.csv")

# rename actors
Data_Select_Split <- Data_Select_Split %>% mutate(Actors = map(Actors,
                     .f = function(x) {y <- as_tibble(x) %>% left_join(LookupAct, by = join_by(value == Actors)) %>% select(-value);
                       if (all(is.na(y$Actors_New))) {return(as.vector(y$Actors_New))} else
                       {return(as.vector(filter(y, !is.na(Actors_New))$Actors_New))}}))

# recategorise cross-cutting issues - note that this creates new categories based on the "other" responses - change code here to avoid this or to do something else

# get look up table so as to rename cross cutting issue types
LookupCC <- unique(unlist(Data_Select_Split$CrossCut)) %>% as_tibble() %>%
                mutate(value = str_remove(value, fixed("other: ", ignore_case = TRUE))) %>%
                mutate(value = str_squish(value)) %>% mutate(value = str_split(value, ",")) %>%
                mutate(value = map(value, .f = str_squish))
LookupCC <-  unique(unlist(LookupCC$value)) %>% as_tibble() %>%
                rename(CrossCut = value) %>% mutate(CrossCut_New = NA) %>% mutate(CrossCut_New = case_when(
                  str_detect(CrossCut, fixed("equity", ignore_case = TRUE)) ~ "Equity",
                  str_detect(CrossCut, fixed("poverty", ignore_case = TRUE)) ~ "Poverty",
                  str_detect(CrossCut, fixed("economic", ignore_case = TRUE)) ~ "Economy",
                  str_detect(CrossCut, fixed("employment", ignore_case = TRUE)) ~ "Employment",
                  str_detect(CrossCut, fixed("indigenous", ignore_case = TRUE)) ~ "ILK",
                  str_detect(CrossCut, fixed("education", ignore_case = TRUE)) ~ "Education",
                  str_detect(CrossCut, fixed("energy", ignore_case = TRUE)) ~ "Energy",
                  str_detect(CrossCut, fixed("mining", ignore_case = TRUE)) ~ "Mining",
                  str_detect(CrossCut, fixed("waste", ignore_case = TRUE)) ~ "Waste",
                  str_detect(CrossCut, fixed("peace", ignore_case = TRUE)) ~ "Armed Conflict",
                  str_detect(CrossCut, fixed("transport", ignore_case = TRUE)) ~ "Infrastructure",
                  str_detect(CrossCut, fixed("trade", ignore_case = TRUE)) ~ "Trade",
                  str_detect(CrossCut, fixed("justice", ignore_case = TRUE)) ~ "Justice",
                  str_detect(CrossCut, fixed("land", ignore_case = TRUE)) &
                    !str_detect(CrossCut, fixed("policy landscape", ignore_case = TRUE))  ~ "Land",
                  str_detect(CrossCut, fixed("LULC", ignore_case = TRUE)) ~ "Land",
                  str_detect(CrossCut, fixed("gender", ignore_case = TRUE)) ~ "Gender",
                  str_detect(CrossCut, fixed("political", ignore_case = TRUE)) ~ "Politics and Democracy",
                  str_detect(CrossCut, fixed("security", ignore_case = TRUE)) ~ "Security",
                  str_detect(CrossCut, fixed("tourism", ignore_case = TRUE)) ~ "Tourism",
                  str_detect(CrossCut, fixed("resilience", ignore_case = TRUE)) ~ "Resilience",
                  str_detect(CrossCut, fixed("social cohesion", ignore_case = TRUE)) ~ "Social Cohesion",
                  str_detect(CrossCut, fixed("power", ignore_case = TRUE)) ~ "Power Dynamics",
                  str_detect(CrossCut, fixed("energy", ignore_case = TRUE)) ~ "Energy",
                  str_detect(CrossCut, fixed("emergency response", ignore_case = TRUE)) ~ "Disaster Recovery",
                  str_detect(CrossCut, fixed("infrastructure", ignore_case = TRUE)) ~ "Infrastructure",
                  str_detect(CrossCut, fixed("flood recovery", ignore_case = TRUE)) ~ "Disaster Recovery",
                  str_detect(CrossCut, fixed("emergency response", ignore_case = TRUE)) ~ "Disaster Recovery",
                  str_detect(CrossCut, fixed("population growth", ignore_case = TRUE)) ~ "Population Growth",
                  str_detect(CrossCut, fixed("corruption", ignore_case = TRUE)) ~ "Corruption",
                  str_detect(CrossCut, fixed("livelihoods", ignore_case = TRUE)) ~ "Livelihoods",
                  str_detect(CrossCut, fixed("urbanization", ignore_case = TRUE)) ~ "Urbanisation",
                  str_detect(CrossCut, fixed("migration", ignore_case = TRUE)) ~ "Migration",
                  str_detect(CrossCut, fixed("soil", ignore_case = TRUE)) ~ "Land",
                  str_detect(CrossCut, fixed("safety", ignore_case = TRUE)) ~ "Security",
                  str_detect(CrossCut, fixed("services", ignore_case = TRUE)) ~ "Infrastructure",
                  str_detect(CrossCut, fixed("civil rights", ignore_case = TRUE)) ~ "Civil Rights",
                  str_detect(CrossCut, fixed("democracy", ignore_case = TRUE)) ~ "Politics and Democracy",
                  str_detect(CrossCut, fixed("rule of law", ignore_case = TRUE)) ~ "Rule of Law",
                  str_detect(CrossCut, fixed("policy landscape", ignore_case = TRUE)) ~ "Politics and Democracy"
                ))

# write to csv
write_csv(LookupCC, "crosscut_lookup.csv")

# rename cross-cutting issue types
Data_Select_Split <- Data_Select_Split %>% mutate(CrossCut = map(CrossCut,
                     .f = function(x) {y <- as_tibble(x) %>%
                        mutate(value = str_remove(value, fixed("other: ", ignore_case = TRUE))) %>%
                        mutate(value = str_squish(value)) %>% mutate(value = str_split(value, ",")) %>%
                        mutate(value = map(value, .f = str_squish)); y <- unlist(y) %>% as_tibble() %>%
                        left_join(LookupCC, by = join_by(value == CrossCut)) %>% select(-value);
                        if (all(is.na(y$CrossCut_New))) {return(as.vector(y$CrossCut_New))} else
                          {return(as.vector(filter(y, !is.na(CrossCut_New))$CrossCut_New))}
                     }))

# summarise paper types

# get which papers are reviews
# and count them
Count_Review <- map(Data_Select_Split$PaperType, .f = function(x)
      {any(x == "Review")}) %>%
      unlist() %>% which() %>% length()
Count_Review

# get which papers are only perspective, cocceptual/theoretical, or opinion papers
# and count them
Count_Pers_Concept_Opinion_Only <- map(Data_Select_Split$PaperType, .f = function(x)
      {any((x == "Perspective") | (x == "Conceptual/theoretical") | (x == "Opinion")) &
      !any((x == "Review") | (x == "Empirical") | (x == "Modelling"))}) %>%
      unlist() %>% which() %>% length()
Count_Pers_Concept_Opinion_Only

# get which papers which contain empirical data
# and count them
Count_Empirical <- map(Data_Select_Split$PaperType, .f = function(x)
      {any(x == "Empirical")}) %>%
      unlist() %>% which() %>% length()
Count_Empirical

# get which papers which contain modelling studies
# and count them
Count_Modelling <- map(Data_Select_Split$PaperType, .f = function(x)
      {any(x == "Modelling")}) %>%
      unlist() %>% which() %>% length()
Count_Modelling

# identify which papers are assessments or reports
# and count them
Count_Assessment <- map(Data_Select_Split$PaperType, .f = function(x)
      {any(str_detect(x, fixed("assessment", ignore_case = TRUE)) |
      str_detect(x, fixed("report", ignore_case = TRUE)))}) %>%
      unlist() %>% which() %>% length()
Count_Assessment

# summarise regions

# get counts
Regions <- unlist(Data_Select_Split$Region) %>% as_tibble() %>%
      mutate(Region = ifelse(is.na(value), "Not applicable (no specific spatial location)", value)) %>%
        count(Region)

# write to csv
write_csv(Regions, "regions_counts.csv")

# summarise spatial scales

# get counts
Scales <- unlist(Data_Select_Split$Scale) %>% as_tibble() %>%
      mutate(Scale = ifelse(is.na(value), "Not applicable (no specific spatial location)", value)) %>%
        count(Scale)

# write to csv
write_csv(Regions, "scales_counts.csv")

# summarise nexus challenges

# get counts
NChallenges <- unlist(Data_Select_Split$NChallenge) %>% as_tibble() %>%
      mutate(NChallenge = value) %>%
      count(NChallenge)

# write to csv
write_csv(NChallenges, "nchallenges_counts.csv")

# summarise nexus elements -  remove "other" responses

# get counts of each nexus element
Nexuses <- unlist(Data_Select_Split$Nexus) %>% as_tibble() %>%
      mutate(Nexus = value) %>% count(Nexus)

# write to csv
write_csv(Nexuses, "nexuses_counts.csv")

# get counts of the numbers of nexus elements considered
NumNexuses <- Data_Select_Split$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% count(NumNexus)

# write to csv
write_csv(NumNexuses, "num_nexuses_counts.csv")

# get median number of nexus elements considered
Data_Select_Split$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% summarise(across(NumNexus, \(x) median(x, na.rm = TRUE)))

# get mean number of nexus elements considered
Data_Select_Split$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% summarise(across(NumNexus, \(x) mean(x, na.rm = TRUE)))

# summarise governance types

# get counts of each governace type considered
Govs <- unlist(Data_Select_Split$Gov) %>% as_tibble() %>%
      mutate(Gov = value) %>% count(Gov)

# write to csv
write_csv(Govs, "governances_counts.csv")

# summarise policy instruments

# get counts of each polcy instrument considered
Policies <- unlist(Data_Select_Split$Policy) %>% as_tibble() %>%
      mutate(Policy = value) %>% count(Policy)

# write to csv
write_csv(Policies, "policies_counts.csv")

# create some plots

# get alluvial plot of nexus challenges versus nexus elements

# get all combinations of nexus challenges and nexus elements for each study
Challenges_Nexuses <- Data_Select_Split$NChallenge %>% map2(Data_Select_Split$Nexus,
                       .f = get_challenge_nexus) %>% map(.f = function(x) {
                         y <- length(unique(x$Nexus));
                         return(x %>% mutate(NumNexus = as.character(y)))})
# row bind list and remove NA values
Challenges_Nexuses <- do.call(bind_rows, Challenges_Nexuses) %>%
      filter(!is.na(NChallenge))
# group by challenges and nexus elements and calculatye the frequency of each unique
# combination
Challenges_Nexuses <- Challenges_Nexuses %>% group_by(NChallenge, Nexus, NumNexus) %>%
    summarize(Freq = n()) %>% ungroup()

# create alluvial plot
ggplot(Challenges_Nexuses, aes(y = Freq, axis1 = NChallenge, axis2 = Nexus)) +
    geom_alluvium(aes(fill = NumNexus)) +
    geom_stratum() +
    geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Nexus Challenges", "Nexus Elements"),
                   expand = c(0.05, 0.05)) +
    theme_void() +
    guides(fill = guide_legend(title = "Number of Nexus Elements")) +
    theme(legend.position = "bottom")

ggsave("challenges_nexus_alluvial.jpg", width = 20, height = 10, units = "cm")

# create heatmap plots for figure on governance, policy instruments, and nexus challenges

# get all unique nexus challenges
Unique_Challenges <- sort(unique(unlist(Data_Select_Split$NChallenge)))[c(1, 3, 5, 4, 2)]

# get all unique governance types
Unique_Governance <- sort(unique(unlist(Data_Select_Split$Gov)))[c(4, 5, 7, 2, 6, 8, 10, 1, 9, 3)]

# get all unique policy instruments
Unique_Policy <- sort(unique(unlist(Data_Select_Split$Policy)))[c(1, 2, 4, 3)]

Matrix_List <- list()
# loop through unique challenges and get governance types and policy instruments combinations
for (i in 1:length(Unique_Challenges)) {

  # get rows in data that match this challenge
  ThisChallenge <- Data_Select_Split[which(unlist(Data_Select_Split$NChallenge %>% map(.f = function(x) {return(any(x == Unique_Challenges[i]))}))),]

  # get the cross-tabbed matrix of governance approaches versus policy intruments for this challenge
  # ensure missing categories are included by using factors
  Matrix_List[[i]] <- get_crosstab(Data = ThisChallenge, Var1 = "Gov", Var2 = "Policy", Merge1 = FALSE,
                        Merge2 = FALSE, Factors1 = rev(Unique_Governance), Factors2 = Unique_Policy)
}

# give list entries names
names(Matrix_List) <- Unique_Challenges

# loop through list and save plots
for (i in 1:length(Unique_Challenges)) {

  PlotData <- as_tibble(Matrix_List[[i]]) %>% mutate(Gov = factor(Gov, levels = rev(Unique_Governance)), Policy = factor(Policy, levels = Unique_Policy))

  ggplot(PlotData, aes(Policy, Gov, col = n, fill = n, label = n)) +
    geom_tile(color = "white", lwd = 4, linetype = 1) +
    geom_text(col = "black") +
    theme_minimal() +
    scale_fill_gradientn(colours = c("grey", "yellow", "purple"), limits = c(0, 35)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")
  ggsave(paste(names(Matrix_List)[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")
}

# create a special zeros plot
PlotData <- as_tibble(Matrix_List[[1]]) %>% mutate(Gov = factor(Gov, levels = rev(Unique_Governance)), Policy = factor(Policy, levels = Unique_Policy)) %>% mutate(n = 0)

ggplot(PlotData, aes(Policy, Gov, col = n, fill = n, label = n)) +
   geom_tile(color = "white", lwd = 4, linetype = 1) +
   geom_text(col = "black") +
   theme_minimal() +
   scale_fill_gradientn(colours = c("grey"), limits = c(0, 0)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")

ggsave("Blank.jpg", width = 10, height = 10, units = "cm")

# create a special template plot
PlotData <- as_tibble(Matrix_List[[1]]) %>% mutate(Gov = factor(Gov, levels = rev(Unique_Governance)), Policy = factor(Policy, levels = Unique_Policy)) %>% mutate(n = 0)

ggplot(PlotData, aes(Policy, Gov, col = n, fill = n, label = n)) +
   geom_tile(color = "white", lwd = 4, linetype = 1) +
   theme_minimal() +
   scale_fill_gradientn(colours = c("grey"), limits = c(0, 0)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")

ggsave("Template.jpg", width = 10, height = 10, units = "cm")

#Code to create figure 4.11 - Geographic distribution of studies
#Can download shp file here (not official UN data): https://thematicmapping.org/downloads/world_borders.php
#Can download the official UN shpfile here (but it only goes to region, not the subregions that we use): https://data.unhabitat.org/datasets/GUO-UN-Habitat::m49-regions/about
#Can download the UN country to region table here: https://unstats.un.org/unsd/methodology/m49/overview/
# https://data.unhabitat.org/search?collection=Dataset&q=M49%20regions
# Read the countries shapefile 
shp_data <- st_read("./data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
# Rename the column in shp_data to match the UN table
shp_data <- shp_data %>%
  rename(ISO.alpha3.Code = ISO3)
# Read the UN table file into a data frame
csv_data <- read.csv("./data/UNSD — Methodology.csv", sep = ";")
#Merge together
merged_data <- merge(shp_data, csv_data, by = "ISO.alpha3.Code")

# Read in the region counts from review
region_counts <- read.csv("./regions_counts.csv")
# Rename region column name to match UN table
colnames(region_counts)[colnames(region_counts) == "Region"] <- "Sub.region.Name"
#Merge together
merged_data <- merge(merged_data, region_counts, by = "Sub.region.Name")

# Define the color scale from red (highest) to yellow (lowest)
color_scale <- scale_fill_gradient(low = "#B2D78C", high = "#67518A")

# Plot the shapefile without displaying country polygon borders
my_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = n), color = "NA") +  
  color_scale +
  labs(title = "Number of studies in each region", fill = "Count") +
  theme_minimal()

# Export the ggplot as a PNG image
ggsave(filename = "./studies_region_counts.png", plot = my_plot, width = 6, height = 4, dpi = 300)


#Code to create figure 4.14 - heatmap showing nexus elements, governance types and policy instruments
# Read the Review data
csv_data <- Data
governance_lookup <- read.csv("./governance_lookup.csv")

# Define the list of elements
elements <- c("Biodiversity", "Climate", "Health", "Food", "Water")
# Generate all possible combinations of elements
all_combinations <- unlist(sapply(1:length(elements), function(n) combn(elements, n, paste, collapse = "; ")))

#Need to use this later on for merging the generated tables together
table_names_policy <- c()
table_names_governance <- c()
#combo <- all_combinations[2:2]
# Create and name dataframes for each combination
for (combo in all_combinations) {
  # Define a unique name for each dataframe based on the combination
  df_name <- paste("subset_", gsub("[^[:alnum:]]", "_", combo), sep = "")
  # Filter for exact matches of the combination
  subset_df <- csv_data %>%
    filter(`Which.nexus.elements.are.considered.` == combo)
  # Assign the dataframe to the dynamically generated name
  assign(df_name, subset_df)
  # Print or do something with the subset DataFrame
  # For example, print the first few rows
  # cat("Subset for combination:", combo, "\n")
  # print(head(get(df_name)))
  df <- get(df_name)
  # Extract the columns of interest
  column_data_policy <- df$`What.type.of.policy.instruments.are.considered.to.operationalise.the.response.options.proposed.or.assessed.`
  column_data_governance <- df$Are.any.of.the.following.governance.approaches.proposed.or.assessed.as.solutions.to.the.above.nexus.challenges..Use.your.judgement.to.select.one.of.the.four.governance.approaches.listed.and.then.use.the..other..category.to.list.any.specific.governance.approaches.referred.to.in.the.study..separate.multiple.governance.approaches.with.....
  # Split the elements in each row by semicolon
  split_elements_policy <- strsplit(column_data_policy, "; ")
  split_elements_governance <- strsplit(column_data_governance, "; ")
  # Flatten the list of split elements
  all_elements_policy <- unlist(split_elements_policy)
  all_elements_governance <- unlist(split_elements_governance)
  
  # Create a frequency table for policy
  table_result_policy <- table(all_elements_policy)
  table_df_policy <- as.data.frame(table_result_policy)
  
  if (nrow(table_df_policy)>0){
    # Convert the frequency table to a data frame
    # Rename the columns for clarity
    colnames(table_df_policy) <- c("Policy Instrument", "Count")
  } else{
    table_df_policy <- table_df_policy
  }
  # Define the name for the table as "table." followed by df_name
  table_name_pol <- paste("tablepol.", df_name, sep = "")
  # Assign the table to the name
  assign(table_name_pol, table_df_policy)
  # Display the resulting table
  print(table_df_policy)
  table_names_policy <- c(table_names_policy, table_name_pol)
  
  # Create a frequency table for policy
  table_result_governance <- table(all_elements_governance)
  table_df_governance <- as.data.frame(table_result_governance)
  
  if (nrow(table_df_governance)>0){
    # Convert the frequency table to a data frame
    # Rename the columns for clarity
    colnames(table_df_governance) <- c("Governance type", "Count")
  } else{
    table_df_governance <- table_df_governance
  }
  # Define the name for the table as "table." followed by df_name
  table_name_gov <- paste("tablegov.", df_name, sep = "")
  # Assign the table to the name
  assign(table_name_gov, table_df_governance)
  # Display the resulting table
  print(table_df_governance)
  table_names_governance <- c(table_names_governance, table_name_gov)
}

#Put the tables in the correct format
# Create an empty list to store the tables
table_list <- list()

# Loop through each table name
for (table_name in table_names_policy) {
  table_data <- get(table_name)
  if (nrow(table_data)>0){
    # Pivot the table
    pivot_table <- pivot_wider(table_data, 
                               names_from = "Policy Instrument",
                               values_from = "Count",
                               values_fill = 0)  # Fill missing values with 0
    
    # Convert the tibble to a data frame
    pivot_table_df <- as.data.frame(pivot_table)
    
    # Add a column with the table name (without "table.subset_")
    rownames(pivot_table_df) <- gsub("table.subset_", "", table_name)
    add_table_name <- gsub("table.subset_", "", table_name)
    # Append the table to the list
    table_list[[add_table_name]] <- pivot_table_df
    
  } else {
    print("table empty")
  }
}
#Merge them together
# Merge the tables using bind_rows and create new columns for unique column names
merged_table <- bind_rows(table_list, .id = "Table_Name")
# Delete the extra column of names that gets created
merged_table <- merged_table[, -1]
# Replace NA values with 0 in the merged table
merged_table[is.na(merged_table)] <- 0
#Merge all of the "other" columns together
# Get the column names containing the word "other"
other_columns <- grep("other", colnames(merged_table), ignore.case = TRUE)
# Sum the selected columns
sum_other_columns <- rowSums(merged_table[, other_columns])
# Remove the selected columns
merged_table <- merged_table[, -other_columns]
merged_table$Other <- sum_other_columns
# Print the result
print(sum_other_columns)
# Display the merged table
print(merged_table)
rownames(merged_table)
merged_table_pol <- merged_table

#Put the tables in the correct format Gov
# Create an empty list to store the tables
table_list <- list()
# Loop through each table name
for (table_name in table_names_governance) {
  table_data <- get(table_name)
  if (nrow(table_data)>0){
    # Pivot the table
    pivot_table <- pivot_wider(table_data, 
                               names_from = "Governance type",
                               values_from = "Count",
                               values_fill = 0)  # Fill missing values with 0
    
    # Convert the tibble to a data frame
    pivot_table_df <- as.data.frame(pivot_table)
    
    # Add a column with the table name (without "table.subset_")
    rownames(pivot_table_df) <- gsub("table.subset_", "", table_name)
    add_table_name <- gsub("table.subset_", "", table_name)
    # Append the table to the list
    table_list[[add_table_name]] <- pivot_table_df
    
  } else {
    print("table empty")
  }
}

#Merge them together
# Merge the tables using bind_rows and create new columns for unique column names
merged_table <- bind_rows(table_list, .id = "Table_Name")
# Delete the extra column of names that gets created
merged_table <- merged_table[, -1]

#Use governance_lookup table to reclassify accordingly
# Loop through column names of merged_table
for (col_name in names(merged_table[2:nrow(merged_table)])) {
  # Find the closest matching value in the lookup table
  closest_match <- find_approximate_match(col_name, governance_lookup$Gov)
  # Check if a matching value was found
  if (!is.na(closest_match)) {
    # Find the index of the matching value in the lookup table
    index <- which(governance_lookup$Gov == closest_match)
    # Extract the corresponding replacement value
    replacement <- governance_lookup$Gov_New[index]
    # Rename the column in merged_table
    names(merged_table)[names(merged_table) == col_name] <- replacement
  }else{
    print("no match")
  }
}

names(merged_table)
# Find column indices that are NA and delete them
na_columns <- which(is.na(names(merged_table)))
# Remove columns with NA labels
merged_table <- merged_table[, -na_columns]

# Replace NA values with 0 in the merged table
merged_table[is.na(merged_table)] <- 0

#Merge all of the "other" columns together
# Get the column names containing the word "other"
other_columns <- grep("other", colnames(merged_table), ignore.case = TRUE)
# Sum the selected columns
sum_other_columns <- rowSums(merged_table[, other_columns])
# Remove the selected columns
merged_table <- merged_table[, -other_columns]
merged_table$Other <- sum_other_columns
# Print the resulting merged_table
View(merged_table)

#Merge together the two "Adaptive Governance" columns
# Get the column names containing the word "other"
ad_gov <- grep("Adaptive Governance", colnames(merged_table), ignore.case = TRUE)
# Sum the selected columns
sum_ad_gov_columns <- rowSums(merged_table[, ad_gov])
# Remove the selected columns
merged_table <- merged_table[, -ad_gov]
merged_table$"Adaptive Governance" <- sum_ad_gov_columns
merged_table_gov <- merged_table

#Clean up the rownames
# Remove the prefix "tablegov.subset_" from row names
rownames(merged_table_gov) <- gsub("tablegov\\.subset_", "", rownames(merged_table_gov))
rownames(merged_table_pol) <- gsub("tablepol\\.subset_", "", rownames(merged_table_pol))

merged_table_gov$rowname <- rownames(merged_table_gov)
merged_table_pol$rowname <- rownames(merged_table_pol)

# Rename the 'OldColumnName' to 'NewColumnName'
merged_table_gov <- merged_table_gov %>%
  rename(Other_gov = Other)
# Rename the 'OldColumnName' to 'NewColumnName'
merged_table_pol <- merged_table_pol %>%
  rename(Other_pol = Other)

test <- merge(merged_table_gov, merged_table_pol, all = TRUE)
# Assuming your dataframe is named 'test'
result_dissolved <- test %>%
  group_by(rowname) %>%
  summarize_all(sum, na.rm = TRUE)

# View the dissolved dataframe
View(result_dissolved)
# Replace "_" with " " in a specific column 
result_dissolved$rowname <- gsub("_", " ", result_dissolved$rowname)
merged_table_remove_names <- result_dissolved
merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:ncol(result_dissolved)])
colnames(merged_table_remove_names) <- NULL
rownames(merged_table_remove_names) <- NULL

# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
color_palette <- colorRampPalette(c("#B2D78C", "#0D7674", "#67518A"))(100)

# Create the heatmap
# my_plot <- heatmap(
#   as.matrix(merged_table_remove_names),  # Convert the data to a matrix
#   col = color_palette,  # Set the color palette
#   Rowv = NA,  # Do not cluster rows
#   Colv = NA,  # Do not cluster columns
#   labRow = result_dissolved$rowname,  # Use the row names
#   labCol = colnames(result_dissolved[2:11]),  # Use the column names
#   cexRow = 0.8,  # Adjust the row label size
#   cexCol = 0.8,  # Adjust the column label size
#   #margins = c(5, 10),  # Adjust the margins
#   #main = "Heatmap Title"  # Add a title +
#   geom_text(aes(label = value))
# )

#Using pheatmap actually worked better for me!
heatmap <- pheatmap(as.matrix(merged_table_remove_names), display_numbers = T, number_format = "%.0f", color = colorRampPalette(c("#B2D78C", "#0D7674", "#9C85B0"))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 12, labels_row = result_dissolved$rowname, labels_col = colnames(result_dissolved[2:ncol(result_dissolved)]), fontsize_row = 8, fontsize_col = 10, angle_col = "45", border_color = NA)

# Save the heatmap to a PNG file
png("./Heatmap_elements_governance_policy.png", width = 600, height = 600)  # Adjust width and height as needed
print(heatmap)
dev.off()  # Close the PNG device
#Some final touches in ppt




#Code to create figure 4.17 Nexus challenges, Nexus elements, and cross-cutting issues
crosscut_lookup <- read.csv("./crosscut_lookup.csv") 

# Define the list of elements
elements <- c("Biodiversity", "Climate", "Health", "Food", "Water")
# Generate all possible combinations of elements
all_combinations <- unlist(sapply(1:length(elements), function(n) combn(elements, n, paste, collapse = "; ")))

#Need to use this later on for merging the generated tables together
table_names_chal <- c()
table_names_crosscut <- c()
#combo <- all_combinations[2:2]
# Create and name dataframes for each combination
for (combo in all_combinations) {
  # Define a unique name for each dataframe based on the combination
  df_name <- paste("subset_", gsub("[^[:alnum:]]", "_", combo), sep = "")
  # Filter for exact matches of the combination
  subset_df <- csv_data %>%
    filter(`Which.nexus.elements.are.considered.` == combo)
  # Assign the dataframe to the dynamically generated name
  assign(df_name, subset_df)
  # Print or do something with the subset DataFrame
  # For example, print the first few rows
  # cat("Subset for combination:", combo, "\n")
  # print(head(get(df_name)))
  df <- get(df_name)
  # Extract the columns of interest
  column_data_chal <- df$"Does.the.study.provide.evidence.for.addressing.any.of.the.following.nexus.challenges."
  column_data_crosscut <- df$"Which.of.the.following.cross.cutting.issues.are.considered." 
  # Split the elements in each row by semicolon
  split_elements_chal <- strsplit(column_data_chal, "; ")
  split_elements_crosscut<- strsplit(column_data_crosscut, "; ")
  # Flatten the list of split elements
  all_elements_chal <- unlist(split_elements_chal)
  all_elements_crosscut <- unlist(split_elements_crosscut)
  
  # Create a frequency table for policy
  table_result_chal <- table(all_elements_chal)
  table_df_chal <- as.data.frame(table_result_chal)
  
  if (nrow(table_df_chal)>0){
    # Convert the frequency table to a data frame
    # Rename the columns for clarity
    colnames(table_df_chal) <- c("Challenge", "Count")
  } else{
    table_df_policy <- table_df_chal
  }
  # Define the name for the table as "table." followed by df_name
  table_name_chal <- paste("tablechal.", df_name, sep = "")
  # Assign the table to the name
  assign(table_name_chal, table_df_chal)
  # Display the resulting table
  print(table_df_chal)
  table_names_chal <- c(table_names_chal, table_name_chal)
  
  # Create a frequency table for policy
  table_result_crosscut <- table(all_elements_crosscut)
  table_df_crosscut <- as.data.frame(table_result_crosscut)
  
  if (nrow(table_df_crosscut)>0){
    # Convert the frequency table to a data frame
    # Rename the columns for clarity
    colnames(table_df_crosscut) <- c("Cross cutting issue", "Count")
  } else{
    table_df_crosscut <- table_df_crosscut
  }
  # Define the name for the table as "table." followed by df_name
  table_name_crosscut <- paste("tablecrosscut.", df_name, sep = "")
  # Assign the table to the name
  assign(table_name_crosscut, table_df_crosscut)
  # Display the resulting table
  print(table_df_crosscut)
  table_names_crosscut <- c(table_names_crosscut, table_name_crosscut)
}


#Put the tables in the correct format
# Create an empty list to store the tables
table_list <- list()

# Loop through each table name
for (table_name in table_names_chal) {
  table_data <- get(table_name)
  if (nrow(table_data)>0){
    # Pivot the table
    pivot_table <- pivot_wider(table_data, 
                               names_from = "Challenge",
                               values_from = "Count",
                               values_fill = 0)  # Fill missing values with 0
    
    # Convert the tibble to a data frame
    pivot_table_df <- as.data.frame(pivot_table)
    
    # Add a column with the table name (without "table.subset_")
    rownames(pivot_table_df) <- gsub("table.subset_", "", table_name)
    add_table_name <- gsub("table.subset_", "", table_name)
    # Append the table to the list
    table_list[[add_table_name]] <- pivot_table_df
    
  } else {
    print("table empty")
  }
}
#Merge them together
# Merge the tables using bind_rows and create new columns for unique column names
merged_table <- bind_rows(table_list, .id = "Table_Name")
# Delete the extra column of names that gets created
merged_table <- merged_table[, -1]
# Replace NA values with 0 in the merged table
merged_table[is.na(merged_table)] <- 0
#Merge all of the "other" columns together
# Get the column names containing the word "other"
other_columns <- grep("other", colnames(merged_table), ignore.case = TRUE)
# Sum the selected columns
sum_other_columns <- rowSums(merged_table[, other_columns])
# Remove the selected columns
merged_table <- merged_table[, -other_columns]
merged_table$Other <- sum_other_columns
# Print the result
print(sum_other_columns)
# Display the merged table
print(merged_table)
rownames(merged_table)
merged_table_chal <- merged_table

#Put the tables in the correct format Gov
# Create an empty list to store the tables
table_list <- list()
# Loop through each table name
for (table_name in table_names_crosscut) {
  table_data <- get(table_name)
  if (nrow(table_data)>0){
    # Pivot the table
    pivot_table <- pivot_wider(table_data, 
                               names_from = "Cross cutting issue",
                               values_from = "Count",
                               values_fill = 0)  # Fill missing values with 0
    
    # Convert the tibble to a data frame
    pivot_table_df <- as.data.frame(pivot_table)
    
    # Add a column with the table name (without "table.subset_")
    rownames(pivot_table_df) <- gsub("table.subset_", "", table_name)
    add_table_name <- gsub("table.subset_", "", table_name)
    # Append the table to the list
    table_list[[add_table_name]] <- pivot_table_df
    
  } else {
    print("table empty")
  }
}

#Merge them together
# Merge the tables using bind_rows and create new columns for unique column names
merged_table <- bind_rows(table_list, .id = "Table_Name")
# Delete the extra column of names that gets created
merged_table <- merged_table[, -1]

#Use governance_lookup table to reclassify accordingly
# Function to find approximate matches
find_approximate_match <- function(col_name, lookup_table) {
  distances <- stringdist::stringdistmatrix(col_name, lookup_table)
  closest_match_idx <- which.min(distances)
  closest_match <- lookup_table[closest_match_idx]
  return(closest_match)
}


# Loop through column names of merged_table
for (col_name in names(merged_table[2:nrow(merged_table)])) {
  # Find the closest matching value in the lookup table
  closest_match <- find_approximate_match(col_name, crosscut_lookup$CrossCut)
  # Check if a matching value was found
  if (!is.na(closest_match)) {
    # Find the index of the matching value in the lookup table
    index <- which(crosscut_lookup$CrossCut == closest_match)
    # Extract the corresponding replacement value
    replacement <- crosscut_lookup$CrossCut_New[index]
    # Rename the column in merged_table
    names(merged_table)[names(merged_table) == col_name] <- replacement
  }else{
    print("no match")
  }
}


names(merged_table)
# Find column indices that are NA and delete them
na_columns <- which(is.na(names(merged_table)))
# Remove columns with NA labels
merged_table <- merged_table[, -na_columns]

# Replace NA values with 0 in the merged table
merged_table[is.na(merged_table)] <- 0

#Merge all of the "other" columns together
# Get the column names containing the word "other"
other_columns <- grep("other", colnames(merged_table), ignore.case = TRUE)
# Sum the selected columns
sum_other_columns <- rowSums(merged_table[, other_columns])
# Remove the selected columns
merged_table <- merged_table[, -other_columns]
merged_table$Other <- sum_other_columns
# Print the resulting merged_table
View(merged_table)

#Merge together the duplicate columns
#Land
# List of column names to be combined
columns_to_combine <- c("Land.1", "Land")
# Create a new column "Land" that is the sum of selected columns
merged_table$Land <- rowSums(merged_table[columns_to_combine], na.rm = TRUE)
# Remove the original columns that were summed
merged_table <- merged_table[, !(names(merged_table) %in% "Land.1")]
#Migration
# List of column names to be combined
columns_to_combine <- c("Migration.1", "Migration")
# Create a new column "Land" that is the sum of selected columns
merged_table$Migration <- rowSums(merged_table[columns_to_combine], na.rm = TRUE)
# Remove the original columns that were summed
merged_table <- merged_table[, !(names(merged_table) %in% "Migration.1")]
#Politics and Democracy
# List of column names to be combined
columns_to_combine <- c("Politics and Democracy.1", "Politics and Democracy")
# Create a new column "Land" that is the sum of selected columns
merged_table$"Politics and Democracy" <- rowSums(merged_table[columns_to_combine], na.rm = TRUE)
# Remove the original columns that were summed
merged_table <- merged_table[, !(names(merged_table) %in% "Politics and Democracy.1")]
#Security
# List of column names to be combined
columns_to_combine <- c("Security.1", "Security")
# Create a new column "Land" that is the sum of selected columns
merged_table$Security <- rowSums(merged_table[columns_to_combine], na.rm = TRUE)
# Remove the original columns that were summed
merged_table <- merged_table[, !(names(merged_table) %in% "Security.1")]
#Waste
# List of column names to be combined
columns_to_combine <- c("Waste.1", "Waste")
# Create a new column "Land" that is the sum of selected columns
merged_table$Waste <- rowSums(merged_table[columns_to_combine], na.rm = TRUE)
# Remove the original columns that were summed
merged_table <- merged_table[, !(names(merged_table) %in% "Waste.1")]
#Power Dynamics
# List of column names to be combined
columns_to_combine <- c("Power Dynamics.1", "Power Dynamics")
# Create a new column "Land" that is the sum of selected columns
merged_table$"Power Dynamics" <- rowSums(merged_table[columns_to_combine], na.rm = TRUE)
# Remove the original columns that were summed
merged_table <- merged_table[, !(names(merged_table) %in% "Power Dynamics.1")]
merged_table_crosscut <- merged_table

#Clean up the rownames
# Remove the prefix "tablegov.subset_" from row names
rownames(merged_table_chal) <- gsub("tablechal\\.subset_", "", rownames(merged_table_chal))
rownames(merged_table_crosscut) <- gsub("tablecrosscut\\.subset_", "", rownames(merged_table_crosscut))

merged_table_chal$rowname <- rownames(merged_table_chal)
merged_table_crosscut$rowname <- rownames(merged_table_crosscut)

# Rename the 'OldColumnName' to 'NewColumnName'
merged_table_chal <- merged_table_chal %>%
  rename(Other_chal = Other)

# Define new column names
#Look up table doesnt quite match, have hard coded for now
new_column_names <- c("Values Challenges", "Governance Challenges", "Scaling Challenges", "Complexity Challenges", "Financing Challenges", "Other", "rowname")
# Rename the columns
colnames(merged_table_chal) <- new_column_names

# Rename the 'OldColumnName' to 'NewColumnName'
# merged_table_crosscut <- merged_table_crosscut %>%
#   rename(Other_crosscut = Other)

test <- merge(merged_table_chal, merged_table_crosscut, all = TRUE)
# Assuming your dataframe is named 'test'
result_dissolved <- test %>%
  group_by(rowname) %>%
  summarize_all(sum, na.rm = TRUE)

# View the dissolved dataframe
View(result_dissolved)
# Replace "_" with " " in a specific column 
result_dissolved$rowname <- gsub("_", " ", result_dissolved$rowname)
merged_table_remove_names <- result_dissolved
merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:ncol(result_dissolved)])
colnames(merged_table_remove_names) <- NULL
rownames(merged_table_remove_names) <- NULL

# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
color_palette <- colorRampPalette(c("#B2D78C", "#0D7674", "#67518A"))(100)

# Create the heatmap
# my_plot <- heatmap(
#   as.matrix(merged_table_remove_names),  # Convert the data to a matrix
#   col = color_palette,  # Set the color palette
#   Rowv = NA,  # Do not cluster rows
#   Colv = NA,  # Do not cluster columns
#   labRow = result_dissolved$rowname,  # Use the row names
#   labCol = colnames(result_dissolved[2:11]),  # Use the column names
#   cexRow = 0.8,  # Adjust the row label size
#   cexCol = 0.8,  # Adjust the column label size
#   #margins = c(5, 10),  # Adjust the margins
#   #main = "Heatmap Title"  # Add a title +
#   geom_text(aes(label = value))
# )

#Using pheatmap actually worked better for me!
heatmap <- pheatmap(as.matrix(merged_table_remove_names), display_numbers = T, number_format = "%.0f", color = colorRampPalette(c("#B2D78C", "#0D7674", "#9C85B0"))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 24, labels_row = result_dissolved$rowname, labels_col = colnames(result_dissolved[2:ncol(result_dissolved)]), fontsize_row = 8, fontsize_col = 10, angle_col = "45", border_color = NA)

# Save the heatmap to a PNG file
png("./heatmap_chal_crosscut.png", width = 1200, height = 1200)  # Adjust width and height as needed
print(heatmap)
dev.off()  # Close the PNG device









# FROM HERE ON IS OLD STUFF - IGNORE FOR NOW BUT SOME OF THE CODE MAY BE REUSABLE

# make simple histograms of frequencies

# types of paper
# get data and replace different types of "Other" with just "Other" and remove NA values
Types <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$PaperType)) %>% na.omit())
names(Types) <- c("Paper Type")
ggplot(Types, aes(x = `Paper Type`, fill = `Paper Type`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count")
ggsave("types.jpg", width = 20, height = 10, units = "cm")

# regions
# get data and replace different types of "Other" with just "Other" and remove NA values
Regions <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$Region)) %>% na.omit())
names(Regions) <- c("Region")
ggplot(Regions, aes(x = `Region`, fill = `Region`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text.x = element_text(angle = -90, hjust = 0))
ggsave("regions.jpg", width = 10, height = 15, units = "cm")

# nexus elements
# get data and replace different types of "Other" with just "Other" and remove NA values
Nexus <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$Nexus)) %>% na.omit())
names(Nexus) <- c("Nexus Element")
ggplot(Nexus, aes(x = `Nexus Element`, fill = `Nexus Element`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count")
ggsave("nexus.jpg", width = 10, height = 10, units = "cm")

# scale
# get data and replace different types of "Other" with just "Other" and remove NA values
Scale <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$Scale)) %>% na.omit())
names(Scale) <- c("Scale")
ggplot(Scale, aes(x = `Scale`, fill = `Scale`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text.x = element_text(angle = -90, hjust = 0))
ggsave("scale.jpg", width = 10, height = 15, units = "cm")

# governance
# get data and replace different types of "Other" with just "Other" and remove NA values
Gov <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$Gov)) %>% na.omit())
names(Gov) <- c("Governance Types")
ggplot(Gov, aes(x = `Governance Types`, fill = `Governance Types`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text.x = element_text(angle = -90, hjust = 0))
ggsave("governance.jpg", width = 10, height = 15, units = "cm")

# policy
# get data and replace different types of "Other" with just "Other" and remove NA values
Policy <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$Policy)) %>% na.omit())
names(Policy) <- c("Policy Instrument Types")
ggplot(Policy, aes(x = `Policy Instrument Types`, fill = `Policy Instrument Types`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text.x = element_text(angle = -90, hjust = 0))
ggsave("policy.jpg", width = 10, height = 15, units = "cm")

# actors
# get data and replace different types of "Other" with just "Other" and remove NA values
Actors <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$Actors)) %>% na.omit())
names(Actors) <- c("Actor Types")
ggplot(Actors, aes(x = `Actor Types`, fill = `Actor Types`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text.x = element_text(angle = -90, hjust = 0))
ggsave("actors.jpg", width = 10, height = 15, units = "cm")

# nexus challenges
# get data and replace different types of "Other" with just "Other" and remove NA values
Challenges <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$NChallenge)) %>% na.omit())
names(Challenges) <- c("Nexus Challenges")
# map onto new nexus challenges
Lookup <- read_csv("challenge_lookup.csv")
Challenges <- Challenges %>% left_join(Lookup, by = join_by(`Nexus Challenges` == `Original`)) %>%
                  select(-`Nexus Challenges`) %>% rename('Nexus Challenges' = New)
ggplot(Challenges, aes(x = `Nexus Challenges`, fill = `Nexus Challenges`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text.x = element_text(angle = -90, hjust = 0))
ggsave("challenges.jpg", width = 10, height = 15, units = "cm")

# cross-cutting issues
# get data and replace different types of "Other" with just "Other" and remove NA values
CrossCut <- as_tibble(gsub(".*Other.*", "Other", unlist(Data_New$CrossCut)) %>% na.omit())
names(CrossCut) <- c("Cross-cutting Issues")
ggplot(CrossCut, aes(x = `Cross-cutting Issues`, fill = `Cross-cutting Issues`)) + geom_bar() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text.x = element_text(angle = -90, hjust = 0))
ggsave("crosscut.jpg", width = 10, height = 15, units = "cm")

# GET OCCURENCE OF GOVERNANCE AND POLICY INSTRUMENTS FOR EACH NEXUS CHALLENGE

# get unique nexus challenges
Unique_Challenges <- sort(unique(Challenges)$`Nexus Challenges`)

# get unique governance types
Unique_Governance <- sort(unique(Gov)$`Governance Types`)[c(2, 3, 4, 1, 6, 5)]

# get unique policy instruments
Unique_Policy <- sort(unique(Policy)$`Policy Instrument Types`)[c(2, 4, 1, 5, 3)]

Matrix_List <- list()
# loop through unique challenges and get governance types and policy instruments combinations
for (i in 1:length(Unique_Challenges)) {

  # get rows in data that match this challenge
  ThisChallenge <- unlist(map(Data_New$NChallenge, .f = check_challenge, Chall = Unique_Challenges[i], ChallLookup = Lookup))
  Data_This_Challenge <- Data_New[ThisChallenge, ]

  # get the cross-tabbed matrix of governance approaches versus policy intruments for this challenge
  Matrix_List[[i]] <- bind_rows(map2(Data_This_Challenge$Gov, Data_This_Challenge$Policy, .f = get_crossed, Names = c("Governance", "Policy Instrument")))

  # ensure missing categories are included by using factors
  Matrix_List[[i]] <- Matrix_List[[i]] %>% mutate(Governance = factor(Governance, levels = rev(Unique_Governance)), `Policy Instrument` = factor(`Policy Instrument`, levels = Unique_Policy))

  # create cross-tabulated data
  Matrix_List[[i]] <- Matrix_List[[i]] %>% table()
}

# give list entries names
names(Matrix_List) <- Unique_Challenges

# loop through list and save plots
for (i in 1:length(Unique_Challenges)) {

  PlotData <- as_tibble(Matrix_List[[i]]) %>% mutate(Governance = factor(Governance, levels = rev(Unique_Governance)), `Policy Instrument` = factor(`Policy Instrument`, levels = Unique_Policy))

  ggplot(PlotData, aes(`Policy Instrument`, Governance, col = n, fill = n, label = n)) +
    geom_tile(color = "white", lwd = 4, linetype = 1) +
    geom_text(col = "black") +
    theme_minimal() +
    scale_fill_gradientn(colours = c("grey", "yellow", "red"), limits = c(0, 5)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")
  ggsave(paste(names(Matrix_List)[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")
}

# create a special zeros plot
PlotData <- as_tibble(Matrix_List[[1]]) %>% mutate(Governance = factor(Governance, levels = rev(Unique_Governance)), `Policy Instrument` = factor(`Policy Instrument`, levels = Unique_Policy)) %>% mutate(n = 0)

ggplot(PlotData, aes(`Policy Instrument`, Governance, col = n, fill = n, label = n)) +
   geom_tile(color = "white", lwd = 4, linetype = 1) +
   geom_text(col = "black") +
   theme_minimal() +
   scale_fill_gradientn(colours = c("grey", "yellow", "red"), limits = c(0, 5)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")

ggsave("Blank.jpg", width = 10, height = 10, units = "cm")

# create a special template plot
PlotData <- as_tibble(Matrix_List[[1]]) %>% mutate(Governance = factor(Governance, levels = rev(Unique_Governance)), `Policy Instrument` = factor(`Policy Instrument`, levels = Unique_Policy)) %>% mutate(n = 0)

ggplot(PlotData, aes(`Policy Instrument`, Governance, col = n, fill = n, label = n)) +
   geom_tile(color = "white", lwd = 4, linetype = 1) +
   theme_minimal() +
   scale_fill_gradientn(colours = c("grey", "yellow", "red"), limits = c(0, 5)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")

ggsave("Template.jpg", width = 10, height = 10, units = "cm")

# GET OCCURENCE OF POLICY INSTRUMENTS, SPATIAL SCALES, AND ACTORS FOR EACH NEXUS ELEMENT

# get unique nexus elements
Unique_Nexus <- sort(unique(Nexus)$`Nexus Element`)[c(1, 2, 3, 4, 6, 5)]

# get unique policy instruments
Unique_Policy <- sort(unique(Policy)$`Policy Instrument Types`)[c(2, 4, 1, 5, 3)]

# get unique spatial scales
Unique_Scale <- sort(unique(Scale)$`Scale`)[c(1, 5, 2, 4, 3)]

# get unique actors
Unique_Actors <- sort(unique(Actors)$`Actor Types`) [c(1:8, 10, 11, 9)]

source("functions.R")

# loop through unique nexus elements and get policy instruments, spatial scales and actors
# here we don't consider "Other" in the Nexus Elements
# create plots
for (i in 1:(length(Unique_Nexus) - 1)) {

  # get rows in data that match this challenge
  ThisNexus <- unlist(map(Data_New$Nexus, .f = check_nexus, Nex = Unique_Nexus[i]))
  Data_This_Nexus <- Data_New[ThisNexus, ]

  # plot policy instruments

  # get data
  Plot_Data <- as_tibble(unlist(map(Data_This_Nexus$Policy, .f = replace_other)))
  names(Plot_Data) <- c("Policy Instrument")

  # ensure mssing categories are included by using factors
  Plot_Data <- Plot_Data %>% mutate(`Policy Instrument` = factor(`Policy Instrument`, levels = Unique_Policy))

  # create plot
  ggplot(Plot_Data, aes(x = `Policy Instrument`, fill = `Policy Instrument`)) + geom_bar() + scale_x_discrete(drop = FALSE) + scale_fill_discrete(drop = FALSE) + theme_minimal() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) + coord_polar(start = 0)

  # save plot
  ggsave(paste("Policy_", Unique_Nexus[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")

  # plot spatial scales

  # get data
  Plot_Data <- as_tibble(unlist(map(Data_This_Nexus$Scale, .f = replace_other)))
  names(Plot_Data) <- c("Spatial Scale")

  # ensure missing categories are included by using factors
  Plot_Data <- Plot_Data %>% mutate(`Spatial Scale` = factor(`Spatial Scale`, levels = Unique_Scale))

  # create plot
  ggplot(Plot_Data, aes(x = `Spatial Scale`, fill = `Spatial Scale`)) + geom_bar() + scale_x_discrete(drop = FALSE) + scale_fill_discrete(drop = FALSE) + theme_minimal() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) + coord_polar(start = 0)

  # save plot
  ggsave(paste("Scale_", Unique_Nexus[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")

  # plot actors

  # get data
  Plot_Data <- as_tibble(unlist(map(Data_This_Nexus$Actors, .f = replace_other)))
  names(Plot_Data) <- c("Actors")

  # ensure mssing categories are included by using factors
  Plot_Data <- Plot_Data %>% mutate(`Actors` = factor(`Actors`, levels = Unique_Actors))

  # create plot
  ggplot(Plot_Data, aes(x = `Actors`, fill = `Actors`)) + geom_bar() + scale_x_discrete(drop = FALSE) + scale_fill_discrete(drop = FALSE) + theme_minimal() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) + coord_polar(start = 0)

  # save plot
  ggsave(paste("Actors_", Unique_Nexus[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")
}

# SYNTHESISE ENABLERS AND BARRIERS DATA

# load enablers and barriers lookup table
EBLookup <- read_csv("enablers_barriers_lookup.csv")
Unique_Enablers_Barriers <- unique(EBLookup$New)

# loop through unique challenges and get governance types and policy instruments combinations
for (i in 1:length(Unique_Challenges)) {

  # get data for this challenge
  Chall_Enablers_Barriers <- Data %>% mutate(NChallenge = str_split(str_squish(`Does the study provide evidence for addressing any of the following nexus challenges?`),"; ")) %>% select(NChallenge)
  This_Challenge <- unlist(map(Chall_Enablers_Barriers$NChallenge, .f = check_challenge, Chall = Unique_Challenges[i], ChallLookup = Lookup))
  Data_This_Challenge <- Data[This_Challenge, ]

  # format data
  Data_Enablers_Barriers <- Data_This_Challenge %>% select((contains("Barrier") | contains("Enabler")) & !contains("Notes")) %>% replace(. != "x" & . != "X" & !is.na(.), "0") %>% replace(is.na(.), "0") %>% replace(. == "X", "1") %>% replace(. == "x", "1")  %>% mutate_if(is.character, as.numeric)

  # sum the columns
  Enablers_Barriers_Summary <- Data_Enablers_Barriers %>% summarise_all(sum)

  # get enablers data
  Enablers_Summary <- tibble(Enablers = names(Enablers_Barriers_Summary), Count = Enablers_Barriers_Summary %>% slice(1) %>% unlist(., use.names = FALSE)) %>% mutate(Enablers = str_squish(Enablers)) %>% filter(str_detect(Enablers, "Enabler")) %>% mutate(Enablers = str_squish(str_remove(Enablers, " Enabler"))) %>% left_join(EBLookup, by = join_by(Enablers == Original)) %>% select(-Enablers) %>% rename(Enablers = New) %>% group_by(Enablers) %>% summarise(Count = sum(Count)) %>% mutate(Enablers = factor(Enablers, levels = Unique_Enablers_Barriers))

  # get barriers data
  Barriers_Summary <- tibble(Barriers = names(Enablers_Barriers_Summary), Count = Enablers_Barriers_Summary %>% slice(1) %>% unlist(., use.names = FALSE)) %>% mutate(Barriers = str_squish(Barriers)) %>% filter(str_detect(Barriers, "Barrier")) %>% mutate(Barriers = str_squish(str_remove(Barriers, " Barrier"))) %>% left_join(EBLookup, by = join_by(Barriers == Original)) %>% select(-Barriers) %>% rename(Barriers = New) %>% group_by(Barriers) %>% summarise(Count = sum(Count)) %>% mutate(Barriers = factor(Barriers, levels = Unique_Enablers_Barriers))

  # plot enablers
  ggplot(Enablers_Summary, aes(x = Enablers, y = Count, fill = Enablers)) + geom_col() + theme_minimal() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank())
  ggsave(paste("Enablers_", Unique_Challenges[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")

  # plot barriers
  ggplot(Barriers_Summary, aes(x = Barriers, y = Count, fill = Barriers)) + geom_col() + theme_minimal() + theme(legend.position = "none") + labs(y = "Count") + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank())
  ggsave(paste("Barriers_", Unique_Challenges[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")
}
