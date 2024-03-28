# load packages
library(tidyverse)
library(ggalluvial)
library(gplots)
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
if (!require("ComplexHeatmap", quietly = TRUE))
    BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(stringdist)
library(sf)
library(RColorBrewer)
library(viridis)

# load functions
source("functions.R")

# load review data from covidence
Data <- read_csv("review_283729_20240320131759.csv") %>% mutate(`Covidence #` = paste0("#", as.character(`Covidence #`)))

# load list of references with tags from covidence
Refs <- read_csv("review_283729_included_csv_20240320131744.csv")

# get the references for grey literature
RefsGrey <- Refs %>% select('Covidence #', Tags) %>% filter(Tags == "Grey Literature")

# joint to data
JoinedData <- Data %>% left_join(RefsGrey, by = join_by(`Covidence #`))

# split data into peer reviwed and grey literature sets
Data <- filter(JoinedData, is.na(Tags)) %>% select(-Tags)
Data_Grey <- filter(JoinedData, !is.na(Tags)) %>% select(-Tags)

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
                  str_detect(Gov, fixed("transformational", ignore_case = TRUE)) ~ "Transformative Governance",
                  str_detect(Gov, fixed("collaborative", ignore_case = TRUE)) ~ "Collaborative Governance",
                  str_detect(Gov, fixed("cooperative", ignore_case = TRUE)) ~ "Cooperative Governance",
                  str_detect(Gov, fixed("flexible", ignore_case = TRUE)) ~ "Flexible Governance",
                  str_detect(Gov, fixed("inclusive", ignore_case = TRUE)) ~ "Inclusive Governance",
                  str_detect(Gov, fixed("multi-lateral", ignore_case = TRUE)) ~ "Multi-lateral Governance",
                  str_detect(Gov, fixed("multilateral", ignore_case = TRUE)) ~ "Multi-lateral Governance",
                  str_detect(Gov, fixed("participatory", ignore_case = TRUE)) ~ "Participatory Governance",
                  str_detect(Gov, fixed("polycentric", ignore_case = TRUE)) ~ "Polycentric Governance",
                  str_detect(Gov, fixed("cross-sectoral", ignore_case = TRUE)) ~ "Cross-sectoral Governance",
                  str_detect(Gov, fixed("integrative", ignore_case = TRUE)) ~ "Integrative Governance",
                  str_detect(Gov, fixed("integrated", ignore_case = TRUE)) ~ "Integrative Governance",
                  str_detect(Gov, fixed("nested", ignore_case = TRUE)) ~ "Nested Governance",
                  str_detect(Gov, fixed("meta-governance", ignore_case = TRUE)) ~ "Meta-governance",
                  str_detect(Gov, fixed("coordinated", ignore_case = TRUE)) ~ "Coordinated Governance",
                  str_detect(Gov, fixed("coordination", ignore_case = TRUE)) ~ "Coordinated Governance",
                  str_detect(Gov, fixed("reflexive", ignore_case = TRUE)) ~ "Reflexive Governance",
                  str_detect(Gov, fixed("multi-modal", ignore_case = TRUE)) ~ "Multi-modal Governance",
                  str_detect(Gov, fixed("decentralised", ignore_case = TRUE)) ~ "Decentralised Governance",
                  str_detect(Gov, fixed("nexus", ignore_case = TRUE)) ~ "Nexus Governance",
                  str_detect(Gov, fixed("system", ignore_case = TRUE)) ~ "System Governance",
                  str_detect(Gov, fixed("biocultural", ignore_case = TRUE)) ~ "Biocultural Governance",
                  str_detect(Gov, fixed("resource based", ignore_case = TRUE)) ~ "Resource-based Governance",
                  str_detect(Gov, fixed("centralized", ignore_case = TRUE)) ~ "Centralised Governance",
                  str_detect(Gov, fixed("consumption-based", ignore_case = TRUE)) ~ "Consumption-based Governance",
                  str_detect(Gov, fixed("transboundary", ignore_case = TRUE)) ~ "Transboundary Governance",
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
                  str_detect(CrossCut, fixed("consumption", ignore_case = TRUE)) ~ "Consumption",
                  str_detect(CrossCut, fixed("poverty", ignore_case = TRUE)) ~ "Poverty",
                  str_detect(CrossCut, fixed("economic", ignore_case = TRUE)) ~ "Economy",
                  str_detect(CrossCut, fixed("capital", ignore_case = TRUE)) ~ "Economy",
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
                  str_detect(CrossCut, fixed("urbanisation", ignore_case = TRUE)) ~ "Urbanisation",
                  str_detect(CrossCut, fixed("migration", ignore_case = TRUE)) ~ "Migration",
                  str_detect(CrossCut, fixed("soil", ignore_case = TRUE)) ~ "Land",
                  str_detect(CrossCut, fixed("safety", ignore_case = TRUE)) ~ "Security",
                  str_detect(CrossCut, fixed("services", ignore_case = TRUE)) ~ "Infrastructure",
                  str_detect(CrossCut, fixed("rights", ignore_case = TRUE)) ~ "Civil Rights",
                  str_detect(CrossCut, fixed("democracy", ignore_case = TRUE)) ~ "Politics and Democracy",
                  str_detect(CrossCut, fixed("rule of law", ignore_case = TRUE)) ~ "Rule of Law",
                  str_detect(CrossCut, fixed("social inclusion", ignore_case = TRUE)) ~ "Equity",
                  str_detect(CrossCut, fixed("technology", ignore_case = TRUE)) ~ "Technology",
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
write_csv(Scales, "scales_counts.csv")

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

# summarise actors

# get counts of each actor type considered
Actorss <- unlist(Data_Select_Split$Actors) %>% as_tibble() %>%
      mutate(Actors = value) %>% count(Actors)

# write to csv
write_csv(Actorss, "actors_counts.csv")

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
# Define the color palette
custom_palette <- colorRampPalette(c("#D9AA80", "#B65719", "#791E32"))(4)
#custom_palette1 <- scale_fill_gradient(low = "#D9AA80", high = "#791E32")
#custom_palette2 <- scale_fill_gradient2(low = "#D9AA80", mid = "#B65719", high = "#791E32")
#custom_palette3 <-  scale_fill_gradientn(colors = c("#D9AA80", "#B65719", "#791E32"))

ggplot(Challenges_Nexuses, aes(y = Freq, axis1 = NChallenge, axis2 = Nexus)) +
    geom_alluvium(aes(fill = NumNexus)) +
    geom_stratum() +
    geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Nexus Challenges", "Nexus Elements"),
                   expand = c(0.05, 0.05)) +
    scale_fill_manual(values = color_palette) +
  theme_void() +
    guides(fill = guide_legend(title = "Number of Nexus Elements")) +
    theme(legend.position = "bottom")

ggsave("challenges_nexus_alluvial.jpg", width = 20, height = 10, units = "cm")

# create circular plot

# code modified from https://r-graph-gallery.com/299-circular-stacked-barplot.html

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
    summarize(Freq = n()) %>% ungroup() %>% mutate(NChallenge = str_remove(NChallenge, " Challenges$"))

Challenges_Nexus <- Challenges_Nexuses %>% pivot_wider(names_from = NumNexus, values_from = Freq) %>% mutate(Two = ifelse(is.na(`2`), 0, `2`)) %>% mutate(Three = ifelse(is.na(`3`), 0, `3`)) %>% mutate(Four = ifelse(is.na(`4`), 0, `4`)) %>% mutate(Five = ifelse(is.na(`5`), 0, `5`)) %>% select(-`2`, -`3`, -`4`, -`5`)

# Transform data in a tidy format (long format)
Challenges_Nexus <- Challenges_Nexus %>% gather(key = "observation", value="value", -c(1,2))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(Challenges_Nexus$observation))
to_add <- data.frame(matrix(NA, empty_bar * nlevels(as.factor(Challenges_Nexus$NChallenge)) * nObsType, ncol(Challenges_Nexus)))
colnames(to_add) <- colnames(Challenges_Nexus)
to_add$NChallenge <- rep(levels(as.factor(Challenges_Nexus$NChallenge)), each = empty_bar * nObsType)
Challenges_Nexus <- rbind(Challenges_Nexus, to_add)
Challenges_Nexus <- Challenges_Nexus %>% arrange(NChallenge, Nexus)
Challenges_Nexus$id <- rep(seq(1, nrow(Challenges_Nexus) / nObsType) , each = nObsType)

# Get the name and the y position of each label
label_data <- Challenges_Nexus %>% group_by(id, Nexus) %>% summarize(tot = sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- Challenges_Nexus %>% group_by(NChallenge) %>% summarize(start = min(id), end = max(id) - empty_bar) %>% rowwise() %>% mutate(title = mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(Challenges_Nexus) +

  # Add the stacked bar
  geom_bar(aes(x = as.factor(id), y = value, fill = observation), stat="identity", alpha = 0.5) + scale_fill_manual(values = c("#B65719", "#C6D68A", "#791E32", "#4A928F")) +

  # Add a val = 125/100/75/50/25/0 lines. I do it at the beginning to make sure barplots are OVER it.
  geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 75, xend = start, yend = 75), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 125, xend = start, yend = 125), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE) +

  # Add text showing the value of each of the 125/100/75/50/25/0 lines
  ggplot2::annotate("text", x = rep(max(Challenges_Nexus$id), 6), y = c(0, 25, 50, 75, 100, 125), label = c("0", "25", "50", "75", "100", "125"), color = "grey", size = 4 , angle = 0, fontface = "bold", hjust = 1.5) +

  ylim(-125, max(label_data$tot + 50, na.rm = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) + labs(color = "Number of Nexus Elements") +

  coord_polar() +

  # Add labels on top of each bar
  geom_text(data = label_data, aes(x = id, y= tot + 10, label = Nexus, hjust = hjust), color="black", fontface = "bold",alpha = 0.6, size = 4, angle = label_data$angle, inherit.aes = FALSE) +

  # Add base line information
  geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6 , inherit.aes = FALSE) +
  geom_text(data = base_data, aes(x = title, y = -18, label = NChallenge), hjust=c(1, 1, 0.75, 0, 0), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE)

# Save figure
ggsave(p, file = "challenges_nexus_circular.jpg", width = 25, height = 20, units = "cm")






# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2))

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(as.factor(data$group))*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(as.factor(data$group)), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>%
  group_by(group) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data) +

  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +

  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +

  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +

  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

# Save at png
ggsave(p, file="output.png", width=10, height=10)




# create heatmap plots for figure on governance, policy instruments, and nexus challenges

# get all unique nexus challenges
Unique_Challenges <- sort(unique(unlist(Data_Select_Split$NChallenge)))[c(1, 3, 5, 4, 2)]

# get all unique governance types
Unique_Governance <- sort(unique(unlist(Data_Select_Split$Gov)))[c(2, 3, 4, 1, 5)]

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
    theme_minimal() +
    scale_fill_gradientn(colours = c("#D9AA80", "#B65719", "#791E32"), limits = c(0, 32)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")
  ggsave(paste(names(Matrix_List)[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")
}

# create a special template plot
PlotData <- as_tibble(Matrix_List[[1]]) %>% mutate(Gov = factor(Gov, levels = rev(Unique_Governance)), Policy = factor(Policy, levels = Unique_Policy)) %>% mutate(n = 0)

ggplot(PlotData, aes(Policy, Gov, col = n, fill = n, label = n)) +
   geom_tile(color = "black", lwd = 1, linetype = 1) +
   theme_minimal() +
   scale_fill_gradientn(colours = c("white"), limits = c(0, 0)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")

ggsave("Template.jpg", width = 10, height = 10, units = "cm")

#Code to create figure 4.11 - Geographic distribution of studies
#Can download shp file here (not official UN data): https://thematicmapping.org/downloads/world_borders.php
#Can download the official UN shpfile here (but it only goes to region, not the subregions that we use): https://data.unhabitat.org/datasets/GUO-UN-Habitat::m49-regions/about
#Can download the UN country to region table here: https://unstats.un.org/unsd/methodology/m49/overview/
# https://data.unhabitat.org/search?collection=Dataset&q=M49%20regions
# Read the countries shapefile 
shp_data <- st_read("D:/IPBES_review/data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
# Transform the projection to Mollweide (EPSG:54009)
shp_data <- st_transform(shp_data, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# Rename the column in shp_data to match the UN table
shp_data <- shp_data %>%
  rename(ISO.alpha3.Code = ISO3)
# Read the UN table file into a data frame
csv_data <- read.csv("D:/IPBES_review/data/UNSD — Methodology.csv", sep = ";")
#Merge together
merged_data <- merge(shp_data, csv_data, by = "ISO.alpha3.Code")

# Read in the region counts from review
region_counts <- read.csv("./regions_counts.csv")
# Rename region column name to match UN table
colnames(region_counts)[colnames(region_counts) == "Region"] <- "Sub.region.Name"
#Merge together
merged_data <- merge(merged_data, region_counts, by = "Sub.region.Name")

# Define the color scale from red (highest) to yellow (lowest)
color_scale <-  scale_fill_gradientn(colors = c("#D9AA80", "#B65719", "#791E32"))

# Plot the shapefile without displaying country polygon borders
my_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = n), color = "NA") +  
  color_scale +
  labs(title = "Number of studies in each region", fill = "Count") +
  theme_minimal()

# Export the ggplot as a PNG image
ggsave(filename = "./studies_region_counts.png", plot = my_plot, width = 6, height = 4, dpi = 300)


#Code to create figure 4.13 - heatmap showing nexus elements, governance types and policy instruments
#Option 1
# crosstab of governance approaches versus nexus elements (nexus elements merged)
Gov_Nexus <- get_crosstab(Data_Select_Split, "Nexus", "Gov", Merge1 = TRUE, Merge2 = FALSE)
#Pol_Nexus <- get_crosstab(Data_Select_Split, "Nexus", "Policy", Merge1 = TRUE, Merge2 = FALSE, Factors2 = Unique_Policy)
Pol_Nexus <- get_crosstab(Data_Select_Split, "Nexus", "Policy", Merge1 = TRUE, Merge2 = FALSE)
Gov_Nexus <- as.data.frame(Gov_Nexus)
Pol_Nexus <- as.data.frame(Pol_Nexus)

# Use pivot_wider to reshape the data frame
reshaped_Gov_Nexus <- pivot_wider(
  data = Gov_Nexus,
  names_from = "Nexus",
  values_from = "Freq"
)

# Use pivot_wider to reshape the data frame
reshaped_Pol_Nexus <- pivot_wider(
  data = Pol_Nexus,
  names_from = "Nexus",
  values_from = "Freq"
)

#merged <- merge(reshaped_Gov_Nexus, reshaped_Pol_Nexus, by = "Nexus")

colnames(reshaped_Pol_Nexus)[1] <- "Gov"

#merged <- rbind(reshaped_Gov_Nexus, reshaped_Pol_Nexus)

# Assuming 'reshaped_Gov_Nexus' and 'reshaped_Pol_Nexus' are your data frames
merged <- bind_rows(reshaped_Gov_Nexus, reshaped_Pol_Nexus, .id = "Source") %>%
  group_by_all() %>%
  summarise_all(~coalesce(., 0)) %>%
  ungroup()

merged <- merged[, !colnames(merged) %in% "Food"]
# Remove row where "nexus" is "food"
#merged <- subset(merged, Nexus != "Food")
merged_table_remove_names <- merged
merged_table_remove_names <- as.data.frame(merged_table_remove_names[3:ncol(merged_table_remove_names)])

# Replace 'your_data_frame' with the name of your data frame
merged_table_remove_names <- merged_table_remove_names %>%
  select(sort(colnames(.)))

# Replace all NAs with 0 in the entire dataframe
merged_table_remove_names[is.na(merged_table_remove_names)] <- 0

# Calculate the total count for each column
total_counts <- colSums(merged_table_remove_names)
#total_counts <- sum(total_counts)
# Your merged table
test <- matrix(0, nrow = 29, ncol = 22) # Example matrix dimension

# Iterate over each column and divide by the corresponding total count
for (i in 1:ncol(test)) {
  test[,i] <- total_counts[i]
}

# Print the updated table
print(merged_table_remove_names)

# Convert counts to percentages
merged_table_perc <- merged_table_remove_names / test * 100

# Show the table with percentages
print(merged_table_perc)

#colnames(merged_table_remove_names) <- NULL
#rownames(merged_table_remove_names) <- NULL

# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
#color_palette <- colorRampPalette(c("#B2D78C", "#0D7674", "#67518A"))(100)

#merged_table_remove_names <- as.data.frame(merged_table_remove_names)
# Convert all columns to numeric in the dataframe
#merged_table_remove_names <- data.frame(lapply(merged_table_remove_names, as.numeric))


# Define the color palette
color_palette <- colorRampPalette(c("#D9AA80", "#B65719", "#791E32"))(100)

#merged_gov_string <- c(reshaped_Gov_Nexus$Gov, reshaped_Pol_Nexus$Gov)

# Create the heatmap
heatmap_obj <- ComplexHeatmap::Heatmap(
  as.matrix(merged_table_perc),
  name = "Percentage 
of studies",
  col = color_palette,
  cluster_rows = F,
  cluster_columns = F,
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.text(sprintf("%.0f", merged_table_perc[i, j]), x, y, gp = gpar(fontsize = 12))
  },
  row_labels = merged$Gov,
  show_row_names = TRUE,
  show_column_names = TRUE,  # Turn off column labels at the bottom
  column_names_rot = 40,
  column_names_side = c("top"),
  show_row_dend = FALSE,
  #column_title = "Column Labels",  # Add a title for column labels
  column_title_side = "top",  # Place the title at the top
  column_title_gp = gpar(fontsize = 14),  # Customize the title font size
  row_names_gp = gpar(fontsize = 10)
)


# Save the heatmap to a PNG file
png("./heatmap_elements_governance_policy1.png", width = 6000, height = 6000, res = 600 )  # Adjust width and height as needed
print(heatmap_obj)
dev.off()  # Close the PNG device
#Some final touches in ppt

#Option 2
# crosstab of governance approaches versus nexus elements (nexus elements merged)
# Gov_Nexus <- get_crosstab(Data_Select_Split, "Nexus", "Gov", Merge1 = FALSE, Merge2 = FALSE, Factors2 = Unique_Governance)
# Pol_Nexus <- get_crosstab(Data_Select_Split, "Nexus", "Policy", Merge1 = FALSE, Merge2 = FALSE, Factors2 = Unique_Policy)
# Gov_Nexus <- as.data.frame(Gov_Nexus)
# Pol_Nexus <- as.data.frame(Pol_Nexus)
# 
# # Use pivot_wider to reshape the data frame
# reshaped_Gov_Nexus <- pivot_wider(
#   data = Gov_Nexus,
#   names_from = "Gov",
#   values_from = "Freq"
# )
# 
# # Use pivot_wider to reshape the data frame
# reshaped_Pol_Nexus <- pivot_wider(
#   data = Pol_Nexus,
#   names_from = "Policy",
#   values_from = "Freq"
# )
# 
# merged <- merge(reshaped_Gov_Nexus, reshaped_Pol_Nexus, by = "Nexus")
# merged_table_remove_names <- merged
# merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:ncol(merged)])
# #colnames(merged_table_remove_names) <- NULL
# #rownames(merged_table_remove_names) <- NULL
# 
# # Define the color palette
# my_colors <- colorRampPalette(c("#B2D78C", "#0D7674", "#9C85B0"))(100)
# 
# # Create the heatmap
# heatmap_obj <- ComplexHeatmap::Heatmap(
#   as.matrix(merged_table_remove_names),
#   name = "No. of
# studies",
#   col = my_colors,
#   cluster_rows = T,
#   cluster_columns = F,
#   cell_fun = function(j, i, x, y, width, height, fill) {
#     grid.text(sprintf("%.0f", merged_table_remove_names[i, j]), x, y, gp = gpar(fontsize = 12))
#   },
#   row_labels = c(merged$Nexus),
#   show_row_names = TRUE,
#   show_column_names = TRUE,  # Turn off column labels at the bottom
#   column_names_rot = 40,
#   column_names_side = c("top"),
#   show_row_dend = FALSE,
#   #column_title = "Column Labels",  # Add a title for column labels
#   column_title_side = "top",  # Place the title at the top
#   column_title_gp = gpar(fontsize = 14),  # Customize the title font size
#   row_names_gp = gpar(fontsize = 12)
# )
# 
# # Save the heatmap to a PNG file
# png("./heatmap_elements_governance_policy_option2.png", width = 6000, height = 6000, res = 600 )  # Adjust width and height as needed
# print(heatmap_obj)
# dev.off()  # Close the PNG device
#Some final touches in ppt

#Option 3
# Select specific columns by name
selected_columns <- Data_Select_Split[, c("Nexus", "Gov", "Policy")]

# Assuming you want to extract Nexus, Gov, and Policy for each entry
info_list <- Data_Select_Split %>%
  select(Nexus, Gov, Policy)  # Select the columns of interest

# View the extracted information
head(info_list)

# group by challenges and nexus elements and calculatye the frequency of each unique
# combination
Nex_gov_pol <- info_list %>% group_by(Nexus, Gov, Policy) %>%
  summarize(Freq = n()) %>% ungroup()

# Remove row where "nexus" is "food"
Nex_gov_pol <- subset(Nex_gov_pol, Nexus != "Food")
Nex_gov_pol <- subset(Nex_gov_pol, Gov != "NA")
Nex_gov_pol <- subset(Nex_gov_pol, Policy != "NA")
Nex_gov_pol <- subset(Nex_gov_pol, Nexus != "NA")

is_alluvia_form(as.data.frame(Nex_gov_pol), axes = 1:3, silent = TRUE)

# Assuming `Nex_gov_pol$Nexus` is a list of lists
list_of_lists <- Nex_gov_pol$Nexus
# Create a dataframe where each list is a separate row
df <- data.frame(Nexus = unlist(lapply(list_of_lists, paste, collapse = ", ")), stringsAsFactors = FALSE)

# Assuming `Nex_gov_pol$Gov` is a list of lists
list_of_lists_gov <- Nex_gov_pol$Gov
# Create a dataframe where each list is a separate row for Nexus
df <- data.frame(Nexus = unlist(lapply(list_of_lists, paste, collapse = ", ")), stringsAsFactors = FALSE)
# Create a dataframe where each list is a separate row for Gov
df$Gov <- unlist(lapply(list_of_lists_gov, paste, collapse = ", "))

# Assuming `Nex_gov_pol$Policy` is a list of lists
list_of_lists_policy <- Nex_gov_pol$Policy
# Create a dataframe where each list is a separate row for Nexus
df <- data.frame(Nexus = unlist(lapply(list_of_lists, paste, collapse = ", ")), stringsAsFactors = FALSE)
# Create a dataframe where each list is a separate row for Gov
df$Gov <- unlist(lapply(list_of_lists_gov, paste, collapse = ", "))
# Create a dataframe where each list is a separate row for Policy
df$Policy <- unlist(lapply(list_of_lists_policy, paste, collapse = ", "))
df$Freq <- Nex_gov_pol$Freq

# create alluvial plot
ggplot(df, aes(y = Freq, axis1 = Gov, axis2 = Policy )) +
  geom_alluvium(aes(fill = Nexus)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Nexus Challenges", "Nexus Elements"),
                   expand = c(0.05, 0.05)) +
  theme_void() +
  guides(fill = guide_legend(title = "Number of Nexus Elements")) +
  theme(legend.position = "bottom")

ggsave("nexus_gov_policy_alluvial.jpg", width = 20, height = 10, units = "cm")



#Code to create figure 4.17 Nexus challenges, Nexus elements, and cross-cutting issues
#Option 1
# crosstab of governance approaches versus nexus elements (nexus elements merged)
Nexus_CrossCut <- get_crosstab(Data_Select_Split, "Nexus", "CrossCut", Merge1 = TRUE, Merge2 = FALSE)
Nexus_NChallenge <- get_crosstab(Data_Select_Split, "Nexus", "NChallenge", Merge1 = TRUE, Merge2 = FALSE)
Nexus_CrossCut <- as.data.frame(Nexus_CrossCut)
Nexus_NChallenge <- as.data.frame(Nexus_NChallenge)

# Use pivot_wider to reshape the data frame
reshaped_Nexus_CrossCut <- pivot_wider(
  data = Nexus_CrossCut,
  names_from = "Nexus",
  values_from = "Freq"
)
#reshaped_CrossCut_Nexus$rowname <- rownames(reshaped_CrossCut_Nexus)

# Use pivot_wider to reshape the data frame
reshaped_Nexus_NChallenge <- pivot_wider(
  data = Nexus_NChallenge,
  names_from = "Nexus",
  values_from = "Freq"
)

colnames(reshaped_Nexus_NChallenge)[1] <- "CrossCut"


#reshaped_CrossCut_NChallenge$rowname <- rownames(reshaped_CrossCut_NChallenge)
# Assuming 'reshaped_Gov_Nexus' and 'reshaped_Pol_Nexus' are your data frames
merged <- bind_rows(reshaped_Nexus_CrossCut, reshaped_Nexus_NChallenge) %>%
  group_by_all() %>%
  summarise_all(~coalesce(., 0)) %>%
  ungroup()

merged <- merged[, !colnames(merged) %in% "Food"]
# Remove row where "nexus" is "food"
#merged <- subset(merged, Nexus != "Food")
merged_table_remove_names <- merged
merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:ncol(merged_table_remove_names)])

# Replace 'your_data_frame' with the name of your data frame
merged_table_remove_names <- merged_table_remove_names %>%
  select(sort(colnames(.)))

# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
color_palette <- colorRampPalette(c("#D9AA80", "#B65719", "#791E32"))(100)

# Replace all NAs with 0 in the entire dataframe
merged_table_remove_names[is.na(merged_table_remove_names)] <- 0


# Calculate the total count for each column
total_counts <- colSums(merged_table_remove_names)
#total_counts <- sum(total_counts)
# Your merged table
test <- matrix(0, nrow = 33, ncol = 22) # Example matrix dimension

# Iterate over each column and divide by the corresponding total count
for (i in 1:ncol(test)) {
  test[,i] <- total_counts[i]
}

# Print the updated table
print(merged_table_remove_names)
# Convert counts to percentages
merged_table_perc <- merged_table_remove_names / test * 100

# Show the table with percentages
print(merged_table_perc)



# Create the heatmap
heatmap_obj <- ComplexHeatmap::Heatmap(
  as.matrix(merged_table_perc),
  name = "Percentage 
of studies",
  col = color_palette,
  cluster_rows = F,
  cluster_columns = F,
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.text(sprintf("%.0f", merged_table_perc[i, j]), x, y, gp = gpar(fontsize = 12))
  },
  row_labels = c(merged$CrossCut),
  show_row_names = TRUE,
  show_column_names = TRUE,  # Turn off column labels at the bottom
  column_names_rot = 40,
  column_names_side = c("top"),
  show_row_dend = FALSE,
  #column_title = "Column Labels",  # Add a title for column labels
  column_title_side = "top",  # Place the title at the top
  column_title_gp = gpar(fontsize = 14),  # Customize the title font size
  row_names_gp = gpar(fontsize = 12)
)


# Save the heatmap to a PNG file
png("./heatmap_chal_crosscut2.png", width = 8000, height = 6000, res = 600)  # Adjust width and height as needed
print(heatmap_obj)
dev.off()  # Close the PNG device

#Option 2
# crosstab of governance approaches versus nexus elements (nexus elements merged)
CrossCut_Nexus <- get_crosstab(Data_Select_Split, "CrossCut", "Nexus", Merge1 = FALSE, Merge2 = FALSE)
CrossCut_NChallenge <- get_crosstab(Data_Select_Split, "CrossCut", "NChallenge", Merge1 = FALSE, Merge2 = FALSE)
CrossCut_Nexus <- as.data.frame(CrossCut_Nexus)
CrossCut_NChallenge <- as.data.frame(CrossCut_NChallenge)

# Use pivot_wider to reshape the data frame
reshaped_CrossCut_Nexus <- pivot_wider(
  data = CrossCut_Nexus,
  names_from = "Nexus",
  values_from = "Freq"
)
#reshaped_CrossCut_Nexus$rowname <- rownames(reshaped_CrossCut_Nexus)

# Use pivot_wider to reshape the data frame
reshaped_CrossCut_NChallenge <- pivot_wider(
  data = CrossCut_NChallenge,
  names_from = "NChallenge",
  values_from = "Freq"
)
#reshaped_CrossCut_NChallenge$rowname <- rownames(reshaped_CrossCut_NChallenge)

merged <- merge(reshaped_CrossCut_Nexus, reshaped_CrossCut_NChallenge, by = "CrossCut")

# Replace "_" with " " in a specific column 
merged_table_remove_names <- merged
merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:ncol(merged)])

# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
color_palette <- colorRampPalette(c("#D9AA80", "#B65719", "#791E32"))(100)

# Create the heatmap
heatmap_obj <- ComplexHeatmap::Heatmap(
  as.matrix(merged_table_remove_names),
  name = "No. of
studies",
  col = my_colors,
  cluster_rows = T,
  cluster_columns = F,
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.text(sprintf("%.0f", merged_table_remove_names[i, j]), x, y, gp = gpar(fontsize = 12))
  },
  row_labels = c(merged$CrossCut),
  show_row_names = TRUE,
  show_column_names = TRUE,  # Turn off column labels at the bottom
  column_names_rot = 40,
  column_names_side = c("top"),
  show_row_dend = FALSE,
  #column_title = "Column Labels",  # Add a title for column labels
  column_title_side = "top",  # Place the title at the top
  column_title_gp = gpar(fontsize = 14),  # Customize the title font size
  row_names_gp = gpar(fontsize = 12)
)

# Save the heatmap to a PNG file
png("./heatmap_chal_crosscut_option2.png", width = 8000, height = 6000, res = 600)  # Adjust width and height as needed
print(heatmap_obj)
dev.off()  # Close the PNG device



##Nexus vs actors
# crosstab of governance approaches versus nexus elements (nexus elements merged)
Actors_Nexus <- get_crosstab(Data_Select_Split, "Actors", "Nexus", Merge1 = FALSE, Merge2 = TRUE)
#Actors_NChallenge <- get_crosstab(Data_Select_Split, "Actors", "NChallenge", Merge1 = FALSE, Merge2 = FALSE)
Actors_Nexus <- as.data.frame(Actors_Nexus)
#Actors_NChallenge <- as.data.frame(Actors_NChallenge)

# Use pivot_wider to reshape the data frame
reshaped_Actors_Nexus <- pivot_wider(
  data = Actors_Nexus,
  names_from = "Nexus",
  values_from = "Freq"
)
#reshaped_CrossCut_Nexus$rowname <- rownames(reshaped_CrossCut_Nexus)
# Use pivot_wider to reshape the data frame
# reshaped_Actors_NChallenge <- pivot_wider(
#   data = Actors_NChallenge,
#   names_from = "NChallenge",
#   values_from = "Freq"
# )
#reshaped_CrossCut_NChallenge$rowname <- rownames(reshaped_CrossCut_NChallenge)
# merged <- merge(reshaped_Actors_Nexus, reshaped_Actors_NChallenge, by = "Actors")
# 
# # Replace "_" with " " in a specific column 
# merged_table_remove_names <- merged
# merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:ncol(merged)])

merged_table_remove_names <- as.data.frame(reshaped_Actors_Nexus[2:ncol(reshaped_Actors_Nexus)])
merged_table_remove_names <- merged_table_remove_names[, !colnames(merged_table_remove_names) %in% "Food"]
# Replace 'your_data_frame' with the name of your data frame
merged_table_remove_names <- merged_table_remove_names %>%
  select(sort(colnames(.)))


# Calculate the total count for each column
total_counts <- colSums(merged_table_remove_names)
#total_counts <- sum(total_counts)
# Your merged table
test <- matrix(0, nrow = 8, ncol = 21) # Example matrix dimension

# Iterate over each column and divide by the corresponding total count
for (i in 1:ncol(test)) {
  test[,i] <- total_counts[i]
}

# Print the updated table
print(merged_table_remove_names)

# Convert counts to percentages
merged_table_perc <- merged_table_remove_names / test * 100

# Show the table with percentages
print(merged_table_perc)




# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
color_palette <- colorRampPalette(c("#D9AA80", "#B65719", "#791E32"))(100)

# Original vector
actors <- c(
  "Civil Society and 
Community-Based Organisations",
  "Financial Institutions",
  "Global/Regional Institutions 
and Science-Policy Interfaces",
  "IPLCs",
  "Knowledge and 
Educational Communities",
  "Local/National Governments 
and Municipalities",
  "Media and the Arts",
  "Private Sector and 
Business Organisations"
)

# Create the heatmap
heatmap_obj <- ComplexHeatmap::Heatmap(
  as.matrix(merged_table_perc),
  name = "Percentage 
of studies",
  col = color_palette,
  cluster_rows = T,
  cluster_columns = F,
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.text(sprintf("%.0f", merged_table_perc[i, j]), x, y, gp = gpar(fontsize = 12))
  },
  row_labels = actors,
  show_row_names = TRUE,
  show_column_names = TRUE,  # Turn off column labels at the bottom
  column_names_rot = 40,
  column_names_side = c("top"),
  show_row_dend = FALSE,
  #column_title = "Column Labels",  # Add a title for column labels
  column_title_side = "top",  # Place the title at the top
  column_title_gp = gpar(fontsize = 14),  # Customize the title font size
  row_names_gp = gpar(fontsize = 12)
)

# Save the heatmap to a PNG file
png("./heatmap_actors1.png", width = 8000, height = 6000, res = 600)  # Adjust width and height as needed
print(heatmap_obj)
dev.off()  # Close the PNG device


#Nexus vs challenges
# crosstab of governance approaches versus nexus elements (nexus elements merged)
#Actors_Nexus <- get_crosstab(Data_Select_Split, "Actors", "Nexus", Merge1 = FALSE, Merge2 = TRUE)
Nexus_NChallenge <- get_crosstab(Data_Select_Split, "Nexus", "NChallenge", Merge1 = TRUE, Merge2 = FALSE)
Nexus_NChallenge <- as.data.frame(Nexus_NChallenge)
#Actors_NChallenge <- as.data.frame(Actors_NChallenge)

# Use pivot_wider to reshape the data frame
reshaped_Nexus_NChallenge <- pivot_wider(
  data = Nexus_NChallenge,
  names_from = "Nexus",
  values_from = "Freq"
)

names <- reshaped_Nexus_NChallenge$NChallenge            

#reshaped_CrossCut_Nexus$rowname <- rownames(reshaped_CrossCut_Nexus)
# Use pivot_wider to reshape the data frame
# reshaped_Actors_NChallenge <- pivot_wider(
#   data = Actors_NChallenge,
#   names_from = "NChallenge",
#   values_from = "Freq"
# )
#reshaped_CrossCut_NChallenge$rowname <- rownames(reshaped_CrossCut_NChallenge)

#merged <- merge(reshaped_Actors_Nexus, reshaped_Actors_NChallenge, by = "Actors")

# Replace "_" with " " in a specific column 
merged_table_remove_names <- reshaped_Nexus_NChallenge
merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:ncol(merged)])
merged_table_remove_names <- merged_table_remove_names[, !colnames(merged_table_remove_names) %in% "Food"]

reshaped_Nexus_NChallenge <- merged_table_remove_names %>%
  select(sort(colnames(.)))

# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
color_palette <- colorRampPalette(c("#D9AA80", "#B65719", "#791E32"))(100)

# # Original vector
# actors <- c(
#   "Civil Society and 
# Community-Based Organisations",
#   "Financial Institutions",
#   "Global/Regional Institutions 
# and Science-Policy Interfaces",
#   "IPLCs",
#   "Knowledge and 
# Educational Communities",
#   "Local/National Governments 
# and Municipalities",
#   "Media and the Arts",
#   "Private Sector and 
# Business Organisations"
# )

# Create the heatmap
heatmap_obj <- ComplexHeatmap::Heatmap(
  as.matrix(merged_table_remove_names),
  name = "No. of
studies",
  col = color_palette,
  cluster_rows = T,
  cluster_columns = F,
  cell_fun = function(j, i, x, y, width, height, fill) {
    grid.text(sprintf("%.0f", merged_table_remove_names[i, j]), x, y, gp = gpar(fontsize = 12))
  },
  row_labels = names,
  show_row_names = TRUE,
  show_column_names = TRUE,  # Turn off column labels at the bottom
  column_names_rot = 40,
  column_names_side = c("top"),
  show_row_dend = FALSE,
  #column_title = "Column Labels",  # Add a title for column labels
  column_title_side = "top",  # Place the title at the top
  column_title_gp = gpar(fontsize = 14),  # Customize the title font size
  row_names_gp = gpar(fontsize = 12)
)

# Save the heatmap to a PNG file
png("./heatmap_chal.png", width = 8000, height = 6000, res = 600)  # Adjust width and height as needed
print(heatmap_obj)
dev.off()  # Close the PNG device






# #Option 2
# # crosstab of governance approaches versus nexus elements (nexus elements merged)
# Actors_Nexus <- get_crosstab(Data_Select_Split, "Actors", "Nexus", Merge1 = FALSE, Merge2 = FALSE)
# Actors_NChallenge <- get_crosstab(Data_Select_Split, "Actors", "NChallenge", Merge1 = FALSE, Merge2 = FALSE)
# Actors_Nexus <- as.data.frame(Actors_Nexus)
# Actors_NChallenge <- as.data.frame(Actors_NChallenge)
# 
# # Use pivot_wider to reshape the data frame
# reshaped_Actors_Nexus <- pivot_wider(
#   data = Actors_Nexus,
#   names_from = "Nexus",
#   values_from = "Freq"
# )
# #reshaped_CrossCut_Nexus$rowname <- rownames(reshaped_CrossCut_Nexus)
# 
# # Use pivot_wider to reshape the data frame
# reshaped_Actors_NChallenge <- pivot_wider(
#   data = Actors_NChallenge,
#   names_from = "NChallenge",
#   values_from = "Freq"
# )
# #reshaped_CrossCut_NChallenge$rowname <- rownames(reshaped_CrossCut_NChallenge)
# 
# merged <- merge(reshaped_Actors_Nexus, reshaped_Actors_NChallenge, by = "Actors")
# 
# # Replace "_" with " " in a specific column 
# merged_table_remove_names <- merged
# merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:ncol(merged)])
# 
# # Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
# color_palette <- colorRampPalette(c("#D9AA80", "#B65719", "#791E32"))(100)
# 
# # Original vector
# actors <- c(
#   "Civil Society and 
# Community-Based Organisations",
#   "Financial Institutions",
#   "Global/Regional Institutions 
# and Science-Policy Interfaces",
#   "IPLCs",
#   "Knowledge and 
# Educational Communities",
#   "Local/National Governments 
# and Municipalities",
#   "Media and the Arts",
#   "Private Sector and 
# Business Organisations"
# )
# 
# # Create the heatmap
# heatmap_obj <- ComplexHeatmap::Heatmap(
#   as.matrix(merged_table_remove_names),
#   name = "No. of
# studies",
#   col = my_colors,
#   cluster_rows = T,
#   cluster_columns = F,
#   cell_fun = function(j, i, x, y, width, height, fill) {
#     grid.text(sprintf("%.0f", merged_table_remove_names[i, j]), x, y, gp = gpar(fontsize = 14))
#   },
#   row_labels = actors,
#   show_row_names = TRUE,
#   show_column_names = TRUE,  # Turn off column labels at the bottom
#   column_names_rot = 40,
#   column_names_side = c("top"),
#   show_row_dend = FALSE,
#   #column_title = "Column Labels",  # Add a title for column labels
#   column_title_side = "top",  # Place the title at the top
#   #column_title_gp = gpar(fontsize = 16),  # Customize the title font size
#   row_names_gp = gpar(fontsize = 13),
#   column_names_gp = gpar(fontsize = 13),
# )
# 
# # Save the heatmap to a PNG file
# png("./heatmap_chal_actors_option2.png", width = 8000, height = 6000, res = 600)  # Adjust width and height as needed
# print(heatmap_obj)
# dev.off()  # Close the PNG device




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
