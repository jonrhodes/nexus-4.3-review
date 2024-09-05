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

# join to data
JoinedData <- Data %>% left_join(RefsGrey, by = join_by(`Covidence #`))

# split data into peer reviewed and grey literature sets
Data <- filter(JoinedData, is.na(Tags)) %>% select(-Tags)
Data_Grey <- filter(JoinedData, !is.na(Tags)) %>% select(-Tags)

# extract data on paper types, regions, scale, nexus elements, nexus challenges
# governance types, policy instruments, actors, and cross cutting issues
Data_Select <- Data %>%
              select(CovidenceID = `Covidence #`, Title, PaperType = `What type of paper is this?`, Region = `Select all geographic regions the paper focusses on according to UN standard area codes (https://unstats.un.org/unsd/methodology/m49/)`, Scale = `Select the relevant spatial scales (extent) of the study. Choose all that apply.`, Nexus = `Which nexus elements are considered?`, NChallenge = `Does the study provide evidence for addressing any of the following nexus challenges?`, Gov = `Are any of the following governance approaches proposed or assessed as solutions to the above nexus challenges? Use your judgement to select one of the four governance approaches listed and then use the \"other\" category to list any specific governance approaches referred to in the study (separate multiple governance approaches with \",\")`, Policy = `What type of policy instruments are considered to operationalise the response options proposed or assessed?`, Actors = `Which types of actors are involved in the implementation of the response options proposed or assessed?`, CrossCut = `Which of the following cross cutting issues are considered?`)

# change "Climate" to "Climate change" in nexus elements
Data_Select <- Data_Select %>% mutate(Nexus = str_replace_all(Nexus, "Climate", "Climate change"))

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

# remove duplicates
Data_Select_Split <- Data_Select_Split %>% mutate(NChallenge = map(NChallenge,
                           .f = function(x) {if (length(x) > 1) {return(unique(x))} else {return(x)}}))

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

# remove duplicates
Data_Select_Split <- Data_Select_Split %>% mutate(Gov = map(Gov,
                           .f = function(x) {if (length(x) > 1) {return(unique(x))} else {return(x)}}))

# remove "other" category from policy instruments - change code here to avoid this or to do something else
Data_Select_Split <- Data_Select_Split %>% mutate(Policy = map(Policy,
                        .f = function(x) {if (all(is.na(x))) {return(NA)} else
                          {y <- as_tibble(x) %>% rename(Policy = value) %>%
                          mutate(Policy = ifelse(str_detect(Policy, fixed("other",
                                                                                ignore_case = TRUE)), NA, Policy));
                          if (all(is.na(y$Policy))) {return(as.vector(y$Policy))} else
                          {return(as.vector(filter(y, !is.na(Policy))$Policy))}
                        }}))

# remove duplicates
Data_Select_Split <- Data_Select_Split %>% mutate(Policy = map(Policy,
                           .f = function(x) {if (length(x) > 1) {return(unique(x))} else {return(x)}}))

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

# remove duplicates
Data_Select_Split <- Data_Select_Split %>% mutate(Actors = map(Actors,
                           .f = function(x) {if (length(x) > 1) {return(unique(x))} else {return(x)}}))

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

# remove duplicates
Data_Select_Split <- Data_Select_Split %>% mutate(CrossCut = map(CrossCut,
                           .f = function(x) {if (length(x) > 1) {return(unique(x))} else {return(x)}}))

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

# get counts of each governance type considered
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

# get circular plot of nexus challenges versus nexus elements - figure 4.5

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
  geom_bar(aes(x = as.factor(id), y = value, fill = observation), stat="identity", alpha = 1) + scale_fill_manual(values = c("#B65719", "#C6D68A", "#791E32", "#4A928F")) +

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
    #legend.position = "bottom",
    legend.position = c(0.35, 0.1),
    legend.justification = c("left", "top"),
    legend.direction = "horizontal",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-5, 4), "cm")
  ) + guides(fill = guide_legend(title = "Number of Nexus Elements:")) +

  coord_polar() +

  # Add labels on top of each bar
  geom_text(data = label_data, aes(x = id, y= tot + 10, label = Nexus, hjust = hjust), color="black", fontface = "bold",alpha = 0.6, size = 4, angle = label_data$angle, inherit.aes = FALSE) +

  # Add base line information
  geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6 , inherit.aes = FALSE) +
  geom_text(data = base_data, aes(x = title, y = -18, label = NChallenge), hjust=c(1, 1, 0.75, 0, 0), colour = "black", alpha = 0.8, size = 4, fontface = "bold", inherit.aes = FALSE)

# Save figure as jpg and eps format
ggsave(p, file = "challenges_nexus_circular.jpg", width = 25, height = 30, units = "cm")
ggsave(p, file = "challenges_nexus_circular.eps", width = 25, height = 30, units = "cm")

# create heatmap plots for figure on governance, policy instruments, and nexus challenges - figure 4.4

# reclass governance types to include "Other"
Data_Select_Split_GovRcl <- Data_Select_Split %>% mutate(Gov = map(Gov, .f = function (x) {ifelse((x == "Community Governance" | x == "Hierarchical Governance" | x == "Market Governance" | x == "Network Governance"), x, "Other")}))

# get all unique nexus challenges
Unique_Challenges <- sort(unique(unlist(Data_Select_Split_GovRcl$NChallenge)))[c(1, 3, 5, 4, 2)]

# get all unique governance types
Unique_Governance <- sort(unique(unlist(Data_Select_Split_GovRcl$Gov)))[c(2, 3, 4, 1, 5)]

# get all unique policy instruments
Unique_Policy <- sort(unique(unlist(Data_Select_Split_GovRcl$Policy)))[c(1, 2, 4, 3)]

Matrix_List <- list()
# loop through unique challenges and get governance types and policy instruments combinations
for (i in 1:length(Unique_Challenges)) {

  # get rows in data that match this challenge
  ThisChallenge <- Data_Select_Split_GovRcl[which(unlist(Data_Select_Split_GovRcl$NChallenge %>% map(.f = function(x) {return(any(x == Unique_Challenges[i]))}))),]

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

  p <- ggplot(PlotData, aes(Policy, Gov, col = n, fill = n, label = n)) +
    geom_tile(color = "white", lwd = 4, linetype = 1) + geom_text(size = 10, color = "black") +
    theme_minimal() +
    scale_fill_gradientn(colours = c("#D9AA80", "#C3773E", "#B65719"), limits = c(0, 32)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")
  ggsave(p, file = paste(names(Matrix_List)[i], ".jpg", sep = ""), width = 10, height = 10, units = "cm")
}

# create a special template plot
PlotData <- as_tibble(Matrix_List[[1]]) %>% mutate(Gov = factor(Gov, levels = rev(Unique_Governance)), Policy = factor(Policy, levels = Unique_Policy)) %>% mutate(n = 0)

p <- ggplot(PlotData, aes(Policy, Gov, col = n, fill = n, label = n)) +
   geom_tile(color = "black", lwd = 1, linetype = 1) +
   theme_minimal() +
   scale_fill_gradientn(colours = c("white"), limits = c(0, 0)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(legend.position = "none")

ggsave(p, file = "Template.jpg", width = 10, height = 10, units = "cm")

#Code to create figure 4.3 - Geographic distribution of studies
#Can download shp file here (not official UN data): https://thematicmapping.org/downloads/world_borders.php
#Can download the official UN shpfile here (but it only goes to region, not the subregions that we use): https://data.unhabitat.org/datasets/GUO-UN-Habitat::m49-regions/about
#Can download the UN country to region table here: https://unstats.un.org/unsd/methodology/m49/overview/
# https://data.unhabitat.org/search?collection=Dataset&q=M49%20regions
# Read the countries shapefile 
shp_data <- st_read("./TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
# Transform the projection to Mollweide (EPSG:54009)
shp_data <- st_transform(shp_data, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# Rename the column in shp_data to match the UN table
shp_data <- shp_data %>%
  rename(ISO.alpha3.Code = ISO3)
# Read the UN table file into a data frame
csv_data <- read.csv("./UNSD — Methodology.csv", sep = ";")
#Merge together
merged_data <- merge(shp_data, csv_data, by = "ISO.alpha3.Code")

# Read in the region counts from review
region_counts <- read.csv("./regions_counts.csv")
# Rename region column name to match UN table
colnames(region_counts)[colnames(region_counts) == "Region"] <- "Sub.region.Name"
#Merge together
merged_data <- merge(merged_data, region_counts, by = "Sub.region.Name")

# Define the color scale from red (highest) to yellow (lowest)
#color_scale <-  scale_fill_gradientn(colors = c("#D9AA80", "#B65719", "#791E32"))

color_scale <-  scale_fill_gradientn(colors = c("#D9AA80", "#C3773E", "#B65719"))

# Plot the shapefile without displaying country polygon borders
my_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = n), color = NA, size = 0, linetype = "blank") +  
  color_scale +
  labs(title = "Number of studies in each region", fill = "Count") +
  theme_minimal()

# Export the ggplot as a png and an eps image
ggsave(filename = "./studies_region_counts_300324.jpg", plot = my_plot, width = 6, height = 4, dpi = 300)
ggsave(filename = "./studies_region_counts_300324.eps", plot = my_plot, width = 6, height = 4, dpi = 300)
