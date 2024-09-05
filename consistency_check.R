# THIS SCRIPT DOES A CONSISTENCY CHECK BASED ON THE SUBSET OF ARTICLES THAT WERE REVIEWED BY TWO REVIEWERS

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

# load review data from covidence and remove "consensus" entries
Data <- read_csv("review_283729_20240528164400_consistency_check.csv") %>% mutate(`Covidence #` = paste0("#", as.character(`Covidence #`))) %>% filter(`Reviewer Name` != "Consensus")

# extract data on paper types, regions, scale, nexus elements, nexus challenges
# governance types, policy instruments, actors, and cross cutting issues
Data_Select <- Data %>%
              select(CovidenceID = `Covidence #`, Title, PaperType = `What type of paper is this?`, Region = `Select all geographic regions the paper focusses on according to UN standard area codes (https://unstats.un.org/unsd/methodology/m49/)`, Scale = `Select the relevant spatial scales (extent) of the study. Choose all that apply.`, Nexus = `Which nexus elements are considered?`, NChallenge = `Does the study provide evidence for addressing any of the following nexus challenges?`, Gov = `Are any of the following governance approaches proposed or assessed as solutions to the above nexus challenges? Use your judgement to select one of the four governance approaches listed and then use the \"other\" category to list any specific governance approaches referred to in the study (separate multiple governance approaches with \",\")`, Policy = `What type of policy instruments are considered to operationalise the response options proposed or assessed?`, Actors = `Which types of actors are involved in the implementation of the response options proposed or assessed?`, CrossCut = `Which of the following cross cutting issues are considered?`)

# save data for manual error checking - we used this to ckeck for errors and correct in
# covidence where necessary
write_csv(Data_Select, "consistency_check/error_check.csv")

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
write_csv(LookupNC, "consistency_check/nchallenges_lookup.csv")

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
write_csv(LookupGov, "consistency_check/governance_lookup.csv")

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
write_csv(LookupAct, "consistency_check/actors_lookup.csv")

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
write_csv(LookupCC, "consistency_check/crosscut_lookup.csv")

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

# split into two data sets reviewed by different reviewers
Data_Select_Split1 <- Data_Select_Split[duplicated(Data_Select_Split$CovidenceID, fromLast = FALSE), ]
Data_Select_Split2 <- Data_Select_Split[duplicated(Data_Select_Split$CovidenceID, fromLast = TRUE), ]

# then do the analysis for both data sets

# summarise paper types

# get which papers are reviews
# and count them
Count_Review1 <- map(Data_Select_Split1$PaperType, .f = function(x)
      {any(x == "Review")}) %>%
      unlist() %>% which() %>% length()
Count_Review1
Count_Review2 <- map(Data_Select_Split2$PaperType, .f = function(x)
      {any(x == "Review")}) %>%
      unlist() %>% which() %>% length()
Count_Review2

# get which papers are only perspective, cocceptual/theoretical, or opinion papers
# and count them
Count_Pers_Concept_Opinion_Only1 <- map(Data_Select_Split1$PaperType, .f = function(x)
      {any((x == "Perspective") | (x == "Conceptual/theoretical") | (x == "Opinion")) &
      !any((x == "Review") | (x == "Empirical") | (x == "Modelling"))}) %>%
      unlist() %>% which() %>% length()
Count_Pers_Concept_Opinion_Only1
Count_Pers_Concept_Opinion_Only2 <- map(Data_Select_Split2$PaperType, .f = function(x)
      {any((x == "Perspective") | (x == "Conceptual/theoretical") | (x == "Opinion")) &
      !any((x == "Review") | (x == "Empirical") | (x == "Modelling"))}) %>%
      unlist() %>% which() %>% length()
Count_Pers_Concept_Opinion_Only2

# get which papers which contain empirical data
# and count them
Count_Empirical1 <- map(Data_Select_Split1$PaperType, .f = function(x)
      {any(x == "Empirical")}) %>%
      unlist() %>% which() %>% length()
Count_Empirical1
Count_Empirical2 <- map(Data_Select_Split2$PaperType, .f = function(x)
      {any(x == "Empirical")}) %>%
      unlist() %>% which() %>% length()
Count_Empirical2

# get which papers which contain modelling studies
# and count them
Count_Modelling1 <- map(Data_Select_Split1$PaperType, .f = function(x)
      {any(x == "Modelling")}) %>%
      unlist() %>% which() %>% length()
Count_Modelling1
Count_Modelling2 <- map(Data_Select_Split2$PaperType, .f = function(x)
      {any(x == "Modelling")}) %>%
      unlist() %>% which() %>% length()
Count_Modelling2

# identify which papers are assessments or reports
# and count them
Count_Assessment1 <- map(Data_Select_Split1$PaperType, .f = function(x)
      {any(str_detect(x, fixed("assessment", ignore_case = TRUE)) |
      str_detect(x, fixed("report", ignore_case = TRUE)))}) %>%
      unlist() %>% which() %>% length()
Count_Assessment1
Count_Assessment2 <- map(Data_Select_Split2$PaperType, .f = function(x)
      {any(str_detect(x, fixed("assessment", ignore_case = TRUE)) |
      str_detect(x, fixed("report", ignore_case = TRUE)))}) %>%
      unlist() %>% which() %>% length()
Count_Assessment2

# summarise regions

# get counts
Regions1 <- unlist(Data_Select_Split1$Region) %>% as_tibble() %>%
      mutate(Region = ifelse(is.na(value), "Not applicable (no specific spatial location)", value)) %>% count(Region)

# write to csv
write_csv(Regions1, "consistency_check/regions_counts1.csv")

# get counts
Regions2 <- unlist(Data_Select_Split2$Region) %>% as_tibble() %>%
      mutate(Region = ifelse(is.na(value), "Not applicable (no specific spatial location)", value)) %>% count(Region)

# write to csv
write_csv(Regions2, "consistency_check/regions_counts2.csv")

# summarise spatial scales

# get counts
Scales1 <- unlist(Data_Select_Split1$Scale) %>% as_tibble() %>%
      mutate(Scale = ifelse(is.na(value), "Not applicable (no specific spatial location)", value)) %>% count(Scale)

# write to csv
write_csv(Scales1, "consistency_check/scales_counts1.csv")

# get counts
Scales2 <- unlist(Data_Select_Split2$Scale) %>% as_tibble() %>%
      mutate(Scale = ifelse(is.na(value), "Not applicable (no specific spatial location)", value)) %>% count(Scale)

# write to csv
write_csv(Scales2, "consistency_check/scales_counts2.csv")

# summarise nexus challenges

# get counts
NChallenges1 <- unlist(Data_Select_Split1$NChallenge) %>% as_tibble() %>%
      mutate(NChallenge = value) %>% count(NChallenge)

# write to csv
write_csv(NChallenges1, "consistency_check/nchallenges_counts1.csv")

# get counts
NChallenges2 <- unlist(Data_Select_Split2$NChallenge) %>% as_tibble() %>%
      mutate(NChallenge = value) %>% count(NChallenge)

# write to csv
write_csv(NChallenges2, "consistency_check/nchallenges_counts2.csv")

# summarise nexus elements -  remove "other" responses

# get counts of each nexus element
Nexuses1 <- unlist(Data_Select_Split1$Nexus) %>% as_tibble() %>%
      mutate(Nexus = value) %>% count(Nexus)

# write to csv
write_csv(Nexuses1, "consistency_check/nexuses_counts1.csv")

# get counts of each nexus element
Nexuses2 <- unlist(Data_Select_Split2$Nexus) %>% as_tibble() %>%
      mutate(Nexus = value) %>% count(Nexus)

# write to csv
write_csv(Nexuses2, "consistency_check/nexuses_counts2.csv")

# get counts of the numbers of nexus elements considered
NumNexuses1 <- Data_Select_Split1$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% count(NumNexus)

# write to csv
write_csv(NumNexuses1, "consistency_check/num_nexuses_counts1.csv")

# get counts of the numbers of nexus elements considered
NumNexuses2 <- Data_Select_Split2$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% count(NumNexus)

# write to csv
write_csv(NumNexuses2, "consistency_check/num_nexuses_counts2.csv")

# get median number of nexus elements considered
Data_Select_Split1$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% summarise(across(NumNexus, \(x) median(x, na.rm = TRUE)))

# get mean number of nexus elements considered
Data_Select_Split1$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% summarise(across(NumNexus, \(x) mean(x, na.rm = TRUE)))

# get median number of nexus elements considered
Data_Select_Split2$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% summarise(across(NumNexus, \(x) median(x, na.rm = TRUE)))

# get mean number of nexus elements considered
Data_Select_Split2$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% summarise(across(NumNexus, \(x) mean(x, na.rm = TRUE)))

# summarise governance types

# get counts of each governance type considered
Govs1 <- unlist(Data_Select_Split1$Gov) %>% as_tibble() %>%
      mutate(Gov = value) %>% count(Gov)

# write to csv
write_csv(Govs1, "consistency_check/governances_counts1.csv")

# get counts of each governance type considered
Govs2 <- unlist(Data_Select_Split2$Gov) %>% as_tibble() %>%
      mutate(Gov = value) %>% count(Gov)

# write to csv
write_csv(Govs2, "consistency_check/governances_counts2.csv")

# summarise policy instruments

# get counts of each polcy instrument considered
Policies1 <- unlist(Data_Select_Split1$Policy) %>% as_tibble() %>%
      mutate(Policy = value) %>% count(Policy)

# write to csv
write_csv(Policies1, "consistency_check/policies_counts1.csv")

# get counts of each polcy instrument considered
Policies2 <- unlist(Data_Select_Split2$Policy) %>% as_tibble() %>%
      mutate(Policy = value) %>% count(Policy)

# write to csv
write_csv(Policies2, "consistency_check/policies_counts2.csv")

# summarise actors

# get counts of each actor type considered
Actorss1 <- unlist(Data_Select_Split1$Actors) %>% as_tibble() %>%
      mutate(Actors = value) %>% count(Actors)

# write to csv
write_csv(Actorss1, "consistency_check/actors_counts1.csv")

# get counts of each actor type considered
Actorss2 <- unlist(Data_Select_Split2$Actors) %>% as_tibble() %>%
      mutate(Actors = value) %>% count(Actors)

# write to csv
write_csv(Actorss2, "consistency_check/actors_counts2.csv")

# do the Fisher exact tests

# regions
RegionsFisher <- full_join(Regions1, Regions2, by = join_by(Region)) %>% mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>% mutate(n.y = ifelse(is.na(n.y), 0, n.y))

fisher.test(t(RegionsFisher[,2:3]))

# scales
ScalesFisher <- full_join(Scales1, Scales2, by = join_by(Scale)) %>% mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>% mutate(n.y = ifelse(is.na(n.y), 0, n.y))

fisher.test(t(ScalesFisher[,2:3]))

# challenges
ChallengesFisher <- full_join(NChallenges1, NChallenges2, by = join_by(NChallenge)) %>% mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>% mutate(n.y = ifelse(is.na(n.y), 0, n.y))

fisher.test(t(ChallengesFisher[1:5,2:3]))

# nexuses
NexusesFisher <- full_join(Nexuses1, Nexuses2, by = join_by(Nexus)) %>% mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>% mutate(n.y = ifelse(is.na(n.y), 0, n.y))

fisher.test(t(NexusesFisher[1:5,2:3]))

# numnexuses
NumNexusesFisher <- full_join(NumNexuses1, NumNexuses2, by = join_by(NumNexus)) %>% mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>% mutate(n.y = ifelse(is.na(n.y), 0, n.y))

fisher.test(t(NumNexusesFisher[1:4,2:3]))

# governance
GovsFisher <- full_join(Govs1, Govs2, by = join_by(Gov)) %>% mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>% mutate(n.y = ifelse(is.na(n.y), 0, n.y))

fisher.test(t(GovsFisher[c(1:11, 13, 14),2:3]))

# policies
PoliciesFisher <- full_join(Policies1, Policies2, by = join_by(Policy)) %>% mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>% mutate(n.y = ifelse(is.na(n.y), 0, n.y))

fisher.test(t(PoliciesFisher[1:4,2:3]))

# actors
ActorsFisher <- full_join(Actorss1, Actorss2, by = join_by(Actors)) %>% mutate(n.x = ifelse(is.na(n.x), 0, n.x)) %>% mutate(n.y = ifelse(is.na(n.y), 0, n.y))

fisher.test(t(ActorsFisher[1:8,2:3]))


