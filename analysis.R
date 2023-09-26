# load packages
library(tidyverse)
library(ggalluvial)

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

# use lookup tables to recategorise nexus challenges, governance, and actors

# nexus challenges

# get look up table so as to rename nexus challenges with correct names
LookupNC <- unique(unlist(Data_Select_Split$NChallenge))[unique(unlist(Data_Select_Split$NChallenge)) %>%
          str_detect(fixed("other", ignore_case = TRUE), negate = TRUE) %>% which()]
LookupNC <- LookupNC %>% as_tibble() %>% rename(NChallenge = value) %>% mutate(NChallenge_New = NChallenge) %>%
              mutate(NChallenge_New = case_when(
                str_detect(NChallenge, "Complexity") ~ "Complexity Challenges",
                str_detect(NChallenge, "Values") ~ "Values Challenges",
                str_detect(NChallenge, "Governance") ~ "Governance Challenges",
                str_detect(NChallenge, "Financial") ~ "Financing Challenges",
                str_detect(NChallenge, "Knowledge") ~ "Scaling Challenges"
              ))

# write to csv
write_csv(LookupNC, "nchallenges_lookup.csv")

# rename nexus challenges - note that this removes "other" responses - change code here to avoid this or do something else
Data_Select_Split <- Data_Select_Split %>% mutate(NChallenge = map(NChallenge,
                     .f = function(x) {y <- as_tibble(x) %>% left_join(LookupNC, by = join_by(value == NChallenge)) %>% select(-value);
                       if (all(is.na(y$NChallenge_New))) {return(as.vector(y$NChallenge_New))} else
                       {return(as.vector(filter(y, !is.na(NChallenge_New))$NChallenge_New))}}))

# actors

# get look up table so as to rename actors with new actor categories
LookupAct <- unique(unlist(Data_Select_Split$Actors))[unique(unlist(Data_Select_Split$Actors)) %>%
          str_detect(fixed("other", ignore_case = TRUE), negate = TRUE) %>% which()]
LookupAct <- LookupAct %>% as_tibble() %>% rename(Actors = value) %>% mutate(Actors_New = Actors) %>%
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

# rename actors - note that this removes "other" responses - change code here to avoid this or do something else
Data_Select_Split <- Data_Select_Split %>% mutate(Actors = map(Actors,
                     .f = function(x) {y <- as_tibble(x) %>% left_join(LookupAct, by = join_by(value == Actors)) %>% select(-value);
                       if (all(is.na(y$Actors_New))) {return(as.vector(y$Actors_New))} else
                       {return(as.vector(filter(y, !is.na(Actors_New))$Actors_New))}}))

# governance - STILL TO FINISH THIS

# get look up table so as to rename governance types
LookupGov <- unique(unlist(Data_Select_Split$Gov)) %>% as_tibble() %>%
                mutate(value = str_remove(value, fixed("other: ", ignore_case = TRUE)) %>%
                  str_squish() %>% str_split(","))

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

# summarise nexus elements -  remove "Other" responses

# get counts of each nexus elements
Nexuses <- unlist(Data_Select_Split$Nexus) %>% as_tibble() %>%
      mutate(Nexus = value) %>% count(Nexus)

# write to csv
write_csv(NChallenges, "nexuses_counts.csv")

# get counts of the numbers of nexus elements considered
NumNexuses <- Data_Select_Split$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% count(NumNexus)

# write to csv
write_csv(NumNexuses, "num_nexuses_counts.csv")

# get median number of nexus elements considered
Data_Select_Split$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% summarise(across(NumNexus, median, na.rm = TRUE))

# get mean number of nexus elements considered
Data_Select_Split$Nexus %>% map(.f = function (x)
      {ifelse((length(x) == 1) & is.na(x[1]), NA, length(x))}) %>% unlist() %>%
      as_tibble() %>% mutate(NumNexus = value) %>% summarise(across(NumNexus, mean, na.rm = TRUE))

# get alluvial plot of nexus challenges versus nexus elements

# get all combinations of nexus challenges and nexus elementsd for each study
Challenges_Nexuses <- Data_Select_Split$NChallenge %>% map2(Data_Select_Split$Nexus,
                       .f = get_challenge_nexus)
# row bind list and remove NA values
Challenges_Nexuses <- do.call(bind_rows, Challenges_Nexuses) %>%
      filter(!is.na(NChallenge))
# group by challenges and nexus elements and calculatye the frequency of each unique
# combination
Challenges_Nexuses <- Challenges_Nexuses %>% group_by(NChallenge, Nexus) %>%
    summarize(Freq = n()) %>% ungroup()

# create alluvium plot - I GOT STUCK HERE
ggplot(Challenges_Nexuses, aes(y = Freq, axis1 = NChallenge, axis2 = Nexus)) +
    geom_alluvium(aes(fill = Freq), width = 1/12) +
    geom_stratum(width = 1/12, fill = "black", color = "grey") +
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Nexus challenge", "Nexus element"), expand = c(.05, .05)) +
    scale_fill_brewer(type = "qual", palette = "Set1")

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
