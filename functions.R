# function to get crossing combinations
get_crossed <- function(X, Y, Names) {

  X <- as_tibble(gsub(".*Other.*", "Other", X) %>% na.omit())
  names(X) <- Names[1]

  Y <- as_tibble(gsub(".*Other.*", "Other", Y) %>% na.omit())
  names(Y) <- Names[2]

  Output <- crossing(X, Y)

  return(Output)
}

# function to check for a nexus challenge
check_challenge <- function(Data, Chall, ChallLookup) {
  Data <- as_tibble(gsub(".*Other.*", "Other", Data) %>% na.omit())
  names(Data) <- c("Nexus Challenges")
  Data <- Data %>% left_join(Lookup, by = join_by(`Nexus Challenges` == `Original`)) %>%
                  select(-`Nexus Challenges`) %>% rename('Nexus Challenges' = New)
  return(any(Data == Chall))
}

# function to check for a nexus element
check_nexus <- function(Data, Nex) {
  Data <- as_tibble(gsub(".*Other.*", "Other", Data) %>% na.omit())
  names(Data) <- c("Nexus Element")
  return(any(Data == Nex))
}

# funciton to replace text containing "other" with just "other"
replace_other <- function(X) {
  return(as_tibble(gsub(".*Other.*", "Other", X) %>% na.omit()))
}
