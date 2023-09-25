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

# function to replace text containing "other" with just "other"
replace_other <- function(X) {
  return(as_tibble(gsub(".*Other.*", "Other", X) %>% na.omit()))
}

# function to create all combinations of nexus challenges and nexus elements for a given paper
# X = vector of nexus challenges, Y = vector of nexus elements
get_challenge_nexus <- function(X, Y) {
  if (is.na(X[1]) | is.na(Y[1])) {
      return(c(NA, NA) %>% t() %>% as_tibble() %>% mutate(NChallenge = as.character(V1), Nexus = as.character(V2)) %>% select(-V1, -V2))
  }
  else {
    return(expand_grid(X, Y) %>% rename(NChallenge = names(.)[1], Nexus = names(.)[2]))
  }
}