# function to create a crosstab table between two variables
# to use to create heatmaps - outputs the number of popers
# that appear against each combination
# Data = the data of the cleaned variables from the revie3w output (i.e., Data_Select_Split)
# Var1 = name of varialbe 1 (a character string)
# Var2 = name of variable 2 (a character string)
# Merge1 = TRUE/FALSE - whether to merge text string for Var1 for a given study together (e.g., for nexus elements) (optional and defaults to FALSE)
# Merge2 = TRUE/FALSE - whether to merge text string for Var2 for a given study together (e.g., for nexus elements) (optional and defaults to FALSE)
# Factors1 = optional argument to define factor levels for Var1 (either blank or a character vector of required factor levels)
# Factors2 = optional argument to define factor levels for Var1 (either blank or a character vector of required factor levels)
get_crosstab <- function(Data, Var1, Var2, Merge1 = FALSE, Merge2 = FALSE, Factors1 = NA, Factors2 = NA) {
  Output <- map2(Data[[Var1]], Data[[Var2]], .f = function(x, y) {
            if (all(is.na(x)) | all(is.na(y))) {
              return(as_tibble_row(setNames(rep(NA_character_, 2), c("Name1", "Name2"))))
            } else {
              if (Merge1 == FALSE) {
                Data1 <- x %>% as_tibble() %>% rename(Name1 = value) %>% filter(!is.na(Name1))
              } else {
                Data1 <- x %>% sort() %>% paste(collapse = " ") %>%
                    as_tibble() %>% rename(Name1 = value) %>% filter(!is.na(Name1))
              }
              if (Merge2 == FALSE) {
                Data2 <- y %>% as_tibble() %>% rename(Name2 = value) %>% filter(!is.na(Name2))
              } else {
                Data2 <- y %>% sort() %>% paste(collapse = " ") %>%
                    as_tibble() %>% rename(Name2 = value) %>% filter(!is.na(Name2))
              }
              Cross <- crossing(Data1, Data2)
              return(Cross)
            }})

  Output <- do.call(bind_rows, Output)
  Output <- Output[which(!is.na(Output[,1])),]
  if (all(is.na(Factors1))) {
    Output <- Output %>% mutate(Name1 = factor(Name1))
  } else {
    Output <- Output %>% mutate(Name1 = factor(Name1, levels = Factors1))
  }
  if (all(is.na(Factors2))) {
    Output <- Output %>% mutate(Name2 = factor(Name2))
  } else {
    Output <- Output %>% mutate(Name2 = factor(Name2, levels = Factors2))
  }
  names(Output) <- c(Var1, Var2)
  Output <- Output %>% table()
  return(Output)
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

# Function to find approximate matches
find_approximate_match <- function(col_name, lookup_table) {
  distances <- stringdist::stringdistmatrix(col_name, lookup_table)
  closest_match_idx <- which.min(distances)
  closest_match <- lookup_table[closest_match_idx]
  return(closest_match)
}