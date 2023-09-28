#Libraries
library(dplyr)
library(stringr)
library(gplots)
library(tidyr)
library(pheatmap)
library(stringdist)
rm(list = ls())

# Read the Review data
csv_data <- read.csv("D:/IPBES_review/data/review_283729_20230922170037.csv")
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
png("./heatmap_gov_policy.png", width = 600, height = 600)  # Adjust width and height as needed
print(heatmap)
dev.off()  # Close the PNG device


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

















#Code to create figure 4.14 - heatmap showing nexus elements, governance types and policy instruments
#Result_nexus_gov <- get_crosstab(Data = Data_Select_Split, Var1 = "Nexus", Var2 = "Gov", Merge1 = FALSE, Merge2 = FALSE, Factors1 = unique(Data_Select_Split$Nexus), Factors2 = rev(Unique_Governance))
#Result_nexus_policy <- get_crosstab(Data = Data_Select_Split, Var1 = "Nexus", Var2 = "Policy", Merge1 = FALSE, Merge2 = FALSE, Factors1 = unique(Data_Select_Split$Nexus), Factors2 = Unique_Policy)

#library(reshape2)
#Result_nexus_gov <- as.data.frame(Result_nexus_gov)
# Use dcast to create the matrix
#result_matrix <- dcast(Result_nexus_gov, Nexus ~ Gov, value.var = "Freq", fill = 0)
# Read the Review data
#csv_data <- Data
#governance_lookup <- read.csv("./governance_lookup.csv")

# Define the list of elements
#elements <- c("Biodiversity", "Climate", "Health", "Food", "Water")
# Generate all possible combinations of elements
#all_combinations <- unlist(sapply(1:length(elements), function(n) combn(elements, n, paste, collapse = "; ")))
Data_Select_Split <- as.data.frame(Data_Select_Split)
all_combinations <- unique(Data_Select_Split$Nexus)
#combo <- all_combinations[28]

#Need to use this later on for merging the generated tables together
table_names_policy <- c()
table_names_governance <- c()
#combo <- all_combinations[2:2]
# Create and name dataframes for each combination
for (x in 1:length(all_combinations)) {
  # Define a unique name for each dataframe based on the combination
  #df_name <- paste("subset_", gsub("[^[:alnum:]]", "_", combo), sep = "")
  # Concatenate the elements with underscores
  # Convert combo to a comma-separated character vector
  
  # Convert list elements to characters and concatenate
  #df_name <- paste("subset_", gsub("[^[:alnum:]]", "_", combo), sep = "")
  
  # Create the desired string
  df_name <- paste(all_combinations[x])
  # Add "subset_" to the beginning of the string
  #df_name <- paste("subset", df_name, sep = "_")
  # Filter for exact matches of the combination
  # subset_df <- Data_Select_Split %>%
  #   filter(Nexus == combo)
  # 
  subset_df <- Data_Select_Split %>%
    filter(Nexus %in% all_combinations[x])
  # Assign the dataframe to the dynamically generated name
  assign(df_name, subset_df)
  # Print or do something with the subset DataFrame
  # For example, print the first few rows
  # cat("Subset for combination:", combo, "\n")
  # print(head(get(df_name)))
  df <- get(df_name)
  # Extract the columns of interest
  column_data_policy <- df$Policy
  column_data_governance <- df$Gov
  # Split the elements in each row by semicolon
  #split_elements_policy <- strsplit(column_data_policy, "; ")
  #split_elements_governance <- strsplit(column_data_governance, "; ")
  # Flatten the list of split elements
  all_elements_policy <- unlist(column_data_policy)
  all_elements_governance <- unlist(column_data_governance)
  
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
  
  # Create a frequency table for governance
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
for (i in table_names_policy) {
  table_data <- get(i)
  if (nrow(table_data)>0){
    # Pivot the table
    pivot_table <- pivot_wider(table_data, 
                               names_from = "Policy Instrument",
                               values_from = "Count",
                               values_fill = 0)  # Fill missing values with 0
    
    # Convert the tibble to a data frame
    pivot_table_df <- as.data.frame(pivot_table)
    
    # Add a column with the table name (without "table.subset_")
    rownames(pivot_table_df) <- gsub("table.subset_", "", i)
    add_table_name <- gsub("table.subset_", "", i)
    # Append the table to the list
    table_list[[add_table_name]] <- pivot_table_df
    
  } else {
    print("table empty")
  }
}

#Merge them together
# Merge the tables using bind_rows and create new columns for unique column names
merged_table <- bind_rows(table_list, .id = "Table_Name")
View(merged_table)
# Delete the extra column of names that gets created
merged_table <- merged_table[, -1]
# Replace NA values with 0 in the merged table
merged_table[is.na(merged_table)] <- 0
#Merge all of the "other" columns together
# Get the column names containing the word "other"
#other_columns <- grep("other", colnames(merged_table), ignore.case = TRUE)
# Sum the selected columns
#sum_other_columns <- rowSums(merged_table[, other_columns])
# Remove the selected columns
#merged_table <- merged_table[, -other_columns]
#merged_table$Other <- sum_other_columns
# Print the result
#print(sum_other_columns)
# Display the merged table
#print(merged_table)
#rownames(merged_table)
merged_table_pol <- merged_table

#Put the tables in the correct format Gov
# Create an empty list to store the tables
table_list <- list()
# Loop through each table name
for (i in table_names_governance) {
  table_data <- get(i)
  if (nrow(table_data)>0){
    # Pivot the table
    pivot_table <- pivot_wider(table_data, 
                               names_from = "Governance type",
                               values_from = "Count",
                               values_fill = 0)  # Fill missing values with 0
    
    # Convert the tibble to a data frame
    pivot_table_df <- as.data.frame(pivot_table)
    
    # Add a column with the table name (without "table.subset_")
    rownames(pivot_table_df) <- gsub("table.subset_", "", i)
    add_table_name <- gsub("table.subset_", "", i)
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
# for (col_name in names(merged_table[2:nrow(merged_table)])) {
#   # Find the closest matching value in the lookup table
#   closest_match <- find_approximate_match(col_name, governance_lookup$Gov)
#   # Check if a matching value was found
#   if (!is.na(closest_match)) {
#     # Find the index of the matching value in the lookup table
#     index <- which(governance_lookup$Gov == closest_match)
#     # Extract the corresponding replacement value
#     replacement <- governance_lookup$Gov_New[index]
#     # Rename the column in merged_table
#     names(merged_table)[names(merged_table) == col_name] <- replacement
#   }else{
#     print("no match")
#   }
# }

names(merged_table)
# Find column indices that are NA and delete them
#na_columns <- which(is.na(names(merged_table)))
# Remove columns with NA labels
#merged_table <- merged_table[, -na_columns]

# Replace NA values with 0 in the merged table
merged_table[is.na(merged_table)] <- 0

#Merge all of the "other" columns together
# Get the column names containing the word "other"
# other_columns <- grep("other", colnames(merged_table), ignore.case = TRUE)
# # Sum the selected columns
# sum_other_columns <- rowSums(merged_table[, other_columns])
# # Remove the selected columns
# merged_table <- merged_table[, -other_columns]
# merged_table$Other <- sum_other_columns
# Print the resulting merged_table
View(merged_table)

# #Merge together the two "Adaptive Governance" columns
# # Get the column names containing the word "other"
# ad_gov <- grep("Adaptive Governance", colnames(merged_table), ignore.case = TRUE)
# # Sum the selected columns
# sum_ad_gov_columns <- rowSums(merged_table[, ad_gov])
# # Remove the selected columns
# merged_table <- merged_table[, -ad_gov]
# merged_table$"Adaptive Governance" <- sum_ad_gov_columns
merged_table_gov <- merged_table
# 
# #Clean up the rownames
# # Remove the prefix "tablegov.subset_" from row names
# rownames(merged_table_gov) <- gsub("tablegov\\.subset_", "", rownames(merged_table_gov))
# rownames(merged_table_pol) <- gsub("tablepol\\.subset_", "", rownames(merged_table_pol))
# 
merged_table_gov$rowname <- rownames(merged_table_gov)
# Remove "tablegov" from the entire dataframe
merged_table_gov <- merged_table_gov %>%
  mutate(across(everything(), ~gsub("tablegov", "", .)))
merged_table_pol$rowname <- rownames(merged_table_pol)
merged_table_pol <- merged_table_pol %>%
  mutate(across(everything(), ~gsub("tablepol", "", .)))
# 
# # Rename the 'OldColumnName' to 'NewColumnName'
# merged_table_gov <- merged_table_gov %>%
#   rename(Other_gov = Other)
# # Rename the 'OldColumnName' to 'NewColumnName'
# merged_table_pol <- merged_table_pol %>%
#   rename(Other_pol = Other)

test <- merge(merged_table_gov, merged_table_pol, by = "rowname", all = TRUE)
# result_dissolved <- test %>%
#   group_by(rowname) %>%
#   summarize_all(sum, na.rm = TRUE)

# Remove rows where "rowname" contains ".NA"
test <- subset(test, !grepl(".NA", rowname, fixed = TRUE))
View(test)
# Replace NA values with 0 in the entire dataframe
test[is.na(test)] <- 0
result_dissolved <- test

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
merged_table_remove_names <- as.data.frame(merged_table_remove_names)
# Convert all columns to numeric in the dataframe
merged_table_remove_names <- data.frame(lapply(merged_table_remove_names, as.numeric))

# Create a color matrix for text color (black)
text_color_matrix <- matrix("black", nrow = nrow(merged_table_remove_names), ncol = ncol(merged_table_remove_names))

#Using pheatmap actually worked better for me!
heatmap <- pheatmap(as.matrix(merged_table_remove_names), 
                    display_numbers = T, 
                    number_format = "%.0f", 
                    color = colorRampPalette(c("#B2D78C", "#0D7674", "#9C85B0"))(100), 
                    cluster_rows = F, 
                    cluster_cols = F, 
                    fontsize_number = 18, 
                    labels_row = result_dissolved$rowname, 
                    labels_col = colnames(result_dissolved[2:ncol(result_dissolved)]), 
                    fontsize_row = 8, 
                    fontsize_col = 10, 
                    angle_col = "45", 
                    border_color = NA,
                    annotation_colors = list(
                      values = text_color_matrix,
                      labels_col = TRUE, # Set to TRUE if you want to color the column labels
                      labels_row = TRUE  # Set to TRUE if you want to color the row labels
                    ))

# Save the heatmap to a PNG file
png("./Heatmap_elements_governance_policy.png", width = 3000, height = 3000, res = 600 )  # Adjust width and height as needed
print(heatmap)
dev.off()  # Close the PNG device
#Some final touches in ppt

















