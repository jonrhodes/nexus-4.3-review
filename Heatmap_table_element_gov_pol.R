#Libraries
library(dplyr)
library(stringr)
library(gplots)
library(tidyr)
library(pheatmap)
rm(list = ls())

# Read the Review data
csv_data <- read.csv("D:/IPBES_review/data/review_283729_20230922170037.csv")
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
merged_table_gov <- merged_table

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

merged_table_remove_names <- result_dissolved
merged_table_remove_names <- as.data.frame(merged_table_remove_names[2:11])
colnames(merged_table_remove_names) <- NULL
rownames(merged_table_remove_names) <- NULL

# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
color_palette <- colorRampPalette(c("#B2D78C", "#0D7674", "#D25B1D"))(100)

# Create the heatmap
my_plot <- heatmap(
  as.matrix(merged_table_remove_names),  # Convert the data to a matrix
  col = color_palette,  # Set the color palette
  Rowv = NA,  # Do not cluster rows
  Colv = NA,  # Do not cluster columns
  labRow = result_dissolved$rowname,  # Use the row names
  labCol = colnames(result_dissolved[2:11]),  # Use the column names
  cexRow = 0.8,  # Adjust the row label size
  cexCol = 0.8,  # Adjust the column label size
  #margins = c(5, 10),  # Adjust the margins
  #main = "Heatmap Title"  # Add a title +
  geom_text(aes(label = value))
)

heatmap <- pheatmap(as.matrix(merged_table_remove_names), display_numbers = T, number_format = "%.0f", color = colorRampPalette(c("#B2D78C", "#0D7674", "#D25B1D"))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 10, labels_row = result_dissolved$rowname, labels_col = colnames(result_dissolved[2:11]), fontsize_row = 8, fontsize_col = 10, angle_col = "45", border_color = NA)


# Save the heatmap to a PNG file
png("D:/IPBES_review/nexus-4.3-review/heatmap_policy.png", width = 600, height = 600)  # Adjust width and height as needed
print(heatmap)
dev.off()  # Close the PNG device
