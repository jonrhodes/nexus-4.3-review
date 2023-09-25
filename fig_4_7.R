#Libraries
library(dplyr)
library(stringr)
library(gplots)
library(tidyr)
rm(list = ls())

# Read the Review data
csv_data <- read.csv("D:/IPBES_review/data/review_283729_20230922170037.csv")
# Define the list of elements
elements <- c("Biodiversity", "Climate", "Health", "Food", "Water")
# Generate all possible combinations of elements
all_combinations <- unlist(sapply(1:length(elements), function(n) combn(elements, n, paste, collapse = "; ")))

#Need to use this later on for merging the generated tables together
table_names <- c()
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
  # Extract the column of interest
  column_data <- df$`What.type.of.policy.instruments.are.considered.to.operationalise.the.response.options.proposed.or.assessed.`
  # Split the elements in each row by semicolon
  split_elements <- strsplit(column_data, "; ")
  # Flatten the list of split elements
  all_elements <- unlist(split_elements)
  # Create a frequency table
  table_result <- table(all_elements)
  table_df <- as.data.frame(table_result)
  if (nrow(table_result)>0){
  # Convert the frequency table to a data frame
  # Rename the columns for clarity
  colnames(table_df) <- c("Policy Instrument", "Count")
  } else{
  table_df <- table_df
  }
  # Define the name for the table as "table." followed by df_name
  table_name <- paste("table.", df_name, sep = "")
  # Assign the table to the name
  assign(table_name, table_df)
  # Display the resulting table
  print(table_df)
  table_names <- c(table_names, table_name)
}


#Put the tables in the correct format
# Create an empty list to store the tables
table_list <- list()

# Loop through each table name
for (table_name in table_names) {
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

merged_table_remove_names <- merged_table
colnames(merged_table_remove_names) <- NULL
rownames(merged_table_remove_names) <- NULL

# Specify the color palette you want to use (e.g., "viridis" or "RdYlBu")
color_palette <- colorRampPalette(c("blue", "red"))(100)

# Create the heatmap
my_plot <- heatmap(
  as.matrix(merged_table_remove_names),  # Convert the data to a matrix
  col = color_palette,  # Set the color palette
  Rowv = NA,  # Do not cluster rows
  Colv = NA,  # Do not cluster columns
  labRow = rownames(merged_table),  # Use the row names
  labCol = colnames(merged_table),  # Use the column names
  cexRow = 0.8,  # Adjust the row label size
  cexCol = 0.8,  # Adjust the column label size
  #margins = c(5, 10),  # Adjust the margins
  main = "Heatmap Title"  # Add a title
)

# Save the heatmap to a PNG file
png("D:/IPBES_review/nexus-4.3-review/heatmap_policy.png", width = 800, height = 600)  # Adjust width and height as needed
heatmap(
  as.matrix(merged_table_remove_names),  # Convert the data to a matrix
  col = color_palette,  # Set the color palette
  Rowv = NA,  # Do not cluster rows
  Colv = NA,  # Do not cluster columns
  labRow = rownames(merged_table),  # Use the row names
  labCol = colnames(merged_table),  # Use the column names
  cexRow = 0.8,  # Adjust the row label size
  cexCol = 0.8,  # Adjust the column label size
  #margins = c(5, 10),  # Adjust the margins
  main = "Heatmap Title"  # Add a title
)

dev.off()  # Close the PNG device
