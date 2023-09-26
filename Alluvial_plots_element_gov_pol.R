# Load the required libraries
library(ggplot2)

# Read the Review data
csv_data <- read.csv("D:/IPBES_review/data/review_283729_20230922170037.csv")

# List of column names to keep
columns_to_keep <- c("Which.nexus.elements.are.considered.", "Are.any.of.the.following.governance.approaches.proposed.or.assessed.as.solutions.to.the.above.nexus.challenges..Use.your.judgement.to.select.one.of.the.four.governance.approaches.listed.and.then.use.the..other..category.to.list.any.specific.governance.approaches.referred.to.in.the.study..separate.multiple.governance.approaches.with.....", "What.type.of.policy.instruments.are.considered.to.operationalise.the.response.options.proposed.or.assessed.")
# Subset the dataframe based on the selected columns
subset_dataframe <- csv_data[, columns_to_keep]
# Assuming 'my_dataframe' is your dataframe
# Define a vector of new column names
new_column_names <- c("Elements", "Governance", "Policy")
# Rename the columns using colnames()
colnames(subset_dataframe) <- new_column_names

subset_dataframe <- separate_rows(subset_dataframe, Governance, sep = "; ")
subset_dataframe <- separate_rows(subset_dataframe, Policy, sep = "; ")

# Assuming 'subset_dataframe' is your dataframe and 'Governance' is the target column
subset_dataframe <- subset_dataframe %>%
  mutate(Governance = ifelse(grepl("Other", Governance, ignore.case = TRUE), "Other", Governance))

subset_dataframe <- subset_dataframe %>%
  mutate(Policy = ifelse(grepl("Other", Policy, ignore.case = TRUE), "Other", Policy))
#Replace other text with just "other"
# Assuming 'subset_dataframe' is your dataframe
# Replace "other" with "other" (including accompanying text)
# Group the dataframe by all columns and create a new count column
result <- subset_dataframe %>%
  group_by_all() %>%
  summarize(Count = n())

# View the result
print(result)

# # Create a sample dataset
# data <- data.frame(
#   Category = rep(c("A", "B", "C"), each = 3),
#   Subcategory1 = rep(c("X", "Y", "Z"), times = 3),
#   Subcategory2 = rep(c("M", "N", "O"), times = 3),
#   Count = c(20, 15, 10, 12, 18, 7, 8, 10, 5)
# )

result <- as.data.frame(result)

result <- result %>%
  mutate(Policy = ifelse(Policy == "Economic and Financial Instruments", 
                         "Economic and Financial 
  Instruments", Policy))

result <- result %>%
  mutate(Policy = ifelse(Policy == "Legal and Regulatory Instruments", 
                         "Legal and Regulatory
  Instruments", Policy))

result <- result %>%
  mutate(Policy = ifelse(Policy == "Rights-Based Instruments and Customary Norms", 
                         "Rights-Based Instruments 
 and Customary Norms", Policy))

# Create a custom color palette
my_colors <- c("#B2D78C", "#679539", "#0D7674", "#D25B1D", "#9C85B0", "#67518A")  # Replace with your desired colors

#OPTION1
# Create an alluvial plot
my_plot <- ggplot(result, aes(axis1 = Elements, axis2 = Governance, axis3 = Policy, y = Count)) +
  geom_alluvium(aes(fill = Elements)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 1.5) +
  scale_fill_manual(values = my_colors) +
  theme_void()

ggsave(filename = "Element_governance_Policy_option1.png", plot = my_plot, width = 6, height = 4)  # Adjust width and height as needed


#OPTION2
result <- separate_rows(result, Elements, sep = "; ")
#This didn't work
# result %>% group_by(Elements, Governance, Policy) %>%
#   summarize(Count = n())

# Assuming 'result' is your dataframe
# Initialize an empty dataframe to store the result
result_summarized <- data.frame(
  Elements = character(0),
  Governance = character(0),
  Policy = character(0),
  Count = integer(0)
)

# Loop through each row of the original dataframe 'result'
for (i in 1:nrow(result)) {
  # Get the current row
  current_row <- result[i, ]
  
  # Check if the combination of 'Elements', 'Governance', and 'Policy' already exists in the summarized dataframe
  existing_row <- result_summarized %>%
    filter(
      Elements == current_row$Elements,
      Governance == current_row$Governance,
      Policy == current_row$Policy
    )
  
  if (nrow(existing_row) == 0) {
    # If the combination does not exist in the summarized dataframe, add a new row
    new_row <- data.frame(
      Elements = current_row$Elements,
      Governance = current_row$Governance,
      Policy = current_row$Policy,
      Count = 1
    )
    result_summarized <- bind_rows(result_summarized, new_row)
  } else {
    # If the combination already exists, increment the 'Count' in the summarized dataframe
    result_summarized[result_summarized$Elements == current_row$Elements &
                        result_summarized$Governance == current_row$Governance &
                        result_summarized$Policy == current_row$Policy, "Count"] <- 
      result_summarized[result_summarized$Elements == current_row$Elements &
                          result_summarized$Governance == current_row$Governance &
                          result_summarized$Policy == current_row$Policy, "Count"] + 1
  }
}

# View the summarized result
print(head(result_summarized))

# Replace the value in the "Policy" column
result_summarized <- result_summarized %>%
  mutate(Policy = ifelse(Policy == "Economic and Financial Instruments", 
  "Economic and Financial 
  Instruments", Policy))

result_summarized <- result_summarized %>%
  mutate(Policy = ifelse(Policy == "Legal and Regulatory Instruments", 
                         "Legal and Regulatory
  Instruments", Policy))

result_summarized <- result_summarized %>%
  mutate(Policy = ifelse(Policy == "Rights-Based Instruments and Customary Norms", 
                         "Rights-Based Instruments 
 and Customary Norms", Policy))

# Check the updated dataframe
print(head(result_summarized))

# Create a custom color palette
my_colors <- c("#B2D78C", "#679539", "#0D7674", "#D25B1D", "#9C85B0", "#67518A")  # Replace with your desired colors

# Create an alluvial plot
my_plot <- ggplot(result_summarized, aes(axis1 = Governance, axis2 = Policy, y = Count)) +
  geom_alluvium(aes(fill = Elements)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  scale_fill_manual(values = my_colors) + 
  theme_void()

ggsave(filename = "Element_governance_Policy.png", plot = my_plot, width = 6, height = 4)  # Adjust width and height as needed








