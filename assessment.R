library(dplyr)
# Filter the data for Variation #1 and Variation #2
filtered_data <- assessment %>%
  filter(variation_name %in% c("Variation #1", "Variation #2"))

# Count the occurrences of each event_name for each variation
event_counts <- filtered_data %>%
  group_by(variation_name, event_key) %>%
  summarize(count = n(), .groups = 'drop')

event_keys <- c("checkout.loaded", "suggested_modal.shop_list.clicked", 
                "suggested_popup.closed", "suggested_shop.clicked", "transaction")

chi_square_results <- list()

# Loop through each event_key and perform the chi-square test
for (event_key in event_keys) {
  
  # Filter counts for the current event_key
  event_counts_filtered <- event_counts %>%
    filter(event_key == !!event_key)
  
  # Check if there are counts available for both variations
  if (nrow(event_counts_filtered) == 2) {
    # Create the contingency table
    contingency_table <- matrix(c(
      event_counts_filtered$count[event_counts_filtered$variation_name == "Variation #1"],
      event_counts_filtered$count[event_counts_filtered$variation_name == "Variation #2"]
    ), nrow = 2, byrow = TRUE, 
    dimnames = list("Variation" = c("Variation #1", "Variation #2"),
                    event_key = c("Count")))
    
    # Perform the chi-square test
    chi_square_result <- chisq.test(contingency_table)
    
    # Store the result in the list
    chi_square_results[[event_key]] <- chi_square_result
  } else {
    chi_square_results[[event_key]] <- "Insufficient data for chi-square test"
  }
}

# Print results for each event_key
for (event_key in event_keys) {
  cat("\nChi-square test results for", event_key, ":\n")
  print(chi_square_results[[event_key]])
}

event_counts_filtered <- event_counts %>%
  filter(event_key == "suggested_modal.shop_list.clicked")

print(event_counts_filtered)
