# Extract bills from LDAs

# Load necessary libraries
library(tidyverse)
library(stringr)

# Read the CSV file
#ldabills_df <- read.csv("ldabills.csv")

#ldabills_df <- read.csv("..//downloads//lobby_filing.csv")
ldabills_df <- read.csv('lobbying_bills.csv')


# Function to extract bill numbers
extract_bill_numbers <- function(text) {

  bill_pattern <- "(?<![a-z\\d])\\b(H\\.?R|S)\\.? ?\\d+"

  str_extract_all(text, bill_pattern)[[1]]
  bills <- str_extract_all(text, bill_pattern)[[1]]
  bills <- str_replace_all(bills, "\\s", "") # Remove whitespace
  return(bills)
}

extract_bill_numbers2 <- function(text) {

  bill_pattern <- "(?<![a-zA-Z\\d])\\b(H\\.?R|S)\\.? ?\\d+" # Original pattern

  bills <- str_extract_all(text, stringr::regex(bill_pattern, ignore_case = TRUE))[[1]]
  bills <- str_replace_all(bills, "\\s", "") # Remove whitespace
  
  return(bills)
}

# Apply the function to the 'specific_issue' column
ldabills_df$extracted_bills <- sapply(ldabills_df$specific_issue, extract_bill_numbers, USE.NAMES = FALSE)

# Convert list to a dataframe and join with the original dataframe
bills_df <- data.frame(
  lobby_report_issue_id = rep(ldabills_df$lobby_report_issue_id, lengths(ldabills_df$extracted_bills)),
  extracted_bills = unlist(ldabills_df$extracted_bills)
)

# Add the report year
bills_df <- merge(ldabills_df[,c("report_year","lobby_report_issue_id")], bills_df, by = 'lobby_report_issue_id')

# Remove NA values and view the first few rows
bills_df <- bills_df %>% filter(!is.na(extracted_bills))
head(bills_df)

# Get ip4 bill linkages 
ip4_bills <- read.csv("ip4_bill_links.csv")

# normalize bill numbers

ip4_bills$bill_number <- gsub("\\.| ", "", ip4_bills$bill_number)

bills_df$extracted_bills <- gsub("\\.| ", "", bills_df$extracted_bills)

# combined bill identifier

ip4_bills$bill_identifier <- paste0(ip4_bills$lobby_report_issue_id, "_", ip4_bills$bill_number)
bills_df$bill_identifier <- paste0(bills_df$lobby_report_issue_id, "_", bills_df$extracted_bills)

bills_df$bill_identifier <- toupper(bills_df$bill_identifier)


#bills in new extraction that are not in existing ip4 data
bills_df %>% filter(!(bill_identifier %in% ip4_bills$bill_identifier)) %>% count(report_year)

#bills in existing ip4 data that is not in new extraction
ip4_bills %>% filter(!(bill_identifier %in% bills_df$bill_identifier)) %>% count(report_year)

#random examples 
bills_df %>% filter(!(bill_identifier %in% ip4_bills$bill_identifier) & report_year == 2022) %>% sample_n(10)

missing_ip4 <- merge(bills_df, lda_df, by = 'lobby_report_issue_id')
missing_bills_df <- merge(ip4_bills, lda_df, by = 'lobby_report_issue_id')