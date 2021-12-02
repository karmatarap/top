# Convert texts to variable names
namify <- function(text, prefix = "") {
  
  text %>% 
    tolower() %>% 
    str_replace_all("[^a-z]", "_") %>% # replace all none alpha chars with underscore
    str_replace_all("[_]+","_") %>% # remove duplicate underscores
    str_replace_all("^[_]","") %>% # remove leading underscores
    str_replace_all("[_]$","") %>% # remove trailing underscores
    paste0(ifelse(prefix == "", prefix, paste0(prefix, "_")), .) # add prefix if needed
  
}
testthat::expect_equal(namify("(name)"),"name")
testthat::expect_equal(namify(" one  two three "),"one_two_three")