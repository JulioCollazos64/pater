is_name_safe <- function(name) {
  make.names(name) == name
}
test_first_letter <- function(char) {
  regex <- "[[:alpha:].]"
  grepl(pattern = regex, char, perl = TRUE)
}
test_next_letters <- function(char) {
  regex <- "[[:alpha:]]"
  grepl(pattern = regex, char, perl = TRUE)
}
