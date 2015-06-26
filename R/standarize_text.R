standarize_text <- function(char) {
  char <- tolower(char)
  char <- remove_accents(char)
  return(char)
}

remove_accents <- function(char) {
  char <- chartr("á", "a", char)
  char <- chartr("Á", "A", char)
  char <- chartr("é", "e", char)
  char <- chartr("É", "E", char)
  char <- chartr("í", "i", char)
  char <- chartr("Í", "I", char)
  char <- chartr("ó", "o", char)
  char <- chartr("Ó", "O", char)
  char <- chartr("ú", "u", char)
  char <- chartr("Ú", "U", char)
  return(char)
}