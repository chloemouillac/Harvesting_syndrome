# Charger les bibliothèques nécessaires
library(stringr)
library(readr)

# Définir le chemin du fichier d'entrée et de sortie
input_file <- "/home/mouillac/Documents/mails.txt"  # Remplacez par le nom réel du fichier
output_file <- "/home/mouillac/Documents/mails.csv"


# Lire le fichier
content <- read_file(input_file)

# Définir les expressions régulières pour extraire les emails et nombres (4 ou 5 chiffres)
email_pattern <- "[\\w\\.-]+@[\\w\\.-]+\\.\\w+"
number_pattern <- "\\b\\d{4,5}\\b"  # Capturer uniquement les nombres de 4 ou 5 chiffres

# Extraire toutes les adresses e-mail avec leurs positions
email_matches <- str_locate_all(content, email_pattern)[[1]]
emails <- str_extract_all(content, email_pattern)[[1]]

# Extraire toutes les occurrences de nombres avec leurs positions
number_matches <- str_locate_all(content, number_pattern)[[1]]
numbers <- str_extract_all(content, number_pattern)[[1]]
# Forcer les nombres extraits à être des chaînes de caractères, en gardant les zéros si nécessaire
numbers <- as.character(numbers)

# Initialiser la liste des codes postaux associés aux e-mails
postal_codes <- c()

# Parcourir les e-mails et récupérer les nombres entre eux
for (i in 1:(nrow(email_matches) - 1)) {
  start_pos <- email_matches[i, 2]  # Fin de l'email actuel
  end_pos <- email_matches[i + 1, 1]  # Début de l'email suivant
  
  # Filtrer les nombres qui apparaissent entre ces deux e-mails
  in_between <- which(number_matches[, 1] > start_pos & number_matches[, 2] < end_pos)
  valid_numbers <- numbers[in_between]
  
  # Sélectionner le code postal à garder
  if (length(valid_numbers) > 0) {
    # Si un code postal à 5 chiffres existe, on le prend en priorité, sinon on prend un 4 chiffres
    selected_code <- ifelse(any(nchar(valid_numbers) == 5), 
                            valid_numbers[nchar(valid_numbers) == 5][1], 
                            valid_numbers[1])
    postal_codes <- c(postal_codes, selected_code)
  } else {
    postal_codes <- c(postal_codes, NA)  # Si aucun code trouvé
  }
}

# Construire un dataframe avec les résultats
paired_data <- data.frame(
  Email = emails[2:length(emails)],  # Associer au mail suivant
  Code_Postal = postal_codes
)

# Sauvegarder les résultats dans un fichier CSV
write_csv(paired_data, output_file)

# Message de confirmation
cat("Extraction terminée. Résultats enregistrés dans", output_file)


##############
verif_dupli <- read.csv(output_file)
verif_dupli[duplicated(verif_dupli$Mail),]


#################
# Concatenate duplicated emails

# Load the necessary library
library(dplyr)

# Example dataframe
df <- read.csv(output_file)

# Group by 'Mail' and concatenate the columns of interest
df_fused <- df %>%
  group_by(Mail) %>%
  summarise(
    Source = paste(unique(Source), collapse = ", "),
    Zone_geographique = paste(unique(Zone_geographique), collapse = ", "),
    Reponse = paste(unique(Reponse), collapse = ", ")
  )


verif_dupli[duplicated(df_fused$Mail),] # good !


# Sauvegarder les résultats dans un fichier CSV
write_csv(df_fused, output_file)




###################

# Load necessary library
library(dplyr)
library(stringr)

# Read CSV files
csv_A <- read.csv("/home/mouillac/Documents/mails_a_joindre.csv", stringsAsFactors = FALSE)
csv_B <- read.csv("/home/mouillac/Documents/mails.csv", stringsAsFactors = FALSE)

# Trim spaces and convert to lowercase for case-insensitive matching
csv_A$Emails <- tolower(trimws(csv_A$Emails))
csv_B$Mail <- trimws(csv_B$Mail)

# Function to check if any email in B's row exists in A
check_emails_in_A <- function(email_list, email_vector) {
  emails <- tolower(trimws(unlist(strsplit(email_list, ";"))))  # Split, trim, and lowercase
  any(emails %in% email_vector)  # Check if any email is in A
}

# Apply function to each row in B
csv_B$Exists_in_A <- sapply(csv_B$Mail, check_emails_in_A, email_vector = csv_A$Emails)

# Save the updated B CSV file
write.csv(csv_B, "/home/mouillac/Documents/mails_checked.csv", row.names = FALSE)

# Print sample result
head(csv_B)


#######
# create list of emails
mails <- read.csv("/home/mouillac/Documents/mails_checked.csv") %>%
  subset(Reponse=="", select=Mail)

paste0(mails$Mail, collapse=";")


####

# create list of emails
library(dplyr)
mails <- read.csv("/home/mouillac/Documents/onf.csv")

paste0(mails$mail, collapse=";")
