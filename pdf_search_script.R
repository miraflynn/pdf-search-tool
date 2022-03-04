library(tidyverse)
library(pdftools)

# Load CSV of species names
species_names_filename <- "sample_species_names.csv"
# This file should be a file with each species name on a new line


# All PDFs to search should be located in this folder.
pdf_folder <- "pdf-files"


# This is the filename of the output
output_filename <- "output.csv"



species_names <- read.csv(species_names_filename)$Species

# For each page in the PDF, for each species, see if the genus and species
# both appear in the page. If they do, add to the found_species list

find_species <- function(filename){
  pdf_text <- pdf_text(filename)
  found_species <- c()
  for(page in pdf_text){
    for(fullname in species_names){
      split = strsplit(fullname, " ")
      genus = split[[1]][1]
      species = split[[1]][2]
      if(grepl(genus, page, ignore.case = TRUE) && grepl(species, page, ignore.case = TRUE)){
        found_species <- c(found_species, fullname)
      }
    }
  }
  found_species <- found_species %>% unique()
  return(found_species)
}

# Load all filenames
files <- list.files(pdf_folder, pattern="*.pdf") %>% 
  tibble() %>%
  rename(
    "Filename" = "."
  )


# Apply the find_species function on all filenames, and put it as a new
# column in the files dataframe
files["Found Species"] <- paste(pdf_folder, files$Filename, sep = "/") %>%
  lapply(find_species) %>%
  tibble() %>%
  rename(
    "Found Species" = "."
  )

# Some unholy code to output a CSV with an arbitrary number of columns

output_rows <- c()
for(r in 1:nrow(files)){
  row <- files[r,]
  # print(row[[2]])
  output <- gsub(",","",row[[1]])
  # output <- paste(output, row[[1]], sep = ",")
  for(species in row[[2]][[1]]){
    output <- paste(output, species, sep = ",")
  }
  output_rows <- c(output_rows,output)
}
write(output_rows,output_filename)
