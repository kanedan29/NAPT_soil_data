library(rJava)      # Needed for tabulizer
library(tabulizer)  # Handy tool for PDF Scraping
library(tidyverse)  # Core data manipulation and visualization 
library(here)

#specify URL leading text
lead <- "https://www.naptprogram.org"

#fetch html from lab results page
url <- "https://www.naptprogram.org/content/laboratory-results"
html <- paste(readLines(url), collapse="\n")

#identify URL locations for all pdfs
library(stringr)
matched <- (str_match_all(html, "<a href=\"(.*?)\""))

# generate list of pdfs of soil data specifically
pdf.list <- as.data.frame(matched[[1]])%>%
  filter(grepl(V2,pattern = "soil")) %>%
  filter(!grepl(V2,pattern = "env"))

pdf.list <- paste(lead, pdf.list[1:38,2], sep = "")

# Create function for fetching data from pdfs
extract.dat <- function(pdf) {
  table <- extract_tables(file   = pdf,
                          method = "stream",
                          output = "data.frame")
  
  soil.names <-
    c(names(table[[1]])[grepl((names(table[[1]])), pattern = "Soil.")])
  
  
  test <-
    lapply(table, function(x)
      Filter(function(y)
        ! all(is.na(y)), x))
  test <- lapply(test, function(x)
    rbind(x, names(x)))
  test <- lapply(test, function(x)
    cbind(x, " "))
  
  test <- lapply(test, function(x) {
    return(x[, 1:13])
  })
  test <-
    lapply(
      test,
      set_names,
      c(
        "Metric",
        "Units",
        "n",
        "Median.1",
        "MAD.1",
        "Median.2",
        "MAD.2",
        "Median.3",
        "MAD.3",
        "Median.4",
        "MAD.4",
        "Median.5",
        "MAD.5"
      )
    )
  
  test[[1]] <- test[[1]] %>%
    separate(Median.1,
             into = c("Median.1", "MAD.1"),
             sep = " ") %>%
    separate(Median.2,
             into = c("Median.2", "MAD.2"),
             sep = " ") %>%
    separate(Median.3,
             into = c("Median.3", "MAD.3"),
             sep = " ") %>%
    separate(Median.4,
             into = c("Median.4", "MAD.4"),
             sep = " ") %>%
    separate(Median.5,
             into = c("Median.5", "MAD.5"),
             sep = " ")
  
  test <- dplyr::bind_rows(test)
  
  
  test2 <- test %>%
    select(-Units,-n) %>%
    mutate_if(
      grepl(names(.), pattern = "Median|MAD"),
      .funs = function(x)
        as.numeric(gsub(",|X", "", x))
    ) %>%
    filter(!is.na(MAD.5)) %>%
    tidyr::pivot_longer(cols = starts_with(c("Median", "MAD")), names_to = "Stat") %>%
    separate(Stat, c("Stat", "Soil"), sep = "\\.") %>%
    mutate(Soil = plyr::mapvalues(Soil, from = c("1", "2", "3", "4", "5"), to = soil.names)) %>%
    filter(grepl(Metric, pattern = "SOM|Combustion|CaCO3|pH")) %>%
    tidyr::pivot_wider(names_from = Stat, values_from = value)
  
  return(test2)
}

# Apply function across PDF list
all.dat <- lapply(pdf.list, function(x) extract.dat(pdf = x))

# Clean data
all.dat.2 <- dplyr::bind_rows(all.dat) %>%
  #select(-value) %>%
  filter(grepl(Metric, pattern = "SOM|CaCO3|Combustion|Water")) %>%
  filter(Metric != "pH  (1:2) Water") %>%
  mutate(Metric = case_when(Metric == "SOM - Walkley-Black" ~ "SOM...Walkley.Black",
                            Metric == "Soil TOC (Combustion)" ~ "Soil.TOC..Combustion.",
                            TRUE ~ .$Metric)) %>%
  pivot_wider(names_from = Metric, values_from=c(Median, MAD)) %>%
  janitor::clean_names()


# write to file 
write_csv(all.dat.2, here("NAPT_data.csv"))
