# This script cleans new datasets and merges them with the old dataset
library(data.table)
library(tidyr)
library(countrycode)
#library(googleLanguageR)
library(lubridate)
library(rgdal)
library(rlang)

dir <- "...updating_df/"
output.dir <- ".../final_for_viz/"

# Dataframes
df <- read.csv(paste0(dir, "final_for_viz.csv")) # old dataset to add the new year(s) to
violations <- read.csv(paste0(dir, "violations_df.csv"))
non_violations <- read.csv(paste0(dir, "non_violations_df.csv"))
important_cases <- read.csv(paste0(dir, "important_cases_df.csv"))

# Join new datasets -------------------------------------------------------
df_merge_non_violations <- merge.data.frame(non_violations, important_cases, by.x = "Application_Number", by.y = "Application_Number", all.x = TRUE)
df_merge_non_violations <- df_merge_non_violations[c("Application_Number", "Document_Title.x", "Document_Type.x", "Originating_Body.x", "Date.x",
                       "Conclusion.x", "Violation", "Article", "Paragraph", "Subparagraph", "Language.x", 
                       "Document_Collection.x", "Importance")]
df_merge_non_violations <- df_merge_non_violations[!is.na(df_merge_non_violations$Document_Title.x),]
df_merge_non_violations$Judgement_type <- "non-violation"

df_merge_violations <- merge.data.frame(violations, important_cases, by.x = "Application_Number", by.y = "Application_Number", all.x = TRUE)
df_merge_violations <- df_merge_violations[c("Application_Number", "Document_Title.x", "Document_Type.x", "Originating_Body.x", "Date.x",
                                                     "Conclusion.x", "Violation", "Article", "Paragraph", "Subparagraph", "Language.x", 
                                                     "Document_Collection.x", "Importance")]
df_merge_violations <- df_merge_violations[!is.na(df_merge_violations$Document_Title.x),]
df_merge_violations$Judgement_type <- "violation"

df_new <- rbind(df_merge_violations, df_merge_non_violations)
# clean column names
names(df_new) <- sub(".x", "", names(df_new))

# Clean new datasets ------------------------------------------------------
# Get country names from case titles
hudoc_shp <- readOGR(paste0(dir, "/data"),"ne_50m_admin_0_countries")
hudoc_shp <- as.data.frame(hudoc_shp)

### match country names to case titles_______________________________________________________
### remove the first part of the title before the "v." or "c." or "AGAINST" to get country name
df_new$Document_Title <- as.character(df_new$Document_Title)
df_new$rawCountry <- (lapply(strsplit(df_new$Document_Title, split = "v. "), function(x)x[2]))
df_new$rawCountry[is.na(df_new$rawCountry)] <- (lapply(strsplit(df_new$Document_Title[is.na(df_new$rawCountry)], split = " AGAINST "), function(x)x[2]))
df_new$rawCountry[is.na(df_new$rawCountry)] <- (lapply(strsplit(df_new$Document_Title[is.na(df_new$rawCountry)], split = "c. "), function(x)x[2]))
df_new$rawCountry[is.na(df_new$rawCountry)] <- (lapply(strsplit(df_new$Document_Title[is.na(df_new$rawCountry)], split = "CASE \"RELATING TO CERTAIN ASPECTS OF THE LAWS ON THE USE OF LANGUAGES IN EDUCATION IN "), function(x)x[2]))

# Deal with typos
df_new$rawCountry <- sub('"', "", df_new$rawCountry)
df_new$rawCountry <- sub("THE ", "", df_new$rawCountry)

# Match names extracted from doc title to country names in shapefile
get_country_name <- function(data_frame){
  for (row in 1:nrow(data_frame)){
    country <- hudoc_shp$NAME_EN[toupper(hudoc_shp$NAME_EN) %in% unlist(data_frame$rawCountry[row])]
    if (is_empty(country)) { 
      country <- hudoc_shp$NAME_EN[toupper(hudoc_shp$SOVEREIGNT) %in% unlist(data_frame$rawCountry[row])]
    }
    if (is_empty(country)) { 
      country <- hudoc_shp$NAME_EN[toupper(hudoc_shp$NAME_FR) %in% unlist(data_frame$rawCountry[row])]
    }
    if (is_empty(country)) { 
      country <- hudoc_shp$NAME_EN[toupper(hudoc_shp$NAME_DE) %in% unlist(data_frame$rawCountry[row])]
    }
    if (is_empty(country)) { 
      country <- hudoc_shp$NAME_EN[toupper(hudoc_shp$FORMAL_EN) %in% unlist(data_frame$rawCountry[row])]
    }
    
    if (is_empty(country)) {
      country <- NA
    }
    
    if (length(country) == 2) {
      country <- paste(country[1], country[2])
    }
    
    if (length(country) == 3) {
      country <- paste(country[1], country[2], country[3])
    }
    
    data_frame$Country[row] <- country
  }
  return(data_frame)
}
df_new <- get_country_name(df_new)

# For rows that didn't return a country value split string
df_new$rawCountry[is.na(df_new$Country)] <- strsplit(df_new$rawCountry[is.na(df_new$Country)], split = " \\(")
df_new <- get_country_name(df_new)
df_new$rawCountry[is.na(df_new$Country)] <- sub('\\"', "", df_new$rawCountry[is.na(df_new$Country)])
df_new <- get_country_name(df_new)

# separate cases where there are multiple countries and run 
# get country names function again
df_new <- separate_rows(df_new, rawCountry, sep = " AND ")

## match with country names from country code database
df_new$Country[is.na(df_new$Country)] <- countrycode(df_new$rawCountry[is.na(df_new$Country)], 'country.name', 'country.name')

# **** Check for nans and deal with special cases if ther are any ****
nans <- df_new[is.na(df_new$Country),]

# format dates
df_new$Date <- as.character(df_new$Date)
df_new$date <- as.Date(df_new$Date, format="%d/%m/%y", optional = TRUE)
df_new$year <- format(df_new$date,"%Y")

# ***** Check for wierdness with dates they are often formatted differently ********

# Join new dataset with old --------------------------------------

## attach new cases to old dataset
df <- df[c("Application_Number", "Document_Title","Document_Type", "Originating_Body",
           "Conclusion", "Violation", "Article", "Paragraph", "Subparagraph", "Language", "Document_Collection", 
           "Importance", "Judgement_type", "Country", "date", "year")]  
df_new <- df_new[c("Application_Number", "Document_Title","Document_Type", "Originating_Body",
                   "Conclusion", "Violation", "Article", "Paragraph", "Subparagraph", "Language", "Document_Collection", 
                   "Importance", "Judgement_type", "Country", "date", "year")]
df_new <- rbind(df, df_new)

# Prep for viz ------------------------------------------------------------
## add labels column for app
definitions <- read.csv(paste0(dir, "/data/Article_definitions.csv"),
                        stringsAsFactors = FALSE)

### add definitions to master df
df_left <- left_join(df_new, definitions, by="Article")
df_left$label <- paste0(as.character(df_left$Article), ": ", as.character(df_left$Definition))

### remove duplicate rows
df_left <- distinct(df_left)

### create column of concatenated violation labels 
df_label = df_left %>%
  select(Application_Number, label)

df_label <- distinct(df_label)

df_label = df_label %>% 
  group_by(Application_Number) %>% 
  mutate(all_labels = paste0(unique(label)[order(nchar(unique(label)), unique(label))],
                             collapse = ","))

### join concatenate labels with master dataframe
df_master <- left_join(df_left, df_label, by="Application_Number", keep=FALSE)

### clean up dataframe
df_master <- df_master %>%
  select(Application_Number, Document_Title, Document_Type, Originating_Body,
         Conclusion, Violation, Article, Paragraph, Subparagraph, Language,
         Document_Collection, Country, date, year, Importance, 
         Judgement_type, Definition, label.x, all_labels)
df_master <- rename(df_master, label = label.x)
df_master <- distinct(df_master)

### write dataframe
write.csv(df_master, paste0(output.dir, "final_for_viz.csv"), row.names = FALSE)
