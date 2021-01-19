# updating HUDOC data file
library(rvest)
#install.packages('xml2')
#library('xml2')
library(data.table)
library(tidyr)

# ouput folder for updatead dataset
output.dir <- ""
dir <- ""

# read current data and get last date
xdf <- read.csv(paste0(dir, '/final_for_viz.csv'))
xdf_date <- max(as.Date(xdf$date, format = "%Y-%m-%d"))
xdf_month <- format(xdf_date, "%m")

# if the last month is December then 
# all cases for that year have already been scraped
if (xdf_month=="12"){
  xdf_year <- as.numeric(format(xdf_date, "%Y")) + 1
} else{
  xdf_year <- format(xdf_date, "%Y")
}

# get todays date
todays_date <- Sys.Date()
todays_year <- as.numeric(format(todays_date, "%Y"))

# years to scrape
years <- xdf_year:todays_year
years <- 2020

get_violations <- function(years) {
  months <- 1:12
  ## field definitions
  articles <- c(2:14,18, 25, 34, 38,46, "P1", "P4", "P6", "P7", "P12") #articles and protocols
  paragraphs <- c(0:5)
  subparagraphs <- c(0:4, "a","b", "c", "d", "e", "f")
  
  master <- data.frame("Document_Title"=NA,"Application_Number"=NA,"Document_Type"=NA,"Originating_Body"=NA,"Date"=NA,"Conclusion"=NA,
                       "Violation"=NA,"Article"=NA,"Paragraph"=NA,"Subparagraph"=NA, "Language"=NA,"Document_Collection"=NA)
  
  for (year in years){
    for (month in months){ 
      start_date <- paste0(year,"-",month,"-01")
      start_date <- as.Date(start_date)
      start_date <- as.character(start_date)
      if (month==12) {
        end_date <- paste0(year,"-",month,"-31")
        end_date <- as.Date(end_date)
        end_date <- as.character(end_date)
      } else {
        end_date <- paste0(year,"-",month+1,"-01")
        end_date <- as.Date(end_date)
        end_date <- as.character(end_date)
      }
      
      for (article in articles){
        for (paragraph in paragraphs){
          for(subparagraph in subparagraphs){
            
            violation <- paste0(article,"-",paragraph,"-",subparagraph)
            print(paste0(violation,":", month, ":", year))
            
            if(paragraph==0 & subparagraph==0){
              url_path <- paste0("https://hudoc.echr.coe.int/app/transform/csv?library=echreng&query=contentsitename%3AECHR%20AND%20(NOT%20(doctype%3DPR%20OR%20doctype%3DHFCOMOLD%20OR%20doctype%3DHECOMOLD))%20AND%20((languageisocode%3D%22ENG%22))%20AND%20((documentcollectionid%3D%22JUDGMENTS%22))%20AND%20(kpdate%3E%3D%22",
                                 start_date,"T00%3A00%3A00.0Z%22%20AND%20kpdate%3C%3D%22",
                                 end_date,"T00%3A00%3A00.0Z%22)%20AND%20((violation%3D",
                                 article,"))&sort=&start=0&length=500&rankingModelId=11111111-0000-0000-0000-000000000000")
            } else {
              if(subparagraph==0){
                url_path <- paste0("https://hudoc.echr.coe.int/app/transform/csv?library=echreng&query=contentsitename%3AECHR%20AND%20(NOT%20(doctype%3DPR%20OR%20doctype%3DHFCOMOLD%20OR%20doctype%3DHECOMOLD))%20AND%20((languageisocode%3D%22ENG%22))%20AND%20((documentcollectionid%3D%22JUDGMENTS%22))%20AND%20(kpdate%3E%3D%22",
                                   start_date,"T00%3A00%3A00.0Z%22%20AND%20kpdate%3C%3D%22",
                                   end_date,"T00%3A00%3A00.0Z%22)%20AND%20((violation%3D",
                                   article,"-",
                                   paragraph,"))&sort=&start=0&length=500&rankingModelId=11111111-0000-0000-0000-000000000000")
              } else {
                url_path <- paste0("https://hudoc.echr.coe.int/app/transform/csv?library=echreng&query=contentsitename%3AECHR%20AND%20(NOT%20(doctype%3DPR%20OR%20doctype%3DHFCOMOLD%20OR%20doctype%3DHECOMOLD))%20AND%20((languageisocode%3D%22ENG%22))%20AND%20((documentcollectionid%3D%22JUDGMENTS%22))%20AND%20(kpdate%3E%3D%22",
                                   start_date,"T00%3A00%3A00.0Z%22%20AND%20kpdate%3C%3D%22",
                                   end_date,"T00%3A00%3A00.0Z%22)%20AND%20((violation%3D",
                                   violation,"))&sort=&start=0&length=500&rankingModelId=11111111-0000-0000-0000-000000000000")
              }
            }
            
            tryCatch({
              df <- fread(url_path)
              names(df) <- names(master)[1:6]
              df$Violation <- violation
              df$Article <- article
              df$Paragraph <- paragraph
              df$Subparagraph <- subparagraph
              df$Language <- "english"
              df$Document_Collection <- "judgments"
              master <- rbind(master, df)
            }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
            
          }
        } 
      }
    }
  }
  return(master)
}

get_non_violations <- function(years){
  months <- 1:12
  ## field definitions
  articles <- c(2:14,18, 25, 34, 38,46, "P1", "P4", "P6", "P7", "P12") #articles and protocols
  paragraphs <- c(0:5)
  subparagraphs <- c(0:4, "a","b", "c", "d", "e", "f")
  
  master <- data.frame("Document_Title"=NA,"Application_Number"=NA,"Document_Type"=NA,"Originating_Body"=NA,"Date"=NA,"Conclusion"=NA,
                       "Violation"=NA,"Article"=NA,"Paragraph"=NA,"Subparagraph"=NA, "Language"=NA,"Document_Collection"=NA)
  
  for (year in years){
    for (month in months){ 
      start_date <- paste0(year,"-",month,"-01")
      start_date <- as.Date(start_date)
      start_date <- as.character(start_date)
      if (month==12) {
        end_date <- paste0(year,"-",month,"-31")
        end_date <- as.Date(end_date)
        end_date <- as.character(end_date)
      } else {
        end_date <- paste0(year,"-",month+1,"-01")
        end_date <- as.Date(end_date)
        end_date <- as.character(end_date)
      }
      
      for (article in articles){
        for (paragraph in paragraphs){
          for(subparagraph in subparagraphs){
            
            violation <- paste0(article,"-",paragraph,"-",subparagraph)
            print(paste0(violation,":", year))
            
            if(paragraph==0 & subparagraph==0){
              url_path <- paste0("https://hudoc.echr.coe.int/app/transform/csv?library=echreng&query=contentsitename%3AECHR%20AND%20(NOT%20(doctype%3DPR%20OR%20doctype%3DHFCOMOLD%20OR%20doctype%3DHECOMOLD))%20AND%20((languageisocode%3D%22ENG%22))%20AND%20((documentcollectionid%3D%22JUDGMENTS%22))%20AND%20(kpdate%3E%3D%22",
                                 start_date,"T00%3A00%3A00.0Z%22%20AND%20kpdate%3C%3D%22",
                                 end_date,"T00%3A00%3A00.0Z%22)%20AND%20((nonviolation%3D",
                                 article,"))&sort=&start=0&length=500&rankingModelId=11111111-0000-0000-0000-000000000000")
            } else {
              if(subparagraph==0){
                url_path <- paste0("https://hudoc.echr.coe.int/app/transform/csv?library=echreng&query=contentsitename%3AECHR%20AND%20(NOT%20(doctype%3DPR%20OR%20doctype%3DHFCOMOLD%20OR%20doctype%3DHECOMOLD))%20AND%20((languageisocode%3D%22ENG%22))%20AND%20((documentcollectionid%3D%22JUDGMENTS%22))%20AND%20(kpdate%3E%3D%22",
                                   start_date,"T00%3A00%3A00.0Z%22%20AND%20kpdate%3C%3D%22",
                                   end_date,"T00%3A00%3A00.0Z%22)%20AND%20((nonviolation%3D",
                                   article,"-",
                                   paragraph,"))&sort=&start=0&length=500&rankingModelId=11111111-0000-0000-0000-000000000000")
              } else {
                url_path <- paste0("https://hudoc.echr.coe.int/app/transform/csv?library=echreng&query=contentsitename%3AECHR%20AND%20(NOT%20(doctype%3DPR%20OR%20doctype%3DHFCOMOLD%20OR%20doctype%3DHECOMOLD))%20AND%20((languageisocode%3D%22ENG%22))%20AND%20((documentcollectionid%3D%22JUDGMENTS%22))%20AND%20(kpdate%3E%3D%22",
                                   start_date,"T00%3A00%3A00.0Z%22%20AND%20kpdate%3C%3D%22",
                                   end_date,"T00%3A00%3A00.0Z%22)%20AND%20((nonviolation%3D",
                                   violation,"))&sort=&start=0&length=500&rankingModelId=11111111-0000-0000-0000-000000000000")
              }
            }
            
            tryCatch({
              df <- fread(url_path)
              names(df) <- names(master)[1:6]
              df$Violation <- violation
              df$Article <- article
              df$Paragraph <- paragraph
              df$Subparagraph <- subparagraph
              df$Language <- "english"
              df$Document_Collection <- "judgments"
              master <- rbind(master, df)
            }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
            
          }
        } 
      }
    }
  }
  master <- master[!is.na(master$Application_Number),]
  return(master)
}

get_important_cases <- function(years){
  months <- 1:12
  
  ## field definitions
  ## code for importance levels decifered by comparting urls on HUDOC site:
  ## key cases = 221, 1 = 222, 2=223 , 3=224 
  importance_codes <- c(221, 222, 223, 224)
  importance_names <- data.frame("names"=c("key_cases","1","2","3"), "code"=importance_codes)
  
  master <- data.frame("Document_Title"=NA,"Application_Number"=NA,"Document_Type"=NA,"Originating_Body"=NA,"Date"=NA,"Conclusion"=NA,
                       "Language"=NA,"Document_Collection"=NA, "Importance"=NA)
  
  for (imp in importance_names$names){
    
    imp_code <- importance_names$code[importance_names$names==imp]
    
    for (year in years){
      for (month in months){ 
        start_date <- paste0(year,"-",month,"-01")
        start_date <- as.Date(start_date)
        start_date <- as.character(start_date)
        if (month==12) {
          end_date <- paste0(year,"-",month,"-31")
          end_date <- as.Date(end_date)
          end_date <- as.character(end_date)
        } else {
          end_date <- paste0(year,"-",month+1,"-01")
          end_date <- as.Date(end_date)
          end_date <- as.character(end_date)
        }
        print(paste0(month,":", year))
        url_path <- paste0("https://hudoc.echr.coe.int/app/transform/csv?library=echreng&query=contentsitename%3AECHR%20AND%20(NOT%20(doctype%3DPR%20OR%20doctype%3DHFCOMOLD%20OR%20doctype%3DHECOMOLD))%20AND%20((languageisocode%3D%22ENG%22))%20AND%20((importance%3D%",
                           imp_code,"%22))%20AND%20((documentcollectionid%3D%22JUDGMENTS%22))%20AND%20(kpdate%3E%3D%22",
                           start_date,"T00%3A00%3A00.0Z%22%20AND%20kpdate%3C%3D%22",
                           end_date,"T00%3A00%3A00.0Z%22)&sort=&start=0&length=500&rankingModelId=11111111-0000-0000-0000-000000000000")
        
        tryCatch({
          df <- fread(url_path)
          names(df) <- names(master)[1:6]
          df$Language <- "english"
          df$Document_Collection <- "judgments"
          df$Importance <- imp
          master <- rbind(master, df)
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
        
      }
    } 
  }
  master <- master[!is.na(master$Application_Number),]
  return(master)
}

violations_df <- get_violations(years)
write.csv(violations_df, paste0(output.dir, "violations_df.csv"), row.names = FALSE)

non_violations_df <- get_non_violations(years)
write.csv(non_violations_df, paste0(output.dir, "non_violations_df.csv"), row.names = FALSE)

important_cases <- get_important_cases(years)
write.csv(important_cases, paste0(output.dir, "important_cases_df.csv"), row.names = FALSE)


