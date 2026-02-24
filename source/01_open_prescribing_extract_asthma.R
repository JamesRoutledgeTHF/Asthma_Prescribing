library(dplyr)
library(readxl)
library(writexl)

codes_df <- read_excel("C:/Users/James.Routledge/The Health Foundation/Data Analytics - 10-IAU/1. Work Programmes/Asthma project/BNFcodesasthma.xlsx")

#run this code for ICB level data
data_list <- list()

failed_codes <- c()

for (i in 1:nrow(codes_df)) {
  
  code <- codes_df$BNFcode[i]
  medication <- codes_df$medication[i]
  category <- codes_df$category[i]
  chemical <- codes_df$chemical[i]
  
  message(sprintf("Processing code %d of %d: %s", i, nrow(codes_df), code))
  
  url <- paste0("https://openprescribing.net/api/1.0/spending_by_org/?org_type=icb&code=", code, "&format=csv")
  
  temp_df <- tryCatch({
    
    df <- read.csv(url, stringsAsFactors = FALSE)
    
    if (nrow(df) > 0) {
      df$BNFcode <- code
      df$medication <- medication
      df$category <- category
      df$chemical <- chemical
      df
    } else {
      message("No data returned for BNF code: ", code)
      NULL
    }
    
  }, error = function(e) {
    message("Failed for BNF code ", code, ": ", e$message)
    failed_codes <<- c(failed_codes, code)  
    NULL
  })
  
  if (!is.null(temp_df)) {
    data_list[[length(data_list) + 1]] <- temp_df
  }
  
  Sys.sleep(1)  
}

combined_df <- bind_rows(data_list)

write_xlsx(combined_df, "Asthma_BNF_combined.xlsx")

if (length(failed_codes) > 0) {
  message("The following BNF codes failed and can be retried later: ", paste(failed_codes, collapse = ", "))
} else {
  message("All BNF codes processed successfully.")
}



#run this code for practice level


codes_df <- read_excel(
  "C:/Users/James.Routledge/The Health Foundation/Data Analytics - 10-IAU/1. Work Programmes/Asthma project/BNFcodesasthma.xlsx"
)


date_df <- data.frame(
  date = seq(as.Date("2020-12-01"),
             as.Date("2025-11-01"),
             by = "month")
)

date_df$date <- format(date_df$date, "%Y-%m-%d")

all_data <- list()
counter <- 1

failed_requests <- data.frame(
  BNFcode = character(),
  date = character(),
  stringsAsFactors = FALSE
)

total_requests <- nrow(codes_df) * nrow(date_df)
progress_counter <- 0

for(i in 1:nrow(codes_df)){
  
  BNFcode   <- trimws(as.character(codes_df$BNFcode[i]))
  medication <- as.character(codes_df$medication[i])
  category   <- as.character(codes_df$category[i])
  chemical   <- as.character(codes_df$chemical[i])
  
  for(date_str in date_df$date){
    
    progress_counter <- progress_counter + 1
    
    message(sprintf(
      "Processing %d of %d | Code: %s | Date: %s",
      progress_counter,
      total_requests,
      BNFcode,
      date_str
    ))
    
    url <- paste0(
      "https://openprescribing.net/api/1.0/spending_by_org/",
      "?org_type=practice",
      "&code=", BNFcode,
      "&date=", date_str,
      "&format=csv"
    )
    
    temp_df <- tryCatch({
      
      df <- read.csv(url, stringsAsFactors = FALSE)
      
      if(nrow(df) == 0) stop("No data returned")
      
      df[] <- lapply(df, as.character)
      
      df$BNFcode    <- BNFcode
      df$date_requested <- date_str
      df$medication <- medication
      df$category   <- category
      df$chemical   <- chemical
      
      df
      
    }, error = function(e) {
      
      message(sprintf("FAILED → Code: %s | Date: %s", BNFcode, date_str))
      
      failed_requests <<- rbind(
        failed_requests,
        data.frame(
          BNFcode = BNFcode,
          date = date_str,
          stringsAsFactors = FALSE
        )
      )
      
      NULL
    })
    
    if(!is.null(temp_df)){
      all_data[[counter]] <- temp_df
      counter <- counter + 1
    }
    
    Sys.sleep(0.5)  
  }
}


combined_df <- bind_rows(all_data)

if("date" %in% names(combined_df)){
  combined_df$date <- format(as.Date(combined_df$date), "%Y-%m")
}


write.csv(
  combined_df,
  "Asthma_practice_level.csv",
  row.names = FALSE
)

write_xlsx(
  failed_requests,
  "Asthma_BNF_failed_requests.xlsx"
)


message("Finished.")
