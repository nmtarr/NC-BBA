## R functions to retrieve data from Google Sheets

if(!require(googlesheets4)) install.packages(
  "googlesheets4", repos = "http://cran.us.r-project.org")

## connect to block needs spreadsheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1NVBSHU5cOTCHmW067cPD8NQne60g-P3cAwY04RYPiWw/edit?usp=sharing"
gs4_auth(cache = ".secrets", email = "ncbirdatlas@gmail.com")



get_block_needs <- function(block = "NONE") {
  # get data from google sheet
  # has to be done on each call, otherwise will not update :(
  bn_data <- read_sheet(sheet_url)

  # filter results by block and return
  result <- bn_data[
    bn_data$ID_NCBA_BLOCK == block & bn_data$ACCESS == "Public",
    c("SEASON", "PRIORITY", "CRITERIA", "DESCRIPTION")
    ]
}
