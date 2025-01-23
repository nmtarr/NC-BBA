if(!require(googlesheets4)) install.packages(
  "googlesheets4", repos = "http://cran.us.r-project.org")
  library("DT")

gs4_auth(cache = ".secrets", email = "ncbirdatlas@gmail.com")

sheet_url <- "https://docs.google.com/spreadsheets/d/1NVBSHU5cOTCHmW067cPD8NQne60g-P3cAwY04RYPiWw/edit?usp=sharing"

bn_data <- read_sheet(sheet_url)
block <- "BEAR_CREEK-SE"

test <- bn_data[
    bn_data$ID_NCBA_BLOCK == block & bn_data$ACCESS == "Public",
    c("SEASON", "PRIORITY", "CRITERIA", "DESCRIPTION")
    ]

datatable(
  test,
  list(
    paging = FALSE,
    searching = FALSE,
    rownames = FALSE,
    selection = "none"
    )
)