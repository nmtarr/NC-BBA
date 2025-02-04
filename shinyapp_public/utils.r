# utility function to use throughout

percent <- function(num, digits = 2, multiplier = 100, ...) {       
  percentage <-formatC(num * multiplier, format = "f", digits = digits, ...) 
    
  # appending "%" symbol at the end of 
  # calculate percentage value 
  paste0(percentage, "%") 
}

dec_places <- function (num, digits = 2, ...) {
  number <- format(round(num, digits), nsmall = digits)

  number
}
#############################################################################
# MongoDB
# this is a read only account
HOST = "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
DB = "ebd_mgmt"
COLLECTION = "ebd"
source("ncba_config.r")
# other relevant collections include: blocks and ebd_taxonomy

URI = sprintf(
  paste0("mongodb://%s:%s@%s/%s?authSource=admin&replicaSet=",
    "atlas-3olgg1-shard-0&readPreference=primary&ssl=true"),
  ncba_db_user,
  ncba_db_pass,
  HOST,
  DB)

# connect to a specific collection (table)
m <- mongo(
  COLLECTION,
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_spp <- mongo(
  "ebd_taxonomy",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_blocks <- mongo(
  "blocks",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_block_summaries <- mongo(
  "BLOCK_SUMMARIES",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_spp_summaries <- mongo(
  "SPECIES_SUMMARIES",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_observations <- mongo(
  "ebd_observations",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_sd <- mongo(
  "safe_dates",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

m_status <- mongo(
  "db_status",
  url = URI,
  options = ssl_options(weak_cert_validation = T))

get_safe_dates <- function(){
  sd <- m_sd$find("{}","{}")

  #ADD JULIAN DATE COLUMNS
  sd$B_SAFE_START_JULIAN <- apply(
    sd['B_SAFE_START_DATE'],1,function(x){yday(x[1])})
  
  sd$B_SAFE_END_JULIAN <- apply(
    sd['B_SAFE_END_DATE'],1,function(x){yday(x[1])})

  return(
    sd[
      c(
        'TAX_NO',
        'COMMON_NAME',
        'B_SAFE_START_JULIAN',
        'B_SAFE_END_JULIAN'
      )
    ]
  )
}

safe_dates <- get_safe_dates()

# this query follows JSON based query syntax
#   (see here for the basics:
#   https://jeroen.github.io/mongolite/query-data.html#query-syntax)

# TESTING INFO
# low checklist block -> "PAMLICO_BEACH-CW" or "GRIMESLAND-NW"
# this works:
#   get_mongo_data(
#     '{"ID_NCBA_BLOCK":"GRIMESLAND-CW"}',
#     '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}',
#     FALSE)
#   get_mongo_data(
#     '{"OBSERVATIONS.COMMON_NAME":"Cerulean Warbler"}',
#     '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1',
#     '"OBSERVATIONS.COMMON_NAME":1, "OBSERVATIONS.OBSERVATION_COUNT":1,'
#     '"OBSERVATIONS.BEHAVIOR_CODE":1, "OBSERVATIONS.BREEDING_CATEGORY":1}')

aggregate_ebd_data <- function (pipeline) {
  # Perform aggregation on ebd collection in MongoDB Atlas implementation
  #
  # Description:
  #   Returns records resulting from the passed aggregation pipeline
  #
  # Arguments:
  # pipeline -- valid JSON formatted aggregation pipeline

  mongodata <- m$aggregate(pipeline)
  return(mongodata)
}
aggregate_spp_data <- function (pipeline) {  

  mongodata <- m_observations$aggregate(pipeline)
  # mongodata <- m_spp_summaries$aggregate(pipeline)
  return(mongodata)
}

get_spp_by_block <- function(species){
# Retrieves data from MongoDB Atlas with max behavior category for each 
# spp/block combination for the passed species common name

#   pipeline <- sprintf(
#     '[
#     {
#         "$match": {
#             "PRIORITY_BLOCK": "1", 
#             "OBSERVATIONS.COMMON_NAME": "%s"
#         }
#     }, {
#         "$unwind": {
#             "path": "$OBSERVATIONS"
#         }
#     }, {
#         "$match": {
#             "OBSERVATIONS.COMMON_NAME": "%s"
#         }
#     }, {
#         "$project": {
#             "species": "$OBSERVATIONS.COMMON_NAME", 
#             "breedcat": "$OBSERVATIONS.BREEDING_CATEGORY", 
#             "ID_NCBA_BLOCK": 1
#         }
#     }, {
#         "$group": {
#             "_id": "$ID_NCBA_BLOCK", 
#             "ID_NCBA_BLOCK" : {
#               "$first":"$ID_NCBA_BLOCK"
#             },
#             "bc": {
#                 "$max": "$breedcat"
#             }
#         }
#     }
# ]',
# species,
# species
#   )
#   mongodata <- m$aggregate(pipeline)

# pulls from block_summaries collection
  pipeline <- sprintf(
    '[
      {
        "$unwind":
          {
            "path": "$sppList"
          }
      },
      {
        "$match": {
            "sppList.COMMON_NAME": "%s"
          }
      },
      {
        "$project":
          {
            "ID_NCBA_BLOCK": 1,
            "bc": "$sppList.breedMaxCategory"
          }
      }
    ]',
    species
    )
  mongodata <- m_block_summaries$aggregate(pipeline)
# pulls from ebd_observations  collection
  # pipeline <- sprintf(
  #   '[
  #     {
  #       "$match": {
  #           "COMMON_NAME": "%s"
  #         }
  #     },
  #     {
  #       "$group": {
  #         "_id": "$ID_NCBA_BLOCK",
  #         "bcat": {
  #           "$max": "$BREEDING_CATEGORY" 
  #         },
  #         "ID_NCBA_BLOCK" : {
  #           "$first" : "$ID_NCBA_BLOCK" 
  #         }
  #       }
  #     },
  #     {
  #       "$project":
  #         {
  #           "ID_NCBA_BLOCK": 1,
  #           "bc": "$bcat"
  #         }
  #     }
  #   ]',
  #   species
  #   )
  # mongodata <- m_observations$aggregate(pipeline)




  return(mongodata)

}

get_ebd_data <- function(query="{}", filter="{}", sd=safe_dates){
# Retrieves data from MongoDB Atlas implementation
#
# Description:
#   Returns a dataframe of records from the NC Bird Atlas MongoDB
#     implementation. If OBSERVATION fields are included in the requested
#     output, flattens the dataframe. If a species is specificed, all
#     observations from the checklist are returned.
#
# Arguments:
# query -- JSON formatted MongoDB query
# fitler -- JSON formatted "project" parameter in MongoDB format
#
# Examples:
#   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns
#       from checklists in the GRIMESLAND-CW block
#       get_ebd_data('{"ID_NCBA_BLOCK":"GRIMESLAND-CW"}',
#         '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}')
#   2. Retrieve OBSERVATION_DATE, SAMPLING_EVENT_IDENTIFIER, OBSERVATIONS.
#       COMMON_NAME, OBSERVATIONS.OBSERVATION_COUNT, OBSERVATIONS.
#       BEHAVIOR_CODE, OBSERVATIONS.BREEDING_CATEGORY
#       for all Cerulean Warbler detections.
#       get_ebd_data('{"OBSERVATIONS.COMMON_NAME":"Cerulean Warbler"}',
#         '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1,',
#         '"OBSERVATIONS.COMMON_NAME":1, "OBSERVATIONS.OBSERVATION_COUNT":1',
#         '"OBSERVATIONS.BEHAVIOR_CODE":1, ',
#         '"OBSERVATIONS.BREEDING_CATEGORY":1}')

#specify the default  fields to return if no filter passed
  # do not run if no query passed
  if (query != "{}"){
    # don't pass blank queries!
    sortquery <- paste0(
      '{"OBSERVATION_DATE":1, ',
      '"TIME_OBSERVATIONS_STARTED":1, ',
      '"SAMPLING_EVENT_IDENTIFIER":1}')
    if (grepl("OBSERVATIONS", filter, fixed=TRUE) | filter=="{}"){
      # WORKING VERSION - downloads and returns all checklist obs
      if (filter == "{}") {
        # DEFINE DEFAULT FILTER that excludes unused fields
        filter <- paste0(
          '{"ALL_SPECIES_REPORTED":1,"ATLAS_BLOCK":1,"BCR_CODE":1,',
          '"COUNTRY":1,"COUNTRY_CODE":1,"COUNTY":1,"COUNTY_CODE":1,',
          '"DURATION_MINUTES":1,"EFFORT_AREA_HA":1,"EFFORT_DISTANCE_KM":1,',
          '"GROUP_IDENTIFIER":1,"IBA_CODE":1,"ID_BLOCK_CODE":1,',
          '"ID_NCBA_BLOCK":1,"LAST_EDITED_DATE":1,"LATITUDE":1,',
          '"LOCALITY":1,"LOCALITY_ID":1,"LOCALITY_TYPE":1,"LONGITUDE":1,',
          '"MONTH":1,"NUMBER_OBSERVERS":1,"OBSERVATIONS":1,',
          '"OBSERVATION_DATE":1,"OBSERVER_ID":1,"PRIORITY_BLOCK":1,',
          '"PROJECT_CODE":1,"PROTOCOL_CODE":1,"PROTOCOL_TYPE":1,',
          '"SAMPLING_EVENT_IDENTIFIER":1,"STATE":1,"STATE_CODE":1,',
          '"TIME_OBSERVATIONS_STARTED":1,"TRIP_COMMENTS":1,',
          '"USFWS_CODE":1,"YEAR":1, "EBD_NOCTURNAL":1}')

        # fields excluded
        # "GEOM.coordinates":1,"GEOM.type":1,"NCBA_APPROVED":1,"NCBA_BLOCK":1,
        # "NCBA_COMMENTS":1,"NCBA_REVIEWED":1,"NCBA_REVIEWER":1,
        # "NCBA_REVIEW_DATE":1,
      }

      # print("getting Observations from AtlasCache")
      mongodata <- m$find(query, filter)
      # mongodata <- m$find(
      #   query,
      #   filter,
      #   sort=sortquery) #sorting breaks for big queries

      if (nrow(mongodata)>0) {
      # print("unnesting observation records")
        mongodata <- unnest(mongodata, cols = (c(OBSERVATIONS)))

        #ADD SEASON COLUMN FROM SAFE DATES TABLE AND POPULATE
        gen_breeding_start = yday("2021-04-01")
        gen_breeding_end = yday("2021-08-31")

      # print("adding Season (Breeding = April 1 - Aug 31)")
        mongodata$SEASON <- apply(
          mongodata[c('OBSERVATION_DATE','COMMON_NAME')],1,
          function(x) {
            odj = yday(x[1]) #Convert observation_date to julian day
            #lookup spp safe dates (if any)
            # spp_s_d = sd[sd$COMMON_NAME == x[2],]
            # 
            # if (nrow(spp_s_d) == 0 ) {
            #   begin = gen_breeding_start
            #   end = gen_breeding_end
            # } else {
            #   begin = spp_s_d['B_SAFE_START_JULIAN']
            #   end = spp_s_d['B_SAFE_END_JULIAN']
            # }

            if ( gen_breeding_start <= odj & odj <= gen_breeding_end){
              season = "Breeding"
            } else {
              if(
                yday("2021-08-31") <= odj & odj <=yday("2021-10-31") | yday("2021-03-01") <= odj & odj <=yday("2021-03-31")){ 
                      season = "Migration"
              } else {
              season = "Non-Breeding"
              }
            }
            return(season)

          })
      # print("Season (Breeding or Winter)")
      } # Expand observations if records returned


      # EXAMPLE/TESTING
    # print("AtlasCache records retrieved")
      # print(head(mongodata))
      # USE aggregation pipeline syntax to return only needed observations
      # pipeline <- str_interp(
        # '[{$match: ${query}}, {$project:${filter}},',
        # '{$unwind: {path: "$OBSERVATIONS"}}]')
      #
      # mongodata <- m$aggregate(pipeline) %>%
      # unnest(cols = (c(OBSERVATIONS)))

    } else {
      mongodata <- m$find(query, filter)
    }
    return(mongodata)
  }
}




get_block_data <- function() {
  # Retrieves block data table from MongoDB Atlas implementation
  filter <- paste0(
    '{"_id": 1, "COUNTY": 1, "GEOM": 1, "ID_BLOCK": 1, "ID_BLOCK_CODE": 1,',
    ' "ID_EBD_NAME": 1, "ID_NCBA_BLOCK": 1, "ID_OLD_ID": 1, "NW_X": 1, ',
    '"NW_Y": 1, "PRIORITY": 1, "QUADID": 1, "QUAD_BLOCK": 1, "QUAD_NAME": 1, ',
    '"REGION": 1, "SE_X": 1, "SE_Y": 1, "SUBNAT2": 1, "TYPE": 1, ',
    '"ID_S123_NOSPACES_TEMP": 1, "ID_S123_SPACES_TEMP": 1}')

  # blockdata <- m_blocks$find("{}","{}")
  blockdata <- m_blocks$find("{}",filter)
  return(blockdata)
}

get_block_gap_spp <- function(blockid){
# Retrieves list of species for the passed block id
  filter <- '{"GAP_SPP":1}'
  query <- '{"_id":"${blockid}"}'
  blockdata <- m_blocks$find("{}", filter)

# add code here to unnest data (like observations above)

  return(blockdata)

}

#############################################################################
# Species
get_spp_obs <- function(species, filter){
  # wrapper function for retrieving species records
  #
  # Description:
  #   Returns datafram of requested observations from the EBD collection
  # Arguments:
  # species -- Common name of the species data to be retrieved
  # fitler -- JSON formatted "project" parameter in MongoDB format
  #
  # Examples:
  #   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns from
  #       checklists where Cerulean Warbler was observed
  #     get_spp_obs(
  #       'Cerulean Warbler',
  #        '{"OBSERVATION_DATE":1, "SAMPLING_EVENT_IDENTIFIER":1}')

  query <- str_interp('{"OBSERVATIONS.COMMON_NAME":"${species}"}')
  results <- get_ebd_data(query, filter) %>%
    filter(COMMON_NAME == species) #remove other obervations from the checklist

  return(results)
}

# Get Species List
get_spp_list <- function( query = "{}", filter = "{}" ) {

  mongodata <- m_spp$find(query, filter)

  return(mongodata)
}

species_list = sort(get_spp_list(
  query = '{"NC_STATUS":"definitive"}',
  filter = '{"PRIMARY_COM_NAME":1}'
  )$PRIMARY_COM_NAME, decreasing=FALSE)

# sort(species_list)
# print(head(species_list))

###############################################################################
# Block level summaries
# block_data <- read.csv("input_data/blocks.csv") %>% filter(COUNTY == "WAKE")
block_data <- get_block_data()
# priority_block_geojson <- readLines("input_data/blocks_priority.geojson")
# priority_block_data <- block_data

priority_block_data <- filter(
  block_data, PRIORITY == 1)[c(
    "ID_NCBA_BLOCK",
    "ID_BLOCK_CODE",
    "NW_X",
    "NW_Y",
    "SE_X",
    "SE_Y",
    "PRIORITY",
    "COUNTY",
    "REGION")]

# merge block summary data with priority_block data
priority_block_data <- priority_block_data %>%
  merge(
    get_block_summaries(),
    by = "ID_NCBA_BLOCK",
    all = TRUE
  )

print("filtering block records")

block_hours_month <- read.csv("input_data/block_month_year_hours.csv")
block_hours_total <- read.csv("input_data/block_total_hours.csv")


get_block_hours <- function(id_ncba_block) {
  # place holder for function to summarize hours in blocks
  #
  # Description:
  #   Returns datafram of requested observations from the EBD collection
  # Arguments:
  # species -- Common name of the species data to be retrieved
  # fitler -- JSON formatted "project" parameter in MongoDB format
  #
  # Examples:
  #   1. Retrieve OBSERVATION_DATE and SAMPLING_EVENT_IDENTIFIER columns
  #       from checklists where Cerulean Warbler was observed

# print(id_ncba_block)
  if (length(id_ncba_block) >0){
    result <- filter(block_hours_month, ID_NCBA_BLOCK == id_ncba_block)
  }
  if (length(result)>0){
    return(result)

  }
}

get_block_summary <- function(id_ncba_block) {

    q <- str_interp('{"ID_NCBA_BLOCK":"${cblock}"}')
}

## for overview map
get_block_summary_table <- function(season) {
  
  if (season == "wintering") {
    blocksum_filter <- paste0(
      '[{ "$project" : {',
      '"Block_Name" : "$ID_NCBA_BLOCK",',
      # '"Block_Code" : "$ID_BLOCK_CODE",',
      '"Status" : "$STATUS",',
      '"County" : "$county",',
      '"Region" : "$region",',
      '"Species_Detected" : "$winterCountDetected",',
      '"Hours" : "$winterHrsDiurnal",',
      '"Checklists" : "$winterCountDiurnalChecklists",',
      '"Early_Checklists" : "$winter1CountDiurnalChecklists",',
      '"Late_Checklists" : "$winter2CountDiurnalChecklists",',
      '"Detected_Criteria_Met" : "$wbcgDetected",',
      '"Hours_Criteria_Met" : "$wbcgTotalEffortHrs",',
      '"Checklist_Criteria_Met" : "$wbcgDiurnalVisits"',
      '}}]'
    )

    blocksum <- m_block_summaries$aggregate(blocksum_filter)

    blocksum <- blocksum %>%
      mutate('_id' = NULL) %>%
      mutate('Hours' = dec_places(Hours, digits = 1))

  } else if (season == "breeding"){
    
    blocksum_filter <- paste0(
      '[{ "$project" : {',
      '"Block_Name" : "$ID_NCBA_BLOCK",',
      # '"Block_Code" : "$ID_BLOCK_CODE",',
      '"Status" : "$STATUS",',
      '"County" : "$county",',
      '"Region" : "$region",',
      '"Hours" : "$breedHrsDiurnal",',
      '"Species_Coded" : "$breedCountCoded",',
      '"Species_Possible" : "$breedPctPossible",',
      '"Species_Confirmed" : "$breedPctConfirmed",',
      '"Early_Checklists" : "$breed1CountDiurnalChecklists",',
      '"Mid_Checklists" : "$breed2CountDiurnalChecklists",',
      '"Late_Checklists" : "$breed3CountDiurnalChecklists",',
      '"Hours_Criteria_Met" : "$bbcgTotalEffortHrs",',
      '"Coded_Criteria_Met" : "$bbcgCoded",',
      '"Confirmed_Criteria_Met" : "$bbcgConfirmed"',
      '}}]'
    )
    blocksum <- m_block_summaries$aggregate(blocksum_filter)

    blocksum <- blocksum %>%
      mutate('_id' = NULL) %>%
      mutate('Species_Possible' = percent(Species_Possible, digits = 1)) %>%
      mutate('Species_Confirmed' = percent(Species_Confirmed, digits = 1)) %>%
      mutate('Hours' = dec_places(Hours, digits = 1))
  }

  num_cols <- ncol(blocksum)
  response <- list(
    "blocksum" = blocksum,
    "num_cols" = num_cols
  )
  return(response)
}

## for overview map
get_block_summaries <- function() {
  blocksum_filter <- '{"sppList": 0, "ebird_web_data" : 0, "NCBA_EBD_VER": 0, "MOST_RECENT_EBD_DATE": 0}'

  blocksum <- m_block_summaries$find("{}", blocksum_filter)

  blocksum <- as.data.frame(blocksum)
  
  return(blocksum)
}

get_db_status <- function() {
  # query <- '{}'
  query <- '{"_id": "summary"}'
  result <- m_status$find(query, "{}")
  result <- as.data.frame(result)
  last_date <- result$MOST_RECENT_EBD_DATE_TEXT
  # print(result)
  return(last_date)
}

get_status_text <- function(b) {
  r <- "missing"
  if (b) {
    r <- "COMPLETED!"
  }
  return(r)
}