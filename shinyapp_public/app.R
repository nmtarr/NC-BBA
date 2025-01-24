# NC Bird Atlas Shiny App - Public Version
# v1
# 01/24/2024
# Scott K. Anderson, Elsa Chen, Nathan Tarr, Scott Pearson
# https://github.com/nmtarr/NCBA/shinyapp_public


if(!require(ggiraph)) install.packages(
  "ggiraph", repos = "http://cran.us.r-project.org")
if(!require(reactlog)) install.packages(
  "reactlog", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages(
  "shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages(
  "shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org")
if(!require(mongolite)) install.packages(
  "mongolite", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages(
  "dplyr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages(
  "leaflet", repos = "http://cran.us.r-project.org")
if(!require(leaflegend)) install.packages(
  "leaflegend",
  repos = "http://cran.us.r-project.org"
)
if(!require(htmltools)) install.packages(
  "htmltools", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages(
  "shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages(
  "shinydashboard",
  repos = "http://cran.us.r-project.org"
)
if(!require(polished)) install.packages(
  "polished", repos = "http://cran.us.r-project.org")
# if(!require(waffle)) install.packages(
#   "waffle", repos = "https://cinc.rud.is")

#adds functions for tooltips
if(!require(shinyBS)) install.packages(
  "shinyBS", repos = "http://cran.us.r-project.org")
# if(!require(geojsonio)) install.packages(
#   "geojsonio", repos = "http://cran.us.r-project.org")

#libraries for spp data
if(!require(lubridate)) install.packages(
  "lubridate", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages(
  "grid", repos = "http://cran.us.r-project.org")
if(!require(gridBase)) install.packages(
  "gridBase", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages(
  "RColorBrewer", repos = "http://cran.us.r-project.org")

# data tables
if(!require(DT)) install.packages(
  "DT", repos = "http://cran.us.r-project.org")

DT:::DT2BSClass(c("compact", "cell-border"))
dt_opts <- list(
  info = FALSE,
  paging = FALSE,
  searching = FALSE,
  scrollX = FALSE,
  scrollY = FALSE,
  ordering = FALSE
  )

#get functions from other files
source("blocks.r")
source("utils.r") #utilities file
source("spp.r") #species function file
source("blkcumhrfunc.r") #block hour graph
source("google.r") # google spreadsheet block_needs integration
# source("ncba_functions_shiny.R") #add this later to sync with Nathan work
# source("ncba_functions.R") #add this later to sync with Nathan work
# print(block_predicted_spp(block = block, source = "GAP"))

# MAP CONSTANTS
nc_center_lat <- 35.5
nc_center_lng <- -79.2
nc_center_zoom <- 7
nc_block_zoom <- 13
ncba_blue <- "#2a3b4d"
ncba_half_blue <- "#B8C5D3"
checklists_found <- 0
ncba_failed <- "#dd0000"
ncba_success <- "#43AA8B"
ncba_white <- "#ffffff"
ncba_gray <- "#aaaaaa"
priority_colors <- c(
  "High" = "#dd0000",
  "Medium" = "#daaa00",
  "Low" = "#2c3e50")

# SETUP FILES
current_block <- ""
enableBookmarking("url")
sd <- get_safe_dates()


# Define UI for Quackalacky Data app -----------------------------------------
ui <- bootstrapPage(
  # titlePanel("NC Bird Atlas Explorer"),
  navbarPage(
    theme = shinytheme("flatly"), 
    collapsible = TRUE,
    # theme = shinytheme("cosmo"), collapsible=TRUE,
    position = "static-top",
    # header = 
    htmlOutput("navbar_title"),
    # HTML(
    #   paste0(
    #     '<a style="text-decoration:none;cursor:default;color:#FFFFFF;"',
    #     ' class="active" href="#">NC Bird Atlas Explorer</a>',
    #     '<p style="font-style:italic;font-size: 0.62em;">',
    #     'Last Updated Dec 31, 2023</p>'
    #     )
    #   ),
    # ),
    id = "nav",
    windowTitle = "NCBA Explorer",
    tags$head(includeCSS("styles.css")),
    tags$head(tags$link(rel="icon", href="/input_data/ncba_blue_wbnu.ico")),
    tabPanel("Blocks",
      div(class="row", id="top_row_panel",
        div(class="col-md-2 panel sidebar", id = "block_controls",
          h3("Blocks", class="tab-control-title"),
          tags$p(
            "Summary statistics page for block-level data.",
            class="desc-text"
            ),
          div(class="tab-control-group",
            selectInput(inputId = "APBlock", #name of input
            label = "Selected Priority Block", #label displayed in ui
            choices = c(
              as.character(unique(priority_block_data$ID_NCBA_BLOCK)),
              "NONE"),
            # calls unique values from the ID_NCBA_BLOCK column
            # in the previously created table
            selected = "NONE")
          ),
          div(class="tab-control-group",
            radioButtons("season_radio",label = h4("Season"),
              choices = list(
                "All Records" = "All",
                "Breeding" = "Breeding",
                "Non-Breeding" = "Non-Breeding"
              ),
              selected = "All")

          )
        ),
        div(class = "col-md-10 panel",
          leafletOutput("mymap", height = "50vh")
        ),
      ),
      div(class = "row", id = "mid_row_panel",
        div(class = "col-md-4 panel",
          h3("Statistics"),
          h4(htmlOutput("block_status")),
          tabsetPanel(
            tabPanel("Breeding", htmlOutput("block_breeding_stats")),
            tabPanel("Wintering", htmlOutput("block_wintering_stats")),
          ),
          downloadButton("download_block_checklists", "Download Checklists")
        ),
        div(class = "col-md-4 panel",
          h3("Block Needs"),
          htmlOutput("block_needs_table")
          # dataTableOutput("block_needs_table")
        ),
        div(class = "col-md-4 panel",
          h3("Survey Hours"),
          plotOutput("blockhours")
        ),
      ),
      div(class = "row", id = "spp_list_row",
        div(class = "col-md-4 panel",
          h3("Species Accumulation"),
          plotOutput("spp_accumulation")
        ),
        div(class = "col-md-8 panel",
          h3("Species List"),
          dataTableOutput("spp_observed"),
          downloadButton("download_spplist", "Download")

        )
      )
    ),
    tabPanel("Block Table",
        div(
          class = "col-md-12",
          style = "background: #eee;",
          div(
            class = "tab-control-group",
            style = "width: 60%",
            radioButtons("block_table_radio",label = h4("Season"),
              choices = list(
                "Breeding" = "breeding",
                "Non-Breeding" = "wintering"
              ),
              selected = "wintering"
            )
          ),
          div(
            class = "float-end",
            style = "width: 30%",
            downloadButton("download_block_table", "Download")
          )
        ),
        div(
          class = "col-md-12",
          style = "font-size: 13px; margin-top: 10px;",
 
          div(dataTableOutput("block_table"))
        )
    ),
    tabPanel("Species Map",
        div(
          class = "col-md-2",
          div(
            selectizeInput(
              "sppmap_select",
              h3("Species"),
            choices = species_list, options = list(
              placeholder = 'Select species',
              onInitialize = I('function() {this.setValue(""); }')
              )
            )
          ),
          div(tableOutput("block_breedcode_table"))
        ),
        div(
          class = "col-md-10",
          id = "spp-block-map",
          leafletOutput("mysppmap")
          )
    ),
    tabPanel(
      "Overview",
      div(
        class = "col-md-11",
        leafletOutput("overview_map", height = "70vh")
      ),
      div(
        class = "col-md-1",
        h5("Legend"),
        plotOutput("overview_legend")
      )
    ),
    tabPanel(
      "About",
      tags$div(
        tags$h4("NC Bird Atlas Data Explorer"),
        tags$p(
          paste0("This site is a work in progress, designed to provide access",
          " to data collected through the North Carolina Bird Atlas. ",
          "Data submitted to eBird is updated on a monthly basis, ",
          "and are unvetted.")),
        tags$br(),
        tags$img(
          src = "ncba_blue_halo_final.png"
          # width = "150px",
          # height = "75px"
          )

        )
    ),
    # selected = "Overview"
  )

)

# Define server logic to plot various variables against mpg
server <- function(input, output, session) {

## GET SERVER URL PARAMETERS (if they exist)

observe({
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query[['block']])) {
    # print(query[['block']])
    # b <- query[['block']]
    rv_block$id = query[['block']]
    # if ( b %in% input$APBlock & b != input$APBlock) {
    #   selected = input$APBlock[input$APBlock != b]
    # } else {
    #   selected = c(input$APBlock, b)
    #   updateSelectInput(
    #     session,
    #     "APBlock",
    #     selected = selected
    #   )
    # }
    # updateQueryString('https://ncbirdconservation.shinyapps.io/shinyapp_public/')
  }

  remove_query_string (
    session = shiny::getDefaultReactiveDomain(),
    mode = "replace"
  )
})


# output$url <- renderText({
#   'http://127.0.0.1:3658/'
#   # 'https://ncbirdconservation.shinyapps.io/shinyapp_public/'
# })

## BLOCK TAB  ----------------------------------------------------
  # latest record
  output$navbar_title <- reactive({
    htmltools::HTML(
      paste0(
        '<a style="text-decoration:none;cursor:default;color:#FFFFFF;"',
        ' class="active" href="#">NC Bird Atlas Explorer</a>',
        '<p style="font-style:italic;font-size: 0.62em;">',
        'Last Updated ', get_db_status(), '</p>'
        )
      )
  })
  # CHECKLISTS
  checklist_count <- reactive({
    unique_sei <- unique(current_block_ebd()$SAMPLING_EVENT_IDENTIFIER)
    length(unique_sei)
  })

  checklist_filtered_count <- reactive({
    unique_sei <- 
      unique(current_block_ebd_filtered()$SAMPLING_EVENT_IDENTIFIER)
    length(unique_sei)
  })

  ### BLOCK TAB BLOCKS ----------------------------------------------------
  # reactive listener for block select
  #
  # Creating a reactive value so it can be updated outside of rv_block$id
  # I think rv_block$id could just be totally replaced with rv_block$id?

  rv_block <- reactiveValues(chosen = NULL, id = NULL)

  #observe events where block changes
  # block map click
  observeEvent(
    input$mymap_shape_click,
    {
      rv_block$id = input$mymap_shape_click$id
      # print("block map clicked")
    }
  )
  # block select change
  observeEvent(
    input$APBlock,
    {
      rv_block$id = input$APBlock
      # print("Block selected from drop down list")
      if(input$APBlock == "NONE")
        rv_block$id = NULL
      else
        rv_block$id = input$APBlock
    },
    ignoreInit = TRUE
  )

  # overview map click
  observeEvent(
    input$overview_map_shape_click,
    {
      rv_block$id = input$overview_map_shape_click$id
      # print("overview map clicked")

      #change focus to blocks tab
      updateNavbarPage(
        session,
        "nav",
        selected = "Blocks"
      )
    }
  )

  # when rv_block changes, do the following
  observeEvent(
    rv_block$id,
    {
      # print(paste0("rv_block changed to ", rv_block$id))
      b <- rv_block$id
      # if (b %in% input$APBlock & b != input$APBlock){
      #   # block is in drop-down, and is not the current block
      #   selected = input$APBlock[input$APBlock != b]
      # } else {
        # selected = b
        updateSelectInput(
          session,
          "APBlock",
          selected = b
        )
      # }

      ## Update map - zoom in
      block_info <- dplyr::filter(priority_block_data, ID_NCBA_BLOCK == rv_block$id)

      #calculate the center of the block
      block_center_lat <- block_info$SE_Y +
        ((block_info$NW_Y - block_info$SE_Y)/2)
      block_center_lng <- block_info$SE_X - 
        ((block_info$SE_X - block_info$NW_X)/2)

      leafletProxy("mymap", session) %>%
        setView(
          lat = block_center_lat,
          lng = block_center_lng,
          zoom = nc_block_zoom
          )
    }
  )

  # # grab id from map click
  # observeEvent (input$mymap_shape_click, {
  #   blockmap_info <- input$mymap_shape_click
  #   blockmap_info_id <- input$mymap_shape_click$id

  #   # rv_block$chosen <- blockmap_info
  #   rv_block$id <-  blockmap_info_id
  # })

  # current block changes when map is clicked
  # current_block_r <- reactive({
  #   print("map clicked")
  #   print(rv_block$id)
  #   paste(rv_block$id)
  # })

  # when map block clicked, update select input drop down list
  # and when clicking current block it doesn't clear the name
  # from the drop down list
  # observeEvent(input$mymap_shape_click, {
  #   print("Updating drop down list to match clicked block")
  #   click <- input$mymap_shape_click
  #   if(click$id %in% input$APBlock & click$id != input$APBlock)
  #     selected = input$APBlock[input$APBlock != click$id]
  #   else
  #     selected = c(input$APBlock, click$id)
  #   updateSelectInput(session, "APBlock",
  #                     selected = selected)
  # })

  #### React to Change of Selected Block in Drop down list instead of click,
  ## a bit redundant with the one above but I am not sure how to combine them?
  # observeEvent(input$APBlock, {
  #   print("Block selected from drop down list")
  #   blockmap_list_id <- input$APBlock
  #   if(input$APBlock == "NONE")
  #     rv_block$id = NULL
  #   else
  #     rv_block$id = blockmap_list_id
  # })

  # retrieves current block records when rv_block$id changes
  current_block_ebd <- reactive({
    req(rv_block$id)
    # print("retrieving current block data")
    #build query string from parameters
    #philosophy:
    #   - if block changes, rerun query to retrieve from
    #       AtlasCache (this function)
    #       - this returns a flattened dataset,
    #           with one row per checklist-species combination
    #   - if other parameters change,
    #       filter existing data with "current_block_ebd_filtered"
    #   - current_block_ebd_checklistsonly_filtered
    #       returns checklist level data

    cblock <- rv_block$id

    q <- str_interp('{"ID_NCBA_BLOCK":"${cblock}", "PROJECT_CODE": "EBIRD_ATL_NC"}')

    get_ebd_data(q, "{}") #get all fields

  })
  current_block_summary <- reactive({
    req(rv_block$id)
    cblock <- rv_block$id

    q <- str_interp('{"ID_NCBA_BLOCK":"${cblock}"}')

    m_block_summaries$find(q, "{}") #get all fields

  })
  portal_records_switch <- reactive({
    # input$portal_records()
    TRUE
  })

  # Applies filter WHEN CRITERIA CHANGES
  current_block_ebd_filtered <- reactive({
    req(current_block_ebd())
    # print(head(current_block_ebd()$EBD_NOCTURNAL))
    #make sure there are records to filter!
    validate(
      need(checklist_count()>0,"")
    )
    # print("applying filters to block records")
    current_block_ebd() %>%
      dplyr::filter(
        if(portal_records_switch())
        PROJECT_CODE == "EBIRD_ATL_NC"
        else TRUE
      ) %>%
      dplyr::filter(
        if (input$season_radio == "Breeding")
          MONTH %in% c("3","4","5","6","7","8")
          else if (input$season_radio == "Non-Breeding") 
            MONTH %in% c("1","2","11","12")
            else if (input$season_radio == "Custom")
            MONTH %in% input$month_range
        else TRUE
      )
  })


  # UPDATE CHECKLIST COUNT
  # output$checklist_counter <- renderUI({
  #   req(
  #     current_block_ebd(),
  #     current_block_ebd_filtered(),
  #     checklist_count(),
  #     checklist_filtered_count())

  #   all <- paste(checklist_count(), " Total Checklists Found")

  #   filtered <- paste(
  #     checklist_filtered_count(),
  #     " Filtered Checklists Found")

  #   HTML(paste(all, filtered, sep='<br/>'))

  # })

  #### FILTERS BLOCK RECORDS WHEN CRITERIA CHANGES
  #     RETURNS CHECKLIST LEVEL DATA ------
  current_block_ebd_checklistsonly_filtered <- reactive({

    req(current_block_ebd(), current_block_ebd_filtered())

    current_block_ebd_filtered() %>%
      dplyr::filter(CATEGORY == "species") %>% # make sure only species counted
      group_by(SAMPLING_EVENT_IDENTIFIER) %>%  # nolint
      mutate(SPP_COUNT = length(unique(GLOBAL_UNIQUE_IDENTIFIER))) %>%
      ungroup(SAMPLING_EVENT_IDENTIFIER) %>%
      distinct(SAMPLING_EVENT_IDENTIFIER, .keep_all = TRUE) %>%
      select(ALL_SPECIES_REPORTED, SPP_COUNT, ATLAS_BLOCK,BCR_CODE,COUNTRY, # nolint
        COUNTRY_CODE,COUNTY,COUNTY_CODE,DURATION_MINUTES,EFFORT_AREA_HA, # nolint
        EFFORT_DISTANCE_KM,GROUP_IDENTIFIER,IBA_CODE,ID_BLOCK_CODE, # nolint
        ID_NCBA_BLOCK,LAST_EDITED_DATE,LATITUDE,LOCALITY,LOCALITY_ID, # nolint
        LOCALITY_TYPE,LONGITUDE,MONTH,NUMBER_OBSERVERS,OBSERVATION_DATE, # nolint
        OBSERVER_ID,PRIORITY_BLOCK,PROJECT_CODE,PROTOCOL_CODE,PROTOCOL_TYPE, # nolint
        SAMPLING_EVENT_IDENTIFIER,STATE,STATE_CODE,TIME_OBSERVATIONS_STARTED, # nolint
        TRIP_COMMENTS,USFWS_CODE,YEAR,EBD_NOCTURNAL) %>% # nolint
      # ADD ADDITIONAL FILTERS HERE AS NEEDED
      # create a column with the html link https://ebird.org/checklist/SID
      mutate(
        link = paste(htmlEscape("https://ebird.org/checklist"),
        SAMPLING_EVENT_IDENTIFIER, sep = "/")) %>%
      # then a column with the HTML code for part of the popup label
      mutate(
        ebird_link = paste0("",'<a style="font-weight:bold" href="',
          link,
          '"target="_blank">',htmlEscape(SAMPLING_EVENT_IDENTIFIER),
          '</a> <br>',"Date: ", htmlEscape(OBSERVATION_DATE), "<br>",
          "Start Time: ", htmlEscape(TIME_OBSERVATIONS_STARTED), "<br>",
          "Length (Minutes): ", htmlEscape(DURATION_MINUTES), "<br>",
          "Distance (km): ", htmlEscape(EFFORT_DISTANCE_KM), "<br>"
          )
        )
  })




  # # POPULATE LABEL FOR CURRENT BLOCK

  output$download_block_checklists <- downloadHandler(
    filename = function() {
      p <- ""
      if (portal_records_switch()){ p <- "portal"}

      paste(
        rv_block$id,
        input$season_radio,p,
        "blockchecklists.csv",
        sep = "_")
    },
    content = function(file) (
      write.csv(
        current_block_ebd_checklistsonly_filtered(),
        file,
        row.names = TRUE)
    )

  )

### For downloading testing datasets when conditions change (Deprecated?) ####

  ## BREEDING STATS ----------------------------------------------------

  ## BLOCK STATUS
  output$block_status <- renderUI({

    complete_text <- ifelse(
      current_block_summary()$STATUS == "Complete",
      "<h3>COMPLETED!</h3>",
      "<h5>Incomplete</h5>"
    )
    HTML(complete_text)
  })

  output$block_breeding_stats <- renderUI({
    req(current_block_summary())
    # print("rendering block stats")
    #ensure records returned
    validate(
      need(current_block_summary(), "No checklists submitted.")
    )

    breed_stat_col <- as.character(
      c(
        "Detected, Uncoded",
        "Coded",
        "Confirmed",
        "Probable",
        "Possible",
        "Diurnal Hrs",
        "Diurnal Visits (Mar/Apr)",
        "Diurnal Visits (May/Jun)",
        "Diurnal Visits (Jul/Aug)",
        "Nocturnal Visits",
        "Nocturnal Hrs"
      )
    )

    breed_val_col <- as.character(
      c(
        current_block_summary()$breedCountDetected,
        current_block_summary()$breedCountCoded,
        sprintf(
          "%s (%.0f%%)",
          current_block_summary()$breedCountConfirmed,
          current_block_summary()$breedPctConfirmed * 100
        ),
        sprintf(
          "%s (%.0f%%)",
          current_block_summary()$breedCountProbable,
          current_block_summary()$breedPctProbable * 100
        ),
        sprintf(
          "%s (%.0f%%)",
          current_block_summary()$breedCountPossible,
          current_block_summary()$breedPctPossible * 100
        ),
        format(current_block_summary()$breedHrsDiurnal, digits = 1),
        current_block_summary()$breed1CountDiurnalChecklists,
        current_block_summary()$breed2CountDiurnalChecklists,
        current_block_summary()$breed3CountDiurnalChecklists,
        current_block_summary()$breedCountNocturnalChecklists,
        format(current_block_summary()$breedHrsNocturnal, digits = 1)
      )
    )

    breed_target_col <- as.character(
      c(
        "-",
        ">= 55",
        ">= 25%",
        "-",
        "<= 25%",
        ">= 20",
        ">= 1",
        ">= 1",
        ">= 1",
        "2 (preferred)",
        "-"
      )
    )
    breed_status_col <- as.character(
      c(
        "-",
        get_status_text(current_block_summary()$bbcgCoded),
        get_status_text(current_block_summary()$bbcgConfirmed),
        "-",
        get_status_text(current_block_summary()$bbcgPossible),
        get_status_text(current_block_summary()$bbcgTotalEffortHrs),
        get_status_text(current_block_summary()$breed1CountDiurnalChecklists),
        get_status_text(current_block_summary()$breed2CountDiurnalChecklists),
        get_status_text(current_block_summary()$breed3CountDiurnalChecklists),
        "-",
        "-"
      )
    )

    breed_stats_colnames <- c(
      "Statistic",
      "Value",
      "Criteria",
      "Status"
    )
    breed_stats_dt <- data.frame(
      breed_stat_col,
      breed_val_col,
      breed_target_col,
      breed_status_col
    )
    # print(breed_stats_dt)
    # print(dt_opts)

    DT::datatable(
      breed_stats_dt,
      options = dt_opts,
      colnames = breed_stats_colnames,
      rownames = FALSE
    ) %>%
    formatStyle(
      columns = c(0,1,2,3),
      fontSize = "80%"
    ) %>%
    formatStyle(
      columns = c(1,2,3),
      className = "dt-center"
    )

    # End Data Table Experiment
  })

  ## WINTERING STATISTICS ----------------------------------------------------
  output$block_wintering_stats <- renderUI({
    req(current_block_summary())
  # print("rendering winter block stats")
    #ensure records returned
    validate(
      need(current_block_summary(), "No checklists submitted.")
    )

    # Data Table Experiment
    
    winter_stat_col <- as.character(
      c(
        "Total Species",
        "Diurnal Visits (Nov-Dec)",
        "Diurnal Visits (Jan-Feb)",
        "Diurnal Visits Total",
        "Diurnal Hrs",
        "Nocturnal Visits",
        "Nocturnal Hrs"
      )
    )

    winter_val_col <- as.character(
      c(
        current_block_summary()$winterCountDetected,
        current_block_summary()$winter1CountDiurnalChecklists,
        current_block_summary()$winter2CountDiurnalChecklists,
        current_block_summary()$winterCountDiurnalChecklists,
        format(current_block_summary()$winterHrsDiurnal, digits = 1),
        current_block_summary()$winterCountNocturnalChecklists,
        format(current_block_summary()$winterHrsNocturnal, digits = 1)
      )
    )

    winter_target_col <- as.character(
      c(
        ">= 55",
        ">= 1",
        ">= 1",
        ">= 2",
        ">= 5",
        "1 (preferred)",
        "-"
      )
    )
    winter_status_col <- as.character(
      c(
        get_status_text(current_block_summary()$wbcgDetected),
        get_status_text(current_block_summary()$winter1CountDiurnalChecklists),
        get_status_text(current_block_summary()$winter2CountDiurnalChecklists),
        get_status_text(current_block_summary()$wbcgDiurnalVisits),
        get_status_text(current_block_summary()$wbcgTotalEffortHrs),
        "-",
        "-"
      )
    )

    winter_stats_colnames <- c(
      "Statistic",
      "Value",
      "Criteria",
      "Status"
    )
    winter_stats_dt <- data.frame(
      winter_stat_col,
      winter_val_col,
      winter_target_col,
      winter_status_col
    )

    DT::datatable(
      winter_stats_dt,
      options = dt_opts,
      colnames = winter_stats_colnames,
      rownames = FALSE
    ) %>%
    formatStyle(
      columns = c(0,1,2,3),
      fontSize = "80%"
    ) %>%
    formatStyle(
      columns = c(1,2,3),
      className = "dt-center"
    )


  })
  
  #### DISPLAY BLOCK HOURS SUMMARY PLOT ------
  block_hrs_results <- reactive({
    req(current_block_ebd_checklistsonly_filtered())
  # print("running block hrs results")
    block_hrs(current_block_ebd_checklistsonly_filtered())

  })
  output$blockhours <- renderPlot({
    req(current_block_ebd_checklistsonly_filtered())
    block_hrs_results()$hr_plot
  })

  #### DISPLAY SPECIES ACCUMULATION PLOT ------
  spp_accumulation_results <- reactive({
    req(current_block_ebd(), current_block_ebd_filtered())
    # pass only those columns needed
    sa <- dplyr::filter(
      current_block_ebd_filtered(),
      CATEGORY == "species"
      )[c("SAMPLING_EVENT_IDENTIFIER", "OBSERVATION_DATE", "DURATION_MINUTES",
      "BREEDING_CODE", "BREEDING_CATEGORY", "COMMON_NAME", "CATEGORY")]

    cblock <- rv_block$id

    ## Retrieves appropriate data through aggregation query
    ## _id = combination of block name and species
    ## spp = species common name
    ## maxBC = maximum Breeding Code
    pipeline <- str_interp(c(
      '[{"$match": {"ID_NCBA_BLOCK": "${cblock}"}},',
      '{"$unwind": {"path": "$OBSERVATIONS"}}, ',
      '{"$match": {"OBSERVATIONS.CATEGORY": "species"}},',
      '{"$group": {"_id": {"blockName": "$ID_NCBA_BLOCK",',
      '"spp": "$OBSERVATIONS.COMMON_NAME"},',
      '"maxBC": {"$max": "$OBSERVATIONS.BREEDING_CATEGORY"}}}]'
      ))

    spp_bcs <- aggregate_ebd_data(pipeline)
    plot_spp_accumulation(sa, spp_bcs)

  })

  output$spp_accumulation <- renderPlot({
    req(spp_accumulation_results())
    # print(spp_accumulation_results()$spp_acc_data)
    spp_accumulation_results()$plot

  })

  #### DISPLAY BLOCK NEEDS ------
  bn_block <- reactive({
    if (length(rv_block$id) > 0) {
      rv_block$id
    } else {
      "NONE"
    }
  })
  output$block_needs_table <- renderUI({
    bn <- get_block_needs(block = bn_block())
    
    bn_breed_html <- "<h4>Breeding</h4>"
    bn_winter_html <- "<h4>Wintering</h4>"
    key_html <- paste0(
      "<div style='font-size:0.95rem;padding:10px 0px;text-align:center;'>",
      "<span style='color:", as.character(priority_colors["High"]),
      "'>High Priority</span>, ",
      "<span style='color:", as.character(priority_colors["Medium"]),
      "'>Medium Priority</span>, ",
      "<span style='color:", as.character(priority_colors["Low"]),
      "'>Low Priority</span></div>"
    )

    if (!is.na(as.character(bn[1, "SEASON"]))) {
      for (row in 1 : nrow(bn)) {

        line <- sprintf(
          "<div style='padding: 0px 10px; color:%s'><strong>%s</strong>: %s</div>",
          as.character(priority_colors[as.character(bn[row, "PRIORITY"])]),
          bn[row, "CRITERIA"],
          bn[row, "DESCRIPTION"]
          # bn[row, "DATE"]
        )
        if (bn[row, "SEASON"] == "Breeding") {
          bn_breed_html <- paste0(bn_breed_html, line)
        } else {
          bn_winter_html <- paste0(bn_winter_html, line)
        }
      }
    } else {
      none_html <- "<div style='padding: 0px 20px;'>None</div>"
      bn_breed_html <- paste0(bn_breed_html, none_html)
      bn_winter_html <- paste0(bn_winter_html, none_html)
    }

    HTML(paste0(bn_breed_html, bn_winter_html, key_html))
  })
  # output$block_needs_table <- renderDataTable(
  #   get_block_needs(block = bn_block()),
  #   options = list(
  #       rownames = FALSE,
  #       paging = FALSE,
  #       searching = FALSE,
  #       selection = "none"
  #       )
  # )

  #### DISPLAY SPECIES LIST ------
  # output$spp_observed <- renderDataTable(
  #   spp_accumulation_results()$spp_unique[
  #     c("spp", "bcat")],
  #     options = list(pageLength=5, autoWidth = TRUE)
  # )
  block_spp_list <- reactive({
    cblock <- rv_block$id
    block_spp_pipeline <- str_interp(c(
      '[{"$match": {"ID_NCBA_BLOCK": "${cblock}"}},',
      '{"$unwind": {"path": "$sppList"}},',
      '{"$lookup": {"from": "ebd_taxonomy",',
      ' "localField" : "sppList.COMMON_NAME",',
      ' "foreignField" : "PRIMARY_COM_NAME",',
      ' "as": "tax"}},',
      '{"$unwind": {"path": "$tax"}},',
      '{"$project": {',
      '"TAX_ORDER": "$tax.TAXON_ORDER_2022",',
      '"SPECIES": "$sppList.COMMON_NAME",',
      '"BREEDING_STATUS": "$sppList.breedStatus",',
      '"BREEDING_DETECTED": "$sppList.breedDetected",',
      '"WINTER_DETECTED": "$sppList.winterDetected"',
      '}}',
      ']'
    ))
    m_block_summaries$aggregate(block_spp_pipeline)
  })

  output$spp_observed <- renderDataTable(block_spp_list(), rownames = FALSE)

  
  output$download_spplist <- downloadHandler(
    filename = function() {
      paste("spp_list", ".csv", sep = "")
    },
    content = function ( file ) {
      write.csv(block_spp_list(), file, row.names = TRUE)
    }
  )
  ## Block Table Tab -------------------------------------------------
  block_table_data <- reactive({
    req(input$block_table_radio)
    get_block_summary_table(input$block_table_radio)
  })

  output$block_table <- DT::renderDT(
    block_table_data()$blocksum,
    filter = "top",
    options = list(
      pageLength = 100,
      columnDefs = list(
        list(
          className = 'dt-center',
          targets = 4:block_table_data()$num_cols
        )
      )
    )
  )
  
  output$download_block_table <- downloadHandler(
    filename = function() {
      paste(
        input$block_table_radio,
        "_block_table",
        ".csv",
        sep = ""
      )
    },
    content = function ( file ) {
      write.csv(
        block_table_data()$blocksum,
        file,
        row.names = TRUE
        )
    }
  )

  ## SPECIES MAP  ----------------------------------------------------
  ### SETUP LEAFLET MAP, RENDER BASEMAP ------
  output$mymap <- renderLeaflet({

    #setup block geojson layer
  # print("starting map, adding blocks")

    ## Add fill value based on block status
    priority_block_data <- priority_block_data %>%
    mutate(
      fillComplete = ifelse(
        STATUS == "Complete", ncba_white, "#777777"
      )
    ) %>%
    mutate(
      colorComplete = ifelse(
        STATUS == "Complete", ncba_blue, "#aaaaaa"
      )
    ) %>%
    mutate(
      fillOpac = ifelse(
        STATUS == "Complete", 0.8, 0.05
      )
    )

    leaflet() %>%
      setView(
        lng = nc_center_lng,
        lat = nc_center_lat,
        zoom = nc_center_zoom) %>%
      # addTiles() %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(opacity = 1),
        group = "Aerial Map"
        ) %>%
      addProviderTiles(
        "OpenStreetMap.Mapnik",
        options = providerTileOptions(opacity = 1),
        group = "Street Map"
      ) %>%
      addRectangles(
        data = priority_block_data,
        layerId = ~ ID_NCBA_BLOCK,
        lng1 = ~ NW_X,
        lat1 = ~ NW_Y,
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        weight = 2,
        # color = ncba_white,
        color = ~ colorComplete,
        opacity = 0.9,
        # fillColor = "#777777",
        fillColor = ~ fillComplete,
        # fillOpacity = 0.05,
        fillOpacity = ~ fillOpac,
        fill = TRUE,
        label = ~ ID_NCBA_BLOCK
        ) %>%
        addLayersControl(
          data = priority_block_data,
          baseGroups = c("Street Map", "Aerial Map"),
          options = layersControlOptions(collapsed = FALSE)
          )
  })

  ### ZOOM MAP TO SELECTED BLOCK ------
  # observeEvent(rv_block$id, {
  #     req(rv_block$id)
  #     block_info <- dplyr::filter(block_data, ID_NCBA_BLOCK==rv_block$id)

  #     #calculate the center of the block
  #     block_center_lat <- block_info$SE_Y +
  #       ((block_info$NW_Y - block_info$SE_Y)/2)
  #     block_center_lng <- block_info$SE_X - 
  #       ((block_info$SE_X - block_info$NW_X)/2)

  #     leafletProxy("mymap", session) %>%
  #       setView(
  #         lat = block_center_lat,
  #         lng = block_center_lng,
  #         zoom = nc_block_zoom
  #         )
  # })

   ## SPECIES MAP  ----------------------------------------------------
  ### SETUP LEAFLET MAP, RENDER BASEMAP ------
  output$mysppmap <- renderLeaflet({

    #setup block geojson layer
  # print("starting map, adding blocks")

    leaflet() %>%
      setView(
        lng = -79.0,
        lat = 35.7,
        zoom = 7) %>%
      addProviderTiles(providers$Esri.WorldTopoMap)

  })
  ### ADD SYMBOLOGY FOR SELECTED SPECIES ------
  sppblock_data <- reactive({
    # print(input$sppmap_select)
      spp <- input$sppmap_select
    # print(spp)
      get_spp_by_block(spp)
  })

  observeEvent(
    sppblock_data(),
    {
    # print("retrieving spp block data")
      sppblockmap_data <- sppblock_data()
    # print("spp block data retrieved")

      if (nrow(sppblockmap_data) > 0){
      # print (paste0("spp by block len = ",nrow(sppblockmap_data)))
      spp_blocks <- merge(
        priority_block_data,
        sppblockmap_data,
        by = "ID_NCBA_BLOCK"
        )
      # print(paste0("spp block len = ",nrow(spp_blocks)))


      spp_blocks <- mutate(
        spp_blocks,
        breedcat = case_when(
          bc == "C4" ~ "C4 Confirmed",
          bc == "C3" ~ "C3 Probable",
          bc == "C2" ~ "C2 Possible",
          TRUE ~ "C1 Observed"
        )
      )
      # print(head(spp_blocks))
      spp_blocks <- mutate(
        spp_blocks,
        blocklink = sprintf(
          'https://ebird.org/atlasnc/block/%s',
          spp_blocks$ID_BLOCK_CODE.x
          )
      )
      output$block_breedcode_table <- renderTable(table(spp_blocks$breedcat))

      block_colors <- colorFactor(
        palette = brewer.pal(4, 'Purples'),
        domain = spp_blocks$breedcat
        )

    # print("adding blocks to map")
      leafletProxy("mysppmap") %>%
      clearShapes() %>%
      clearControls() %>%
      addRectangles(
        data = priority_block_data,
        layerId = ~ ID_BLOCK_CODE.x,
        lng1 = ~ NW_X,
        lat1 = ~ NW_Y,
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        weight= 0.5,
        color=ncba_blue,
        opacity = 0.25,
        fill = FALSE,
        label = priority_block_data$ID_NCBA_BLOCK
        ) %>%
      addRectangles(
        data = spp_blocks,
        layerId = ~ ID_NCBA_BLOCK,
        lng1 = ~ NW_X,
        lat1 = ~ NW_Y,
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        weight = 0.5,
        color = '#000000',
        # opacity = 0,
        fillColor= ~block_colors(breedcat),
        fillOpacity = 0.8,
        fill = TRUE,
        label = sprintf(
          "<strong><a href = '%s'>%s</a></strong><br/>%s",
          spp_blocks$blocklink,
          spp_blocks$ID_NCBA_BLOCK,
          spp_blocks$breedcat
          ) %>%
          lapply(htmltools::HTML)
        ) %>%
      addLegend(
        "bottomright",
        pal = block_colors,
        values = spp_blocks$breedcat,
        labFormat = labelFormat(
          transform = function(x) sort(x, decreasing = FALSE)
        ),
        title = "Breeding Category",
        opacity = 1
        ) 
        }
    }
  )


### DISPLAY BLOCKS

 # popup menu on hover over checklist

  ### DISPLAY CHECKLISTS ON THE MAP ------
  observeEvent(
    current_block_ebd_checklistsonly_filtered(),
    {
      req(current_block_ebd_checklistsonly_filtered())

      # check to make sure records returned!
      checklists <-
        current_block_ebd_checklistsonly_filtered()[
          c("LATITUDE","LONGITUDE", "SAMPLING_EVENT_IDENTIFIER", "LOCALITY_ID",
          "LOCALITY", "OBSERVATION_DATE", "ebird_link")]
      if (length(checklists) > 0){
        leafletProxy("mymap") %>%
        clearMarkerClusters() %>%
        # clearShapes() %>%
        addCircleMarkers(
          data = checklists,
          lat = ~ LATITUDE,
          lng = ~ LONGITUDE,
          radius = 5,
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 10,
            spiderfyDistanceMultiplier = 2),
          color = ncba_blue,
          opacity = 1,
          stroke = TRUE,
          weight = 1.8,
          fillColor = ncba_white,
          fillOpacity = 0.6,
          label = sprintf(
            "<strong>%s</strong><br/>%s<br/>%s",
            checklists$SAMPLING_EVENT_IDENTIFIER,
            checklists$LOCALITY,
            checklists$OBSERVATION_DATE) %>%
          lapply(htmltools::HTML),
          popup = ~ebird_link)
      }
    }
  )



  # SPECIES TAB  ----------------------------------------------------

  # Species info

  output$breeding_code_legend <- renderTable(
    breeding_codes_key,
    striped = TRUE,
    spacing = "xs"
  )

#   output$spp_breedingbox_plot <- renderPlot({

#   # check to make sure species is selected
#   # validate(
#   #   need(input$spp_select, 'select a species from the list')
#   # )
#   ### PLOT BREEDING CODES ------------------------------------------------
#   lump <- list(S = c("S", "S7", "M"), O = c("", "F", "O", "NC"))
#   no_plot_codes <- NULL
#   out_pdf <- NULL
#   spp <- input$spp_select
#   # print(spp)

#   #this pipeline pulls from the ebd_observations collection
#   # slower, but needs less pre-compilation (like spp_summaries)
#   pipeline  <- sprintf(
#     '[{
#       "$match": {"COMMON_NAME": "%s"}
#       },
#       {"$group":
#           {"_id": {
#               "bcode": "$BREEDING_CODE",
#               "odate": "$OBSERVATION_DATE"
#             },
#             "cname": {
#               "$first": "$COMMON_NAME"
#             }
#           }
#       },
#       {
#         "$project": {
#           "_id": "$cname",
#           "COMMON_NAME": "$cname",
#           "OBSERVATION_DATE": "$_id.odate",
#           "BREEDING_CODE": "$_id.bcode"
#         }
#       }
#     ]' ,
#       spp)

#     ebird <- aggregate_spp_data(pipeline)

#   print("aggregate pipeline completed.")
  
#   print("ebd records retrieved, plotting data")
#   breeding_boxplot(
#     spp,
#     ebird,
#     pallet="Paired",
#     out_pdf=NULL,
#     no_plot_codes=no_plot_codes,
#     lump=lump,
#     drop=TRUE)
# })

#############################################################
## OVERVIEW TAB

# BLOCK COMPLETION CRITERIA
breedHrsDiurnalCriteriaMin <- 20
breedPctConfirmedCriteriaMin <- 0.25
breedPctPossibleCriteriaMax <- 0.25
breedCountCodedCriteriaMin <- 55
breedVisitsDiurnalCriteriaMin <- 3
breedVisitsNocturnalCriteriaMin <- 2
winterSppCriteriaMin <- 55
winterHrsDiurnalCriteriaMin <- 5
winterVisitsDiurnalCriteriaMin <- 2
winterVisitsNocturnalCriteriaMin <- 1

insideBlockAdj <- 0.003


### Overview map legend plot
output$overview_legend <- renderPlot(
  {
    criteria <- data.frame(
      type = c("Num Coded", "Pct Confirmed", "Pct Possible",
               "Hours", "Winter Spp"),
      pct = c(1.1, 0.75, 1.2, 1.1, 0.8),
      pct_labels = c("60 Coded", "19% Confirmed", "30% Possible",
                     "22 Hours", "44 Winter Spp")
    )
    criteria$type <- factor(criteria$type, levels = criteria$type)
    # criteria$pct_labels <- factor(criteria$pct_labels, levels = criteria$pct_labels)
    barcolors <- c("#dd0000", "#dd0000","#2a3b4d",  "#dd0000", "#2a3b4d")
    # barorder <- c("confirmed", "winter", "coded",
    #            "possible", "hours")
    ggplot(
      data = criteria,
      aes(
        x = type,
        y = pct,
        fill = as.factor(pct)
        )
    ) +
    geom_bar(
      stat = "identity",
      width = 1
      ) +
    scale_fill_manual(
      values = barcolors
    ) +
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank()
      # axis.text.x = element_text(size = 10)
      ) +
    scale_y_continuous(
      expand = expansion(mult = c(0 , 0))
    ) +
    geom_text(
      # data = criteria,
      # aes(x = factor(type)),
      aes(
        label = pct_labels,
        y = 0.5 * pct
      ),
      angle = 90,
      color = "white"
    ) +
    geom_hline(yintercept = 1, col = ncba_gray)

  }
)


### SETUP LEAFLET MAP, RENDER BASEMAP
num_bars <- 5
# print("loading overview_map")
  output$overview_map <- renderLeaflet({
    #setup block geojson layer
  # print("starting effort map, adding blocks")
    leaflet() %>%
      setView(
        lng = nc_center_lng,
        lat = nc_center_lat,
        zoom = nc_center_zoom
      ) %>%
      ### Base Groups (Satellite Imagery and Blocks )
      addProviderTiles(
        "CartoDB.DarkMatterNoLabels",
        options = providerTileOptions(opacity = 1),
        group = "Dark No Labels"
      ) %>%
      addProviderTiles(
        "OpenStreetMap.Mapnik",
        options = providerTileOptions(opacity = 1),
        group = "Street Map"
      ) %>%
      #add block outlines
      ### Overlay Groups
      # ## Breeding Coded (left Bar)
      addRectangles(
        data = pb_map,
        lng1 = ~ NW_X,
        lat1 = ~ SE_Y + ((NW_Y - SE_Y) * breedingCodedHgtPct),
        lng2 = ~ NW_X - ((NW_X - SE_X) / num_bars),
        lat2 = ~ SE_Y,
        fillColor = ~breedCodedPal(breedCountCoded),
        stroke = FALSE,
        fillOpacity = 1,
        fill = TRUE,
        group = "Breeding Coded"
      ) %>%
      ## Breeding Confirmed (Bar 2)
      addRectangles(
        data = pb_map,
        lng1 = ~ NW_X - ((NW_X - SE_X) / num_bars),
        lat1 = ~ SE_Y + ((NW_Y - SE_Y) * breedConfHgtPct),
        lng2 = ~ NW_X - (((NW_X - SE_X) / num_bars) * 2),
        lat2 = ~ SE_Y,
        fillColor = ~breedConfPal(breedPctConfirmed),
        stroke = FALSE,
        fillOpacity = 1,
        fill = TRUE,
        group = "Breeding Coded"
      ) %>%
      ## Breeding Possible (Bar 3)
      addRectangles(
        data = pb_map,
        lng1 = ~ NW_X - (((NW_X - SE_X) / num_bars) * 2),
        lat1 = ~ SE_Y + ((NW_Y - SE_Y) * breedPossHgtPct),
        lng2 = ~ NW_X - (((NW_X - SE_X) / num_bars) * 3),
        lat2 = ~ SE_Y,
        fillColor = ~breedPossPal(breedPctPossible),
        stroke = FALSE,
        fillOpacity = 1,
        fill = TRUE,
        group = "Breeding Coded"
      ) %>%
      ## Breeding Hours (Bar 4)
      addRectangles(
        data = pb_map,
        lng1 = ~ NW_X - (((NW_X - SE_X) / num_bars) * 3),
        lat1 = ~ SE_Y + ((NW_Y - SE_Y) * breedHrsHgtPct),
        lng2 = ~ NW_X - (((NW_X - SE_X) / num_bars) * 4),
        lat2 = ~ SE_Y,
        fillColor = ~breedHrsPal(breedHrsDiurnal),
        stroke = FALSE,
        fillOpacity = 1,
        fill = TRUE,
        group = "Breeding Coded"
      ) %>%
      ## Wintering Spp (Bar 5)
      addRectangles(
        data = pb_map,
        lng1 = ~ NW_X - (((NW_X - SE_X) / num_bars) * 4),
        lat1 = ~ SE_Y + ((NW_Y - SE_Y) * winterSppHgtPct),
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        fillColor = ~winterSppPal(winterCountDetected),
        stroke = FALSE,
        fillOpacity = 1,
        fill = TRUE,
        group = "Winter Spp"
      ) %>%
      ## Priority Block Boundaries
      addRectangles(
        data = pb_map,
        layerId = ~ ID_NCBA_BLOCK,
        lng1 = ~ NW_X,
        lat1 = ~ NW_Y,
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        stroke = TRUE,
        fillColor = ~ fillComplete,
        fill = TRUE,
        fillOpacity = ~ fillOpac,
        weight = 2.5,
        color = ~ strokeComplete,
        label = ~paste0(
            "<strong>",
            ID_NCBA_BLOCK,
            "<br/>",
            STATUS,
            "</strong>",
            "<br/>Breeding Coded: ",
            breedCountCoded,
            "</strong><br>Breeding Confirmed: ",
            paste(round(100 * breedPctConfirmed, 1), "%", sep = ""),
            "</strong><br>Breeding Possible: ",
            paste(round(100 * breedPctPossible, 1), "%", sep = ""),
            "</strong><br>Breeding Diurnal Hours: ",
            round(breedHrsDiurnal, 1),
            "</strong><br>Breeding Visits: ",
            breedCountDiurnalChecklists,
            "</strong><br>Winter Diurnal Hours: ",
            round(winterHrsDiurnal, 1),
            "</strong><br>Winter Diurnal Visits: ",
            winterCountDiurnalChecklists,
            "</strong><br>Winter Species Detected: ",
            winterCountDetected,
            "</strong><br>Winter Nocturnal Visits: ",
            winterCountNocturnalChecklists
          ) %>%
          lapply(htmltools::HTML),
          group = "Blocks",
          # popup = ~block_popup
      ) %>%
      
      # Layers Control (Making Icons as Overlay Layers)
      addLayersControl(data = pb_map,
        baseGroups = c("Street Map", "Dark No Labels"),
        # overlayGroups = c(
        #   "Breeding Coded",
        #   "Breeding Confirmed",
        #   "Breeding Possible",
        #   "Breeding Diurnal Hrs"
        #   # "Winter Diurnal Hrs"
        #   ),
        options = layersControlOptions(collapsed = FALSE)
      )
      ## Legends for Diurnal Hours, Nocturnal Hours, and Confirmed Species
      # addLegendImage(
      #   images = diurnal_symbols,
      #   labels = c(
      #     "0",
      #     paste(">", breedCountCodedCriteriaMin)
      #     ),
      #   orientation = "vertical",
      #   title = "Breeding Coded Species (top)",
      #   position = "topleft",
      #   width = 16.44,
      #   height = 16.44,
      #   labelStyle = "font-size: 14px; vertical-align: center;"
      # )
    })
  
  ## Add event listener for block click
  # moved above
  # observeEvent(
  #   input$overview_map_shape_click,
  #   {
  #     print("block clicked")
  #     rv_block$id = input$overview_map_shape_click
  #     # click <- input$overview_map_shape_click
  #     # if ( click$id %in% input$APBlock & click$id != input$APBlock){
  #     #   selected = input$APBlock[input$APBlock != click$id]
  #     # } else {
  #     #   selected = c(input$APBlock, click$id)
  #     #   updateSelectInput(
  #     #     session,
  #     #     "APBlock",
  #     #     selected = selected
  #     #   )
  #     # }

  #     #change focus to blocks tab
  #     updateNavbarPage(
  #       session,
  #       "nav",
  #       selected = "Blocks"
  #     )
  #   }
  # )


  ### Merging Block Shapes for mapping and Summary Table
  pb_map <- priority_block_data
  # print(head(pb_map))
  
  ## Convert percentages to numbers instead of decimals
  pb_map <- pb_map %>%
    mutate(PctConfirm = round( breedPctConfirmed * 100, 1)) %>%
  ## Add fill value based on block status
    mutate(
      fillComplete = ifelse(
        STATUS == "Complete", ncba_blue,
        ncba_white
      )
    ) %>%
    mutate(
      strokeComplete = ifelse(
        STATUS == "Complete", ncba_blue,
        ncba_failed
      )
    ) %>%
    mutate(
      fillOpac = ifelse(
        STATUS == "Complete", 0.9,
        0.05
      )
    ) %>%
    mutate(
      fillBool = ifelse(
        STATUS == "Complete", TRUE,
        FALSE
      )
    )

  ### Centroids of Priority Blocks
  centr_x <- (pb_map$NW_X + pb_map$SE_X)/2
  centr_y <- (pb_map$NW_Y + pb_map$SE_Y)/2
  mutate(pb_map, centr_x)
  mutate(pb_map, centr_y)
  
  ### Custom Crow icon is from Font Awesome (https://fontawesome.com/icons)
  
  ### legend components for breeding diurnal hours
  # diurnal_shapes <- c('rect','rect','rect')
  # diurnal_colors <- c(
  #   "#b3b3b3",
  #   "#FDE725FF",
  #   "#3c8d40"
  #   )
  diurnal_shapes <- c('rect','rect')
  diurnal_colors <- c(
    "#b3b3b3",
    ncba_blue
    )
  # diurnal_palette <- palette(diurnal_colors)
  
  ncba_colors <- c(
    ncba_failed,
    ncba_blue
    )
  
  ncba_palette <- palette(ncba_colors)
  
  # # possible colors
  # ncba_colors <- c(
  #   "#dd0000",
  #   ncba_blue
  #   )
  
  # ncba_palette <- palette(ncba_colors)

  diurnal_symbols <- Map(
    f = makeSymbol,
    shape = diurnal_shapes,
    fillColor = diurnal_colors,
    color = diurnal_colors,
    opacity = 1,
    fillOpacity = 0,
    width = 20,
    `stroke-width` = 2
    )

  ### palette for Diurnal Hours
  
  # ncbapal <- colorFactor(
  #   ncba_palette,
  #   levels = c(0, breedCountCodedCriteriaMin)
  #   )

  # breedCodedPal <- colorFactor(
  #   ncba_palette,
  #   levels = c(0, breedCountCodedCriteriaMin),
  #   reverse = TRUE
  # )
  breedCodedPal <- colorBin(
    ncba_palette,
    pb_map$breedCountCoded,
    bins = c(0, (breedCountCodedCriteriaMin - 1), Inf)
  )
  breedingCodedHgtPct <- pmin(
    1.5,
    pb_map$breedCountCoded / breedCountCodedCriteriaMin
  )

  breedConfPal <- colorBin(
    ncba_palette,
    pb_map$breedPctConfirmed,
    bins = c(0, (breedPctConfirmedCriteriaMin), Inf)
  )
  breedConfHgtPct <- pmin(
    1.5,
    pb_map$breedPctConfirmed / breedPctConfirmedCriteriaMin
  )

  breedPossPal <- colorBin(
    ncba_palette,
    pb_map$breedPctPossible,
    bins = c(0, (breedPctPossibleCriteriaMax), Inf),
    reverse = TRUE
  )
  breedPossHgtPct <- pmin(
    1.5,
    pb_map$breedPctPossible / breedPctPossibleCriteriaMax
  )

  breedHrsPal <- colorBin(
    ncba_palette,
    pb_map$breedHrsDiurnal,
    bins = c(0, (breedHrsDiurnalCriteriaMin), Inf)
  )
  breedHrsHgtPct <- pmin(
    1.5,
    pb_map$breedHrsDiurnal / breedHrsDiurnalCriteriaMin
    )

  winterSppPal <- colorBin(
    ncba_palette,
    pb_map$winterCountDetected,
    bins = c(0, (winterSppCriteriaMin), Inf)
  )
  winterSppHgtPct <- pmin(
    1.5,
    pb_map$winterCountDetected / winterSppCriteriaMin
    )
  
  # breedConfPal <- colorBin(
  #   diurnal_palette,
  #   pb_map$breedPctConfirmed,
  #   bins = c(0, (breedPctConfirmedCriteriaMin - 0.1), Inf),
  #   reverse = TRUE
  #   )
  # breedingposspal <- colorBin(
  #   diurnal_palette,
  #   pb_map$breedPctPossible,
  #   bins = c(0, (breedPctPossibleCriteriaMax - 0.1), Inf),
  #   reverse = TRUE
  #   )
  # breedingpal <- colorBin(
  #   diurnal_palette,
  #   pb_map$breedHrsDiurnal,
  #   bins = c(0, (breedHrsDiurnalCriteriaMin - 0.1), Inf),
  #   reverse = FALSE
  #   )
  # winterpal <- colorBin(
  #   diurnal_palette,
  #   pb_map$winterHrsDiurnal,
  #   bins = c(0, (winterHrsDiurnalCriteriaMin - 0.1), Inf),
  #   reverse = TRUE
  #   )

    ## move focus to block tab when clicked


  # breedingpalnum <- colorNumeric(
  #   diurnal_palette,
  #   pb_map$breedHrsDiurnal,
  #   reverse = TRUE
  #   )

  
  # winterbinpal <- colorBin(
  #   diurnal_palette,
  #   pb_map$winterHrsDiurnal,
  #   bins = c(0.1,2.5,5,7.5,10,Inf),
  #   reverse = TRUE
  #   )
  
  # ### palette for Nocturnal Hours
  # binpalnight <- colorBin(
  #   diurnal_palette,
  #   pb_map$breedHrsNocturnal,
  #   bins = c(0.1,0.5,1,1.5,2,Inf),
  #   reverse = TRUE
  #   )
  
  # ### legend for winter diurnal hours
  # winter_shapes <- c('circle','circle','circle','circle','circle','circle')
  # winter_diurnal_symbols <- Map(
  #   f = makeSymbol,
  #   shape = winter_shapes,
  #   fillColor = diurnal_colors,
  #   color = diurnal_colors,
  #   opacity = 1,
  #   fillOpacity = 1,
  #   width = 20,
  #   `stroke-width` = 2
  #   )
  
  # ### legend for breeding nocturnal symbols
  # winter_shapes <- c('circle','circle','circle','circle','circle','circle')
  # breeding_nocturnal_symbols <- Map(
  #   f = makeSymbol,
  #   shape = winter_shapes,
  #   fillColor = diurnal_colors,
  #   color = diurnal_colors,
  #   opacity = 1,
  #   fillOpacity = 0,
  #   width = 20,
  #   `stroke-width` = 2
  #   )
  
 

  ### Change to Block Tab when Clicking a Block in Effort Map
  ### 

}  
#
# ## SUMMARIZE START TIMES --------------------------------------------------
# plot(start_time_boxplot(ebird))

# ## PLOT COORDINATES OF RECORDS --------------------------------------------
# coords.plot <- plot_checklists_coords(ebird)
# plot(coords.plot)
#
# ## SUMMARIZE TRAVEL DISTANCE ----------------------------------------------
# plot(effort_distance_boxplot(ebird))
#
# ## SUMMARIZE MINUTES EFFORT -----------------------------------------------
# plot(duration_minutes_boxplot(ebird))
#
# ## LOCALITY TYPE BREAKDOWN ------------------------------------------------
# plot(locality_type_pie(ebird))

shinyApp(ui, server)
