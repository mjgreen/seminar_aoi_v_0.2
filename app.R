library(shiny)
library(bslib)
library(dplyr)
library(tibble)

ui <- page_fillable(
  layout_columns(
    col_widths = c(4, 8),

    # ------------------------------------------------------------------
    # LEFT COLUMN: tabs for instructions + inputs
    # ------------------------------------------------------------------
    navset_card_pill(
      nav_panel(
        "Instructions",
        card_body(
          tags$div(
            HTML("<p>Put your instructions here.</p>")
          )
        )
      ),

      nav_panel(
        "Inputs",
        card_body(
          # --- Inputs sub-tabs ------------------------------------------------
          navset_pill(
            nav_panel(
              "Fixation report",
              fileInput(
                "upload_fixrep",
                "Upload fixation report",
                accept = c(".csv", ".tsv", ".txt", ".xls")
              ),
              uiOutput("fixrep_mapping_ui"),
              tableOutput("fixrep_preview")
            ),
            nav_panel(
              "Face images",
              fileInput(
                "upload_face",
                "Upload face image",
                accept = c(".png", ".jpg", ".jpeg", ".bmp", ".tif", ".tiff", ".gif", ".webp")
              ),
              tags$div(
                class = "text-muted",
                "Upload a face image here. (Wiring into the Workbench comes next.)"
              )
            )
          ),

          tags$hr(),

          # ---- Exit button ----
          actionButton(
            "exit_app",
            "Exit app",
            class = "btn-danger"
          )
        )
      )
    ),

    # ------------------------------------------------------------------
    # RIGHT COLUMN: tabs for workbench + tables + summaries
    # ------------------------------------------------------------------
    navset_card_pill(
      nav_panel(
        "Workbench",
        card_body(
          layout_columns(
            col_widths = c(6, 6),

            card(
              card_header("Define AOIs (LHS)"),
              plotOutput(
                "face_for_edit",
                click    = "face_for_edit_click",
                dblclick = "face_for_edit_dblclick",
                brush    = brushOpts(id = "face_for_edit_brush", resetOnNew = TRUE)
              )
            ),

            card(
              card_header("Tessellation overview (RHS)"),
              plotOutput("face_for_markup")
            )
          )
        )
      ),

      nav_panel(
        "Fixations annotated",
        card_body(
          DT::DTOutput("fixations_tbl")
        )
      ),

      nav_panel(
        "Summary",
        card_body(
          tableOutput("fixations_summary")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  fixrep_raw <- reactive({
    req(input$upload_fixrep)
    read_fixrep(input$upload_fixrep$datapath)
  })

  output$fixrep_mapping_ui <- renderUI({
    req(fixrep_raw())
    cols <- names(fixrep_raw())

    tagList(
      selectInput(
        "map_participant", "PARTICIPANT",
        choices = cols,
        selected = if ("RECORDING_SESSION_LABEL" %in% cols) "RECORDING_SESSION_LABEL" else cols[1]
      ),
      selectInput(
        "map_face", "FACE",
        choices = cols,
        selected = if ("which_face" %in% cols) "which_face" else cols[1]
      ),
      selectInput(
        "map_trial", "TRIAL",
        choices = cols,
        selected = if ("TRIAL_INDEX" %in% cols) "TRIAL_INDEX" else cols[1]
      ),
      selectInput(
        "map_condition", "CONDITION",
        choices = cols,
        selected = if ("condition" %in% cols) "condition" else cols[1]
      ),
      selectInput(
        "map_fix_x", "FIXATION X",
        choices = cols,
        selected = if ("CURRENT_FIX_X" %in% cols) "CURRENT_FIX_X" else cols[1]
      ),
      selectInput(
        "map_fix_y", "FIXATION Y",
        choices = cols,
        selected = if ("CURRENT_FIX_Y" %in% cols) "CURRENT_FIX_Y" else cols[1]
      ),
      selectInput(
        "map_fix_dur", "FIXATION DURATION",
        choices = cols,
        selected = if ("CURRENT_FIX_DURATION" %in% cols) "CURRENT_FIX_DURATION" else cols[1]
      ),
      selectInput(
        "map_img_x", "IMAGE X",
        choices = cols,
        selected = if ("image_location_x" %in% cols) "image_location_x" else cols[1]
      ),
      selectInput(
        "map_img_y", "IMAGE Y",
        choices = cols,
        selected = if ("image_location_y" %in% cols) "image_location_y" else cols[1]
      )
    )
  })

  fixrep <- reactive({
    req(
      fixrep_raw(),
      input$map_participant, input$map_face, input$map_trial, input$map_condition,
      input$map_fix_x, input$map_fix_y, input$map_fix_dur,
      input$map_img_x, input$map_img_y
    )

    raw <- fixrep_raw()

    tibble(
      SUBJECT   = raw[[input$map_participant]],
      FACE      = as.character(raw[[input$map_face]]),
      TRIAL_ID  = raw[[input$map_trial]],
      CONDITION = raw[[input$map_condition]],
      IMG_X     = suppressWarnings(as.numeric(raw[[input$map_img_x]])),
      IMG_Y     = suppressWarnings(as.numeric(raw[[input$map_img_y]])),
      FIX_X     = suppressWarnings(as.numeric(raw[[input$map_fix_x]])),
      FIX_Y     = suppressWarnings(as.numeric(raw[[input$map_fix_y]])),
      FIX_DUR   = suppressWarnings(as.numeric(raw[[input$map_fix_dur]])),
      AOI       = "Not assigned"
    )
  })

  output$fixrep_preview <- renderTable({
    head(fixrep(), 5)
  })

  observeEvent(input$exit_app, {
    if (interactive()) {
      shiny::stopApp()
    } else {
      session$close()
    }
  })
}

shinyApp(ui, server)
