# ui.R ------------------------------------------------------------------

ui <- page_fillable(
  tags$head(
    tags$style(HTML("
      /* Smaller text for base Shiny tables + DT tables */
      .table, table, .shiny-table, .shiny-table table { font-size: 85%; }
      .dataTables_wrapper { font-size: 85%; }
    "))
  ),
  layout_columns(
    col_widths = c(4, 8),

    navset_card_pill(
      nav_panel(
        "Instructions",
        card_body(tags$div(HTML("<p>Put your instructions here.</p>")))
      ),

      nav_panel(
        "Inputs",
        card_body(
          navset_pill(
            nav_panel(
              "Fixation report",
              fileInput("upload_fixrep", "Upload fixation report", accept = c(".csv", ".tsv", ".txt", ".xls")),
              uiOutput("fixrep_load_msg"),
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
              uiOutput("fixations_showhide_ui"),
              tags$hr(),
              textInput(
                inputId = "aoi_name_next",
                label = "AOI name (optional, applies to the next click)",
                placeholder = "e.g., left_eye"
              ),
              fluidRow(
                column(6, actionButton("finalize_aois", "Finalize AOIs for this face", class = "btn-primary")),
                column(6, uiOutput("finalize_status_ui"))
              ),
              fluidRow(
                column(6, actionButton("commit_next_face", "Next face (commit this face)", class = "btn-success")),
                column(6, uiOutput("commit_status_ui"))
              ),
              fluidRow(
                column(6, actionButton("reset_session", "Reset session annotations", class = "btn-outline-danger")),
                column(6, uiOutput("reset_status_ui"))
              ),
              helpText("LHS: click to add AOI centre. Double-click to delete the nearest AOI centre."),
              tags$hr(),
              tags$h5("Debug: AOIs for current face (deldir tibble)"),
              tableOutput("aoi_debug_tbl")
            )
          )
        )
      )
    ),

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
                click = "face_for_edit_click",
                dblclick = "face_for_edit_dblclick",
                height = "70vh"
              )
            ),

            card(
              card_header("Tessellation overview (RHS)"),
              plotOutput("face_for_markup", height = "70vh")
            )
          )
        )
      ),

      nav_panel(
        "Fixations annotated (current face)",
        card_body(
          tableOutput("fix_assign_debug_tbl")
        )
      ),

      nav_panel(
        "Fixations annotated (all faces so far)",
        card_body(
          DT::DTOutput("fixations_tbl")
        )
      ),

      nav_panel("Summary", card_body(tableOutput("fixations_summary"))),

      nav_panel("Grand means", card_body(tableOutput("grand_means_tbl"))),

      nav_panel(
        "Downloads",
        card_body(
          downloadButton("download_fix_assign_current", "Download assigned fixations (current face)"),
          tags$hr(),
          downloadButton("download_fixations_session", "Download annotated fixations (session)"),
          tags$hr(),
          downloadButton("download_summary_session", "Download summary (session)"),
          tags$hr(),
          downloadButton("download_grand_means_session", "Download grand means (session)")
        )
      )
    )
  )
)
