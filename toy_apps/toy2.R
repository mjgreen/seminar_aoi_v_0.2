library(shiny)
library(bslib)

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
          # ---- uploads / textinputs / selectors go here ----
          fileInput("upload_face", "Upload face image",
                    accept = c("image/png", "image/jpeg", "image/jpg", "image/tiff", "image/bmp")),
          fileInput("upload_fixrep", "Upload fixation report (CSV/TSV)",
                    accept = c(".csv", ".tsv", "text/csv", "text/tab-separated-values")),
          textInput("participant_id", "Participant ID column", value = "subject_id"),
          textInput("trial_id", "Trial ID column", value = "trial_id")
          # add more inputs as needed...
        )
      )

      # You can add more left tabs if you want (e.g., "Settings", "AOIs", etc.)
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

            # LHS plot (AOI definition)
            card(
              card_header("Define AOIs (LHS)"),
              plotOutput(
                "face_for_edit",
                click    = "face_for_edit_click",
                dblclick = "face_for_edit_dblclick",
                brush    = brushOpts(id = "face_for_edit_brush", resetOnNew = TRUE)
              )
            ),

            # RHS plot (full-extent tessellation overview)
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
          # swap to DT::DTOutput("fixations_tbl") if you prefer DT
          tableOutput("fixations_tbl")
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
  # placeholders so the UI runs
  output$face_for_edit <- renderPlot(plot(1, 1, main = "LHS plot"))
  output$face_for_markup <- renderPlot(plot(1, 1, main = "RHS plot"))
  output$fixations_tbl <- renderTable(head(mtcars, 12))
  output$fixations_summary <- renderTable(
    data.frame(n = nrow(mtcars), mean_mpg = mean(mtcars$mpg))
  )
}

shinyApp(ui, server)
