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
          fileInput(
            "upload_fixrep",
            "Upload fixation report (CSV/TSV/XLS/XLSX)",
            accept = c(
              ".csv", ".tsv", ".txt", ".xls", ".xlsx",
              "text/csv", "text/tab-separated-values",
              "application/vnd.ms-excel",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            )
          ),
          # fileInput("upload_fixrep", "Upload fixation report (CSV/TSV)",
          #           accept = c(".csv", ".tsv", "text/csv", "text/tab-separated-values")),
          textInput("participant_id", "Participant ID column", value = "subject_id"),
          textInput("trial_id", "Trial ID column", value = "trial_id"),

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
          #tableOutput("fixations_tbl")
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

  # Wrapper for reading fixation reports with error handling and user feedback
  fixrep_raw <- reactive({
    req(input$upload_fixrep, nzchar(input$upload_fixrep$datapath))

    tryCatch(
      read_fixrep(input$upload_fixrep$datapath),
      error = function(e) {
        validate(need(FALSE, paste(
          "Could not read fixation report.",
          "If your file is a real Excel workbook, ensure it isn't password-protected.",
          "If it's a text export renamed to .xls, that’s fine — but it must be tab-delimited and include the header row.",
          "Reader error:", conditionMessage(e)
        )))
      }
    )
  })

  # Read fixation report uploads (most formats including xls)
  observeEvent(fixrep_raw(), {
    g$fixrep_raw <- fixrep_raw()
  })

  # ---- Exit app handler ----
  observeEvent(input$exit_app, {
    stopApp()
  })

  # placeholders
  output$face_for_edit <- renderPlot(plot(1, 1, main = "LHS plot"))
  output$face_for_markup <- renderPlot(plot(1, 1, main = "RHS plot"))
  output$fixations_tbl <- DT::renderDT({
    req(fixrep_raw())
    fixrep_raw()
  })
  output$fixations_summary <- renderTable(
    data.frame(n = nrow(mtcars), mean_mpg = mean(mtcars$mpg))
  )
}

shinyApp(ui, server)
