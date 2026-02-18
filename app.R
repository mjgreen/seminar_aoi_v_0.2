library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(magick)
library(png)

# ---- Image helpers ------------------------------------------------------

read_image_native <- function(path) {
  img <- magick::image_read(path) |> magick::image_orient()
  if (length(img) > 1) img <- img[1]

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

  magick::image_write(img, path = tmp, format = "png")

  rast <- png::readPNG(tmp, native = TRUE)
  list(
    rast = rast,
    width = ncol(rast),
    height = nrow(rast)
  )
}

plot_native_image_fit <- function(img_obj, panel_w_px, panel_h_px) {
  w_img <- img_obj$width
  h_img <- img_obj$height

  # Fallback if Shiny hasn't reported panel size yet
  if (is.null(panel_w_px) || is.null(panel_h_px) || panel_w_px <= 0 || panel_h_px <= 0) {
    panel_w_px <- w_img
    panel_h_px <- h_img
  }

  ar_img <- w_img / h_img
  ar_panel <- panel_w_px / panel_h_px

  # Compute plot limits in IMAGE UNITS so the image "contains" inside the panel
  if (ar_panel > ar_img) {
    # Panel is wider than image -> pad X
    w_view <- h_img * ar_panel
    xpad <- (w_view - w_img) / 2
    xlim <- c(-xpad, w_img + xpad)
    ylim <- c(h_img, 0)
  } else {
    # Panel is taller than image -> pad Y
    h_view <- w_img / ar_panel
    ypad <- (h_view - h_img) / 2
    xlim <- c(0, w_img)
    ylim <- c(h_img + ypad, -ypad)
  }

  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op), add = TRUE)

  plot(
    x = 0, y = 0, type = "n",
    xlim = xlim, ylim = ylim,
    xaxs = "i", yaxs = "i",
    axes = FALSE, xlab = NA, ylab = NA,
    asp = 1
  )

  rasterImage(img_obj$rast, xleft = 0, ybottom = h_img, xright = w_img, ytop = 0)
}

ui <- page_fillable(
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
              uiOutput("fixrep_mapping_ui"),
              tableOutput("fixrep_preview")
            ),
            nav_panel(
              "Face images",
              fileInput(
                "upload_face",
                "Upload face image",
                accept = c(".png", ".jpg", ".jpeg", ".bmp", ".tif", ".tiff", ".gif", ".webp")
              )
            )
          ),
          tags$hr(),
          actionButton("exit_app", "Exit app", class = "btn-danger")
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
                brush = brushOpts(id = "face_for_edit_brush", resetOnNew = TRUE),
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

      nav_panel("Fixations annotated", card_body(DT::DTOutput("fixations_tbl"))),
      nav_panel("Summary", card_body(tableOutput("fixations_summary")))
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
      selectInput("map_participant", "PARTICIPANT", choices = cols,
                  selected = if ("RECORDING_SESSION_LABEL" %in% cols) "RECORDING_SESSION_LABEL" else cols[1]),
      selectInput("map_face", "FACE", choices = cols,
                  selected = if ("which_face" %in% cols) "which_face" else cols[1]),
      selectInput("map_trial", "TRIAL", choices = cols,
                  selected = if ("TRIAL_INDEX" %in% cols) "TRIAL_INDEX" else cols[1]),
      selectInput("map_condition", "CONDITION", choices = cols,
                  selected = if ("condition" %in% cols) "condition" else cols[1]),
      selectInput("map_fix_x", "FIXATION X", choices = cols,
                  selected = if ("CURRENT_FIX_X" %in% cols) "CURRENT_FIX_X" else cols[1]),
      selectInput("map_fix_y", "FIXATION Y", choices = cols,
                  selected = if ("CURRENT_FIX_Y" %in% cols) "CURRENT_FIX_Y" else cols[1]),
      selectInput("map_fix_dur", "FIXATION DURATION", choices = cols,
                  selected = if ("CURRENT_FIX_DURATION" %in% cols) "CURRENT_FIX_DURATION" else cols[1]),
      selectInput("map_img_x", "IMAGE X", choices = cols,
                  selected = if ("image_location_x" %in% cols) "image_location_x" else cols[1]),
      selectInput("map_img_y", "IMAGE Y", choices = cols,
                  selected = if ("image_location_y" %in% cols) "image_location_y" else cols[1])
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
      SUBJECT = raw[[input$map_participant]],
      FACE = as.character(raw[[input$map_face]]),
      TRIAL_ID = raw[[input$map_trial]],
      CONDITION = raw[[input$map_condition]],
      IMG_X = suppressWarnings(as.numeric(raw[[input$map_img_x]])),
      IMG_Y = suppressWarnings(as.numeric(raw[[input$map_img_y]])),
      FIX_X = suppressWarnings(as.numeric(raw[[input$map_fix_x]])),
      FIX_Y = suppressWarnings(as.numeric(raw[[input$map_fix_y]])),
      FIX_DUR = suppressWarnings(as.numeric(raw[[input$map_fix_dur]])),
      AOI = "Not assigned"
    )
  })

  output$fixrep_preview <- renderTable({
    head(fixrep(), 5)
  })

  # ---- Face image -------------------------------------------------------

  face_ready <- reactive({
    !is.null(input$upload_face) && nzchar(input$upload_face$datapath)
  })

  face_img <- reactive({
    req(face_ready())
    read_image_native(input$upload_face$datapath)
  })

  output$face_for_edit <- renderPlot({
    req(face_img())
    plot_native_image_fit(
      face_img(),
      panel_w_px = session$clientData$output_face_for_edit_width,
      panel_h_px = session$clientData$output_face_for_edit_height
    )
  })

  output$face_for_markup <- renderPlot({
    req(face_img())
    plot_native_image_fit(
      face_img(),
      panel_w_px = session$clientData$output_face_for_markup_width,
      panel_h_px = session$clientData$output_face_for_markup_height
    )
  })

  # ---- Click gating -----------------------------------------------------

  click_on_image <- function(click, img) {
    if (is.null(click) || is.null(img)) return(FALSE)
    x <- click$x
    y <- click$y
    is.finite(x) && is.finite(y) &&
      x >= 0 && x <= img$width &&
      y >= 0 && y <= img$height
  }

  observeEvent(input$face_for_edit_click, {
    req(face_img())

    if (!click_on_image(input$face_for_edit_click, face_img())) {
      showNotification("Click on the image (not the padding).", type = "message", duration = 1.5)
      return()
    }

    x <- input$face_for_edit_click$x
    y <- input$face_for_edit_click$y

    # Your AOI-click logic goes here
  })

  observeEvent(input$face_for_edit_dblclick, {
    req(face_img())

    if (!click_on_image(input$face_for_edit_dblclick, face_img())) {
      showNotification("Double-click on the image (not the padding).", type = "message", duration = 1.5)
      return()
    }

    x <- input$face_for_edit_dblclick$x
    y <- input$face_for_edit_dblclick$y

    # Your AOI-doubleclick logic goes here
  })

  # ---- Exit app ---------------------------------------------------------

  observeEvent(input$exit_app, {
    if (interactive()) {
      shiny::stopApp()
    } else {
      session$close()
    }
  })
}

shinyApp(ui, server)
