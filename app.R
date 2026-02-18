library(shiny)
library(bslib)
library(dplyr)
library(tibble)
library(magick)
library(png)

# ---- read_fixrep  -----------------------------------------------------

read_fixrep <- function(path) {
  stopifnot(length(path) == 1, is.character(path), file.exists(path))

  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("readxl", quietly = TRUE)

  ext <- tolower(tools::file_ext(path))

  read_as_text <- function(path) {
    first_line <- readLines(path, n = 1, warn = FALSE)
    n_tabs   <- stringr::str_count(first_line, "\t")
    n_commas <- stringr::str_count(first_line, ",")

    delim <- if (n_tabs >= n_commas) "\t" else ","

    raw <- readr::read_delim(
      file = path,
      delim = delim,
      col_types = readr::cols(.default = readr::col_character()),
      trim_ws = TRUE,
      progress = FALSE,
      show_col_types = FALSE
    )

    list(data = raw, mode = sprintf("delimited text (%s)", if (delim == "\t") "tab" else "comma"))
  }

  read_as_excel <- function(path) {
    raw <- readxl::read_excel(
      path = path,
      sheet = 1,
      col_types = "text",
      .name_repair = "unique"
    ) |>
      tibble::as_tibble()

    list(data = raw, mode = "Excel workbook")
  }

  res <- NULL

  if (ext %in% c("xls", "xlsx")) {
    res <- tryCatch(
      read_as_excel(path),
      error = function(e) {
        tmp <- read_as_text(path)
        tmp$mode <- paste0(tmp$mode, " (fallback from .", ext, ")")
        tmp
      }
    )
  } else {
    res <- read_as_text(path)
  }

  raw <- res$data

  nm <- names(raw)
  nm <- stringr::str_replace_all(nm, "\uFEFF", "")
  nm <- stringr::str_trim(nm)
  names(raw) <- nm

  if ("location_b1" %in% names(raw)) {
    loc <- raw$location_b1 |>
      as.character() |>
      stringr::str_remove_all("[()]") |>
      stringr::str_split_fixed(",", 2)

    raw$image_location_x <- suppressWarnings(as.numeric(stringr::str_trim(loc[, 1])))
    raw$image_location_y <- suppressWarnings(as.numeric(stringr::str_trim(loc[, 2])))
  }

  out <- raw

  if ("RECORDING_SESSION_LABEL" %in% names(out)) {
    out <- dplyr::mutate(out, RECORDING_SESSION_LABEL = suppressWarnings(as.integer(.data$RECORDING_SESSION_LABEL)))
  }
  if ("TRIAL_INDEX" %in% names(out)) {
    out <- dplyr::mutate(out, TRIAL_INDEX = suppressWarnings(as.integer(.data$TRIAL_INDEX)))
  }
  if ("CURRENT_FIX_INDEX" %in% names(out)) {
    out <- dplyr::mutate(out, CURRENT_FIX_INDEX = suppressWarnings(as.integer(.data$CURRENT_FIX_INDEX)))
  }
  if ("CURRENT_FIX_X" %in% names(out)) {
    out <- dplyr::mutate(out, CURRENT_FIX_X = suppressWarnings(as.numeric(.data$CURRENT_FIX_X)))
  }
  if ("CURRENT_FIX_Y" %in% names(out)) {
    out <- dplyr::mutate(out, CURRENT_FIX_Y = suppressWarnings(as.numeric(.data$CURRENT_FIX_Y)))
  }
  if ("CURRENT_FIX_DURATION" %in% names(out)) {
    out <- dplyr::mutate(out, CURRENT_FIX_DURATION = suppressWarnings(as.numeric(.data$CURRENT_FIX_DURATION)))
  }

  if (all(c("CURRENT_FIX_X", "CURRENT_FIX_Y") %in% names(out))) {
    out <- dplyr::mutate(out, FIX_X = .data$CURRENT_FIX_X, FIX_Y = .data$CURRENT_FIX_Y)
  }
  if ("CURRENT_FIX_DURATION" %in% names(out)) {
    out <- dplyr::mutate(out, FIX_DUR = .data$CURRENT_FIX_DURATION)
  }

  out <- tibble::as_tibble(out)
  attr(out, "read_mode") <- res$mode
  out
}

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

# ---- Plot native image ----------------------------------------------------------

plot_native_image_fit <- function(img_obj, panel_w_px, panel_h_px) {
  w_img <- img_obj$width
  h_img <- img_obj$height

  if (is.null(panel_w_px) || is.null(panel_h_px) || panel_w_px <= 0 || panel_h_px <= 0) {
    panel_w_px <- w_img
    panel_h_px <- h_img
  }

  ar_img <- w_img / h_img
  ar_panel <- panel_w_px / panel_h_px

  if (ar_panel > ar_img) {
    w_view <- h_img * ar_panel
    xpad <- (w_view - w_img) / 2
    xlim <- c(-xpad, w_img + xpad)
    ylim <- c(h_img, 0)
  } else {
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
              helpText("LHS: click to add AOI centre (uses the optional name above). Double-click to delete the nearest AOI centre."),
              tags$hr(),
              tags$h5("Debug: AOIs for current face"),
              tableOutput("aoi_debug_tbl")
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

  # ---- AOI centre state ------------------------------------------------
  # Store ALL AOIs across faces; we'll derive the current-face tibble from this
  aois <- reactiveValues(
    centres = tibble::tibble(
      face_key = character(),
      aoi_id = integer(),
      aoi_name = character(),
      x = numeric(),
      y = numeric()
    )
  )
  next_aoi_id <- reactiveVal(1)

  # ---- Fixation report -------------------------------------------------

  fixrep_raw <- reactive({
    req(input$upload_fixrep)
    read_fixrep(input$upload_fixrep$datapath)
  })

  output$fixrep_load_msg <- renderUI({
    req(fixrep_raw())
    mode <- attr(fixrep_raw(), "read_mode")

    tags$div(
      class = "alert alert-info",
      paste0("Loaded as: ", mode)
    )
  })

  output$fixrep_mapping_ui <- renderUI({
    req(fixrep_raw())
    cols <- names(fixrep_raw())

    tagList(
      selectInput("map_participant", "PARTICIPANT", choices = cols,
                  selected = if ("RECORDING_SESSION_LABEL" %in% cols) "RECORDING_SESSION_LABEL" else cols[1]),
      selectInput(
        "map_face", "FACE", choices = cols,
        selected = dplyr::case_when(
          "which_face" %in% cols ~ "which_face",
          "file_b1"    %in% cols ~ "file_b1",
          TRUE ~ cols[1]
        )
      ),
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

  output$fixations_showhide_ui <- renderUI({
    checkboxInput(
      inputId = "show_fixations",
      label = "Show fixations",
      value = TRUE
    )
  })

  # ---- Face handling ---------------------------------------------------

  face_ready <- reactive({
    !is.null(input$upload_face) && nzchar(input$upload_face$datapath)
  })

  face_img <- reactive({
    req(face_ready())
    read_image_native(input$upload_face$datapath)
  })

  current_face_key <- reactive({
    req(input$upload_face)
    nm <- input$upload_face$name
    nm <- basename(nm)
    tolower(trimws(nm))
  })

  # ---- Fixations for this face ----------------------------------------

  fixrep_this_face <- reactive({
    req(fixrep(), current_face_key())

    fx <- fixrep()
    face_col <- tolower(trimws(basename(as.character(fx$FACE))))
    fx[face_col == current_face_key(), , drop = FALSE]
  })

  fixrep_this_face_mapped <- reactive({
    req(fixrep_this_face(), face_img())

    fx <- fixrep_this_face()
    w <- face_img()$width
    h <- face_img()$height

    img_left <- fx$IMG_X - (w / 2)
    img_top  <- fx$IMG_Y - (h / 2)

    fx$FIX_X_IMG <- fx$FIX_X - img_left
    fx$FIX_Y_IMG <- fx$FIX_Y - img_top

    fx
  })

  # ---- AOI helpers -----------------------------------------------------

  click_on_image <- function(click, img) {
    if (is.null(click) || is.null(img)) return(FALSE)
    x <- click$x
    y <- click$y
    is.finite(x) && is.finite(y) &&
      x >= 0 && x <= img$width &&
      y >= 0 && y <= img$height
  }

  # Current-face AOI tibble (this is the record you asked for)
  aoi_current_face_tbl <- reactive({
    req(current_face_key())
    aois$centres |>
      dplyr::filter(.data$face_key == current_face_key()) |>
      dplyr::select(aoi_id, aoi_name, x, y) |>
      tibble::as_tibble()
  })

  output$aoi_debug_tbl <- renderTable({
    req(current_face_key())
    aoi_current_face_tbl()
  })

  # ---- LHS plot --------------------------------------------------------

  output$face_for_edit <- renderPlot({
    req(face_img())

    plot_native_image_fit(
      face_img(),
      panel_w_px = session$clientData$output_face_for_edit_width,
      panel_h_px = session$clientData$output_face_for_edit_height
    )

    pts <- aoi_current_face_tbl()
    if (nrow(pts) > 0) {
      points(pts$x, pts$y, pch = 4, cex = 2, lwd = 3, col = "cyan")
      lbl <- ifelse(is.na(pts$aoi_name) | pts$aoi_name == "", as.character(pts$aoi_id), pts$aoi_name)
      text(pts$x, pts$y, labels = lbl, pos = 3, cex = 0.9, col = "cyan")
    }
  })

  # ---- RHS plot (unchanged logic) -------------------------------------

  output$face_for_markup <- renderPlot({
    req(face_img())

    plot_native_image_fit(
      face_img(),
      panel_w_px = session$clientData$output_face_for_markup_width,
      panel_h_px = session$clientData$output_face_for_markup_height
    )

    if (isTRUE(input$show_fixations)) {

      req(fixrep_this_face_mapped())

      w <- face_img()$width
      h <- face_img()$height

      fx <- fixrep_this_face_mapped()$FIX_X_IMG
      fy <- fixrep_this_face_mapped()$FIX_Y_IMG

      ok <- is.finite(fx) & is.finite(fy) & fx >= 0 & fx <= w & fy >= 0 & fy <= h
      if (any(ok)) {
        points(fx[ok], fy[ok], pch = 21, cex = 2, bg = "yellow", col = "red", lwd = 4)
      }
    }
  })

  # ---- LHS click: add centre (with optional name) ----------------------

  observeEvent(input$face_for_edit_click, {
    req(face_img(), current_face_key())

    if (!click_on_image(input$face_for_edit_click, face_img())) {
      showNotification("Click on the image (not the padding).", type = "message", duration = 1.5)
      return()
    }

    x <- input$face_for_edit_click$x
    y <- input$face_for_edit_click$y

    nm <- input$aoi_name_next
    nm <- if (is.null(nm)) "" else trimws(as.character(nm))
    if (!nzchar(nm)) nm <- NA_character_

    id <- next_aoi_id()
    next_aoi_id(id + 1)

    aois$centres <- dplyr::bind_rows(
      aois$centres,
      tibble::tibble(face_key = current_face_key(), aoi_id = id, aoi_name = nm, x = x, y = y)
    )

    updateTextInput(session, "aoi_name_next", value = "")
  })

  # ---- LHS double-click: remove nearest centre (no threshold) ----------

  observeEvent(input$face_for_edit_dblclick, {
    req(face_img(), current_face_key())

    if (!click_on_image(input$face_for_edit_dblclick, face_img())) {
      showNotification("Double-click on the image (not the padding).", type = "message", duration = 1.5)
      return()
    }

    pts <- aoi_current_face_tbl()
    if (nrow(pts) == 0) return()

    x <- input$face_for_edit_dblclick$x
    y <- input$face_for_edit_dblclick$y

    d2 <- (pts$x - x)^2 + (pts$y - y)^2
    i <- which.min(d2)

    delete_id <- pts$aoi_id[i]

    aois$centres <- aois$centres |>
      dplyr::filter(!(face_key == current_face_key() & aoi_id == delete_id))
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
