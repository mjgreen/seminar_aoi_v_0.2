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
              radioButtons(
                inputId = "lhs_mode",
                label = "LHS mode",
                choices = c("Navigate (brush-zoom)" = "nav", "Edit AOIs (click/dblclick)" = "edit"),
                selected = "nav",
                inline = FALSE
              ),
              helpText("Navigate mode: use brush to zoom (and double-click plot to reset). Edit mode: click to add an AOI point; double-click to delete nearest point.")
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

  # ---- AOI state (point AOIs for now) ---------------------------------
  aois <- reactiveValues(points = tibble::tibble(face_key = character(), aoi_id = integer(), x = numeric(), y = numeric()))

  next_aoi_id <- reactiveVal(1)

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
          "file_b1"    %in%_
