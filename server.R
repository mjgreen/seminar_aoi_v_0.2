# server.R --------------------------------------------------------------

server <- function(input, output, session) {

  # ---- AOI centre state ------------------------------------------------
  aois <- reactiveValues(
    centres = tibble::tibble(
      subject = character(),
      face_key = character(),
      aoi_id = character(),
      aoi_name = character(),
      deldir_id = character(),
      x = numeric(),
      y = numeric()
    )
  )

  aoi_seq <- reactiveValues(n = tibble::tibble(subject = character(), face_key = character(), next_n = integer()))

  # ---- deldir storage (keyed by subject|face_key) -----------------------
  dd <- reactiveValues(
    store = list() # each element: list(input_pts = tibble, res = deldir_object)
  )

  # ---- assignment storage (keyed by subject|face_key) -------------------
  assign <- reactiveValues(
    store = list(),     # each element: assigned fixation tibble
    finalized = list()  # key -> TRUE/FALSE (for status)
  )

  # ---- session-level annotation store (committed faces) -----------------
  session_ann <- reactiveValues(
    all = tibble::tibble(),   # committed assigned fixations across faces
    committed = list()        # key -> TRUE/FALSE
  )

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
          "file_b1" %in% cols ~ "file_b1",
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
      SUBJECT = as.character(raw[[input$map_participant]]),
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
    format_table_int(head(fixrep(), 5))
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

  current_subject <- reactive({
    req(fixrep(), current_face_key())
    fx <- fixrep()
    face_col <- tolower(trimws(basename(as.character(fx$FACE))))
    subj <- fx$SUBJECT[face_col == current_face_key()]
    subj <- subj[!is.na(subj)]
    if (length(subj) == 0) return(NA_character_)
    as.character(subj[1])
  })

  current_dd_key <- reactive({
    req(current_face_key())
    subj <- current_subject()
    if (is.na(subj) || !nzchar(subj)) subj <- "UNKNOWN_SUBJECT"
    paste0(subj, "|", current_face_key())
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
    img_top <- fx$IMG_Y - (h / 2)

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

  deldir_tibble <- reactive({
    req(current_face_key())
    subj <- current_subject()
    aois$centres |>
      dplyr::filter(.data$face_key == current_face_key(),
                    is.na(subj) | .data$subject == subj) |>
      dplyr::select(subject, face_key, aoi_id, aoi_name, deldir_id, x, y) |>
      tibble::as_tibble()
  })

  output$aoi_debug_tbl <- renderTable({
    req(current_face_key())
    deldir_tibble()
  }, digits = 0)

  next_aoi_id_for <- function(subject, face_key) {
    if (is.na(subject) || !nzchar(subject)) subject <- "UNKNOWN_SUBJECT"
    if (is.na(face_key) || !nzchar(face_key)) face_key <- "UNKNOWN_FACE"

    idx <- which(aoi_seq$n$subject == subject & aoi_seq$n$face_key == face_key)
    if (length(idx) == 0) {
      aoi_seq$n <- dplyr::bind_rows(
        aoi_seq$n,
        tibble::tibble(subject = subject, face_key = face_key, next_n = 1L)
      )
      idx <- nrow(aoi_seq$n)
    }

    n_now <- aoi_seq$n$next_n[idx[1]]
    aoi_seq$n$next_n[idx[1]] <- n_now + 1L

    sprintf("AOI_%03d", n_now)
  }

  # ---- Invalidate "finalized" if AOIs change for this key --------------
  observeEvent(list(deldir_tibble(), current_dd_key()), {
    key <- current_dd_key()
    assign$finalized[[key]] <- FALSE
    assign$store[[key]] <- NULL
  }, ignoreInit = TRUE)

  output$finalize_status_ui <- renderUI({
    req(current_dd_key())
    key <- current_dd_key()
    is_done <- isTRUE(assign$finalized[[key]])
    cls <- if (is_done) "alert alert-success" else "alert alert-warning"
    txt <- if (is_done) "Finalized: YES" else "Finalized: NO"
    tags$div(class = cls, txt)
  })

  output$commit_status_ui <- renderUI({
    req(current_dd_key())
    key <- current_dd_key()
    is_done <- isTRUE(session_ann$committed[[key]])
    cls <- if (is_done) "alert alert-success" else "alert alert-warning"
    txt <- if (is_done) "Committed: YES" else "Committed: NO"
    tags$div(class = cls, txt)
  })

  output$reset_status_ui <- renderUI({
    n_rows <- 0L
    if (is.data.frame(session_ann$all)) n_rows <- nrow(session_ann$all)
    cls <- if (n_rows == 0L) "alert alert-secondary" else "alert alert-info"
    txt <- paste0("Session rows: ", n_rows)
    tags$div(class = cls, txt)
  })

  # ---- Compute + store deldir each time deldir tibble changes ----------
  observeEvent(list(deldir_tibble(), face_img(), current_dd_key()), {
    key <- current_dd_key()

    if (!requireNamespace("deldir", quietly = TRUE)) {
      dd$store[[key]] <- NULL
      return()
    }

    pts <- deldir_tibble()
    if (nrow(pts) < 2) {
      dd$store[[key]] <- NULL
      return()
    }

    in_pts <- pts |>
      dplyr::mutate(
        x = suppressWarnings(as.numeric(.data$x)),
        y = suppressWarnings(as.numeric(.data$y)),
        id = as.character(.data$deldir_id)
      ) |>
      dplyr::filter(is.finite(.data$x), is.finite(.data$y), !is.na(.data$id), nzchar(.data$id)) |>
      dplyr::distinct(.data$id, .keep_all = TRUE) |>
      dplyr::distinct(.data$x, .data$y, .keep_all = TRUE)

    if (nrow(in_pts) < 2) {
      dd$store[[key]] <- NULL
      return()
    }

    w <- face_img()$width
    h <- face_img()$height

    res <- tryCatch(
      deldir::deldir(
        x = in_pts$x,
        y = in_pts$y,
        rw = c(0, w, 0, h),
        id = in_pts$id
      ),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      dd$store[[key]] <- NULL
      showNotification(paste0("deldir error: ", conditionMessage(res)), type = "error", duration = 3)
      return()
    }

    dd$store[[key]] <- list(input_pts = in_pts, res = res)
  }, ignoreInit = TRUE)

  current_deldir_result <- reactive({
    key <- current_dd_key()
    dd$store[[key]]
  })

  # ---- Finalize: assign fixations to nearest AOI centre ----------------
  observeEvent(input$finalize_aois, {
    req(face_img(), current_dd_key())

    key <- current_dd_key()

    dd_obj <- dd$store[[key]]
    if (is.null(dd_obj) || is.null(dd_obj$input_pts) || nrow(dd_obj$input_pts) < 2) {
      showNotification("Need at least 2 AOI centres (and a valid tessellation) before finalizing.", type = "message", duration = 2.5)
      return()
    }

    fx <- fixrep_this_face_mapped()
    if (is.null(fx) || nrow(fx) == 0) {
      showNotification("No fixations found for the current face.", type = "message", duration = 2.5)
      return()
    }

    w <- face_img()$width
    h <- face_img()$height

    ok <- is.finite(fx$FIX_X_IMG) & is.finite(fx$FIX_Y_IMG) &
      fx$FIX_X_IMG >= 0 & fx$FIX_X_IMG <= w &
      fx$FIX_Y_IMG >= 0 & fx$FIX_Y_IMG <= h

    fx_ok <- fx[ok, , drop = FALSE]
    if (nrow(fx_ok) == 0) {
      showNotification("No in-bounds fixations to assign for this face.", type = "message", duration = 2.5)
      return()
    }

    centres <- dd_obj$input_pts |>
      dplyr::select(id, x, y) |>
      tibble::as_tibble()

    assign_one <- function(px, py, centres) {
      d2 <- (centres$x - px)^2 + (centres$y - py)^2
      centres$id[which.min(d2)]
    }

    assigned_id <- vapply(
      seq_len(nrow(fx_ok)),
      function(i) assign_one(fx_ok$FIX_X_IMG[i], fx_ok$FIX_Y_IMG[i], centres),
      character(1)
    )

    out <- fx_ok |>
      dplyr::mutate(AOI_ASSIGNED = assigned_id) |>
      dplyr::select(
        SUBJECT, FACE, TRIAL_ID, CONDITION,
        FIX_X, FIX_Y, FIX_DUR,
        IMG_X, IMG_Y,
        FIX_X_IMG, FIX_Y_IMG,
        AOI_ASSIGNED
      )

    assign$store[[key]] <- tibble::as_tibble(out)
    assign$finalized[[key]] <- TRUE

    showNotification("Assigned fixations to AOIs for the current face.", type = "message", duration = 2)
  })

  # ---- Commit current face into session table --------------------------
  observeEvent(input$commit_next_face, {
    req(current_dd_key())
    key <- current_dd_key()

    if (isTRUE(session_ann$committed[[key]])) {
      showNotification("This face is already committed to the session table.", type = "message", duration = 2.5)
      return()
    }

    if (!isTRUE(assign$finalized[[key]])) {
      showNotification("Finalize AOIs for this face before moving to the next face.", type = "message", duration = 2.5)
      return()
    }

    x <- assign$store[[key]]
    if (is.null(x) || nrow(x) == 0) {
      showNotification("No assigned fixations to commit for this face.", type = "message", duration = 2.5)
      return()
    }

    pts <- deldir_tibble() |>
      dplyr::select(aoi_id, aoi_name, deldir_id) |>
      dplyr::distinct(deldir_id, .keep_all = TRUE)

    x2 <- x |>
      dplyr::left_join(pts, by = c("AOI_ASSIGNED" = "deldir_id")) |>
      dplyr::mutate(
        AOI_ID = .data$aoi_id,
        AOI_NAME = .data$aoi_name
      ) |>
      dplyr::select(-aoi_id, -aoi_name)

    session_ann$all <- dplyr::bind_rows(session_ann$all, x2)
    session_ann$committed[[key]] <- TRUE

    showNotification("Committed this face to the session table. Upload the next face to continue.", type = "message", duration = 2.5)
  })

  # ---- Guarded reset (confirm modal) -----------------------------------
  observeEvent(input$reset_session, {
    showModal(
      modalDialog(
        title = "Reset session annotations?",
        "This will permanently clear all committed face annotations in this session (and the summary / grand means).",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_reset_session", "Yes, reset", class = "btn-danger")
        )
      )
    )
  })

  observeEvent(input$confirm_reset_session, {
    removeModal()
    session_ann$all <- tibble::tibble()
    session_ann$committed <- list()
    showNotification("Session annotations cleared.", type = "message", duration = 2)
  })

  assigned_fixations_current <- reactive({
    req(current_dd_key())
    key <- current_dd_key()
    assign$store[[key]]
  })

  output$fix_assign_debug_tbl <- renderTable({
    x <- assigned_fixations_current()
    if (is.null(x)) return(NULL)
    head(x, 50)
  }, digits = 0)

  output$download_fix_assign_current <- downloadHandler(
    filename = function() {
      req(current_face_key())
      paste0("fixations_assigned_", gsub("[^a-zA-Z0-9]+", "_", current_face_key()), ".csv")
    },
    content = function(file) {
      x <- assigned_fixations_current()
      if (is.null(x)) {
        readr::write_csv(tibble::tibble(message = "No assignments yet. Click 'Finalize AOIs for this face' first."), file)
      } else {
        readr::write_csv(x, file)
      }
    }
  )

  # ---- Fixations annotated (all faces so far): DT table -----------------

  output$fixations_tbl <- DT::renderDT({
    if (!requireNamespace("DT", quietly = TRUE)) return(NULL)

    df <- session_ann$all
    dt <- DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    )

    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(num_cols) > 0) {
      dt <- DT::formatRound(dt, columns = num_cols, digits = 0)
    }
    dt
  })

  # ---- Summary tab (row per subject-face-aoi) ---------------------------

  summary_tbl <- reactive({
    x <- session_ann$all
    if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) return(tibble::tibble())

    x |>
      dplyr::mutate(
        AOI_LABEL = dplyr::if_else(
          !is.na(.data$AOI_NAME) & nzchar(.data$AOI_NAME),
          .data$AOI_NAME,
          .data$AOI_ID
        )
      ) |>
      dplyr::group_by(.data$SUBJECT, .data$FACE, .data$AOI_ASSIGNED, .data$AOI_ID, .data$AOI_NAME, .data$AOI_LABEL) |>
      dplyr::summarise(
        N_FIX = dplyr::n(),
        SUM_FIX_DUR = sum(.data$FIX_DUR, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$SUBJECT, .data$FACE, .data$AOI_LABEL)
  })

  output$fixations_summary <- renderTable({
    x <- summary_tbl()
    if (nrow(x) == 0) return(NULL)
    x
  }, digits = 0)

  # ---- Grand means tab (row per face-aoi; aggregated over subjects) -----

  grand_means_tbl <- reactive({
    s <- summary_tbl()
    if (is.null(s) || !is.data.frame(s) || nrow(s) == 0) return(tibble::tibble())

    s |>
      dplyr::group_by(.data$FACE, .data$AOI_ASSIGNED, .data$AOI_ID, .data$AOI_NAME, .data$AOI_LABEL) |>
      dplyr::summarise(
        GRAND_MEAN_N_FIX = mean(.data$N_FIX, na.rm = TRUE),
        GRAND_MEAN_SUM_FIX_DUR = mean(.data$SUM_FIX_DUR, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$FACE, .data$AOI_LABEL)
  })

  output$grand_means_tbl <- renderTable({
    x <- grand_means_tbl()
    if (nrow(x) == 0) return(NULL)
    x
  }, digits = 0)

  # ---- Downloads tab ---------------------------------------------------

  output$download_fixations_session <- downloadHandler(
    filename = function() {
      paste0("fixations_annotated_session_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      x <- session_ann$all
      if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
        readr::write_csv(tibble::tibble(message = "No session annotations yet. Commit at least one face."), file)
      } else {
        readr::write_csv(x, file)
      }
    }
  )

  output$download_summary_session <- downloadHandler(
    filename = function() {
      paste0("fixations_summary_session_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      x <- summary_tbl()
      if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
        readr::write_csv(tibble::tibble(message = "No summary yet. Commit at least one face."), file)
      } else {
        readr::write_csv(x, file)
      }
    }
  )

  output$download_grand_means_session <- downloadHandler(
    filename = function() {
      paste0("fixations_grand_means_session_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      x <- grand_means_tbl()
      if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
        readr::write_csv(tibble::tibble(message = "No grand means yet. Commit at least one face (and have a summary)."), file)
      } else {
        readr::write_csv(x, file)
      }
    }
  )

  # ---- LHS plot --------------------------------------------------------

  output$face_for_edit <- renderPlot({
    req(face_img())

    plot_native_image_fit(
      face_img(),
      panel_w_px = session$clientData$output_face_for_edit_width,
      panel_h_px = session$clientData$output_face_for_edit_height
    )

    pts <- deldir_tibble()
    if (nrow(pts) > 0) {
      points(pts$x, pts$y, pch = 4, cex = 2, lwd = 3, col = "cyan")
      lbl <- ifelse(is.na(pts$aoi_name) | pts$aoi_name == "", pts$aoi_id, pts$aoi_name)
      text(pts$x, pts$y, labels = lbl, pos = 3, cex = 0.9, col = "cyan")
    }
  })

  # ---- RHS plot: face + fixations + tessellation (on top) --------------

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

    dd_obj <- current_deldir_result()
    if (!is.null(dd_obj) && is.list(dd_obj) && !is.null(dd_obj$res)) {
      segs <- dd_obj$res$dirsgs
      if (!is.null(segs) && nrow(segs) > 0) {
        segments(segs$x1, segs$y1, segs$x2, segs$y2, lwd = 3, col = "cyan")
      }

      in_pts <- dd_obj$input_pts
      if (!is.null(in_pts) && nrow(in_pts) > 0) {
        points(in_pts$x, in_pts$y, pch = 4, cex = 2, lwd = 3, col = "cyan")
        text(in_pts$x, in_pts$y, labels = in_pts$id, pos = 3, cex = 0.9, col = "cyan")
      }
    }
  })

  # ---- LHS click: add centre (with optional name + deldir_id) ----------

  observeEvent(input$face_for_edit_click, {
    req(face_img(), current_face_key())

    if (!click_on_image(input$face_for_edit_click, face_img())) {
      showNotification("Click on the image (not the padding).", type = "message", duration = 1.5)
      return()
    }

    subj <- current_subject()
    if (is.na(subj) || !nzchar(subj)) subj <- "UNKNOWN_SUBJECT"

    x <- input$face_for_edit_click$x
    y <- input$face_for_edit_click$y

    nm <- input$aoi_name_next
    nm <- if (is.null(nm)) "" else trimws(as.character(nm))
    if (!nzchar(nm)) nm <- NA_character_

    new_id <- next_aoi_id_for(subject = subj, face_key = current_face_key())
    deldir_id <- if (!is.na(nm) && nzchar(nm)) nm else new_id

    aois$centres <- dplyr::bind_rows(
      aois$centres,
      tibble::tibble(
        subject = subj,
        face_key = current_face_key(),
        aoi_id = new_id,
        aoi_name = nm,
        deldir_id = deldir_id,
        x = x,
        y = y
      )
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

    pts <- deldir_tibble()
    if (nrow(pts) == 0) return()

    x <- input$face_for_edit_dblclick$x
    y <- input$face_for_edit_dblclick$y

    d2 <- (pts$x - x)^2 + (pts$y - y)^2
    i <- which.min(d2)

    delete_id <- pts$aoi_id[i]
    delete_subject <- pts$subject[i]
    delete_face <- pts$face_key[i]

    aois$centres <- aois$centres |>
      dplyr::filter(!(subject == delete_subject & face_key == delete_face & aoi_id == delete_id))
  })
}
