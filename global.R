read_fixrep <- function(path) {
  stopifnot(length(path) == 1, is.character(path), file.exists(path))

  # Dependencies: readr, dplyr, stringr, tibble
  requireNamespace("readr")
  requireNamespace("dplyr")
  requireNamespace("stringr")
  requireNamespace("tibble")

  # --- Detect delimiter robustly (tab vs comma) -------------------------
  first_line <- readLines(path, n = 1, warn = FALSE)
  n_tabs   <- stringr::str_count(first_line, "\t")
  n_commas <- stringr::str_count(first_line, ",")

  delim <- if (n_tabs >= n_commas) "\t" else ","

  # --- Read -------------------------------------------------------------
  # Keep everything as character initially, then convert safely.
  raw <- readr::read_delim(
    file = path,
    delim = delim,
    col_types = readr::cols(.default = readr::col_character()),
    trim_ws = TRUE,
    progress = FALSE,
    show_col_types = FALSE
  )

  # --- Normalise column names (light touch) -----------------------------
  # Some exports have weird BOM/spacing; clean that up.
  nm <- names(raw)
  nm <- stringr::str_replace_all(nm, "\uFEFF", "")   # drop UTF-8 BOM if present
  nm <- stringr::str_trim(nm)
  names(raw) <- nm

  # --- Parse location_b1 "(x, y)" -> img_cx/img_cy ----------------------
  # location_b1 may be missing in some rows; parse gently.
  loc_parse <- function(x) {
    x <- as.character(x)
    x <- stringr::str_trim(x)
    x <- stringr::str_remove_all(x, "[()]")
    # split on comma
    parts <- stringr::str_split_fixed(x, ",", 2)
    tibble::tibble(
      img_cx = suppressWarnings(as.numeric(stringr::str_trim(parts[, 1]))),
      img_cy = suppressWarnings(as.numeric(stringr::str_trim(parts[, 2])))
    )
  }

  has_loc <- "location_b1" %in% names(raw)
  loc_tbl <- if (has_loc) loc_parse(raw$location_b1) else tibble::tibble(img_cx = NA_real_, img_cy = NA_real_)

  # --- Convert key numeric columns + standardise fixation names ----------
  # Your file uses CURRENT_FIX_X/Y/DURATION; we standardise to FIX_X/FIX_Y/FIX_DURATION.
  out <- raw %>%
    dplyr::mutate(
      RECORDING_SESSION_LABEL = suppressWarnings(as.integer(.data$RECORDING_SESSION_LABEL)),
      TRIAL_INDEX             = suppressWarnings(as.integer(.data$TRIAL_INDEX)),
      CURRENT_FIX_INDEX       = suppressWarnings(as.integer(.data$CURRENT_FIX_INDEX)),
      CURRENT_FIX_X           = suppressWarnings(as.numeric(.data$CURRENT_FIX_X)),
      CURRENT_FIX_Y           = suppressWarnings(as.numeric(.data$CURRENT_FIX_Y)),
      CURRENT_FIX_DURATION    = suppressWarnings(as.numeric(.data$CURRENT_FIX_DURATION))
    )

  # Attach parsed centres (if location_b1 existed)
  if (has_loc) out <- dplyr::bind_cols(out, loc_tbl)

  # Standardise fixation column names (keep originals too if you want; here we create FIX_*)
  out <- out %>%
    dplyr::mutate(
      FIX_X        = .data$CURRENT_FIX_X,
      FIX_Y        = .data$CURRENT_FIX_Y,
      FIX_DURATION = .data$CURRENT_FIX_DURATION
    )

  tibble::as_tibble(out)
}
