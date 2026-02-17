# app.R ---------------------------------------------------------------
# Minimal toy app for:
# - LHS: click add AOI (debounced so drag/brush doesn't add),
#        dblclick remove nearest, brush-to-zoom, reset button
# - RHS: always zoomed out; shows Voronoi tessellation via deldir with 3+ points
#
# Run: shiny::runApp()

library(shiny)
library(deldir)

# ---- Fixed "image" dimensions in native pixel space ------------------
W <- 600
H <- 800

now_ms <- function() as.numeric(Sys.time()) * 1000

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .help { margin: 0.25rem 0 0.75rem 0; color: #444; }
    .panel-title { font-weight: 600; margin-bottom: 0.25rem; }
  "))),

  fluidRow(
    column(
      width = 6,
      div(class = "panel-title", "LHS: Define AOIs (click add, dblclick remove, drag to zoom)"),
      div(
        class = "help",
        "Click = add AOI. Double-click = remove nearest AOI. Drag a rectangle to zoom. Reset to zoom out."
      ),
      plotOutput(
        "lhs",
        height = "650px",
        click = "lhs_click",
        dblclick = "lhs_dblclick",
        brush = brushOpts(id = "lhs_brush", resetOnNew = TRUE)
      ),
      fluidRow(
        column(4, actionButton("reset_view", "Reset view")),
        column(4, actionButton("clear_aois", "Clear AOIs")),
        column(4, verbatimTextOutput("status", placeholder = TRUE))
      )
    ),
    column(
      width = 6,
      div(class = "panel-title", "RHS: Tessellation view (always full extent)"),
      div(
        class = "help",
        "This view stays zoomed out so you can always see the whole tessellation landscape."
      ),
      plotOutput("rhs", height = "650px")
    )
  )
)

server <- function(input, output, session) {

  # ---- State: AOIs in native pixel space -----------------------------
  aois <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))

  # ---- State: camera window for LHS only -----------------------------
  cam <- reactiveValues(
    xlim = c(0, W),
    ylim = c(H, 0)  # inverted so y=0 is top (like image pixels)
  )

  # ---- Debounce state (all reactive, so safe) -------------------------
  pending_click <- reactiveVal(NULL)     # list(x=..., y=..., t=...)
  pending_click_t <- reactiveVal(0)      # timestamp of last click (ms)
  last_brush_t <- reactiveVal(0)         # timestamp of last real brush (ms)
  last_dblclick_t <- reactiveVal(0)      # timestamp of last dblclick (ms)

  # ---- Helpers --------------------------------------------------------
  draw_face_canvas <- function() {
    rect(0, 0, W, H, col = "grey95", border = "grey85")
    segments(W / 2, 0, W / 2, H, col = "grey85")
    segments(0, H / 2, W, H / 2, col = "grey85")
  }

  draw_aois <- function(df) {
    if (nrow(df) == 0) return()
    points(df$x, df$y, pch = 19, cex = 1.1)
    text(df$x, df$y, labels = seq_len(nrow(df)), pos = 3, cex = 0.9)
  }

  voronoi_edges <- function(df) {
    if (nrow(df) < 3) return(NULL)
    dd <- deldir(df$x, df$y, rw = c(0, W, 0, H), suppressMsge = TRUE)
    dd$dirsgs
  }

  brush_is_real <- function(b) {
    if (is.null(b)) return(FALSE)
    dx <- abs(b$xmax - b$xmin)
    dy <- abs(b$ymax - b$ymin)
    (dx >= 10) && (dy >= 10)  # ignore tiny accidental drags
  }

  cancel_pending_click <- function() {
    pending_click(NULL)
    pending_click_t(0)
  }

  # ---- LHS: render with current camera window -------------------------
  output$lhs <- renderPlot({
    par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    plot(
      NA,
      xlim = cam$xlim,
      ylim = cam$ylim,
      asp = 1,
      xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, bty = "n"
    )
    draw_face_canvas()
    draw_aois(aois())
  })

  # ---- RHS: always full extent + tessellation -------------------------
  output$rhs <- renderPlot({
    par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    plot(
      NA,
      xlim = c(0, W),
      ylim = c(H, 0),
      asp = 1,
      xaxt = "n", yaxt = "n", xlab = NA, ylab = NA, bty = "n"
    )
    draw_face_canvas()

    df <- aois()
    edges <- voronoi_edges(df)
    if (!is.null(edges)) segments(edges$x1, edges$y1, edges$x2, edges$y2)
    draw_aois(df)
  })

  # ---- Brush-to-zoom affects LHS camera only --------------------------
  observeEvent(input$lhs_brush, {
    b <- input$lhs_brush
    if (!brush_is_real(b)) return()

    last_brush_t(now_ms())
    cancel_pending_click()

    x0 <- max(0, min(W, min(b$xmin, b$xmax)))
    x1 <- max(0, min(W, max(b$xmin, b$xmax)))
    y0 <- max(0, min(H, min(b$ymin, b$ymax)))
    y1 <- max(0, min(H, max(b$ymin, b$ymax)))

    # Keep aspect ratio by expanding the rectangle to match image aspect
    target_ar <- H / W
    w0 <- (x1 - x0)
    h0 <- (y1 - y0)
    if (w0 <= 0 || h0 <= 0) return()

    current_ar <- h0 / w0
    cx <- (x0 + x1) / 2
    cy <- (y0 + y1) / 2

    if (current_ar > target_ar) {
      new_w <- h0 / target_ar
      x0 <- cx - new_w / 2
      x1 <- cx + new_w / 2
    } else {
      new_h <- w0 * target_ar
      y0 <- cy - new_h / 2
      y1 <- cy + new_h / 2
    }

    # Clamp to image bounds
    x0 <- max(0, x0); x1 <- min(W, x1)
    y0 <- max(0, y0); y1 <- min(H, y1)

    cam$xlim <- c(x0, x1)
    cam$ylim <- c(y1, y0)
  })

  # ---- Click: register as pending (commit later if not a brush/dblclick) ----
  observeEvent(input$lhs_click, {
    clk <- input$lhs_click
    req(!is.null(clk$x), !is.null(clk$y))

    t <- now_ms()
    pending_click(list(x = clk$x, y = clk$y, t = t))
    pending_click_t(t)
  })

  # ---- Double-click: remove nearest AOI and cancel pending click -------
  observeEvent(input$lhs_dblclick, {
    db <- input$lhs_dblclick
    req(!is.null(db$x), !is.null(db$y))

    last_dblclick_t(now_ms())
    cancel_pending_click()

    df <- aois()
    if (nrow(df) == 0) return()

    d2 <- (df$x - db$x)^2 + (df$y - db$y)^2
    i <- which.min(d2)

    # Only remove if "close enough"
    if (sqrt(d2[i]) > 40) return()

    aois(df[-i, , drop = FALSE])
  })

  # ---- Debounced commit loop (runs inside reactive context) ------------
  observe({
    invalidateLater(50, session)  # check 20 times/sec
    pc <- pending_click()
    if (is.null(pc)) return()

    # Wait a short time to see if this "click" was actually part of a brush/dblclick
    if (now_ms() - pending_click_t() < 250) return()

    # If a real brush happened after the click, cancel
    if (last_brush_t() >= pc$t) {
      cancel_pending_click()
      return()
    }

    # If a dblclick happened after the click, cancel
    if (last_dblclick_t() >= pc$t) {
      cancel_pending_click()
      return()
    }

    # Also, if there is currently a real brush selection, cancel
    if (brush_is_real(input$lhs_brush)) {
      cancel_pending_click()
      return()
    }

    # Commit: add AOI
    df <- aois()
    df <- rbind(df, data.frame(x = pc$x, y = pc$y))
    aois(df)
    cancel_pending_click()
  })

  # ---- Buttons ---------------------------------------------------------
  observeEvent(input$reset_view, {
    cam$xlim <- c(0, W)
    cam$ylim <- c(H, 0)
  })

  observeEvent(input$clear_aois, {
    aois(data.frame(x = numeric(0), y = numeric(0)))
    cancel_pending_click()
  })

  output$status <- renderText({
    df <- aois()
    paste0("AOIs: ", nrow(df), if (nrow(df) >= 3) " (Voronoi on RHS)" else " (need 3+ for Voronoi)")
  })
}

shinyApp(ui, server)
