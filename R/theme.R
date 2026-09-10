# One visual identity, consumed by the email preview, the approval previews, the
# charts and both Shiny apps.
#
# The brief is near-monochrome: the logo carries the colour, the interface does
# not compete with it, and a single accent marks the one thing that matters in
# any given view. That is a design decision with a practical payoff -- these
# artifacts appear on slides next to charts, and an interface with its own
# palette fights the evidence it is supposed to be presenting.
#
# Every colour except the hairline greys is lifted from images/chickencloud-logo.svg,
# so the palette and the logo cannot drift apart.
#
# Deviation from the plan's tree, which names apps/theme.R: the email preview and
# the charts both live in R/ and both consume this, so a file under apps/ would
# have R/ depending on apps/. The Shiny-specific part is inbox_bs_theme() below.

# --- Contrast ---------------------------------------------------------------

# WCAG 2.1 relative luminance and contrast ratio. Kept here rather than in the
# tests because the palette comments quote its output, and a number quoted from
# a function nobody can run is a number nobody will re-check.
inbox_relative_luminance <- function(hex) {
  channels <- grDevices::col2rgb(hex)[, 1] / 255
  linear <- ifelse(
    channels <= 0.03928,
    channels / 12.92,
    ((channels + 0.055) / 1.055)^2.4
  )
  sum(c(0.2126, 0.7152, 0.0722) * linear)
}

inbox_contrast_ratio <- function(foreground, background) {
  a <- inbox_relative_luminance(foreground)
  b <- inbox_relative_luminance(background)
  (max(a, b) + 0.05) / (min(a, b) + 0.05)
}

# --- Palette ----------------------------------------------------------------

# Ratios below are against `page` and are asserted in tests/testthat/test-theme.R,
# because §19 lists contrast as a release requirement and a palette nobody checks
# is a palette that quietly fails on a projector.
INBOX_PALETTE <- list(
  # The logo wordmark and outline. Reads as a warm near-black. 12.0:1.
  ink = "#243B34",
  # The logo's ears. Secondary text, axis labels, captions. 6.4:1.
  muted = "#59615A",
  # The dog's coat.
  #
  # DECORATION ONLY -- 2.67:1, which fails AA at every size. It may fill a
  # highlight band, draw a left border or rule a heading. It may never carry
  # text. accent_ink exists for when the accent has to be readable.
  accent = "#BD986B",
  # The same hue darkened until it passes AA for body text. 5.75:1.
  accent_ink = "#7A6242",
  page = "#FFFFFF",
  # A warm off-white so surfaces read as paper rather than as grey panels.
  surface = "#FBF8F1",
  code_surface = "#F4F1E8",
  # Hairlines. Not from the logo -- rules are not branding, and a logo colour
  # here would be too heavy at 1px.
  rule = "#E4DED2",
  rule_strong = "#C9C1B1"
)

inbox_font_stack <- paste(
  "system-ui", "-apple-system", "'Segoe UI'", "Roboto",
  "'Helvetica Neue'", "Arial", "sans-serif",
  sep = ", "
)

inbox_mono_stack <- paste(
  "ui-monospace", "SFMono-Regular", "'SF Mono'", "Menlo",
  "Consolas", "monospace",
  sep = ", "
)

# --- HTML previews ----------------------------------------------------------

# Shared CSS for the rendered email and the approval replays.
#
# A <style> block rather than inlined attributes. Real email clients need CSS
# inlined on every element, but these are previews rendered in a browser and
# screenshotted for slides, and inlining would make the generating R code far
# harder to read for no benefit on screen. Production email delivery is later
# work; when it arrives it inlines from these same tokens.
inbox_preview_css <- function(max_width = "820px") {
  p <- INBOX_PALETTE
  paste(
    # Page
    sprintf(":root { color-scheme: light; }"),
    sprintf("body { font: 17px/1.6 %s; color: %s; background: %s;", inbox_font_stack, p$ink, p$surface),
    sprintf("  margin: 0; padding: 32px 16px; }"),
    sprintf(".sheet { max-width: %s; margin: 0 auto; background: %s;", max_width, p$page),
    sprintf("  border: 1px solid %s; border-radius: 6px; padding: 40px 44px; }", p$rule),

    # Type. Tight headings against generous body leading is most of the polish.
    sprintf("h1, h2, h3 { color: %s; line-height: 1.25; letter-spacing: -0.01em; }", p$ink),
    "h1 { font-size: 27px; margin: 0 0 4px; }",
    "h2 { font-size: 22px; margin: 32px 0 8px; }",
    "h3 { font-size: 16px; margin: 28px 0 8px; }",
    "p { margin: 0 0 14px; }",

    # The small uppercase label that opens a section. Does a lot of the work of
    # making this look designed rather than typed.
    sprintf(".label { font-size: 11.5px; font-weight: 600; letter-spacing: 0.09em;"),
    sprintf("  text-transform: uppercase; color: %s; margin: 0 0 6px; }", p$muted),
    sprintf(".muted { color: %s; }", p$muted),
    ".small { font-size: 14px; }",

    # Header
    sprintf(".masthead { display: flex; align-items: center; gap: 16px;"),
    sprintf("  padding-bottom: 20px; border-bottom: 2px solid %s; }", p$ink),
    ".masthead img { display: block; width: 210px; height: auto; }",
    ".masthead .titles { flex: 1; }",

    # Tables. No vertical rules, hairline horizontals, tabular figures so the
    # digits line up in columns.
    "table { border-collapse: collapse; width: 100%; margin: 12px 0 8px; font-size: 15.5px; }",
    sprintf("caption { caption-side: bottom; text-align: left; font-size: 13.5px;"),
    sprintf("  color: %s; padding-top: 10px; }", p$muted),
    sprintf("th, td { padding: 9px 12px; text-align: right; border-bottom: 1px solid %s;", p$rule),
    "  font-variant-numeric: tabular-nums; }",
    "thead th { font-size: 11.5px; letter-spacing: 0.07em; text-transform: uppercase;",
    sprintf("  color: %s; border-bottom: 1px solid %s; }", p$muted, p$rule_strong),
    "th:first-child, td:first-child, thead th:first-child { text-align: left; }",
    sprintf("tbody th { font-weight: 600; color: %s; }", p$ink),

    # Callouts. Intent is carried by the left border, which is the one place the
    # accent appears in the email.
    sprintf(".callout { border-left: 3px solid %s; background: %s;", p$accent, p$surface),
    "  padding: 12px 16px; margin: 0 0 16px; }",
    ".callout p:last-child { margin-bottom: 0; }",
    sprintf(".callout-quiet { border-left-color: %s; }", p$rule_strong),

    # Code
    sprintf("pre { background: %s; border: 1px solid %s; border-radius: 4px;", p$code_surface, p$rule),
    "  padding: 14px 16px; overflow-x: auto; }",
    sprintf("code { font-family: %s; font-size: 13.5px; }", inbox_mono_stack),
    "pre code { white-space: pre-wrap; overflow-wrap: anywhere; }",

    # Footer
    sprintf("footer { margin-top: 36px; padding-top: 16px; border-top: 1px solid %s;", p$rule),
    sprintf("  font-size: 13px; color: %s; overflow-wrap: anywhere; }", p$muted),
    "footer p { margin: 0 0 5px; }",
    sprintf("a { color: %s; }", p$accent_ink),
    sep = "\n"
  )
}

# --- Charts -----------------------------------------------------------------

# Series colours. Ink is the series that is true; muted grey is the one that is
# explanatory or hypothetical. Reserving the accent for the highlight band means
# the eye lands on the cohort under discussion rather than on a line.
inbox_chart_colours <- function() {
  list(
    primary = INBOX_PALETTE$ink,
    secondary = INBOX_PALETTE$muted,
    highlight = INBOX_PALETTE$accent,
    text = INBOX_PALETTE$ink,
    muted_text = INBOX_PALETTE$muted,
    grid = INBOX_PALETTE$rule
  )
}

# Horizontal gridlines only, no panel border, no minor grid. Everything that is
# not data is pushed back toward the page.
inbox_theme_ggplot <- function(base_size = 16) {
  p <- INBOX_PALETTE
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = p$ink),
      plot.title = ggplot2::element_text(
        face = "bold", size = ggplot2::rel(1.05), colour = p$ink,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.8), colour = p$muted, lineheight = 1.25,
        margin = ggplot2::margin(b = 14)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0, size = ggplot2::rel(0.7), colour = p$muted, lineheight = 1.3,
        margin = ggplot2::margin(t = 14)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title = ggplot2::element_text(size = ggplot2::rel(0.78), colour = p$muted),
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = p$muted),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = p$rule, linewidth = 0.4),
      legend.position = "bottom",
      legend.justification = "left",
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = p$ink),
      legend.margin = ggplot2::margin(t = 2),
      # Wide enough for a dash pattern to be legible in the key. In a
      # near-monochrome palette the linetype is doing the work of telling two
      # series apart, so a key too short to show a dash defeats the scheme --
      # and the alternative, pushing the two colours further apart, costs
      # contrast on the labels that inherit them.
      legend.key.width = grid::unit(1.6, "cm"),
      plot.background = ggplot2::element_rect(fill = p$page, colour = NA),
      plot.margin = ggplot2::margin(16, 20, 12, 16)
    )
}

# --- Shiny ------------------------------------------------------------------

# The apps get the same tokens through bslib rather than a Bootswatch preset.
# A preset would bring its own palette, and then the apps and the email would be
# two designs that happen to sit next to each other.
inbox_bs_theme <- function() {
  p <- INBOX_PALETTE
  bslib::bs_add_rules(
    bslib::bs_theme(
      version = 5,
      bg = p$page,
      fg = p$ink,
      primary = p$ink,
      secondary = p$muted,
      base_font = inbox_font_stack,
      code_font = inbox_mono_stack,
      "body-bg" = p$surface,
      "border-color" = p$rule,
      "border-radius" = "6px",
      "card-border-color" = p$rule,
      "card-cap-bg" = p$page,
      "font-size-base" = "1rem",
      "line-height-base" = "1.6",
      "headings-font-weight" = "600"
    ),
    sprintf("
      .card { box-shadow: none; }
      .card-header {
        border-bottom: 1px solid %s;
        font-size: 0.72rem;
        font-weight: 600;
        letter-spacing: 0.09em;
        text-transform: uppercase;
        color: %s;
      }
      .accent-rule { border-left: 3px solid %s; padding-left: 14px; }
      .text-muted-warm { color: %s !important; }
      .tabular { font-variant-numeric: tabular-nums; }
      .btn-primary { border-color: %s; }
    ", p$rule, p$muted, p$accent, p$muted, p$ink)
  )
}
