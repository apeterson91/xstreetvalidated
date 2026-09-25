# Minimal, dependency-free tabbed panel widget for pairing a table with its
# companion chart in a compact, click-to-switch layout. No external JS/CSS
# libraries required (e.g. xaringanExtra) -- just plain HTML plus one small
# inline script, injected once per document.

panelset_css <- "
.panelset { margin: 1.5em 0 2em; }
.panelset .panel-tabs { margin-bottom: 0; }
.panelset .panel-tab {
  display: inline-block; cursor: pointer; font-family: inherit; font-size: 0.9em;
  padding: 6px 16px; margin: 0 4px -1px 0;
  border: 1px solid #ccc; border-bottom: none; border-radius: 4px 4px 0 0;
  background: #f2f2f2;
}
.panelset .panel-tab.active { background: #fff; font-weight: 600; border-bottom: 1px solid #fff; }
.panelset .panel-pane { display: none; border: 1px solid #ccc; padding: 1em; }
.panelset .panel-pane.active { display: block; }
.panelset .panel-pane img { max-width: 100%; height: auto; display: block; margin: 0 auto; }
.panelset .panel-pane table { margin: 0 auto; }
"

panelset_js <- "
document.addEventListener('click', function(e) {
  var btn = e.target.closest('.panel-tab');
  if (!btn) return;
  var panelset = btn.closest('.panelset');
  panelset.querySelectorAll('.panel-tab').forEach(function(b){ b.classList.remove('active'); });
  panelset.querySelectorAll('.panel-pane').forEach(function(p){ p.classList.remove('active'); });
  btn.classList.add('active');
  panelset.querySelector('#' + btn.dataset.pane).classList.add('active');
});
"

# Emit the shared CSS/JS once for the whole document. Call from a
# `results='asis'` chunk near the top of the Rmd, before any panelset() calls.
panelset_init <- function() {
  cat(sprintf("<style>%s</style>\n<script>%s</script>\n", panelset_css, panelset_js))
}

# Build one tabbed panel from named HTML fragments, e.g.
#   cat(panelset("tab1", list(Table = table_html, Chart = img_html)))
# inside a `results='asis'` chunk. `id` must be unique per panelset on the page.
panelset <- function(id, tabs) {
  stopifnot(!is.null(names(tabs)), all(nzchar(names(tabs))))
  tab_ids <- paste0(id, "-", seq_along(tabs))
  buttons <- vapply(seq_along(tabs), function(i) {
    sprintf(
      '<button type="button" class="panel-tab%s" data-pane="%s">%s</button>',
      if (i == 1) " active" else "", tab_ids[i], names(tabs)[i]
    )
  }, character(1))
  panes <- vapply(seq_along(tabs), function(i) {
    sprintf(
      '<div class="panel-pane%s" id="%s">%s</div>',
      if (i == 1) " active" else "", tab_ids[i], tabs[[i]]
    )
  }, character(1))
  sprintf(
    '<div class="panelset" id="%s">\n<div class="panel-tabs">%s</div>\n%s\n</div>',
    id, paste(buttons, collapse = ""), paste(panes, collapse = "\n")
  )
}

# Render a ggplot to a PNG next to the post's other knitr figures and return
# an <img> tag referencing it (sized to match the document's default figure
# dimensions: fig.width=6.5in, fig.height=4in @ 192dpi retina).
panelset_img <- function(plot, path, alt = "") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(path, plot, width = 6.5, height = 4, units = "in", dpi = 192)
  sprintf('<img src="%s" alt="%s" />', path, alt)
}
