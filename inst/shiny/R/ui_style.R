# Application styling ----------------------------------------------------

gqr_css <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .panel-primary > .panel-heading {
        background-color: #375a7f;
        border-color: #375a7f;
        color: #ffffff;
      }
      .q-container {
        max-width: 1200px;
        margin-left: 60px;
        margin-right: auto;
        padding-bottom: 35px;
      }
      .q-panel-header {
        font-size: 1.2em;
        font-weight: 600;
        color: #ececf0;
        cursor: pointer;
        text-decoration: underline;
        margin-bottom: 8px;
      }
      .q-panel-header:hover {
        color: #ffffff;
        text-decoration: none;
      }
      .q-panel-header i {
        margin-right: 6px;
      }
      .q-panel {
        padding: 10px 15px;
      }
      .q-home {
        padding-top: 20px;
      }
      .q-home .lead {
        max-width: 950px;
        margin-bottom: 24px;
      }
      .q-home-card {
        min-height: 175px;
        padding: 15px 18px;
        margin-bottom: 18px;
        border: 1px solid #d9d9d9;
        border-radius: 4px;
        background: #f8f9fa;
      }
      .q-home-start {
        margin-top: 12px;
        padding: 12px 16px;
        border-left: 4px solid #375a7f;
        background: #f2f5f8;
      }
      details.gqr-info-box {
        margin: 12px 0 18px 0;
        border: 1px solid #b8cee3;
        border-radius: 6px;
        background: #eef6fc;
      }
      details.gqr-info-box > summary {
        padding: 11px 14px;
        cursor: pointer;
        font-weight: 700;
        font-size: 1.08em;
        color: #ffffff;
        background: #375a7f;
        border-radius: 5px;
        list-style-position: inside;
      }
      details.gqr-info-box[open] > summary {
        border-bottom-left-radius: 0;
        border-bottom-right-radius: 0;
      }
      .gqr-info-box-body {
        padding: 12px 16px 8px 16px;
        color: #263746;
      }
      .gqr-info-box-body p:last-child,
      .gqr-info-box-body ul:last-child {
        margin-bottom: 6px;
      }
      @media (max-width: 768px) {
        .q-container {
          margin-left: 0;
          padding-left: 12px;
          padding-right: 12px;
        }
      }
    "))
  )
}

# Reusable collapsible information panel used on analytical tabs.
gqr_info_box <- function(title, ..., open = TRUE) {
  shiny::tags$details(
    class = "gqr-info-box",
    open = if (isTRUE(open)) "open" else NULL,
    shiny::tags$summary(title),
    shiny::div(class = "gqr-info-box-body", ...)
  )
}
