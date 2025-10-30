app_theme <- bs_theme(bootswatch = "minty", base_font = bslib::font_google("Inter"))

ui <- page_navbar(
  title = "Portfolio Risk Lab",
  theme = app_theme,
  fillable = TRUE,
  nav_panel("Optimize", portfolio_ui("optimize")),
  nav_panel("Explore", signal_explorer_ui("explore")),
  nav_panel("Signal Lab", signal_lab_ui("lab")),
  nav_panel("Risk Dashboard", risk_dashboard_ui("risk")),
  nav_panel("Compliance", compliance_ui("compliance")),
  nav_spacer(),
  nav_item(a(href = "https://finance.yahoo.com", target = "_blank", "Data via Yahoo Finance"))
)
