server <- function(input, output, session) {
  portfolio_server("optimize")
  signal_explorer_server("explore")
  signal_lab_server("lab")
  risk_dashboard_server("risk")
  compliance_server("compliance")
}
