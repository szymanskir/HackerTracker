# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

future::plan(future::multiprocess)
pkgload::load_all()
options( "golem.app.prod" = TRUE)
shiny::shinyApp(ui = app_ui, server = app_server)
