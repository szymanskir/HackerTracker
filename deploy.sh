rm .Rprofile
rm -rf renv
Rscript -e 'install.packages(c("renv", "devtools"))'
Rscript -e 'renv::restore()'
Rscript -e 'devtools::install_github("JohnCoene/sigmajs", force = TRUE)'
Rscript -e 'rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_NAME"), secret=Sys.getenv("SHINYAPP_SECRET"), token=Sys.getenv("SHINYAPP_TOKEN"))'
Rscript -e 'rsconnect::deployApp(appName="hackertracker", appFiles = grep(list.files(path=".", recursive = TRUE, all.files = T), pattern="renv|rsconnect|.Rproj|.Rprofile|.git|manifest.json", inv=T, value=T))'