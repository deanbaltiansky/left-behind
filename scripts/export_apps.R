# scripts/export_apps.R
# install.packages("shinylive")  # run once
library(shinylive)
library(fs)

apps <- c(
  "study-aug25" = "app"   # source at study-aug25/app
)

for (study in names(apps)) {
  appdir <- file.path(study, apps[[study]])                 # e.g., "study-aug25/app"
  dest   <- "docs"                                          # Pages root
  sub    <- file.path("studies", study, apps[[study]])      # e.g., "studies/study-aug25/app"
  
  if (!dir_exists(appdir)) {
    message("Skipping (not found): ", appdir)
    next
  }
  dir_create(file.path(dest, sub))
  message("Exporting ", appdir, " -> ", file.path(dest, sub))
  shinylive::export(appdir = appdir, destdir = dest, subdir = sub)
}
