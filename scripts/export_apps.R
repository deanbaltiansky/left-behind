# install.packages("shinylive")
library(shinylive)
library(fs)

apps <- c("study-aug25" = "app")

for (study in names(apps)) {
  appdir <- file.path(study, apps[[study]])
  dest   <- "docs"
  sub    <- file.path("studies", study, "app")
  if (dir_exists(appdir)) shinylive::export(appdir = appdir, destdir = dest, subdir = sub)
}
