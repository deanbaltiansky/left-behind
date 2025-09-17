# scripts/export_apps.R
# Export every Shiny app from top-level study-*/app into docs/studies/<study>/app

# install.packages("fs"); install.packages("shinylive")  # run once if needed
library(fs)
library(shinylive)

# Find only top-level study folders, then append "app"
study_dirs <- dir_ls(".", type = "directory", glob = "study-*", recurse = FALSE)
app_dirs <- path(study_dirs, "app")
app_dirs <- app_dirs[dir_exists(app_dirs)]  # keep only existing app/ dirs

for (app_dir in app_dirs) {
  # Validate it's a real Shiny app
  has_app <- file_exists(path(app_dir, "app.R"))
  has_pair <- file_exists(path(app_dir, "server.R")) && file_exists(path(app_dir, "ui.R"))
  if (!has_app && !has_pair) {
    message("⏭️  Skipping (no app.R/ui.R+server.R): ", app_dir)
    next
  }
  
  study_name <- path_file(path_dir(app_dir))                 # e.g., "study-sep25"
  out_dir    <- path("docs", "studies", study_name, "app")   # target
  
  message("🚀 Exporting ", app_dir, "  -->  ", out_dir)
  if (dir_exists(out_dir)) dir_delete(out_dir)
  dir_create(out_dir, recurse = TRUE)
  
  shinylive::export(app_dir, out_dir)
}

message("✅ Done. Apps are under docs/studies/<study>/app")
