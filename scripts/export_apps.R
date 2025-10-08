# scripts/export_apps.R
# Export Shiny apps from:
#   1) top-level:   study-*/app
#   2) nested apps: study-*/app/*/  (each subdir that contains a Shiny app)
#
# Output:
#   docs/studies/<study>/app                 (for the top-level app)
#   docs/studies/<study>/app/<subapp-name>   (for nested apps)

# install.packages("fs"); install.packages("shinylive")  # run once if needed
library(fs)
library(shinylive)

is_shiny_app_dir <- function(d) {
  file_exists(path(d, "app.R")) ||
    (file_exists(path(d, "ui.R")) && file_exists(path(d, "server.R")))
}

export_one_app <- function(src_dir, dst_dir) {
  message("  → Exporting: ", src_dir, "  -->  ", dst_dir)
  if (dir_exists(dst_dir)) dir_delete(dst_dir)
  dir_create(dst_dir, recurse = TRUE)
  shinylive::export(src_dir, dst_dir)
}

# Find study folders
study_dirs <- dir_ls(".", type = "directory", glob = "study-*", recurse = FALSE)

for (study_dir in study_dirs) {
  app_root <- path(study_dir, "app")
  if (!dir_exists(app_root)) {
    message("⏭️  No app/ dir in: ", study_dir)
    next
  }
  
  study_name <- path_file(study_dir)
  parent_out <- path("docs", "studies", study_name, "app")
  if (!dir_exists(parent_out)) dir_create(parent_out, recurse = TRUE)
  
  # 1) Export top-level app (keeps prior convention)
  if (is_shiny_app_dir(app_root)) {
    message("🚀 Top-level app for ", study_name)
    export_one_app(app_root, parent_out)
  } else {
    message("ℹ️  No top-level app.R/ui.R+server.R in ", app_root)
    # ensure parent_out exists for subapps
    if (!dir_exists(parent_out)) dir_create(parent_out, recurse = TRUE)
  }
  
  # 2) Export nested apps under app/*/
  sub_dirs <- dir_ls(app_root, type = "directory", recurse = FALSE)
  if (length(sub_dirs)) {
    message("🔎 Scanning nested apps in ", app_root)
  }
  
  for (sub in sub_dirs) {
    if (!is_shiny_app_dir(sub)) {
      message("   ⏭️  Skipping (not a Shiny app): ", sub)
      next
    }
    sub_name <- path_file(sub)  # e.g., "lm-table-app"
    out_dir  <- path(parent_out, sub_name)
    export_one_app(sub, out_dir)
  }
}

message("✅ Done. Outputs are under docs/studies/<study>/app[/<subapp>]")
