# scripts/export_apps.R
# Export all Shiny apps in study-* folders to docs/

library(fs)

# find all "app" folders under study-*
app_dirs <- dir_ls(".", recurse = TRUE, type = "directory", regexp = "study-[^/]+/app$")

if (!dir_exists("docs")) dir_create("docs")

for (app_dir in app_dirs) {
  study_name <- path_file(path_dir(app_dir))   # e.g., "study-sep25"
  out_dir <- path("docs", study_name)
  
  message("Exporting app from ", app_dir, " --> ", out_dir)
  
  if (dir_exists(out_dir)) dir_delete(out_dir)
  dir_create(out_dir, recurse = TRUE)
  
  # copy everything inside app_dir to docs/study-xx/
  dir_copy(app_dir, out_dir, overwrite = TRUE)
}

message("✅ All apps exported to docs/")
