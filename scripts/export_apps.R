# scripts/export_apps.R
library(fs)

app_dirs <- dir_ls(".", recurse = TRUE, type = "directory", regexp = "study-[^/]+/app$")

for (app_dir in app_dirs) {
  study_name <- path_file(path_dir(app_dir))                  # e.g., "study-sep25"
  out_dir    <- path("docs", "studies", study_name, "app")    # docs/studies/<study>/app
  
  message("Exporting ", app_dir, " --> ", out_dir)
  if (dir_exists(out_dir)) dir_delete(out_dir)
  dir_create(out_dir, recurse = TRUE)
  dir_copy(app_dir, out_dir, overwrite = TRUE)
}

message("✅ All apps exported under docs/studies/<study>/app/")
