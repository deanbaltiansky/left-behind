# scripts/export_apps.R
# install.packages("shinylive")   # run once if needed
library(fs)
library(shinylive)

app_dirs <- dir_ls(".", recurse = TRUE, type = "directory",
                   regexp = "study-[^/]+/app$")

for (app_dir in app_dirs) {
  study_name <- path_file(path_dir(app_dir))                 # e.g., "study-sep25"
  out_dir    <- path("docs", "studies", study_name, "app")   # docs/studies/<study>/app
  
  message("Exporting ", app_dir, " --> ", out_dir)
  if (dir_exists(out_dir)) dir_delete(out_dir)
  dir_create(out_dir, recurse = TRUE)
  
  shinylive::export(app_dir, out_dir)  # writes index.html + wasm assets
}
message("✅ All apps exported with shinylive")
