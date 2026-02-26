# R/06_food_behavior/99_organize_outputs.R
# Organize food behavior outputs into dedicated folders.

source("config/paths.R", local = TRUE)

fb_out <- file.path(ROOT, "outputs", "food_behavior")
fb_tab <- file.path(fb_out, "tables")
fb_fig <- file.path(fb_out, "figures")
if (!dir.exists(fb_tab)) dir.create(fb_tab, recursive = TRUE)
if (!dir.exists(fb_fig)) dir.create(fb_fig, recursive = TRUE)

move_matches <- function(src_dir, pattern, dst_dir) {
  fs <- list.files(src_dir, pattern = pattern, full.names = TRUE)
  if (length(fs) == 0) return(invisible(NULL))
  for (f in fs) {
    file.rename(f, file.path(dst_dir, basename(f)))
  }
}

move_matches(file.path(ROOT, "outputs", "tables"), "^food_behavior_.*\\.csv$", fb_tab)
move_matches(file.path(ROOT, "outputs", "figures"), "^food_behavior_.*\\.png$", fb_fig)

message("Organized food behavior outputs into outputs/food_behavior/{tables,figures}")
