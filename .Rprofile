
if (interactive()) {
  path_r_profile <- '~/.Rprofile'
  if (file.exists(path_r_profile)) {
    source(path_r_profile)
  }
  rm('path_r_profile')
  
  .library_silently <- function(...) {
    suppressWarnings(suppressPackageStartupMessages(base::library(...)))
  }
  
  .library_silently(devtools)
  .library_silently(usethis)
  # .library_silently(travis)
  # .library_silently(pkgdown)
  rm('.library_silently')
  
  devtools::load_all()
}
