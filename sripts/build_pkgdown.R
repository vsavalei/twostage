# scripts/build_pkgdown.R

detach("package:twostage", unload = TRUE)
pkgdown::build_site()
