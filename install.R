# install all dependencies
renv::install(prompt = FALSE)
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs(force=TRUE)
}
  
