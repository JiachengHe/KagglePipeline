#' @title Save the well-tuned models
#' @description Gather the models into a list and save them as .RData
#' @author Jiacheng He
#'
#'
#' @export



write_models_log <- function(model, model_name, file_name="models_log.RData", path="models/") {

  if (file_name %in% list.files(path)) {

    load(paste0(path, file_name))
    if (model_name %in% names(models)) {
      stop("This model_name already exists.")
    } else {
      models[[model_name]] <- model
    }

  } else {
    models <- list()
    models[[model_name]] <- model
  }

  save(models, file = paste0(path, file_name))
}
