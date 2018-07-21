#' @title Save the well-tuned models
#' @description Gather the models into a list and save them as .RData
#' @author Jiacheng He
#'
#' @importFrom dplyr bind_rows
#' @export



write_model <- function(model, model_name, note=NA, models_file="models.RData", log_file="log.RData", path="model/") {

  if (models_file %in% list.files(path)) {
    load(paste0(path, models_file))
    if (model_name %in% names(models)) {
      input_1 <- readline(prompt = "This model_name already exists. Would you like to overwrite it? (y/n): ")
      if (input_1 == "y") {
        models[[model_name]] <- model
        cat("You overwrite an existing model.")
      } else {
        cat("The model is not saved.")
      }
    } else { models[[model_name]] <- model }
  } else {
    models <- list()
    models[[model_name]] <- model
  }
  save(models, file = paste0(path, models_file))


  bestTuneID = as.numeric(row.names(model$bestTune))
  bestResult <- model$results[bestTuneID, ]
  perfNames <- model$perfNames
  perfNamesSD <- paste0(perfNames, "SD")

  new_model_log <- data_frame(name = model_name, method = model$method, tuneLength = model$call$tuneLength,
                           bestTuneID = bestTuneID, note = note)
  new_model_log[, perfNames] <- bestResult[, perfNames]
  new_model_log[, perfNamesSD] <- bestResult[, perfNamesSD]

  if (log_file %in% list.files(path)) {
    load(paste0(path, log_file))
    if (model_name %in% models_log$name) {
      input_2 <- readline(prompt = "This model_name already exists in the log. Would you like to save it in the log table? (y/n): ")
      if (input_2 == "y") {
        models_log <- bind_rows(models_log, new_model_log)
        cat("Note: There are duplicate model names in the log table.")
      } else {
        cat("The model is not saved to the log table.")
      }
    } else { models_log <- bind_rows(models_log, new_model_log) }
  } else {
    models_log <- new_model_log
  }
  save(models_log, file = paste0(path, log_file))

  invisible(NULL)
}
