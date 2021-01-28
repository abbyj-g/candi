#' model niches
#'
#' A function to create maxent species distribution models for large numbers of taxa.
#' @param occ_data A list of dataframes where each element of the list os a dataframe of occurrence data for an individual species
#' @param enviro_data A rasterstack of environmental rasters. Can be continuous or categorical data
#' @param reps The number of replicates done when modeling. Default is 3
#' @param reptype specifies the Maxent replicatetype to use.Can be "bootstrap", "subsample", or "crossvalidate". Default set to "bootstrap"
#' @param output_dir The output directory of the returned object
#' @return A list of maxent objects for each species and the associated raster of each model
model_niches <- function(occ_data, enviro_data, reps = 3, reptype = "bootstrap", output_dir = "") { #reptype can equal Crossvalidate, Bootstrap, or Subsample

  maxent_objs <- list()
  predictions <- list()

  for (i in 1:length(occ_data)) {
    ##### Prepare occurrence data for each species


      a <- dplyr::select(occ_data[[i]], longitude = longitude, latitude = latitude)
      sp <- raster::as.matrix(a)

    sp_name <- sub(" ", "_", names(occ_data[i]))

    # only proceed if this isn't a duplicate entry

    if (sp_name %in% names(maxent_objs) == FALSE) {

      # print progress in loop

      print(paste0("Working on ", sp_name, " now!"))
      print(paste0("This is species number ", i))


      #####  MAXENT modeling

      mx <- dismo::maxent(clim_data, sp, removeDuplicates=TRUE,
                   replicates=reps, replicatetype=reptype)
      maxent_objs[[i]] <- mx
      names(maxent_objs)[i] <- sp_name
      # check to make sure at least some variables are contributing to model (otherwise it will fail)
      total_perc_contribution <- sum(var.importance(mx)[,2])

      if (total_perc_contribution != 0) {

        tryCatch({

          # set extent based on location of points, plus a 5% buffer
          e2 <- raster::extent(sp) * 1.05

          # save to working directory
          model <- dismo::predict(mx, clim_data, ext=e2,
                           filename = paste0(output_dir, sp_name, ".asc"),
                           overwrite = TRUE,
                           progress='text')

          predictions[[i]] <- model
          names(predictions)[i] <- sp_name

        })
      }
      else {

        predictions[[i]] <- NA
        names(predictions)[i] <- sp_name

        predictions[[i]] <- NA
        names(predictions)[i] <- sp_name
      }
    }
    else {

      predictions[[i]] <- NA
      names(predictions)[i] <- NA

    }

  }
  return(list(maxent_objs, predictions))
}
