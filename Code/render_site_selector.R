# Batch/single-site rendering support for site_selector.Rmd.

# ---- Run settings ----
# Default: render every research site found in the current data.
site_selector_run_all_sites <- TRUE
site_selector_single_site <- NULL

# For a quick single-site inspection, set site_selector_run_all_sites <- FALSE
# and uncomment exactly one site_selector_single_site line below.
# site_selector_single_site <- 'ARDEC 2200'
# site_selector_single_site <- 'Kerbel'
# site_selector_single_site <- 'Upper Yampa'
# site_selector_single_site <- 'Legacy'
# site_selector_single_site <- 'AVRC Star'
# site_selector_single_site <- 'AVRC Cowpea'
# site_selector_single_site <- 'Barley'
# site_selector_single_site <- 'Berthoud'
# site_selector_single_site <- 'Big Hollow'
# site_selector_single_site <- 'Boulder Lake'
# site_selector_single_site <- 'Gunnison'
# site_selector_single_site <- 'Molina'
# site_selector_single_site <- 'Fruita NT'
# site_selector_single_site <- 'Fruita W'
# site_selector_single_site <- 'Fruita B'
# site_selector_single_site <- 'Fruita F'
# site_selector_single_site <- 'Fruita C'
# site_selector_single_site <- 'Fruita A'
# site_selector_single_site <- 'Stagecoach'
# site_selector_single_site <- 'North Hunt Creek'
# site_selector_single_site <- 'Jay Whaley Ranch'
# site_selector_single_site <- 'Yampa 2'
# site_selector_single_site <- 'Yellow Jacket'
# site_selector_single_site <- 'Lab Blank'
# site_selector_single_site <- 'ARDEC South - Conv'
# site_selector_single_site <- 'ARDEC South - Org'
# site_selector_single_site <- 'North Sand Creek'
# site_selector_single_site <- 'Knott Livestock'
# site_selector_single_site <- 'CEAP Boxelder'
# site_selector_single_site <- 'Method Blank'
# site_selector_single_site <- 'Lab Control Sample'

.site_selector_safe_name <- function(x) {
  safe <- trimws(as.character(x))
  safe <- gsub('[<>:"/\\\\|?*]', "-", safe)
  safe <- gsub("[[:space:]]+", " ", safe)
  safe <- gsub("[. ]+$", "", safe)

  if (!nzchar(safe)) {
    stop("A site name could not be converted to a valid folder name.")
  }

  safe
}

render_site_selector <- function(input_file,
                                 encoding = "UTF-8",
                                 run_all_sites = TRUE,
                                 single_site = NULL) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the site selector.")
  }

  if (isTRUE(run_all_sites) && !is.null(single_site)) {
    stop(
      "A single site is selected while run_all_sites is TRUE. ",
      "Set run_all_sites <- FALSE before uncommenting a single-site line."
    )
  }

  input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)
  repo_dir <- normalizePath(
    file.path(dirname(input_file), ".."),
    winslash = "/",
    mustWork = TRUE
  )
  output_root <- file.path(repo_dir, "site_selector_temporary_results")
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  old_working_directory <- getwd()
  on.exit(setwd(old_working_directory), add = TRUE)
  setwd(repo_dir)

  message("Building the cleaned data once for the site-selector run...")
  source(file.path(repo_dir, "Code", "file-merger.R"), local = environment())
  site_selector_data <- returnAllFiles(d = directory, export = FALSE)
  names(site_selector_data) <- tolower(names(site_selector_data))

  if (!"location.name" %in% names(site_selector_data)) {
    stop("The cleaned data do not contain the required 'location.name' column.")
  }

  available_locations <- sort(unique(trimws(as.character(
    site_selector_data$location.name
  ))))
  available_locations <- available_locations[
    !is.na(available_locations) & nzchar(available_locations)
  ]

  qa_locations <- c("Lab Blank", "Method Blank", "Lab Control Sample")
  research_sites <- setdiff(available_locations, qa_locations)

  if (isTRUE(run_all_sites)) {
    locations_to_render <- research_sites
    if (!length(locations_to_render)) {
      stop("No research sites were found in the current cleaned data.")
    }
  } else {
    if (is.null(single_site) || length(single_site) != 1L || !nzchar(single_site)) {
      stop(
        "Single-site mode requires exactly one uncommented single_site line ",
        "at the top of site_selector.Rmd."
      )
    }
    if (!single_site %in% available_locations) {
      stop(
        "The selected site is not present in the current data: ", single_site,
        "\nAvailable locations: ", paste(available_locations, collapse = ", ")
      )
    }
    locations_to_render <- single_site
  }

  data_rds <- tempfile("site_selector_data_", fileext = ".rds")
  saveRDS(site_selector_data, data_rds)
  on.exit(unlink(data_rds), add = TRUE)

  run_date <- format(Sys.Date(), "%Y-%m-%d")
  batch_results <- vector("list", length(locations_to_render))

  for (i in seq_along(locations_to_render)) {
    location <- locations_to_render[[i]]
    safe_location <- .site_selector_safe_name(location)

    if (isTRUE(run_all_sites)) {
      report_dir <- file.path(
        output_root,
        paste0(safe_location, "_", run_date)
      )
      html_name <- paste0(
        "site_selector_", gsub(" ", "_", safe_location), "_", run_date, ".html"
      )
    } else {
      # Preserve the existing quick-inspection behavior for a manual site run.
      report_dir <- output_root
      html_name <- "site_selector.html"
    }

    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
    message(
      sprintf(
        "Rendering site %d of %d: %s",
        i, length(locations_to_render), location
      )
    )

    render_error <- NULL
    rendered_file <- tryCatch(
      rmarkdown::render(
        input = input_file,
        encoding = encoding,
        output_file = html_name,
        output_dir = report_dir,
        params = list(
          location = location,
          output_dir = normalizePath(
            report_dir, winslash = "/", mustWork = FALSE
          ),
          data_rds = normalizePath(data_rds, winslash = "/", mustWork = TRUE),
          run_date = run_date
        ),
        envir = new.env(parent = globalenv()),
        quiet = FALSE
      ),
      error = function(e) {
        render_error <<- conditionMessage(e)
        NA_character_
      }
    )

    batch_results[[i]] <- data.frame(
      location = location,
      run_date = run_date,
      status = if (is.null(render_error)) "completed" else "failed",
      output_folder = normalizePath(
        report_dir, winslash = "/", mustWork = FALSE
      ),
      html_file = if (is.null(render_error)) rendered_file else NA_character_,
      message = if (is.null(render_error)) "" else render_error,
      stringsAsFactors = FALSE
    )
  }

  batch_summary <- do.call(rbind, batch_results)

  if (isTRUE(run_all_sites)) {
    summary_file <- file.path(
      output_root,
      paste0("site_selector_batch_summary_", run_date, ".csv")
    )
    utils::write.csv(batch_summary, summary_file, row.names = FALSE, na = "")
    message("Batch summary saved to: ", summary_file)

    failed_locations <- batch_summary$location[batch_summary$status == "failed"]
    if (length(failed_locations)) {
      stop(
        "Site-selector batch completed with failures for: ",
        paste(failed_locations, collapse = ", "),
        ". See the batch summary CSV for details."
      )
    }

    message(
      "Site-selector batch completed successfully for ",
      nrow(batch_summary), " research sites."
    )
    # Return an HTML report so RStudio's Knit button has a valid preview target.
    return(invisible(batch_summary$html_file[[1]]))
  }

  if (batch_summary$status[[1]] == "failed") {
    stop(batch_summary$message[[1]])
  }

  invisible(batch_summary$html_file[[1]])
}
