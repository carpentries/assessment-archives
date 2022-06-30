library(clisymbols)
library(crayon)
library(digest)
library(dplyr)
library(glue)
library(httr)
library(jsonlite)
library(knitr)
library(kableExtra)
library(purrr)
library(readr)
library(tibble)

##' Get Form from Typeform
##'
##' From a Typeform identifier, returns the JSON file provided by Typeform's
##' API.
##'
##' @param form_id the form identifier
##' @return JSON
##' @export
get_form <- function(form_id) {
  raw <- httr::GET(
    "https://api.typeform.com/",
    path = glue::glue("forms/", "{form_id}")
  )

  ## test that result is JSON
  if (!identical(httr::http_type(raw), "application/json")) {
    stop("HTTP type isn't application/json. Has the API changed?")
  }

  jsonlite::fromJSON(
    httr::content(raw, type = "text", encoding = "utf-8"),
    simplifyVector = FALSE
  )

}

##' Extract Title from Survey
##'
##' @param tf the object returned by get_form()
##' @return the title for the survey
##' @export
get_form_title <- function(tf) {
  tf$title
}

## Remove new lines from survey questions.
process_field_title <- function(title) {
  gsub("\\n", " ", title)
}

## Create level 3 (or below) Markdown headers
create_header_level <- function(level) {
  paste0(rep("#", level + 2), collapse = "")
}

## Extract whether the question is required in the survey
is_required <- function(field) {
  dplyr::if_else(field$validations$required, "*", "")
}

## Build a table with the scale and its values
build_scale <- function(field) {

  if (field$properties$start_at_one) {
    min_scale <- 1
  } else {
    min_scale <- 0
  }

  n_steps <- field$properties$steps
  scale <- seq(from = min_scale, length.out = n_steps)

  if (exists("left", field$properties$labels)) {
    names(scale)[1] <- field$properties$labels$left
  }
  if (exists("right", field$properties$labels)) {
    names(scale)[length(scale)] <- field$properties$labels$right
  }
  if (exists("center", field$properties$labels)) {
    names(scale)[floor(mean(scale))] <- field$properties$labels$center
  }

  res <- t(tibble::enframe(scale))
  res[is.na(res)] <- ""
  colnames(res) <- rep("", ncol(res))
  rownames(res) <- NULL

  knitr::kable(res, align = "c") %>%
    kableExtra::kable_styling(position = "center")
}


## Check whether the multiple-choice question has an "other" option
has_other_option <- function(field) {
  if (identical(field$type, "multiple_choice") &&
        field$properties$allow_other_choice) {
    return("Other (open text)")
  }
}

## Extract possible choices for survey questions
## and turns them into an unordered Markdown list
extract_multiple_choices <- function(field) {
  choices <- c(
    purrr::map_chr(field$properties$choices, "label"),
    has_other_option(field)
  )
  glue::glue_collapse(glue::glue("- {choices} "), sep = "\n")
}


## Extract questions and their possible answers (when appropriate) from the
## survey.
extract_question <- function(field, level = 1) {

  if (field$type == "group") {
    append(
      glue::glue(
        "{header_level} {title}",
        header_level = create_header_level(level),
        title = process_field_title(field$title)
      ),
      purrr::map(field$properties$fields,
        ~ extract_question(., level = level + 1)
      )
    )
  } else if (field$type == "yes_no") {
    glue::glue(
      "{header_level} {title} {required}

       - yes
       - no
    ",
    header_level = create_header_level(level),
    required = is_required(field),
    title = process_field_title(field$title)
    )
  } else if (field$type == "short_text" || field$type == "long_text" ||
               field$type == "number") {
    glue::glue(
      "{header_level} {title} {required}

      ({ type } answer)
      "
     ,
      header_level = create_header_level(level),
      required = is_required(field),
      type = gsub("_", " ", field$type),
      title = process_field_title(field$title)
    )
  } else if (field$type == "statement") {
    glue::glue(
      "{header_level} {title}"
     ,
      header_level = create_header_level(level),
      title = process_field_title(field$title)
    )
  } else if (field$type == "opinion_scale") {
    paste(
      glue::glue(
        "{header_level} {title} {required} \n",
        header_level = create_header_level(level),
        required = is_required(field),
        title = process_field_title(field$title)
      ),
      glue::glue_collapse(build_scale(field), sep = "\n"),
      sep = "\n"
    )
  } else if (field$type == "multiple_choice" ||
               field$type == "dropdown") {
    glue::glue(
      "{header_level} { title } {required}

      {choices}
      ",
      header_level = create_header_level(level),
      required = is_required(field),
      choices = extract_multiple_choices(field),
      title = process_field_title(field$title)
    )
  } else {
    stop("unknown: ", field$type)
  }
}


## Extract the welcome screen
extract_welcome <- function(tf) {

  res <- map_chr(tf$welcome_screens, "title") %>%
    paste0(collapse = "\n")

  paste0(
    "### Welcome screen \n",
    res,
    collapse = "\n"
  )

}

## Extract the (first) thank you screen
extract_thankyou <- function(tf) {

  res <- tf$thankyou_screens[[1]]$"title"
  paste0(
    "### Thank you screen \n",
    res,
    collapse = "\n"
  )

}

## Iterate over the JSON returned by get_form to extract the questions into
## Markdown
extract_questions <- function(tf) {
  purrr::map(tf$fields, extract_question) %>%
    purrr::flatten() %>%
    glue_collapse(sep = "\n\n")
}


## Main function: extract the content of the survey into a Markdown string
extract_survey <- function(tf) {

  c(
    extract_welcome(tf),
    extract_questions(tf),
    extract_thankyou(tf)
  ) %>%
    glue_collapse(sep = "\n\n-----------\n\n")

}

## Generates a Markdown file from the content of the survey
make_md_file <- function(tf, title, out) {

  cnt <- extract_survey(tf)

  f <- tempfile(fileext = ".md")

  cat("# ", title, "\n",
    format(Sys.Date(), "%Y-%m-%d"), "\n\n",
    "----------------\n\n",
    cnt,
    sep = "",
    file = f)

  file.copy(f, out)

  if (file.exists(out)) {
    return(invisible(out))
  } else {
    stop("Something went wrong")
  }

}

hash_content <- function(tf) {
  cnt <- extract_survey(tf)

  digest::digest(cnt, algo = "sha256")
}

get_cache_file_name <- function(survey, cache_path) {
  file.path(cache_path, paste0(survey, ".csv"))
}

has_cache <- function(survey, cache_path) {
  file.exists(get_cache_file_name(survey, cache_path))
}

init_cache <- function(hash, filename, survey, cache_path) {
  tibble::tribble(
    ~hash, ~filename,
    hash, filename
  ) %>%
    write_cache(survey, cache_path)
}

read_cache <- function(survey, cache_path) {
  fn <- get_cache_file_name(survey, cache_path)
  readr::read_csv(fn, col_types = "cc")
}

write_cache <- function(tbl, survey, cache_path) {
  fn <- get_cache_file_name(survey, cache_path)
  readr::write_csv(tbl, file = fn)
}

add_to_cache <- function(hash, filename, survey, cache_path) {
  if (!has_cache(survey, cache_path)) {
    return(init_cache(hash, filename, survey, cache_path))
  }

  cc <- read_cache(survey, cache_path)
  cc %>%
    dplyr::bind_rows(
      tibble::tribble(
        ~hash, ~filename,
        hash, filename
      )
    ) %>%
    write_cache(survey, cache_path)
}

get_latest_hash <- function(survey, cache_path) {
  if (!has_cache(survey, cache_path)) {
    return(NULL)
  }

  read_cache(survey, cache_path) %>%
    dplyr::pull(.data$hash) %>%
    tail(1)

}


##' Cache the content of the survey
##'
##' Create new archive for survey if the content has changed since the last time
##' it was cached.
##'
##' @param tf object returned by `get_form`
##' @param survey the short name for the survey
##' @param title the full title for the survey
##' @param cache_path where the cache for the survey is located
##' @return
##' @export
cache_content <- function(tf, survey, title = get_form_title(tf),
                          cache_path = "cache") {

  latest_hash <- get_latest_hash(survey, cache_path)
  current_hash <- hash_content(tf)

  if (identical(current_hash, latest_hash)) {
    message(
      crayon::green(clisymbols::symbol$tick),
      " the survey hasn't changed, not creating new files."
    )
    invisible(NULL)
  } else {
    filename <- file.path(
      survey,
      paste0(survey, "-",
        format(Sys.Date(), "%Y-%m-%d"), "-",
        substr(current_hash, 1, 6),
        ".md"
      )
    )
    stable_filename <- file.path(dirname(filename), paste0(survey, ".md"))

    if (!dir.exists(dirname(filename))) {
      dir.create(dirname(filename))
    }

    out <- make_md_file(tf, title, out = filename)
    file.copy(out, stable_filename, overwrite = TRUE)
    message(
      crayon::green(clisymbols::symbol$tick),
      " created new files (",
      filename, ", ", stable_filename, ")."
    )
    add_to_cache(current_hash, basename(filename), survey, cache_path)
    message(
      crayon::green(clisymbols::symbol$tick),
      " adding hash to cache."
    )
  }
}
