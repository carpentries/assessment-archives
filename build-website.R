library(clisymbols)
library(crayon)
library(dplyr)
library(purrr)
library(glue)
library(tibble)

list_all_md_files <- function(path = ".", exclude = "README.md") {

  res <- list.files(
    path = path,
    pattern = "\\.md$",
    full.names = TRUE,
    recursive = TRUE
  )

  setdiff(res, file.path(path, exclude))

}

create_dir <- function(path = ".", dir) {
  if (! dir.exists(file.path(path, dir))) {
    dir.create(file.path(path, dir), recursive = TRUE)
    if (dir.exists(file.path(path, dir))) {
      message(
        crayon::green(clisymbols::symbol$tick),
        " ", sQuote(dir), " folder created."
      )
    } else {
      stop(
        crayon::red(clisymbols::symbol$croos),
        " failed to create ", sQuote(dir), " folder."
      )
    }
  } else {
    message(
      crayon::yellow(clisymbols::symbol$info),
      " ", sQuote(dir), " folder already exists."
    )
  }
  invisible(file.path(path, dir))
}

move_md_files <- function(current_path = ".", new_path = "public") {

  mds <- list_all_md_files(path = current_path)

  ## this works because it's a subfolder
  ## but it doesn't generalize well
  new_mds <- file.path(new_path, mds)

  create_dir(current_path, new_path)

  sub_dirs <- unique(dirname(new_mds))

  purrr::walk(sub_dirs, ~ create_dir(path = current_path, dir = .))
  purrr::walk2(mds, new_mds, file.copy)

}

extract_version <- function(filename) {
  pattern <- "(\\d{4}-\\d{2}-\\d{2}-[a-z0-9]{6})"

  purrr::map_chr(filename, function(.x) {
    if (grepl(pattern, .x)) {
      mtches <- regexpr(pattern, .x)
      regmatches(.x, mtches)
    } else {
      NA_character_
    }
  })
}

extract_archive_data <- function(path) {

  pths <- list_all_md_files(path = path)

  df_pths <- pths %>%
    strsplit("/") %>%
    purrr::map_dfr(~ list(
      survey   = .[[2]],
      filename = .[[3]]
    )) %>%
    dplyr::bind_cols(full_path = pths) %>%
    dplyr::mutate(
      version = dplyr::case_when(
        filename == paste0(survey, ".md") ~ "Current",
        TRUE ~ extract_version(filename)
      )
    )

}



generate_archive <- function(archive_data, survey_type, name) {

  res <- archive_data %>%
    dplyr::filter(.data$survey == survey_type)

  current <- res %>%
    dplyr::filter(version == "Current")

  archives <- res %>%
    dplyr::filter(version != "Current")

  if (! identical(nrow(current), 1L)) {
    stop(
      "Something is wrong with the data. There should be only ",
      "one row with data and there: ", nrow(current)
    )
  }

  current_version <- glue::glue(
    "[{ filename }]({ path }) (Current Version)",
    filename = current$filename,
    path = current$full_path
  )

  list_archive <- archives %>%
    dplyr::arrange(dplyr::desc(version)) %>%
    glue::glue_data(
            "- [{ filename }]({ full_path }) ({ version })",
      ) %>%
    glue::glue_collapse(sep = "\n")

  glue::glue("
### { name }

  - { current_version }

#### All versions

  { list_archive }
  ")
}

generate_index <- function(archive_data, output) {

  body <- tibble::tribble(
    ~survey_type,     ~name,
    "pre-workshop",  "Pre-workshop Survey",
    "post-workshop", "Post-workshop Survey",
    "instructor-teaching-online", "Post-workshop Instructor Teaching online",
    "long-term", "Long-term Survey",
    "online-teaching-bonus-pre", "Online Teaching Bonus Module Pre-Survey",
    "online-teaching-bonus-post", "Online Teaching Bonus Module Post-Survey"
  ) %>%
    purrr::pmap(function(survey_type, name) {
      generate_archive(
        archive_data = archive_data,
        survey_type = survey_type,
        name = name
      )}
    )

  intro <- glue::glue("# The Carpentries Survey Archives

  _Last updated: { date }_

  This website is an archive of the surveys used by The Carpentries.

  ",
  date = format(Sys.time())
  )

  res <- glue::glue_collapse(body, sep = "\n\n")
  cat(intro, res, sep = "\n", file = file.path(output, "index.md"))
}


generate_archive_website <- function(path = ".", new_path = "public") {

  move_md_files(current_path = path, new_path = new_path)

  archive_data <- extract_archive_data(path)
  generate_index(archive_data, output = new_path)

}
