![deploy-on-push-and-schedule](https://github.com/carpentries/assessment-archives/workflows/deploy-on-push-and-schedule/badge.svg)

# The Carpentries Survey Archives

The Carpentries uses surveys before and after workshops. This repository is an
automated archive of all the versions of the surveys that are being used. Each
time an edit to the survey is made, a new version will be created and the text
of the surveys will be archived in this repository.

The archive can be browsed at <https://carpentries.github.io/assessment-archives> or the content of this repository can be used to generate diffs between the different versions that are available.

## Repository organization

### Surveys

The surveys are both storred within the `main` branch and the `gh-pages` branch. Each survey is in a directory with a short name that describes the survey. Within these directories, the latest version occurs twice: once under its short name (the same as the directory) and once with its version number. Older versions only occur once using their version number.

The version number is a combination of (1) the date the version was created; (2) the first 6 characters of the SHA256 of the content of the file.


### Code

This repository contains 2 R script files: 

- `archive-typeform.R`: uses Typeform's API to get the content of the surveys
  and generate Markdown versions of their content if they have changed.
- `build-website.R`: moves the Markdown files generated `archive-typeform.R`   into a sub-directory

This code is run daily using GitHub actions. If new files are generated, they get deployed on GitHub pages.

The `DESCRIPTION` file makes it easy for the GitHub Action workflow to install all needed packages.

### Adding new surveys to the archive

(using this repository as the working directory)

1. check that
  `source("archive-typeform.R"); cache_content(get_form("XXXXX"))`
  runs well locally (new surveys may have new answer types that are not
  currently handled by the script).
1. add the survey to the GitHub Actions file (`.github/workflows/main.yml`).
2. add the details for this new survey in the `generate_index()` function (in
   the `build-website.R` file).
