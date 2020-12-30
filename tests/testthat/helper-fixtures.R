
fixture <- local({

  hash <- function(code) {
    tmp <- tempfile()
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    writeBin(charToRaw(code), tmp)
    tools::md5sum(tmp)[[1]]
  }

  get_test_data <- function(expr) {
    code <- enc2utf8(paste0(deparse(expr, backtick = TRUE), collapse = "\n"))
    hash <- hash(code)
    snap <- getOption("testthat.snapshotter")
    list(
      root = testthat::test_path(),
      expr = expr,
      code = code,
      hash = hash,
      file = snap$file,
      test = snap$test,
      seen = snap$test_file_seen
    )
  }

  mkdirp <- function(dir) {
    s <- vlapply(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
    invisible(s)
  }

  find_test_file <- function(ctx, root) {
    ptn <- paste0("^test[-_]", ctx, "[.][Rr]")
    file <- grep(ptn, dir(root), value = TRUE)[1]
    if (is.na(file)) stop("Cannot determine test file for fixtures")
    file
  }

  context_name <- function(filename) {
    # Remove test- prefix
    filename <- sub("^test[-_]", "", filename)
    # Remove terminal extension
    filename <- sub("[.][Rr]$", "", filename)
    filename
  }

  rstudio_tickle <- function() {
    if (!requireNamespace("rstudioapi", quietly = TRUE)) {
      return()
    }

    if (!asNamespace("rstudioapi")$hasFun("executeCommand")) {
      return()
    }

    asNamespace("rstudioapi")$executeCommand("vcsRefresh")
    asNamespace("rstudioapi")$executeCommand("refreshFiles")
  }

  read_lines <- function(path, n = -1L, encoding = "UTF-8") {
    base::readLines(path, n = n, encoding = encoding, warn = FALSE)
  }

  write_lines <- function(text, path) {
    base::writeLines(enc2utf8(text), path, useBytes = TRUE)
  }

  fix_to_md <- function(data) {
    h2 <- paste0("# ", names(data), "\n\n")
    code_block <- function(x) paste0(indent_add(x), collapse = "\n\n---\n\n")
    data <- vapply(data, code_block, character(1))

    paste0(h2, data, "\n\n", collapse = "")
  }

  indent_add <- function(x, prefix = "    ") {
    paste0(prefix, gsub("\n", paste0("\n", prefix), x, fixed = TRUE))
  }

  compact <- function(x) {
    x[lengths(x) > 0]
  }

  new_fixture_reporter <- function(data) {
    FixtureReporter <- R6::R6Class(
      "FixtureReporter",
      inherit = testthat::Reporter,
      public = list(
        fix_dir = "_fixtures",
        file = NULL,
        test = NULL,
        cur_fixs = NULL,
        files_seen = NULL,
        hashes_seen = NULL,

        initialize = function(files_seen = character()) {
          self$files_seen <- files_seen
        },

        start_file = function(path, test = NULL) {
          self$files_seen <- c(self$files_seen, path)
          self$file <- context_name(path)
          self$cur_fixs <- list()
          if (!is.null(test)) {
            self$start_test(NULL, test)
          }
        },

        start_test = function(context, test) {
          self$test <- test

          if (length(self$cur_fixs[[test]]) > 0) {
            warning("Duplicate test, discarding previous fixtures")
          }
          self$cur_fixs[[test]] <- list()
        },

        add_fixture = function(hash, code) {
          value <- paste(collapse = "\n", c(
            "Hash", paste0("  ", hash),
            "Code", paste0("  ", strsplit(code, "\n", fixed = TRUE)[[1]])
          ))
          self$hashes_seen <- c(self$hashes_seen, hash)
          self$cur_fixs <- self$fix_append(self$cur_fixs, value)
        },

        end_file = function() {
          dir.create(self$fix_dir, showWarnings = FALSE)
          self$fixs_write(self$cur_fixs)
        },

        end_reporter = function() {
          tests <- testthat::find_test_scripts(".", full.names = FALSE)
          if (all(tests %in% self$files_seen)) {
            rds <- dir(self$fix_dir, pattern = "[.]rds$")
            seen <- paste0(self$hashes_seen, ".rds")
            unused <- setdiff(rds, seen)
            if (length(unused) > 0) {
              message("Deleting unused fixtures: ", unused)
              unlink(file.path(self$fix_dir, unused))
            }
          }
          if (length(dir(self$fix_dir)) == 0) {
            unlink(self$fix_dir, recursive = TRUE)
          }
          rstudio_tickle()
        },

        fix_append = function(data, fix) {
          data[[self$test]] <- c(data[[self$test]], list(fix))
          data
        },

        # File management ----------------------------------------------------------

        fixs_write = function(data, suffix = "") {
          data <- compact(data)
          if (length(data) > 0) {
            out <- fix_to_md(data)
            # trim off last line since write_lines() adds one
            out <- gsub("\n$", "", out)
            write_lines(out, self$fix_path(suffix))
          } else {
            self$fixs_delete(suffix)
          }
        },

        fixs_delete = function(suffix = "") {
          unlink(self$fix_path(suffix))
        },

        fix_path = function(suffix = "") {
          file.path(self$fix_dir, paste0(self$file, suffix, ".md"))
        }
      )
    )

    fixer <- FixtureReporter$new(data$xseen)

    fixer
  }

  setup_reporter <- function(data) {
    rep <- testthat::get_reporter()
    if (!isTRUE(rep$capabilities$fixture_setup)) {
      rep$capabilities$fixture_setup <- TRUE
      if (class(rep)[1] != "MultiReporter") return()
      fixer <- new_fixture_reporter(data)
      file <- find_test_file(data$file, data$root)
      fixer$start_file(file, data$test)
      rep$reporters <- c(rep$reporters, list(fixer))
    }
    wfix <- sapply(rep$reporters, inherits, "FixtureReporter")
    if (sum(wfix) != 1) stop("Must be exactly one FixtureReporter")
    fixer <- rep$reporters[[which(wfix)]]
    fixer$add_fixture(data$hash, data$code)
  }

  get <- function(expr, envir = parent.frame()) {
    data <- get_test_data(substitute(expr))
    if (!is.null(data$file)) setup_reporter(data)
    fxfile <- paste0(data$hash, ".rds")
    fxpath <- file.path(data$root, "_fixtures", fxfile)
    upd <- isTRUE(getOption("fixtures.update"))
    upd1 <- isTRUE(getOption("fixtures.update1"))
    if (upd || upd1) {
      if (upd1) options(fixture.update1 = NULL)
      value <- force(expr)
      mkdirp(dirname(fxpath))
      jsfile <- paste0(data$hash, ".json")
      jspath <- file.path(data$root, "_fixtures", jsfile)
      saveRDS(value, fxpath, version = 2L)
      value

    } else {
      tryCatch(
        suppressWarnings(readRDS(fxpath)),
        error = function(err) {
          stop("Cannot read fixture file `", fxpath, "`")
        }
      )
    }
  }

  update <- function(files = NULL) {
    opt <- options(fixtures.update = TRUE)
    on.exit(options(opt), add = TRUE)
    testthat::test_local(filter = files)
  }

  update_next <- function() {
    options(fixtures.update1 = TRUE)
  }

  list(
    get = get,
    update = update,
    update_next = update_next,
    .internal = environment()
  )
})
