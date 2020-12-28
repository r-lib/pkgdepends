
fixture <- local({

  hash <- function(obj) {
    tmp <- tempfile()
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    code <- rlang::quo_text(obj)
    dump <- serialize(code, NULL, version = 2)
    # Skip the header, because it contains the R version that created it
    writeBin(dump[-(1:9)], tmp)
    tools::md5sum(tmp)[[1]]
  }

  test_name <- function(x) {
    x <- tolower(x)
    gsub("[^a-z0-9]+", "-", x)
  }

  get_test_data <- function() {
    snap <- getOption("testthat.snapshotter")
    list(
      root = testthat::test_path(),
      file = snap$file,
      test = snap$test
    )
  }

  mkdirp <- function(dir) {
    s <- vlapply(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
    invisible(s)
  }

  get <- function(expr, envir = parent.frame()) {
    where <- get_test_data()
    hsh <- hash(substitute(expr))
    fxfile <- paste0(hsh, ".rds")
    fxpath <- file.path(where$root, "_fixtures", fxfile)

    upd <- isTRUE(getOption("fixtures.update"))
    upd1 <- isTRUE(getOption("fixtures.update1"))
    if (upd || upd1) {
      if (upd1) options(fixture.update1 = NULL)
      value <- force(expr)
      mkdirp(dirname(fxpath))
      jsfile <- paste0(hsh, ".json")
      jspath <- file.path(where$root, "_fixtures", jsfile)
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
