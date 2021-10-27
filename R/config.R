
#' Package configuration (internals)
#'
#' @noRd
#' @details

config <- local({

  `%||%` <- function(l, r) if (is.null(l)) r else l

  # It is used as the check function if no check function is needed
  true <- function(...) TRUE

  standard_name <- function(x) {
    gsub("-", "_", x)
  }

  # Built-in types
  builtin_types <- c("character", "count", "custom", "flag", "string", "string_or_null")

  # Checks for builtin types
  builtin_checks <- list(
    character = function(x) is.character(x) && !anyNA(x),
    custom = function(x) true(x),
    flag = function(x) is_flag(x),
    string = function(x) is_string(x),
    string_or_null = function(x) is_string_or_null(x)
  )

  # Builtin environment variable decoders
  builtin_env_decode <- list(
    character = function(x, ...) {
      strsplit(x, ";", fixed = TRUE)[[1]]
    },
    custom = function(x, name) {
      stop("Don't know how to decode config value from `", name, "`")
    },
    flag = function(x, name) {
      x <- tolower(x)
      if (tolower(x) %in% c("yes", "true", "1", "on")) return(TRUE)
      if (tolower(x) %in% c("no", "false", "0", "off")) return(FALSE)
      stop(
        "Invalid value for `", name, "` envirironment variable, ",
        "must be `true` or `false`."
      )
    },
    string = function(x, ...) x,
    string_or_null = function(x, ...) if (identical(x, "NULL")) NULL else x
  )

  get_internal <- function(env, name) {
    stopifnot(is_string(name))
    name <- standard_name(name)
    if (!name %in% names(env$data)) stop("No such option: `", name, "`")

    rec <- env$data[[name]]

    # was explicitly set?
    if (!is.null(rec$value)) return(list("set", rec$value))

    # set via options()
    optname <- paste0(env$prefix, name)
    opt <- getOption(optname)
    if (!is.null(opt)) {
      if (!is.null(chk <- env$data[[name]]$check)) chk(opt)
      return(list("option", opt))
    }

    # set via env var
    envvname <- toupper(chartr(".", "_", paste0(env$prefix, name)))
    envv <- Sys.getenv(envvname, NA_character_)
    if (!is.na(envv)) {
      return(list("envvar", rec$env_decode(envv)))
    }

    # otherwise the default, but if it is a function, then call it
    def <- rec$default
    if (is.function(def)) {
      def <- def()
      if (!is.null(chk <- env$data[[name]]$check)) chk(def)
    }
    list("default", def)
  }

  #' ## Crate a new configuration
  #'
  #' Typically `new_config()` is called outside of the functions of the
  #' package, so the `config` object is created at install time.
  #'
  #' ### Arguments
  #' * `prefix` the default prefix is the name of the calling package.
  #'
  #' ### Value
  #' `new_config()` returns a `config` object, which is a list containing
  #' the configuration data and functions (methods) to query and change it.
  #'
  #' ```r
  #' conf <- config$new()
  #' ```
  #'
  #' ### Methods

  new_config <- function(prefix = utils::packageName(parent.frame())) {
    env <- new.env(parent = emptyenv())
    env$prefix <- if (!is.null(prefix)) paste0(prefix, ".")
    env$data <- new.env(parent = emptyenv())

    # These can be modified by the user of the config class, as needed,
    # to add user-defined types
    env$types <- builtin_types
    env$checks <- builtin_checks
    env$env_decode = builtin_env_decode

    # --------------------------------------------------------------------
    #' Add a new option. Typically this is called outside of the body of
    #' a function, i.e. at install time, right after `new_config()`, once
    #' for each configuration entry.

    env$add <- function(name, type = env$types, default = NULL,
                        check = type[1], env_decode = type[1]) {
      # Need to explicitly add `env$types` on R 3.4.x
      type <- match.arg(type, env$types)
      stopifnot(
        is_string(name),
        is_string(check) || is.function(check) || is.null(check),
        is_string(env_decode) || is.function(env_decode) || is.null(env_decode)
      )
      name <- standard_name(name)

      if (name %in% names(env$data)) {
        stop("There is already an option called `", name, "`")
      }

      if (is_string(check)) check <- env$checks[[check]]
      check <- check %||% true
      if (!is.null(default) && !is.function(default)) check(default)

      if (is_string(env_decode)) env_decode <- env$env_decode[[env_decode]]
      env_decode <- env_decode %||% identity

      env$data[[name]] <- list(
        type = type, check = check, default = default,
        env_decode = env_decode, value = NULL
      )
      invisible(env)
    }

    # --------------------------------------------------------------------
    #' Query the value of an option.

    env$get <- function(name) {
      get_internal(env, name)[[2]]
    }

    # --------------------------------------------------------------------

    env$set <- function(name, value) {
      stopifnot(is_string(name))
      name <- standard_name(name)
      if (!name %in% names(env$data)) stop("No such option: `", name, "`")
      if (!is.null(chk <- env$data[[name]]$check)) chk(value)
      env$data[[name]]$value <- value
      invisible(env)
    }

    # --------------------------------------------------------------------

    env$unset <- function(name) {
      stopifnot(is_string(name))
      name <- standard_name(name)
      if (!name %in% names(env$data)) stop("No such option: `", name, "`")
      env$data[[name]]$value <- NULL
    }

    # --------------------------------------------------------------------

    env$update <- function(new) {
      for (i in seq_along(new)) {
        if (is.null(new[[i]])) {
          env$unset(names(new)[i])
        } else {
          env$set(names(new)[i], new[[i]])
        }
      }
      invisible(env)
    }

    # --------------------------------------------------------------------

    env$list <- function() {
      names(env$data)
    }

    # --------------------------------------------------------------------

    env$exists <- function(name) {
      stopifnot(is_string(name))
      name <- standard_name(name)
      name %in% env$list()
    }

    # --------------------------------------------------------------------

    env$lock <- function() {
      lockEnvironment(env$data)
    }

    # --------------------------------------------------------------------

    env$fix <- function(name) {
      stopifnot(is_string(name))
      name <- standard_name(name)
      if (!name %in% names(env$data)) stop("No such option: `", name, "`")
      lockBinding(name, env$data)
    }

    # --------------------------------------------------------------------

    env$add_type <- function(type_name, check, env_decode) {
      stopifnot(
        is_string(type_name),
        is.function(check),
        is.function(env_decode)
      )
      if (type_name %in% env$types) {
        stop("Type already exists: `", type_name, "`")
      }
      env$types <- c(env$types, type_name)
      env$checks[[type_name]] <- check
      env$env_decode[[type_name]] <- env_decode
      invisible(env)
    }

    structure(env, class = c("config_v1", "config"))
  }

  print_config <- function(x, ...) {
    nms <- names(x$data)
    all <- structure(
      lapply(nms, function(n) get_internal(x, n)),
      names = nms
    )
    cat0("# ", substr(x$prefix, 1, nchar(x$prefix) - 1), " config\n")
    for (i in seq_along(all)) {
      cat0("## ", names(all)[i], "\n")
      cat0("<", all[[i]][[1]], ">\n")
      print(all[[i]][[2]])
      cat0("\n")
    }
    invisible(x)
  }

  register_if_needed <- function(generic, class, fun) {
    if (is.null(utils::getS3method(generic, class, TRUE))) {
      registerS3method(generic, class, fun, baseenv())
    }
  }

  onload_hook <- function() {
    register_if_needed("print", "config_v1", print_config)
    register_if_needed("print", "config", print_config)
  }

  structure(
    list(
      .internal = environment(),
      new = new_config,
      onload_hook = onload_hook
    ),
    class = c("standalone_config", "standalone"))
})
