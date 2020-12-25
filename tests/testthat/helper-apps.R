
new_check_app <- function() {
  `%||%` <- function(l, r) if (is.null(l)) r else l

  app <- webfakes::new_app()

  app$use(webfakes::mw_json())
  app$use(webfakes::mw_urlencoded())

  app$get("/crandb", function(req, res) {
    pkg <- sub("\"$", "", sub("^\"", "", req$query$key))
    if (pkg == "dbi") {
      res$send_json(list(
        total_rows = 20000,
        offset = 14000,
        rows = list(list(id = "DBI", key = "dbi", value = "DBI"))
      ), auto_unbox = TRUE)
    } else {
      res$send_json(list(
        total_rows = 20000,
        offset = 14000,
        rows = list()
      ))
    }
  })

  app$post("/wikipedia", function(req, res) {
    titles <- strsplit(req$form$titles, "|", fixed = TRUE)[[1]]
    Titles <- tools::toTitleCase(titles)
    ret <- list(query = list(
      normalized = list(list(from = titles, to = Titles)),
      pages = list(`11178` = list(
        pageid = 11178,
        title = Titles,
        extract = "The terms foobar (), foo, bar, and others are used ..."
      ))
    ))
    res$send_json(ret, auto_unbox = TRUE)
  })

  app$all("/echo", function(req, res) {
    out <- list(
      method = req$method,
      query = req$query_string,
      type = req$get_header("Content-Type") %||% NA_character_,
      body = rawToChar(req$.body %||% raw())
    )
    res$send_json(out, auto_unbox = TRUE)
  })

  app
}

show_request <- function(req) {
  x <- fromJSON(rawToChar(req$content))
  cat(toupper(x$method), " ", x$type, sep = "", "\n")
  cat("Query string: ", x$query, sep = "", "\n")
  cat("Body: ", x$body, sep = "", "\n")
}

check_app <- webfakes::new_app_process(
  new_check_app(),
  opts = webfakes::server_opts(num_threads = 4)
)
