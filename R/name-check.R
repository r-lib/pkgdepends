
#' @export

pkg_name_check <- function(name, dictionaries = NULL) {
  synchronise(async_pkg_name_check(name, dictionaries))
}

async_pkg_name_check <- function(name, dictionaries = NULL) {
  stopifnot(
    is.character(name),
    length(name) == 1,
    !is.na(name),
    is.null(dictionaries) || is.character(dictionaries),
    !anyNA(dictionaries)
  )

  default_dictionaries <- c(
    "wikipedia",
    "wiktionary",
    "acromine",
    "profanity",
    "sentiment",
    "urban"
  )
  dicts <- dictionaries %||% default_dictionaries

  result <- when_all(
    valid =    valid_pkg_name_check(name),
    cranlike = async_cranlike_check(name),
    crandb =   async_crandb_check(name),

    wikipedia =  if ("wikipedia"  %in% dicts) async_wikipedia_get (name),
    wiktionary = if ("wiktionary" %in% dicts) async_wiktionary_get(name),
    acromine =   if ("acromine"   %in% dicts) async_acromine_get  (name),
    profanity =  if ("profanity"  %in% dicts) async_profanity_get (name),
    sentiment =  if ("sentiment"  %in% dicts) async_sentiment_get (name),
    urban =      if ("urban"      %in% dicts) async_urban_get     (name)
  )
}

valid_pkg_name_check <- function(name) {
  if (!grepl(pkg_rx()$pkg_name, name)) return(FALSE)
  if (any(charToRaw(name) > 127)) return(FALSE)
  TRUE
}

async_cranlike_check <- function(name) {
  name

  repos <- c(
    CRAN = "https://cran.r-project.org",
    CRANextra = "https://www.stats.ox.ac.uk/pub/RWin"
  )

  meta <- pkgcache::cranlike_metadata_cache$new(
    platforms = "source",
    bioc = TRUE,
    cran_mirror = "https://cran.r-project.org",
    repos = repos,
    update_after = as.difftime(5, units = "mins"))

  meta$async_check_update()$
    then(function(data) {
      mch <- match(name, data$pkgs$package)
      ret <- list(cran = TRUE, bioc = TRUE)
      if (!is.na(mch)) {
        type <- data$pkgs$type[mch]
        if (type == "cran") ret$cran <- FALSE
        if (type == "bioc") ret$bioc <- FALSE
      }
      ret
    })$
    then(function(res) add_class(res, "pkg_cranlike_check"))
}

async_crandb_check <- function(name) {
  url <- paste0("https://crandb.r-pkg.org/", name)
  http_get(url)$
    then(function(ret) {
      if (ret$status_code == 200) return(list(crandb = FALSE))
      if (ret$status_code == 404) return(list(crandb = TRUE))
      http_stop_for_status(ret)
    })$
    then(function(res) add_class(res, "pkg_crandb_check"))
}

async_wikipedia_get <- function(terms) {
  url <- "https://en.wikipedia.org/w/api.php"
  data <- make_wikipedia_data(terms)
  http_post(url, data = data)$
    then(http_stop_for_status)$
    then(function(resp) process_wikipedia_response(terms, resp))$
    then(function(res) add_class(res, "pkg_wikipedia_check"))
}

make_wikipedia_data <- function(terms, intro = TRUE) {
  terms <- enc2utf8(as.character(terms))
  params <- c(
    action = "query",
    format = "json",
    prop = "extracts",
    titles = utils::URLencode(paste(terms, collapse = "|")),
    redirects = 1,
    exintro = if (intro) 1L,
    explaintext = 1
  )
  pstr <- paste0(names(params), "=", params, collapse = "&")
  charToRaw(pstr)
}

process_wikipedia_response <- function(
    terms, resp, base_url = "https://en.wikipedia.org/wiki/") {

  obj <- jsonlite::fromJSON(
    rawToChar(resp$content),
    simplifyVector = FALSE
  )

  map_norm <- structure(
    vcapply(obj$query$normalized, "[[", "to"),
    names = vcapply(obj$query$normalized, "[[", "from")
  )
  nterms <- ifelse(is.na(map_norm[terms]), terms, map_norm[terms])

  map_redir <- structure(
    vcapply(obj$query$redirects, "[[", "to"),
    names = vcapply(obj$query$redirects, "[[", "from")
  )
  rterms <- ifelse(is.na(map_redir[nterms]), nterms, map_redir[nterms])

  map_text <- structure(
    vcapply(obj$query$pages, function(x) x$extract %||% NA_character_),
    names = vcapply(obj$query$pages, "[[", "title")
  )
  text <- map_text[rterms]

  url <- paste0(base_url, gsub(" ", "_", rterms))

  tibble(
    term = terms,
    normalized = unname(nterms),
    redirected = unname(rterms),
    title = unname(rterms),
    text = unname(text),
    url = ifelse(is.na(text), NA_character_, url)
  )
}

async_wiktionary_get <- function(terms) {
  url <- "https://en.wiktionary.org/w/api.php"
  data <- make_wiktionary_data(terms)
  http_post(url, data = data)$
    then(http_stop_for_status)$
    then(function(resp) process_wiktionary_response(terms, resp))$
    then(function(res) add_class(res, "pkg_wiktionary_check"))
}

make_wiktionary_data <- function(terms) {
  make_wikipedia_data(terms, intro = FALSE)
}

process_wiktionary_response <- function(terms, resp) {
  process_wikipedia_response(
    terms,
    resp,
    base_url = "https://en.wiktionary.org/wiki/"
  )[, c("term", "text", "url")]
}

async_acromine_get <- function(term) {
  base <- "http://www.nactem.ac.uk/software/acromine/dictionary.py"
  url <- paste0(base, "?sf=", URLencode(term))
  http_get(url)$
    then(http_stop_for_status)$
    catch(error = function(err) list(content = charToRaw("[]")))$
    then(function(resp) process_acromine_response(term, resp))$
    then(function(res) add_class(res, "pkg_acromine_check"))
}

process_acromine_response <- function(term, resp) {
  obj <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
  if (length(obj) == 0) obj <- list(list())
  tibble(
    term = term,
    short_form = obj[[1]]$sf %||% character(),
    long_form = vcapply(obj[[1]]$lfs, "[[", "lf"),
    frequency = viapply(obj[[1]]$lfs, "[[", "freq"),
    since = viapply(obj[[1]]$lfs, "[[", "since")
  )
}

async_profanity_get <- function(term) {
  base <- "https://www.purgomalum.com/service/containsprofanity"
  url <- paste0(base, "?text=", URLencode(term))
  http_get(url)$
    then(http_stop_for_status)$
    catch(error = function(err) list(content = charToRaw("NA")))$
    then(function(resp) process_profanity_response(term, resp))$
    then(function(res) add_class(res, "pkg_profanity_check"))
}

process_profanity_response <- function(term, resp) {
  txt <- rawToChar(resp$content)
  list (profanity = as.logical(txt))
}

async_sentiment_get <- function(term) {
  start <- if (is.null(pkgd_data$sentiment)) {
    async_sentiment_get_data()
  } else {
    async_constant(pkgd_data$sentiment)
  }

  start$
    then(function(stm) structure(stm[term], names = term))$
    then(function(res) add_class(res, "pkg_sentiment_check"))
}

async_sentiment_get_data <- function() {
  url <- "https://raw.githubusercontent.com/words/afinn-165/master/index.json"
  http_get(url)$
    then(http_stop_for_status)$
    catch(error = function(err) list(content = charToRaw("{}")))$
    then(function(resp) {
      pkgd_data$sentiment <- unlist(fromJSON(rawToChar(resp$content)))
    })
}

async_urban_get <- function(term) {
  base <- "http://api.urbandictionary.com/v0/"
  url <- paste0(base, "define?term=", URLencode(term))
  http_get(url)$
    then(http_stop_for_status)$
    catch(error = function(err) list(content = charToRaw('{"list":[]}')))$
    then(function(resp) process_urban_response(term, resp))$
    then(function(res) add_class(res, "pkg_urban_check"))
}

process_urban_response <- function(term, resp) {
  obj <- jsonlite::fromJSON(
    rawToChar(resp$content),
    simplifyVector = FALSE
    )$list
  tibble(
    definition = vcapply(obj, "[[", "definition"),
    permalink = vcapply(obj, "[[", "permalink"),
    thumbs_up = viapply(obj, "[[", "thumbs_up"),
    word = vcapply(obj, "[[", "word"),
    written_on = vcapply(obj, "[[", "written_on"),
    example = vcapply(obj, "[[", "example"),
    thumbs_down = viapply(obj, "[[", "thumbs_down")
  )
}
