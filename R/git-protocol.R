
#' git protocol notes, for developers
#'
#' Assumptions, they might be relaxed or checked for later:
#' - The server must speak the smart protocol, version 2.
#' - We use HTTP transport, not SSH.
#' - The server should have the `shallow` capability.
#' - The server should have the `filter` capability.
#'
#' Improvements needed:
#' - Tests.
#' - Use async HTTP.
#' - Support packfiles with deltas.
#' - Optionally send authorization.
#' - Better error messages.
#' - Better errors for non-existing user, repository, ref, PR, etc.
#'
#' ## Docs and other helpful links:
#' - <https://github.com/git/git/blob/master/Documentation/gitprotocol-common.txt>
#' - <https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.txt>
#' - <https://github.com/git/git/blob/master/Documentation/gitprotocol-v2.txt>
#' - <https://github.com/calebsander/git-internals/blob/part2/src/main.rs>
#'
#' @keywords internal
#' @name git-protocol
NULL

# -------------------------------------------------------------------------

#' List references in a remote git repository
#'
#' @details
#' * Branches have references named `refs/heads/<branch>`, e.g.
#' `refs/heads/main`.
#' * Tags have references named `refs/tags/<tag>`, e.g. `refs/tags/v1.0.2`.
#' * On GitHub pull requests have references named
#'   `refs/pull/<pr-number>/head`, e.g. `refs/pull/37/head`.
#'   For open pull requests there should be a `refs/pull/<pr-number>/merge`
#'   reference as well, which is the branch after the pull request has been
#'   merged.
#' * There is a special reference called `HEAD`. This points to the default
#'   branch on GitHub.
#'
#' @param url Repository URL, e.g. `https://github.com/r-lib/pak.git`.
#'   It might include authentication information, e.g. a GitHub token.
#' @param prefixes If not `NULL`, then only references of which one of
#'   `prefixes` is a prefix, are listed.
#' @return A list with entries: `refs` and `caps`.
#'   `caps` is a character vector of capabilities advertised by the server.
#'   `refs` is a data frame of git refs, it has columns `ref` and `hash`.
#'
#' @keywords internal
#' @section Examples:
#'
#' ```{r, git-list-refs, cache = TRUE}
#' git_list_refs("https://github.com/r-lib/filelock.git")
#' ```
#'
#' List only references having a certain prefix:
#'
#' ```{r git-list-refs-2, cache = TRUE}
#' git_list_refs("https://github.com/r-lib/filelock.git", "refs/heads/main")
#' ```

git_list_refs <- function(url, prefixes = NULL) {
  synchronize(async_git_list_refs(url, prefixes))
}

async_git_list_refs <- function(url, prefixes = NULL) {
  if (is.null(prefixes)) {
    async_git_list_refs_v1(url)
  } else {
    async_git_list_refs_v2(url, prefixes)
  }
}

# -------------------------------------------------------------------------

#' List files in a remote git repository
#'
#' @inheritParams git_list_refs
#' @param ref Either a SHA or a ref name. See [git_list_refs()] for how
#'   branches, tags and GitHub pull requests are named.
#' @return A list with entries:
#'   * `ref`: The `ref` the function was called with.
#'   * `sha`: SHA of `ref`.
#'   * `commit`: named character vector of the data of the commit object
#'      belonging to `ref`. It has fields named: `tree`, `parent`,
#'      `author`, `committer`, `encoding`, `message`.
#'   * `tree`: SHA of the `tree` object belonging to `commit`.
#'   * `files`: a data frame of files and directories. It has columns:
#'        - `hash`: SHA hash of the tree or blob.
#'        - `type`: Either `"tree"` or `"blob"`.
#'        - `mode`: Unix mode.
#'        - `path`: Relative path from the repository's root directory.
#'
#' @keywords internal
#' @section Examples:
#' ```{r git-list-files, cache = TRUE}
#' fls <- git_list_files("https://github.com/r-lib/pak.git", "refs/heads/main")
#' fls$files
#' ```

git_list_files <- function(url, ref = "HEAD") {
  synchronize(async_git_list_files(url, ref))
}

async_git_list_files <- function(url, ref = "HEAD") {
  url; ref
  sha <- ref

  if (!grepl("^[0-9a-f]{40}$", ref)) {
    get_sha <- async_git_list_refs(url, ref)$
      then(function(refs) {
        if (!ref %in% refs$refs$ref) {
          stop("Unknown ref: '", ref, "'")
        }
        sha <<- refs$refs$hash[refs$refs$ref == ref]
      })

  } else {
    get_sha <- async_constant(ref)
  }

  get_sha$
    then(function(sha) git_fetch(url, sha))$
    then(function(pf) async_git_list_files_process(pf, ref, sha))
}

async_git_list_files_process <- function(packfile, ref, sha) {
  names(packfile) <- vcapply(packfile, "[[", "hash")
  types <- unname(vcapply(packfile, "[[", "type"))
  tree_sizes <- viapply(packfile, function(x) nrow(x$object) %||% NA_integer_)
  num_files <- sum(tree_sizes, na.rm = TRUE)

  files = data.frame(
    stringsAsFactors = FALSE,
    hash = character(num_files),
    type = character(num_files),
    mode = character(num_files),
    path = character(num_files)
  )

  trees <- packfile[types == "tree"]
  done <- logical(length(trees))
  idx <- 1L
  wd <- character()

  process_tree <- function(i) {
    if (done[i]) return()
    done[i] <<- TRUE
    tr <- trees[[i]]$object
    for (l in seq_len(nrow(tr))) {
      files$hash[idx] <<- tr$hash[l]
      files$type[idx] <<- tr$type[l]
      files$mode[idx] <<- tr$mode[l]
      files$path[idx] <<- paste(c(wd, tr$path[l]), collapse = "/")
      idx <<- idx + 1L
      if (tr$type[l] == "tree") {
        tidx <- which(tr$hash[l] == names(trees))[1]
        if (is.na(tidx)) next                                       # nocov
        wd <<- c(wd, tr$path[l])
        process_tree(tidx)
        wd <<- head(wd, -1)
      }
    }
  }

  for (i in seq_along(trees)) process_tree(i)

  commit <- parse_commit(packfile[[which(types == "commit")]]$object)
  tree <- commit[["tree"]]

  list(
    ref = ref,
    sha = sha,
    commit = commit,
    tree = tree,
    files = files
  )
}

# -------------------------------------------------------------------------

#' Download a blob for a remote git repository
#'
#' @inheritParams git_list_refs
#' @param sha SHA hash of the blob.
#' @param output Path where the blob will be written. It's directory is
#'   created if it does not exist.
#' @return A list that corresponds to a git packfile entry, invisibly.
#'   It has entries: `type` (always `"blob"`), `object` (raw object, the
#'   blob itself), `size`, `packed_size`, `hash`.
#'
#' @keywords internal
#' @section Examples:
#' Download a `DESCRIPTION` file from GitHub:
#' ```{r git-download-file, cache = TRUE}
#' pak_repo <- "https://github.com/r-lib/pak.git"
#' fls <- git_list_files(pak_repo, "HEAD")
#' git_download_file(
#'   pak_repo,
#'   fls$files$hash[fls$files$path == "DESCRIPTION"],
#'   output = tmp <- tempfile()
#' )
#' readLines(tmp)[1:5]
#' ```

git_download_file <- function(url, sha, output = sha) {
  synchronize(async_git_download_file(url, sha, output))
}

async_git_download_file <- function(url, sha, output = sha) {
  url; sha; output
  async_git_fetch(url, sha)$
    then(function(packfile) {
      if (length(packfile) != 1) {
        stop("Invalid response from git server, packfile should have a single blob") # nocov
      }
      if (packfile[[1]]$type != "blob") {
        stop("sha is not a blob")                                       # nocov
      }

      mkdirp(dirname(output))
      writeBin(packfile[[1]]$object, output)
      invisible(packfile[[1]])
    })
}

# -------------------------------------------------------------------------

#' Get a packfile for an object from a remote git repository
#'
#' @details
#' It uses `filter blob:none`, so extra blobs are not downloaded, only
#' trees.
#'
#' @inheritParams git_list_refs
#' @param sha SHA of the object to get.
#' @return A list of git objects. Each element has entries:
#'   * `type`: `commit`, `tree`, `blob` or `tag`.
#'   * `object`: the object itself. It is a
#'       - character scalar for commits,
#'       - a data frame for trees, with columns: `type`, `mode`, `path`, `hash`,
#'       - a raw vector for blobs,
#'       - a raw vector for tags. (We could probably do better here.)
#'
#' @keywords internal
#' @section Examples:
#'
#' ```{r, git-fetch, cache = TRUE}
#' ft <- git_fetch(
#'   "https://github.com/r-lib/filelock.git",
#'   "9fdba75a62facaa3e818902f58891166e45eabe9"
#' )
#' ft
#' ```

git_fetch <- function(url, sha) {
  synchronize(async_git_fetch(url, sha))
}

async_git_fetch <- function(url, sha) {

  async_git_send_message(
    url,
    "fetch",
    caps = c(
      paste0("agent=", git_ua(), "\n"),
      "object-format=sha1\n"
    ),
    args = c(
      "deepen 1\n",
      "no-progress\n",
      "filter blob:none\n",
      paste0("want ", sha, "\n"),
      paste0("want ", sha, "\n"),
      "done\n"
    )
  )$then(git_fetch_process)
}

git_fetch_process <- function(reply) {

  # https://github.com/git/git/blob/7c2ef319c52c4997256f5807564523dfd4acdfc7/Documentation/gitprotocol-v2.txt#L240
  #
  #     output = acknowledgements flush-pkt |
  # 	     [acknowledgments delim-pkt] [shallow-info delim-pkt]
  # 	     [wanted-refs delim-pkt] [packfile-uris delim-pkt]
  # 	     packfile flush-pkt
  #
  #     acknowledgments = PKT-LINE("acknowledgments" LF)
  # 		      (nak | *ack)
  # 		      (ready)
  #     ready = PKT-LINE("ready" LF)
  #     nak = PKT-LINE("NAK" LF)
  #     ack = PKT-LINE("ACK" SP obj-id LF)
  #
  #     shallow-info = PKT-LINE("shallow-info" LF)
  # 		   *PKT-LINE((shallow | unshallow) LF)
  #     shallow = "shallow" SP obj-id
  #     unshallow = "unshallow" SP obj-id
  #
  #     wanted-refs = PKT-LINE("wanted-refs" LF)
  # 		  *PKT-LINE(wanted-ref LF)
  #     wanted-ref = obj-id SP refname
  #
  #     packfile-uris = PKT-LINE("packfile-uris" LF) *packfile-uri
  #     packfile-uri = PKT-LINE(40*(HEXDIGIT) SP *%x20-ff LF)
  #
  #     packfile = PKT-LINE("packfile" LF)
  # 	       *PKT-LINE(%x01-03 *%x00-ff)
  #

  # Since we sent a 'done', there must be no acknowledgements section

  if (is.null(reply[[1]]$text) || reply[[1]]$text != "shallow-info") {
    stop("Expected shallow-info section from git server")           # nocov
  }

  # There should be only one 'shallow' line I think
  idx <- 2L
  while (idx <= length(reply) && ! identical(reply[[idx]]$text, "packfile")) {
    idx <- idx + 1L
  }
  idx <- idx + 1L

  if (idx > length(reply)) {
    stop("Invalid response from git, no packfile section")          # nocov
  }

  # check closing flush-pkt
  if (reply[[length(reply)]]$type != "flush-pkt") {
    stop("Invalid response from git, no closing flush-pkt")         # nocov
  }

  data <- reply[idx:(length(reply)-1)]

  if (any(vcapply(data, "[[", "type") != "data-pkt")) {
    stop("Invalid response from git, packfile section must contain data-pkt's") # nocov
  }

  # drop progress messages, although we did not requrest them...
  stream <- viapply(data, function(x) as.integer(x$data[1]))
  if (any(stream == 3)) {
    msg <- tryCatch(                                                # nocov
      rawToChar(reply[[which(stream == 3)[1]]]$data[-1]),             # nocov
      error = function(err) "error message not available"           # nocov
    )                                                               # nocov
    stop("Fatal error message from git server: ", msg)              # nocov
  }
  data <- data[stream == 1L]

  packfile <- do.call("c", lapply(data, function(x) x$data[-1]))

  git_unpack(packfile)
}

# -------------------------------------------------------------------------
# Utility functions
# -------------------------------------------------------------------------

git_ua <- function() {
  "git/2.38.1"
}

#' Comvert raw message from a pkt_line to text, if possible
#'
#' If the input is binary it returns `NA_character_`.
#'
#' @param x Raw vector.
#' @return Character string, if `x` can be an UTF-8 string. Otherwise
#'   `NA_character_`.
#'
#' @noRd

raw_as_utf8 <- function(x) {
  if (is.raw(x)) {
    if (any(x == 0x0)) return(NA_character_)
    if (length(x) > 0 && x[[length(x)]] == 0x0a) {
      x <- x[1:(length(x) - 1)]
    }
    x <- rawToChar(x)
  }

  iconv(x, "UTF-8", "UTF-8")
}

#' Parse a message from git
#'
#' @param msg A raw vector, the full message.
#' @return A list of objects that correspond to _pkt_lines_ of the git
#' message. Each such object is a named list with potential entries:
#' * `type`: this entry is always present, and it is one of
#'   `flush-pkt`, `delim-pkt`, `response-end-pkt` or `data-pkt`. See the
#'   git protocol docs for what these are.
#' * `data`: for `data-pkt` lines this is a raw vector of the data.
#' * `text`: for `data-pkt` lines that are text, this is the text of the
#'   data. We use [raw_as_utf8()] to convert raw data to text, and sometimes
#'   it might interpret binary data as text, especially if the data is
#'   short. So this field is for convenience only.
#'
#' @noRd

git_parse_message <- function(msg) {
  stopifnot(is.raw(msg))
  lines <- list()
  pos <- 1L

  while (pos <= length(msg)) {
    if (pos + 3L > length(msg)) {
      stop("Invalid pkt-line at the end of message")                # nocov
    }
    pkg_len <- msg[pos:(pos+3)]
    if (any(pkg_len < charToRaw('0') | pkg_len > charToRaw('f'))) {
      stop("Invalid pkt-len field, must be four hexa digits (lowercase)") # nocov
    }
    len <- as.integer(as.hexmode(rawToChar(pkg_len)))

    if (len == 0) {
      lines[[length(lines) + 1L]] <- list(type = "flush-pkt")
      pos <- pos + 4L
    } else if (len == 1) {
      lines[[length(lines) + 1L]] <- list(type = "delim-pkt")
      pos <- pos + 4L
    } else if (len == 2) {
      lines[[length(lines) + 1L]] <- list(type = "response-end-pkt") # nocov
      pos <- pos + 4L                                               # nocov
    } else {
      if (pos + len - 1L > length(msg)) {
        stop(                                                       # nocov
          "Incomplete pkg-payload, need ", len, " bytes, found ",   # nocov
          length(msg) - pos + 1L                                    # nocov
        )                                                           # nocov
      }

      data <- msg[(pos + 4):(pos + len - 1L)]
      lines[[length(lines) + 1L]] <- list(
        type = "data-pkt",
        data = data,
        text = raw_as_utf8(data)
      )

      pos <- pos + len
    }
  }

  lines
}

# After receiving the capability advertisement, a client can then issue a
# request to select the command it wants with any particular capabilities
# or arguments.  There is then an optional section where the client can
# provide any command specific parameters or queries.  Only a single
# command can be requested at a time.
#
#     request = empty-request | command-request
#     empty-request = flush-pkt
#     command-request = command
#                       capability-list
#                       delim-pkt
#                       command-args
#                       flush-pkt
#     command = PKT-LINE("command=" key LF)
#     command-args = *command-specific-arg
#
#     command-specific-args are packet line framed arguments defined by
#     each individual command.
#
# https://github.com/git/git/blob/master/Documentation/gitprotocol-v2.txt

#' Create a git protocol version 2 message, to be sent to git
#'
#' @param cmd Command to send. E.g. `ls-refs` or `fetch`.
#' @param caps Capabilities to advertise, a character vector.
#'   If they don't include the trailing `\n` character, it will be added.
#' @param args Arguments to send to git, a character vector. If they
#'   don't include the trailing `\n` character, it will be added.
#'
#' @return The message as a raw vector.
#'
#' @noRd

git_create_message_v2 <- function(
  cmd,
  caps = character(),
  args = character()) {

  caps <- ifelse(last_char(caps) == "\n", caps, paste0(caps, "\n"))
  args <- ifelse(last_char(args) == "\n", args, paste0(args, "\n"))

  c(
    pkt_line("command=", cmd, "\n"),
    unlist(lapply(caps, pkt_line)),
    delim_pkt(),
    unlist(lapply(args, pkt_line)),
    flush_pkt()
  )
}

#' Send a protocol version 2 message to a git server
#'
#' @inheritParams git_list_refs
#' @inheritParams git_create_message_v2
#' @return Response from git, already parsed with [git_parse_message()].
#'
#' @noRd

git_send_message <- function(
  url,
  cmd,
  caps = character(),
  args = character()) {

  synchronize(async_git_send_message(url, cmd, caps = caps, args = args))
}

#' `async_git_send_message()` is the asynchronous variant of
#' `git_send_message()`.
#' @rdname git_send_message
#' @noRd

async_git_send_message <- function(
  url,
  cmd,
  caps = character(),
  args = character()) {

  msg <- git_create_message_v2(cmd, caps = caps, args = args)

  url2 <- paste0(url, "/git-upload-pack")
  headers <- c(
    "Content-Type" = "application/x-git-upload-pack-request",
    "User-Agent" = git_ua(),
    "accept-encoding" = "deflate, gzip",
    "accept" = "application/x-git-upload-pack-result",
    "git-protocol" = "version=2",
    "content-length" = as.character(length(msg))
  )
  http_post(
    url2,
    data = msg,
    headers = headers
  )$then(http_stop_for_status)$
    then(function(res) git_parse_message(res$content))
}

delim_pkt <- function() {
  charToRaw("0001")
}

flush_pkt <- function() {
  charToRaw("0000")
}

#' Create a `pkt_line`, to be sent as part of a message to git
#'
#' Currently it errors if the complete payload is longer than 65516 bytes.
#'
#' @param payload Raw data, or a string to send.
#' @param ... More strings to send. `payload` and `...` are all
#'   concatenated to form the line.
#' @return Raw vector containing the `pkt_line`.
#' @noRd

pkt_line <- function(payload, ...) {
  if (!is.raw(payload)) {
    payload <- charToRaw(payload)
  }
  line <- c(
    payload,
    charToRaw(paste0("", ...))
  )
  len <- length(line) + 4L
  if (len > 65516) {
    stop("packet line longer than 65516 bytes is not implemented yet") # nocov
  }

  line <- c(
    charToRaw(format(as.hexmode(len), width = 4)),
    line
  )

  line
}

#' List references in a remote git repositoty, protocol version 1
#'
#' We use this if we don't want to filter the references, because it
#' needs a single HTTP query, whereas with version 2, we would need two
#' queries.
#'
#' @inheritParams git_list_refs
#' @return Same as [git_list_refs()].
#' @noRd

git_list_refs_v1 <- function(url) {
  synchronize(async_git_list_refs_v1(url))
}

#' `async_git_list_refs_v1()` is the asynchronous variant of
#' `git_list_refs_v1()`.
#' @rdname git_list_refs_v1
#' @noRd

# https://github.com/git/git/blob/master/Documentation/gitprotocol-http.txt

async_git_list_refs_v1 <- function(url) {
  url <- paste0(url, "/info/refs?service=git-upload-pack")
  http_get(url, headers = c("User-Agent" = git_ua()))$
    then(http_stop_for_status)$
    then(git_list_refs_v1_process)
}

#' Process the response to a version 1 reference list query
#'
#' @param response Response from git, as returned by `async::http_get()`, which
#'   is the same as the object from `curl::curl_fetch_memory()`.
#' @return Same as [git_list_refs()].
#' @noRd

git_list_refs_v1_process <- function(response) {
  psd <- git_parse_message(response$content)

  # Clients MUST validate the first five bytes of the response entity
  # matches the regex `^[0-9a-f]{4}#`.  If this test fails, clients
  # MUST NOT continue.
  if (length(psd) == 0 || is.null(psd[[1]]$text) ||
      !grepl("^#", psd[[1]]$text)) {
    stop("Invalid response from git, first pkt-line should start with '#'") # nocov
  }

  # Clients MUST verify the first pkt-line is `# service=$servicename`.
  if (!grepl("^# service=.+", psd[[1]]$text)) {
    stop("Invalid response from git, first pkt-line should be '# service=$servicename'") # nocov
  }

  if (length(psd) < 2 || psd[[2]]$type != "flush-pkt") {
    stop("Invalid response from git, second pkt-line should be a flush-pkt") # nocov
  }

  if (length(psd) < 3 || psd[[3]]$type != "data-pkt") {
    stop("Invalid response from git, line 3 should be a data-pkt")  # nocov
  }

  # The stream MUST include capability declarations behind a NUL on the
  # first ref.
  nul <- which(psd[[3]]$data == 0)
  if (length(nul) != 1) {
    stop("Invalid response from git, line 3, must contain exactly one NUL byte") # nocov
  }

  caps <- if (nul < length(psd[[3]]$data)) {
    rawToChar(psd[[3]]$data[(nul+1):length(psd[[3]]$data)])
  } else {
    ""                                                              # nocov
  }
  caps <- strsplit(trimws(caps), " ", fixed = TRUE)[[1]]
  psd[[3]]$data <- if (nul == 1) raw() else psd[[3]]$data[1:(nul-1)]
  psd[[3]]$text <- rawToChar(psd[[3]]$data)

  if (psd[[length(psd)]]$type != "flush-pkt") {
    stop("Invalid response from git, last line must be a flush-pkt") # nocov
  }

  refs <- git_parse_pkt_line_refs(psd[3:(length(psd)-1)])
  list(refs = refs, caps = caps)
}

#' Helper function to parse a `pkt_line` containing a git ref
#'
#' E.g. an `ls-refs` commans responds with such lines.
#'
#' @param lines List of `pkt_line` objects, they must be all data
#'   packets (`data-pkt`), in the right format.
#' @return Data frame with columns `ref` (name of the reference), and
#'   `hash` (SHA).
#' @noRd

git_parse_pkt_line_refs <- function(lines) {
  res <- data.frame(
    stringsAsFactors = FALSE,
    ref = character(length(lines)),
    hash = character(length(lines))
  )

  for (idx in seq_along(lines)) {
    line <- lines[[idx]]
      if (line$type != "data-pkt") {
      stop("Invalid response from git, line ", idx, " must be a data-pkt") # nocov
    }
    if (is.null(line$text)) {
      stop("Invalid response from git, line ", idx, " is binary")   # nocov
    }
    if (!grepl("^[0-9a-f]{40} .+$", line$text)) {
      stop("Invalid response from git, line ", idx, " must have a sha and a ref name") # nocov
    }

    res$ref[idx] <- substr(line$text, 42, nchar(line$text))
    res$hash[idx] <- substr(line$text, 1, 40)
  }

  res
}

#' List references in a remote git repositoty, protocol version 2
#'
#' We use this to filter all refs using prefixes, if we are only
#' interested in a subset of refs.
#'
#' @inheritParams git_list_refs
#' @return Same as [git_list_refs()].
#' @noRd

git_list_refs_v2 <- function(url, prefixes = character()) {
  synchronize(async_git_list_refs_v2(url, prefixes))
}

#' `async_git_list_refs_v2()` is the asynchronous version of
#' `git_list_refs_v2()`.
#' @rdname git_list_refs_v2
#' @noRd

async_git_list_refs_v2 <- function(url, prefixes = character()) {
  url; prefixes

  url1 <- paste0(url, "/info/refs?service=git-upload-pack")
  headers <- c(
    "User-Agent" = git_ua(),
    "git-protocol" = "version=2"
  )
  http_get(url1, headers = headers)$
    then(http_stop_for_status)$
    then(function(res) async_git_list_refs_v2_process_1(res, url, prefixes))
}

#' Helper function to post-process the response to the initial client
#' request, with protocol version 2
#'
#' @param response The HTTP response from git, from `async::http_get()`,
#'   i.e. from `curl::curl_fetch_memory()`.
#' @param url The original URL is passed in here.
#' @param prefixes The original prefixes are passed in here.
#' @return `async_git_list_refs_v2_process_1()` will send out the
#'   second query, an `ls-refs` command, filtered for `prefixes`, and
#'   return with (the deferred value of) the HTTP response.
#'
#' @noRd

async_git_list_refs_v2_process_1 <- function(response, url, prefixes) {
  psd <- git_parse_message(response$content)

  # Clients MUST validate the first five bytes of the response entity
  # matches the regex `^[0-9a-f]{4}#`.  If this test fails, clients
  # MUST NOT continue.
  if (length(psd) == 0 || is.null(psd[[1]]$text) ||
      !grepl("^#", psd[[1]]$text)) {
    stop("Invalid response from git, first pkt-line should start with '#'") # nocov
  }

  # Clients MUST verify the first pkt-line is `# service=$servicename`.
  if (!grepl("^# service=.+", psd[[1]]$text)) {
    stop("Invalid response from git, first pkt-line should be '# service=$servicename'") # nocov
  }

  if (length(psd) < 2 || psd[[2]]$type != "flush-pkt") {
    stop("Invalid response from git, second pkt-line should be a flush-pkt") # nocov
  }

  if (length(psd) < 3 || psd[[3]]$type != "data-pkt") {
    stop("Invalid response from git, line 3 should be a data-pkt")  # nocov
  }

  # capability-advertisement = protocol-version
  #                capability-list
  #                flush-pkt
  #
  # protocol-version = PKT-LINE("version 2" LF)
  # capability-list = *capability
  # capability = PKT-LINE(key[=value] LF)
  #
  # key = 1*(ALPHA | DIGIT | "-_")
  # value = 1*(ALPHA | DIGIT | " -_.,?\/{}[]()<>!@#$%^&*+=:;")

  if (is.null(psd[[3]]$text) || psd[[3]]$text != "version 2") {
    stop(                                                                        # nocov
      "We only support git protocol version 2 currently, got version string: '", # nocov
      psd[[3]]$text, "'."                                                         # nocov
    )                                                                            # nocov
  }

  if (psd[[length(psd)]]$type != "flush-pkt") {
    stop("Invalid response from git, last line must be a flush-pkt") # nocov
  }

  caps <- unlist(lapply(psd[4:(length(psd)-1)], "[[", "text"))

  args <- if (length(prefixes)) paste0("ref-prefix ", prefixes, "\n")

  async_git_send_message(url, "ls-refs", args = as.character(args))$
    then(function(res) async_git_list_refs_v2_process_2(res, caps))
}

#' Helper function to process the response to an `ls-refs` command
#'
#' @param reply The parsed message from the server.
#' @param caps Capabilities that are passed in from the response to
#'   the initial client request.
#' @return Same as [git_list_refs()].
#' @noRd

async_git_list_refs_v2_process_2 <- function(reply, caps) {
  if (reply[[length(reply)]]$type != "flush-pkt") {
    stop("Invalid response from git, last line must be a flush-pkt") # nocov
  }

  # Any refs at all?
  refs <- if (length(reply) == 1) {
    git_parse_pkt_line_refs(list())
  } else {
    git_parse_pkt_line_refs(reply[1:(length(reply)-1)])
  }
  list(refs = refs, caps = caps)
}

#' Unpack a git packfile in memory
#'
#' @details
#' It cannot currently unpack packfiles with deltas.
#'
#' It checks the packfile checksum.
#'
#' @param pack The git packfile as a raw vector, or the name of the file
#'   containing the packfile.
#' @return List of git objects. Each object is a named list with entries:
#'   * `type`: `commit`, `tree`, `blob` or `tag`.
#'   * `object`: the contents of the object. For `commit` the commit
#'     data and message in a string. For `tree` a data frame, as returned
#'     from [parse_tree()]. For `blob` and `tag` a raw vector.
#'   * `size`: unpacked size.
#'   * `packed_size`: packed size (size header not included).
#'
#' @noRd

git_unpack <- function(pack) {
  # allow file names as well
  if (is.character(pack)) {
    pack <- readBin(pack, "raw", file.size(pack))
  }

  if (length(pack) < 32) {
    stop("Not a git packfile, too short")                           # nocov
  }
  if (! all(pack[1:4] == charToRaw("PACK"))) {
    stop("Not a git packfile, does not have a 'PACK' header")       # nocov
  }
  if (! all(pack[5:8] == as.raw(c(0x00, 0x00, 0x00, 0x02)))) {
    stop("Unknown git packfile version, must be version 2")         # nocov
  }

  chksum <- cli::hash_raw_sha1(head(pack, -20))
  chksum_exp <- paste(format(tail(pack, 20)), collapse = "")
  if (chksum != chksum_exp) {
    stop("Checksum mismatch in git packfile")                       # nocov
  }

  n_obj <- parse_int32_nwb(pack[9:12])

  idx <- 13L
  objects <- vector("list", n_obj)

  types <- c("commit", "tree", "blob", "tag", "reserved", "ofs_delta", "re_delta")

  unpack_object <- function() {
    type <- bitwShiftR(bitwAnd(as.integer(pack[idx]), 0x7f), 4L)
    size <- parse_size(pack, idx)
    idx <<- size$idx + 1
    obj <- zip::inflate(pack, idx, size$size)
    idx <<- idx + obj$bytes_read
    list(
      type = types[type],
      object = obj$output,
      size = size$size,
      packed_size = obj$bytes_read
    )
  }

  for (i in seq_len(n_obj)) {
    objects[[i]] <- unpack_object()
    if (objects[[i]]$type == "commit") {
      objects[[i]]$raw <- objects[[i]]$object
      objects[[i]]$object <- rawToChar(objects[[i]]$object)
    } else if (objects[[i]]$type == "tree") {
      objects[[i]]$raw <- objects[[i]]$object
      objects[[i]]$object <- parse_tree(objects[[i]]$object)
    }
    if (objects[[i]]$type %in% c("commit", "tree", "blob", "tag")) {
      raw2 <- c(
        charToRaw(paste0(objects[[i]]$type, " ", length(objects[[i]]$raw))),
        as.raw(0L),
        objects[[i]]$raw
      )
      objects[[i]]$hash <- cli::hash_raw_sha1(raw2)
    } else {
      stop("Object type '", objects[[i]]$type, "' is not implemented yet") # nocov
    }
  }

  objects
}

#' Parse a four byte integer in network byte order
#'
#' @param x Raw vector or four bytes.
#' @return Integer scalar.
#' @noRd

parse_int32_nwb <- function(x) {
  if (length(x) != 4L || !is.raw(x)) {
    stop("Cannot parse integer, not raw or number of bytes is wrong") # nocov
  }
  sum(as.integer(x) * (256L ** (3:0)))
}

#' Parse a variable length size field
#'
#' @details
#' As in https://git-scm.com/docs/pack-format#_size_encoding.
#' Some notes, because that docs is pretty dense.
#'
#' * First bit is zero for the last byte.
#' * Next three bits of the first byte are the object type.
#' * So we throw away the first four bits of the first byte and use the
#'   last four bits only.
#' * Then we keep processing the last seven bits of bytes, until the
#'   first bit is zero. (We still process that byte.)
#'
#' @param x Raw vector, typically the whole packfile.
#' @param idx Integer scalar, where to start reading the size field in the
#'   `x` raw vector.
#' @return Named list with entries:
#'   * `size`: parsed size,
#'   * `idx`: Integer, point to the last byte of the parsed size field.
#'
#' @noRd

parse_size <- function(x, idx) {
  c <- as.integer(x[idx])
  size <- bitwAnd(c, 0x0f)
  shft <- 4L
  while (c >= 128) {
    idx <- idx + 1L
    if (idx > length(x)) {
      stop("Invalid git packfile, invalid size field")              # nocov
    }
    c <- as.integer(x[idx])
    size <- size + bitwShiftL(bitwAnd(c, 0x7f), shft)
    shft <- shft + 7L
  }

  list(size = size, idx = idx)
}

#' Parse a git tree object
#'
#' @param tree The tree object in a raw vector, _without_ the 'tree' +
#'   content size + `\0` header. Git packfiles do not have these headers,
#'   so we don't require them here, either.
#' @return Data frame with character columns:
#'   * `type`: Either `"blob"` or `"tree"`.
#'   * `mode`: Unix permissions.
#'   * `path`: File or directory name.
#'   * `hash`: Hash of `blob` or tree`.
#'
#' @noRd

parse_tree <- function(tree) {
  nul <- which(tree == 0)
  last <- nul[1]
  for (i in seq_along(nul)[-1]) {
    nl <- nul[i]
    if (nl > last + 20) {
      last <- nl
    } else {
      nul[i] <- NA_integer_
    }
  }

  nul <- nul[!is.na(nul)]
  num <- length(nul)
  res <- data.frame(
    stringsAsFactors = FALSE,
    type = character(num),
    mode = character(num),
    path = character(num),
    hash = character(num)
  )

  beg <- c(1L, nul + 21)
  for (i in seq_along(nul)) {
    txt <- rawToChar(tree[beg[i]:nul[i]])
    res$mode[i] <- sub("[ ].*$", "", txt)
    res$path[i] <- sub("[^ ]*[ ]", "", txt)
    res$hash[i] <- bin_to_sha(tree[(nul[i] + 1):(nul[i] + 20)])
  }
  res$type <- ifelse(substr(res$mode, 1, 1) == "1", "blob", "tree")

  res
}

#' Format a raw vector as a hexa string
#'
#' @param x Raw vector.
#' @return String.
#'
#' @noRd

bin_to_sha <- function(x) {
  paste(format(x), collapse = "")
}

#' Parse a commit object
#'
#' @param commit Commit object as an UTF-8 string.
#' @return Named character vector. Names are taken from the commit object,
#'   plus `message` is the name of the commit message. The commit object
#'   should have names `tree`, `parent` (one for each parent commit),
#'   `author`, `committer`, `encoding`.
#'
#' @noRd

parse_commit <- function(commit) {
  lines <- strsplit(commit, "\n", fixed = TRUE)[[1]]
  delim <- which(lines == "")[1]
  if (delim == 1) stop("Invalid commit object, no 'tree' field")

  message <- if (is.na(delim)) {
    NA_character_
  } else if (delim == length(lines)) {
    ""                                                              # nocov
  } else {
    paste(lines[(delim + 1):length(lines)], collapse = "\n")
  }

  fields <- lines[1:(delim - 1)]
  nms <- sub("[ ].*$", "", fields)
  vls <- sub("^[^ ]+ ", "", fields)

  structure(c(vls, message), names = c(nms, "message"))
}

# Returns `""` for empty strings!

last_char <- function(x) {
  nc <- nchar(x)
  substr(x, nc, nc)
}
