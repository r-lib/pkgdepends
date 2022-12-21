
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
#' - Optionally send authorization.
#' - Better error messages.
#'
#' ## Docs and other helpful links:
#' - https://github.com/git/git/blob/master/Documentation/gitprotocol-common.txt
#' - https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.txt
#' - https://github.com/git/git/blob/master/Documentation/gitprotocol-v2.txt
#' - https://github.com/calebsander/git-internals/blob/part2/src/main.rs
#'
#' @keywords internal
#' @name git-protocol
NULL

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
  if (is.null(prefixes)) {
    git_list_refs_v1(url)
  } else {
    git_list_refs_v2(url, prefixes)
  }
}

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
  sha <- ref
  if (!grepl("^[0-9a-f]{40}$", ref)) {
    refs <- git_list_refs(url, ref)
    if (!ref %in% refs$refs$ref) {
      stop("Unknown ref: '", ref, "'")
    }
    sha <- refs$refs$hash[refs$refs$ref == ref]
  }

  packfile <- git_fetch(url, sha)
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
  packfile <- git_fetch(url, sha)
  if (length(packfile) != 1) {
    stop("Invalid response from git server, packfile should have a single blob") # nocov
  }
  if (packfile[[1]]$type != "blob") {
    stop("sha is not a blob")                                       # nocov
  }

  mkdirp(dirname(output))
  writeBin(packfile[[1]]$object, output)
  invisible(packfile[[1]])
}

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

  reply <- git_send_message(
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
  )

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

git_ua <- function() {
  "git/2.38.1"
}

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

git_create_message_v2 <- function(
  cmd,
  caps = character(),
  args = character()) {

  c(
    pkt_line("command=", cmd, "\n"),
    unlist(lapply(caps, pkt_line)),
    delim_pkt(),
    unlist(lapply(args, pkt_line)),
    flush_pkt()
  )
}

git_send_message <- function(
  url,
  cmd,
  caps = character(),
  args = character()) {

  msg <- git_create_message_v2(cmd, caps = caps, args = args)

  h <- curl::new_handle()
  curl::handle_setopt(h,
    customrequest = "POST",
    postfieldsize = length(msg),
    postfields = msg
  )

  curl::handle_setheaders(h,
    "Content-Type" = "application/x-git-upload-pack-request",
    "User-Agent" = git_ua(),
    "accept-encoding" = "deflate, gzip",
    "accept" = "application/x-git-upload-pack-result",
    "git-protocol" = "version=2",
    "content-length" = as.character(length(msg))
  )

  url2 <- paste0(url, "/git-upload-pack")
  res <- curl::curl_fetch_memory(url2, handle = h)
  if (res$status_code != 200) {
    stop("HTTP failed")                                             # nocov
  }
  git_parse_message(res$content)
}

delim_pkt <- function() {
  charToRaw("0001")
}

flush_pkt <- function() {
  charToRaw("0000")
}

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

# https://github.com/git/git/blob/master/Documentation/gitprotocol-http.txt

git_list_refs_v1 <- function(url) {
  h <- curl::new_handle()
  curl::handle_setheaders(
    h,
    "User-Agent" = git_ua()
  )
  url <- paste0(url, "/info/refs?service=git-upload-pack")
  res1 <- curl::curl_fetch_memory(url, handle = h)
  if (res1$status_code != 200) {
    stop("HTTP failed")                                             # nocov
  }
  psd <- git_parse_message(res1$content)

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

git_list_refs_v2 <- function(url, prefixes = character()) {
  h <- curl::new_handle()
  curl::handle_setheaders(h,
    "User-Agent" = git_ua(),
    "git-protocol" = "version=2"
  )
  url1 <- paste0(url, "/info/refs?service=git-upload-pack")
  res1 <- curl::curl_fetch_memory(url1, handle = h)
  if (res1$status_code != 200) {
    stop("HTTP failed")                                             # nocov
  }
  psd <- git_parse_message(res1$content)

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

  psd2 <- git_send_message(url, "ls-refs", args = as.character(args))

  if (psd2[[length(psd2)]]$type != "flush-pkt") {
    stop("Invalid response from git, last line must be a flush-pkt") # nocov
  }

  # Any refs at all?
  refs <- if (length(psd2) == 1) {
    git_parse_pkt_line_refs(list())
  } else {
    git_parse_pkt_line_refs(psd2[1:(length(psd2)-1)])
  }
  list(refs = refs, caps = caps)
}

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

parse_int32_nwb <- function(x) {
  if (length(x) != 4L || !is.raw(x)) {
    stop("Cannot parse integer, not raw or number of bytes is wrong") # nocov
  }
  sum(as.integer(x) * (256L ** (3:0)))
}

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

bin_to_sha <- function(x) {
  paste(format(x), collapse = "")
}

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
