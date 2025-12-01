# Robust parser: zeeschuimer TikTok comments .ndjson -> tidy tibble (4CAT comment schema)
#
# Dependencies: jsonlite, tibble. dplyr optional.
#
# Usage example:
# install.packages(c("jsonlite","tibble"))
# tib <- parse_tiktok_comments("comments.ndjson", chunk_size = 2000)
#

parse_tiktok_comments <- function(input_path,
                                  chunk_size = 5000L,
                                  collapse_sep = "|",
                                  verbose = TRUE,
                                  encoding = "UTF-8") {
  if (!file.exists(input_path)) stop("input_path does not exist: ", input_path)
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install jsonlite")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Please install tibble")
  use_dplyr <- requireNamespace("dplyr", quietly = TRUE)

  target_cols <- c(
    "id", "thread_id", "author", "author_full", "author_avatar_url", "body",
    "timestamp", "unix_timestamp", "likes", "replies", "post_id", "post_url",
    "post_body", "comment_url", "is_liked_by_post_author", "is_sticky",
    "is_comment_on_comment", "language_guess", "missing_fields"
  )

  # safe navigator
  get_path <- function(x, path) {
    cur <- x
    for (p in path) {
      if (is.null(cur)) return(NULL)
      if (is.list(cur) && !is.null(cur[[p]])) { cur <- cur[[p]] }
      else if (is.list(cur) && !is.null(cur[[as.character(p)]])) { cur <- cur[[as.character(p)]] }
      else if (is.data.frame(cur) && as.character(p) %in% names(cur)) { cur <- cur[[as.character(p)]] }
      else return(NULL)
    }
    if (is.null(cur)) return(NULL)
    if ((is.atomic(cur) || is.list(cur)) && length(cur) == 0) return(NULL)
    cur
  }

  first_nonnull <- function(x, paths) {
    for (pth in paths) {
      val <- get_path(x, pth)
      if (!is.null(val)) return(val)
    }
    NULL
  }

  # Collapse lists or character vectors into a single string
  collapse_val <- function(v) {
    if (is.null(v)) return(NA_character_)
    if (is.list(v)) {
      # try to extract meaningful text from each element
      extracted <- vapply(v, function(el) {
        if (is.null(el)) return(NA_character_)
        if (is.atomic(el) && length(el) == 1) return(as.character(el))
        if (is.list(el)) {
          n <- first_nonnull(el, list(c("text"), c("name"), c("title"), c("label"), c("url"), c("url_list")))
          if (!is.null(n)) {
            if (is.atomic(n)) return(as.character(n))
            return(as.character(jsonlite::toJSON(n, auto_unbox = TRUE, null = "null")))
          }
          return(as.character(jsonlite::toJSON(el, auto_unbox = TRUE, null = "null")))
        }
        as.character(el)
      }, FUN.VALUE = character(1), USE.NAMES = FALSE)
      extracted <- extracted[!is.na(extracted) & nzchar(extracted)]
      if (length(extracted) == 0) return(NA_character_)
      paste(unique(extracted), collapse = collapse_sep)
    } else if (is.atomic(v)) {
      if (length(v) == 0) return(NA_character_)
      # remove NA and empty strings, then collapse
      vv <- as.character(v)
      vv <- vv[!is.na(vv) & nzchar(trimws(vv))]
      if (length(vv) == 0) return(NA_character_)
      paste(unique(vv), collapse = collapse_sep)
    } else {
      tryCatch(as.character(v), error = function(e) NA_character_)
    }
  }

  # Normalize values to scalar-ish so missing-field checks are easy and safe:
  # - NULL -> NULL
  # - list -> collapsed character via collapse_val()
  # - character vector -> collapsed string
  # - numeric vector -> first non-NA numeric (or NA_real_)
  # - logical vector -> first non-NA logical (or NA)
  normalize_value <- function(v) {
    if (is.null(v)) return(NULL)
    if (is.list(v)) return(collapse_val(v))
    if (is.character(v)) {
      vv <- v[!is.na(v) & nzchar(trimws(v))]
      if (length(vv) == 0) return(NA_character_)
      return(paste(unique(vv), collapse = collapse_sep))
    }
    if (is.numeric(v)) {
      vv <- v[!is.na(v)]
      if (length(vv) == 0) return(as.numeric(NA))
      return(vv[[1]])
    }
    if (is.logical(v)) {
      vv <- v[!is.na(v)]
      if (length(vv) == 0) return(as.logical(NA))
      return(as.logical(vv[[1]]))
    }
    # fallback to single string
    tryCatch(as.character(v[[1]]), error = function(e) NA_character_)
  }

  # robustly test emptiness (for missing_fields); accepts scalars, vectors, lists
  is_empty_value <- function(v) {
    if (is.null(v)) return(TRUE)
    # lists: empty if no element that is non-empty
    if (is.list(v)) {
      if (length(v) == 0) return(TRUE)
      elems <- vapply(v, function(el) !is_empty_value(el), FUN.VALUE = logical(1))
      return(!any(elems))
    }
    if (is.atomic(v)) {
      # character: empty if all items NA or blank
      if (is.character(v)) {
        vv <- trimws(v)
        vv <- vv[!is.na(vv)]
        if (length(vv) == 0) return(TRUE)
        return(all(nzchar(vv) == FALSE))
      }
      # numeric/logical: empty if all NA
      return(all(is.na(v)))
    }
    FALSE
  }

  epoch_to_unix_and_iso <- function(x, maybe_ms = TRUE) {
    if (is.null(x)) return(list(unix = NA_integer_, iso = NA_character_))
    if (!is.numeric(x)) {
      suppressWarnings(nx <- as.numeric(x))
      if (is.na(nx)) return(list(unix = NA_integer_, iso = NA_character_))
      x <- nx
    }
    if (maybe_ms && x > 1e12) unix <- as.integer(floor(x / 1000)) else unix <- as.integer(floor(x))
    iso <- as.character(as.POSIXct(unix, origin = "1970-01-01", tz = "UTC"))
    list(unix = unix, iso = iso)
  }

  to_logical <- function(v) {
    if (is.null(v)) return(NA)
    if (is.logical(v)) return(v[[1]])
    if (is.numeric(v)) {
      vv <- v[!is.na(v)]
      if (length(vv) == 0) return(NA)
      return(as.logical(as.integer(vv[[1]])))
    }
    if (is.character(v)) {
      vv <- tolower(as.character(v))
      vv <- vv[!is.na(vv) & nzchar(vv)]
      if (length(vv) == 0) return(NA)
      lv <- vv[[1]]
      if (lv %in% c("true","1","yes","y","t")) return(TRUE)
      if (lv %in% c("false","0","no","n","f")) return(FALSE)
    }
    NA
  }

  extract_comment_url <- function(share_info, item_id, comment_id) {
    if (!is.null(share_info)) {
      url <- first_nonnull(list(share_info = share_info), list(c("share_info","url"), c("url")))
      if (!is.null(url)) return(as.character(url))
      if (!is.null(share_info$share_url)) return(as.character(share_info$share_url))
    }
    if (!is.null(item_id) && !is.null(comment_id))
      return(paste0("https://m.tiktok.com/v/", item_id, ".html?share_comment_id=", comment_id, "&share_item_id=", item_id))
    NA_character_
  }

  # parse one comment object (wrapper or comment)
  parse_comment_obj <- function(obj) {
    id <- first_nonnull(obj, list(c("data","id"), c("data","cid"), c("cid"), c("item_id"), c("id")))
    thread_id <- first_nonnull(obj, list(c("data","aweme_id"), c("aweme_id"), c("data","awemeId"), c("item_id"), c("data","parent_id")))
    author <- first_nonnull(obj, list(c("data","user","unique_id"), c("data","user","uniqueId"), c("data","user","uid"), c("data","user","nickname")))
    author_full <- first_nonnull(obj, list(c("data","user","nickname"), c("user","nickname"), c("data","user","uniqueId")))
    author_avatar_url <- first_nonnull(obj, list(c("data","user","avatar_thumb","url_list"), c("data","user","avatar_thumb","url"), c("data","user","avatar")))
    body <- first_nonnull(obj, list(c("data","text"), c("data","comment_text"), c("text"), c("data","text_display"), c("data","content")))
    create_time <- first_nonnull(obj, list(c("data","create_time"), c("data","createTime"), c("create_time"), c("createTime")))
    timestamp_collected <- first_nonnull(obj, list(c("timestamp_collected"), c("data","timestamp_collected")))
    ts <- epoch_to_unix_and_iso(if (!is.null(create_time)) create_time else timestamp_collected, maybe_ms = TRUE)
    unix_timestamp <- ts$unix
    timestamp <- ts$iso
    likes <- first_nonnull(obj, list(c("data","digg_count"), c("data","diggCount"), c("diggCount"), c("data","like_count")))
    replies <- first_nonnull(obj, list(c("data","reply_comment_total"), c("data","reply_count"), c("reply_comment_total"), c("reply_count")))
    post_id <- first_nonnull(obj, list(c("data","aweme_id"), c("data","awemeId"), c("aweme_id"), c("data","share_info","share_item_id")))
    post_url <- first_nonnull(obj, list(c("data","share_info","url"), c("share_info","url"), c("source_url"), c("data","share_info","share_url")))
    if (is.null(post_url)) post_url <- first_nonnull(obj, list(c("source_platform_url"), c("source_url")))
    post_body <- first_nonnull(obj, list(c("data","share_info","title"), c("data","share_info","desc"), c("data","share_info","share_desc"), c("data","post_body")))
    comment_url <- extract_comment_url(first_nonnull(obj, list(c("data","share_info"), c("share_info"))), post_id, id)
    is_liked_by_post_author <- to_logical(first_nonnull(obj, list(c("data","is_author_digged"), c("is_author_digged"), c("data","user_digged"), c("user_digged"))))
    is_sticky_raw <- first_nonnull(obj, list(c("data","stick_position"), c("stick_position"), c("data","author_pin"), c("author_pin")))
    is_sticky <- NA
    if (!is.null(is_sticky_raw) && is.numeric(is_sticky_raw)) is_sticky <- as.logical(as.integer(is_sticky_raw) > 0) else is_sticky <- to_logical(is_sticky_raw)
    reply_id <- first_nonnull(obj, list(c("data","reply_id"), c("reply_id"), c("data","replyCommentId"), c("reply_to_reply_id")))
    reply_comment <- first_nonnull(obj, list(c("data","reply_comment"), c("reply_comment")))
    is_comment_on_comment <- FALSE
    if (!is.null(reply_id) && !identical(as.character(reply_id), "0")) is_comment_on_comment <- TRUE
    if (!is.null(reply_comment) && length(reply_comment) > 0) is_comment_on_comment <- TRUE
    language_guess <- first_nonnull(obj, list(c("data","comment_language"), c("comment_language"), c("data","text_language"), c("text_language")))

    rec <- list(
      id = normalize_value(id),
      thread_id = normalize_value(thread_id),
      author = normalize_value(author),
      author_full = normalize_value(author_full),
      author_avatar_url = normalize_value(author_avatar_url),
      body = normalize_value(body),
      timestamp = if (!is.null(timestamp)) as.character(timestamp) else NA_character_,
      unix_timestamp = if (!is.null(unix_timestamp)) as.integer(unix_timestamp) else NA_integer_,
      likes = {
        n <- normalize_value(likes); if (is.null(n)) NA_integer_ else as.integer(suppressWarnings(as.numeric(n)))
      },
      replies = {
        n <- normalize_value(replies); if (is.null(n)) NA_integer_ else as.integer(suppressWarnings(as.numeric(n)))
      },
      post_id = normalize_value(post_id),
      post_url = normalize_value(post_url),
      post_body = normalize_value(post_body),
      comment_url = normalize_value(comment_url),
      is_liked_by_post_author = is_liked_by_post_author,
      is_sticky = is_sticky,
      is_comment_on_comment = as.logical(is_comment_on_comment),
      language_guess = normalize_value(language_guess),
      missing_fields = NA_character_
    )

    # compute missing_fields robustly
    miss_flags <- vapply(names(rec)[names(rec) != "missing_fields"], function(nm) {
      is_empty_value(rec[[nm]])
    }, FUN.VALUE = logical(1))
    rec$missing_fields <- paste(names(miss_flags)[which(miss_flags)], collapse = collapse_sep)
    if (identical(rec$missing_fields, "")) rec$missing_fields <- NA_character_
    rec
  }

  # read in chunks and accumulate
  con <- file(input_path, open = "r", encoding = encoding)
  on.exit(close(con), add = TRUE)

  all <- list()
  total <- 0L
  chunk_i <- 0L
  if (verbose) message("Parsing NDJSON into tibble (in-memory). chunk_size=", chunk_size)
  repeat {
    lines <- readLines(con, n = chunk_size, warn = FALSE)
    if (length(lines) == 0) break
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0) next
    chunk_i <- chunk_i + 1L
    parsed <- vector("list", length(lines))
    for (i in seq_along(lines)) {
      parsed[[i]] <- tryCatch(jsonlite::fromJSON(lines[[i]], simplifyVector = FALSE),
                              error = function(e) {
                                if (verbose) warning("Skipping invalid JSON line: ", substring(lines[[i]],1,200))
                                NULL
                              })
    }
    parsed <- Filter(Negate(is.null), parsed)
    if (length(parsed) == 0) next
    recs <- lapply(parsed, parse_comment_obj)
    all <- c(all, recs)
    total <- total + length(recs)
    if (verbose) message("Chunk ", chunk_i, " parsed: ", length(recs), " (total: ", total, ")")
  }

  if (length(all) == 0) {
    empty <- as.list(rep(NA, length(target_cols))); names(empty) <- target_cols
    return(tibble::as_tibble(empty)[0, , drop = FALSE])
  }

  if (use_dplyr) {
    df <- dplyr::bind_rows(all)
    df <- tibble::as_tibble(df)
  } else {
    df <- do.call(rbind, lapply(all, function(rec) {
      vals <- lapply(target_cols, function(cn) if (cn %in% names(rec)) rec[[cn]] else NA)
      names(vals) <- target_cols
      as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    }))
    df <- tibble::as_tibble(df)
  }

  # ensure canonical order (extras appended)
  present <- intersect(target_cols, names(df)); extras <- setdiff(names(df), target_cols)
  df <- df[, c(present, extras), drop = FALSE]
  if (verbose) message("Done. rows=", nrow(df), " cols=", ncol(df))
  df
}
