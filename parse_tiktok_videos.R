# R function to parse zeeschuimer TikTok .ndjson comment data into a tidy tibble
#
# - Returns a tibble with the 4CAT columns requested by the user.
# - Reads the .ndjson file in chunks to avoid blowing memory.
# - Tries multiple likely JSON paths for each 4CAT field (robust to small schema changes).
# - Collapses arrays (hashtags, challenges, stickers, effects, diversification_labels) with collapse_sep.
# - Produces a "missing_fields" column listing which expected 4CAT columns were absent for each record.
#
# Dependencies: jsonlite, tibble, dplyr (dplyr only used for bind_rows; if not available the function will try a fallback)
#
# Usage:
# tib <- parse_tiktok_videos("tiktok.ndjson", chunk_size = 5000, collapse_sep = "|", verbose = TRUE)
#
parse_tiktok_videos <- function(input_path,
                                chunk_size = 5000L,
                                collapse_sep = "|",
                                verbose = TRUE) {
  if (!file.exists(input_path)) stop("input_path does not exist: ", input_path)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the jsonlite package: install.packages('jsonlite')")
  }
  use_dplyr <- requireNamespace("dplyr", quietly = TRUE)
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Please install the tibble package: install.packages('tibble')")
  }

  # The canonical 4CAT columns requested
  target_cols <- c(
    "id", "thread_id", "author", "author_full", "author_followers", "author_likes",
    "author_videos", "author_avatar", "body", "stickers", "timestamp", "unix_timestamp",
    "is_duet", "is_ad", "is_paid_partnership", "is_sensitive", "is_photosensitive",
    "music_name", "music_id", "music_url", "music_thumbnail", "music_author",
    "video_url", "tiktok_url", "thumbnail_url", "likes", "comments", "shares",
    "plays", "hashtags", "challenges", "diversification_labels", "location_created",
    "effects", "warning", "missing_fields"
  )

  # Helper: safe path getter (list-of-names). Returns NULL if not present or length 0.
  get_path <- function(x, path) {
    cur <- x
    for (p in path) {
      if (is.null(cur)) return(NULL)
      # For numeric indices or names specified as integers, allow integer indexing
      if (is.list(cur) && !is.null(cur[[p]])) {
        cur <- cur[[p]]
      } else if (is.list(cur) && !is.null(cur[[as.character(p)]])) {
        cur <- cur[[as.character(p)]]
      } else if (is.data.frame(cur) && p %in% names(cur)) {
        cur <- cur[[p]]
      } else {
        return(NULL)
      }
    }
    if (is.null(cur)) return(NULL)
    # Normalize zero-length to NULL
    if ((is.atomic(cur) || is.list(cur)) && length(cur) == 0) return(NULL)
    cur
  }

  # Helper: try several path alternatives (each is a vector of path components)
  first_nonnull <- function(x, paths) {
    for (pth in paths) {
      val <- get_path(x, pth)
      if (!is.null(val)) return(val)
    }
    NULL
  }

  # Collapse vectors or lists into a single string; NULL -> NA_character_
  collapse_val <- function(v) {
    if (is.null(v)) return(NA_character_)
    if (is.list(v)) {
      # If it's a list of simple scalars or named lists, try to extract meaningful pieces
      # e.g., list of objects with 'name' or 'hashtagName' fields
      simple <- vapply(v, function(el) {
        if (is.null(el)) return(NA_character_)
        if (is.atomic(el) && length(el) == 1) return(as.character(el))
        if (is.list(el)) {
          # try common name fields
          nm <- first_nonnull(el, list(c("name"), c("hashtagName"), c("title"), c("challengeName"), c("label")))
          if (!is.null(nm)) return(as.character(nm))
          # fall back to JSON compact
          return(jsonlite::toJSON(el, auto_unbox = TRUE, null = "null"))
        }
        as.character(el)
      }, FUN.VALUE = character(1), USE.NAMES = FALSE)
      simple <- simple[!is.na(simple) & nzchar(simple)]
      if (length(simple) == 0) return(NA_character_)
      paste(unique(simple), collapse = collapse_sep)
    } else if (is.atomic(v)) {
      if (length(v) == 0) return(NA_character_)
      paste(unique(as.character(v)), collapse = collapse_sep)
    } else {
      # fallback: stringify
      tryCatch(as.character(v), error = function(e) NA_character_)
    }
  }

  # Extract hashtags from text with regex fallback
  extract_hashtags_from_text <- function(text) {
    if (is.null(text) || !nzchar(as.character(text))) return(NA_character_)
    txt <- as.character(text)
    # unicode-aware: match # followed by letters/numbers/_ (simple heuristic)
    tags <- regmatches(txt, gregexpr("#[\\p{L}0-9_]+", txt, perl = TRUE))
    tags <- unique(unlist(tags))
    if (length(tags) == 0) return(NA_character_)
    # strip leading #
    tags <- sub("^#", "", tags)
    paste(tags, collapse = collapse_sep)
  }

  # Build one canonical record from parsed JSON object (list)
  parse_record <- function(x) {
    # Many possible names/locations for fields across different zeeschuimer dumps.
    # We'll try a list of likely paths for each target field.

    # id: prefer top-level item_id, then data.id, then data.video.id, then data.contents[[1]]$id
    id <- first_nonnull(x, list(
      c("item_id"), c("id"), c("data", "id"), c("data", "video", "id"),
      c("data", "contents", "id"), c("data", "contents", "0", "id")
    ))
    # thread_id: not always present - try conversationId, threadId, groupId
    thread_id <- first_nonnull(x, list(c("thread_id"), c("data", "thread_id"), c("conversationId"), c("data", "groupId")))

    # author and author_full and avatar, stats
    author <- first_nonnull(x, list(c("data", "author", "uniqueId"), c("data", "author", "secUid"),
                                    c("data", "author", "id"), c("data", "author", "nickname")))
    author_full <- first_nonnull(x, list(c("data", "author", "nickname"), c("data", "author", "uniqueId")))
    author_avatar <- first_nonnull(x, list(c("data", "author", "avatarThumb"),
                                           c("data", "author", "avatarMedium"),
                                           c("data", "author", "avatarLarger"),
                                           c("data", "author", "avatar")))
    # author stats: many possiblities: data.authorStatsV2.followerCount or data.authorStats.followerCount etc
    author_followers <- first_nonnull(x, list(
      c("data", "authorStatsV2", "followerCount"),
      c("data", "authorStats", "followerCount"),
      c("data", "authorStats", "follower_count"),
      c("authorStatsV2", "followerCount"),
      c("authorStats", "followerCount")
    ))
    author_likes <- first_nonnull(x, list(
      c("data", "authorStatsV2", "heartCount"),
      c("data", "authorStats", "heartCount"),
      c("data", "authorStats", "heart"),
      c("authorStatsV2", "heartCount"),
      c("authorStats", "heartCount")
    ))
    author_videos <- first_nonnull(x, list(
      c("data", "authorStatsV2", "videoCount"),
      c("data", "authorStats", "videoCount"),
      c("authorStats", "videoCount")
    ))

    # body: data.desc or data.contents combined
    body <- first_nonnull(x, list(c("data", "desc"), c("data", "description"), c("desc")))
    # If data.contents is a list of desc segments, join them
    contents_desc <- get_path(x, c("data", "contents"))
    if (is.null(body) && !is.null(contents_desc) && is.list(contents_desc)) {
      # try to concatenate any 'desc' fields inside contents
      segs <- vapply(contents_desc, function(el) {
        if (!is.null(el$desc)) return(as.character(el$desc))
        return(NA_character_)
      }, FUN.VALUE = character(1), USE.NAMES = FALSE)
      segs <- segs[!is.na(segs) & nzchar(segs)]
      if (length(segs) > 0) body <- paste(segs, collapse = " ")
    }

    # stickers/effects: try obvious keys
    stickers <- first_nonnull(x, list(c("data", "stickers"), c("stickers"), c("data", "stickersList")))
    effects <- first_nonnull(x, list(c("data", "effects"), c("effects"), c("effectsList")))

    # timestamp: try createTime (seconds), or timestamp_collected (ms)
    createTime <- first_nonnull(x, list(c("data", "createTime"), c("createTime"), c("data", "create_time")))
    timestamp_collected <- first_nonnull(x, list(c("timestamp_collected"), c("collected_timestamp"), c("timestampCollected")))

    unix_timestamp <- NA_integer_
    timestamp <- NA_character_
    if (!is.null(createTime)) {
      # createTime often in seconds (integer). If extremely large assume milliseconds.
      if (is.numeric(createTime) && createTime > 1e12) {
        unix_timestamp <- as.integer(floor(createTime / 1000))
      } else {
        unix_timestamp <- as.integer(createTime)
      }
    } else if (!is.null(timestamp_collected)) {
      # timestamp_collected often ms
      if (is.numeric(timestamp_collected) && timestamp_collected > 1e12) {
        unix_timestamp <- as.integer(floor(timestamp_collected / 1000))
      } else {
        unix_timestamp <- as.integer(timestamp_collected)
      }
    }
    if (!is.na(unix_timestamp)) {
      # produce ISO8601 in UTC
      timestamp <- as.character(as.POSIXct(unix_timestamp, origin = "1970-01-01", tz = "UTC"))
    }

    # booleans
    is_duet <- first_nonnull(x, list(c("data", "duetDisplay"), c("duetDisplay"), c("data", "isDuet"), c("isDuet")))
    is_ad <- first_nonnull(x, list(c("isAd"), c("data", "isAd"), c("data", "is_ad")))
    is_paid_partnership <- first_nonnull(x, list(c("data", "is_paid_partnership"), c("isPaidPartnership"), c("isPaidPartnership"), c("paid_partnership")))
    # sensitivity flags
    is_sensitive <- first_nonnull(x, list(c("data", "is_sensitive"), c("isSensitive"), c("data", "isSensitive")))
    is_photosensitive <- first_nonnull(x, list(c("data", "is_photosensitive"), c("isPhotosensitive"), c("is_photosensitive")))

    # music metadata
    music_name <- first_nonnull(x, list(c("data", "music", "title"), c("data", "music", "name"), c("music", "title"), c("music", "authorName")))
    music_id <- first_nonnull(x, list(c("data", "music", "id"), c("music", "id"), c("data", "music", "mid")))
    music_url <- first_nonnull(x, list(c("data", "music", "playUrl"), c("music", "playUrl"), c("data", "music", "play_url")))
    music_thumbnail <- first_nonnull(x, list(c("data", "music", "coverLarge"), c("music", "coverLarge"), c("data", "music", "cover"), c("music", "coverThumb")))
    music_author <- first_nonnull(x, list(c("data", "music", "authorName"), c("music", "authorName"), c("music", "author")))

    # video and thumbnails / urls
    video_url <- first_nonnull(x, list(c("data", "video", "playUrl"), c("data", "video", "play_addr"), c("video", "playAddr"), c("video", "playUrl")))
    thumbnail_url <- first_nonnull(x, list(c("data", "video", "originCover"), c("data", "video", "cover"), c("video", "originCover"), c("video", "cover"), c("data", "imagePost", "cover", "imageURL", "urlList", "1")))
    # tiktok_url: prefer source_url, else construct from author+id
    tiktok_url <- first_nonnull(x, list(c("source_url"), c("source_platform_url"), c("source_url")))

    if (is.null(tiktok_url) && !is.null(author) && !is.null(id)) {
      # construct canonical tiktok.com/@USER/video/ID
      tiktok_url <- paste0("https://www.tiktok.com/@", author, "/video/", id)
    }

    # stats: likes (diggCount), comments, shares, plays
    likes <- first_nonnull(x, list(c("data", "stats", "diggCount"), c("data", "statsV2", "diggCount"),
                                   c("data", "stats", "digg_count"), c("stats", "diggCount"), c("diggCount")))
    comments <- first_nonnull(x, list(c("data", "stats", "commentCount"), c("data", "statsV2", "commentCount"),
                                      c("data", "stats", "comment_count"), c("commentCount")))
    shares <- first_nonnull(x, list(c("data", "stats", "shareCount"), c("data", "statsV2", "shareCount"),
                                    c("data", "stats", "share_count"), c("shareCount")))
    plays <- first_nonnull(x, list(c("data", "stats", "playCount"), c("data", "statsV2", "playCount"),
                                   c("data", "stats", "play_count"), c("playCount")))

    # hashtags: try to find explicit arrays, otherwise regex from body
    hashtags_val <- first_nonnull(x, list(c("data", "hashtags"), c("data", "textExtra"), c("textExtra"), c("data", "tagList")))
    hashtags <- NA_character_
    # If textExtra exists and is a list of objects, try to pull hashtag names
    if (!is.null(hashtags_val)) {
      # If it's a list of objects with 'hashtagName' or 'name'
      if (is.list(hashtags_val) && !is.atomic(hashtags_val)) {
        extracted <- vapply(seq_along(hashtags_val), function(i) {
          el <- hashtags_val[[i]]
          n <- first_nonnull(el, list(c("hashtagName"), c("name"), c("text"), c("title")))
          if (is.null(n)) return(NA_character_)
          as.character(n)
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)
        extracted <- extracted[!is.na(extracted) & nzchar(extracted)]
        if (length(extracted) > 0) hashtags <- paste(unique(extracted), collapse = collapse_sep)
      } else if (is.atomic(hashtags_val)) {
        hashtags <- collapse_val(hashtags_val)
      } else {
        hashtags <- collapse_val(hashtags_val)
      }
    }
    if (is.na(hashtags) && !is.null(body)) {
      hashtags <- extract_hashtags_from_text(body)
    }

    # challenges: sometimes explicit 'challenges' or inside textExtra
    challenges_val <- first_nonnull(x, list(c("data", "challenges"), c("challenges"), c("data", "challengeList")))
    challenges <- collapse_val(challenges_val)
    if (is.na(challenges)) {
      # attempt to extract "challenge" style hashtags or mentions; leave NA if none
      challenges <- NA_character_
    }

    # diversification_labels
    diversification_labels <- first_nonnull(x, list(c("data", "diversificationLabels"), c("diversificationLabels"), c("data", "labels")))
    diversification_labels <- collapse_val(diversification_labels)

    # location_created
    location_created <- first_nonnull(x, list(c("data", "location"), c("location"), c("data", "locationCreated")))

    # warning
    warning_val <- first_nonnull(x, list(c("warning"), c("data", "warning"), c("metadata", "warning")))
    warning <- if (!is.null(warning_val)) as.character(warning_val) else NA_character_

    # stickers and effects collapse
    stickers <- collapse_val(stickers)
    effects <- collapse_val(effects)

    # Convert booleans to logical or NA
    to_logical <- function(v) {
      if (is.null(v)) return(NA)
      if (is.logical(v)) return(as.logical(v))
      if (is.numeric(v)) return(as.logical(as.integer(v)))
      if (is.character(v)) {
        lv <- tolower(as.character(v))
        if (lv %in% c("true", "1", "yes", "y")) return(TRUE)
        if (lv %in% c("false", "0", "no", "n")) return(FALSE)
      }
      NA
    }

    rec <- list(
      id = if (!is.null(id)) as.character(id) else NA_character_,
      thread_id = if (!is.null(thread_id)) as.character(thread_id) else NA_character_,
      author = if (!is.null(author)) as.character(author) else NA_character_,
      author_full = if (!is.null(author_full)) as.character(author_full) else NA_character_,
      author_followers = if (!is.null(author_followers)) as.integer(as.numeric(author_followers)) else NA_integer_,
      author_likes = if (!is.null(author_likes)) as.integer(as.numeric(author_likes)) else NA_integer_,
      author_videos = if (!is.null(author_videos)) as.integer(as.numeric(author_videos)) else NA_integer_,
      author_avatar = if (!is.null(author_avatar)) as.character(author_avatar) else NA_character_,
      body = if (!is.null(body)) as.character(body) else NA_character_,
      stickers = stickers,
      timestamp = if (!is.na(timestamp)) timestamp else NA_character_,
      unix_timestamp = if (!is.na(unix_timestamp)) as.integer(unix_timestamp) else NA_integer_,
      is_duet = to_logical(is_duet),
      is_ad = to_logical(is_ad),
      is_paid_partnership = to_logical(is_paid_partnership),
      is_sensitive = to_logical(is_sensitive),
      is_photosensitive = to_logical(is_photosensitive),
      music_name = if (!is.null(music_name)) as.character(music_name) else NA_character_,
      music_id = if (!is.null(music_id)) as.character(music_id) else NA_character_,
      music_url = if (!is.null(music_url)) as.character(music_url) else NA_character_,
      music_thumbnail = if (!is.null(music_thumbnail)) as.character(music_thumbnail) else NA_character_,
      music_author = if (!is.null(music_author)) as.character(music_author) else NA_character_,
      video_url = if (!is.null(video_url)) as.character(video_url) else NA_character_,
      tiktok_url = if (!is.null(tiktok_url)) as.character(tiktok_url) else NA_character_,
      thumbnail_url = if (!is.null(thumbnail_url)) as.character(thumbnail_url) else NA_character_,
      likes = if (!is.null(likes)) as.integer(as.numeric(likes)) else NA_integer_,
      comments = if (!is.null(comments)) as.integer(as.numeric(comments)) else NA_integer_,
      shares = if (!is.null(shares)) as.integer(as.numeric(shares)) else NA_integer_,
      plays = if (!is.null(plays)) as.integer(as.numeric(plays)) else NA_integer_,
      hashtags = if (!is.null(hashtags)) as.character(hashtags) else NA_character_,
      challenges = if (!is.null(challenges)) as.character(challenges) else NA_character_,
      diversification_labels = if (!is.null(diversification_labels)) as.character(diversification_labels) else NA_character_,
      location_created = if (!is.null(location_created)) as.character(location_created) else NA_character_,
      effects = if (!is.null(effects)) as.character(effects) else NA_character_,
      warning = warning,
      missing_fields = NA_character_ # placeholder, fill below
    )

    # Determine missing fields for this record (fields that are NA or empty)
    miss <- vapply(names(rec)[names(rec) != "missing_fields"], function(nm) {
      val <- rec[[nm]]
      if (is.null(val)) return(TRUE)
      if (is.character(val)) return(is.na(val) || !nzchar(trimws(val)))
      if (is.numeric(val) || is.integer(val)) return(is.na(val))
      if (is.logical(val)) return(is.na(val))
      FALSE
    }, FUN.VALUE = logical(1))
    rec$missing_fields <- paste(names(miss)[which(miss)], collapse = collapse_sep)
    if (identical(rec$missing_fields, "")) rec$missing_fields <- NA_character_

    rec
  }

  # Read file in chunks of lines; parse each line and convert to record
  con <- file(input_path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  all_rows <- list()
  rows_accum <- 0L
  total_parsed <- 0L

  if (verbose) message("Reading ", input_path, " in chunks of ", chunk_size, " lines...")

  repeat {
    lines <- readLines(con, n = chunk_size, warn = FALSE)
    if (length(lines) == 0) break
    # Drop empty lines
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0) next

    parsed <- vector("list", length(lines))
    for (i in seq_along(lines)) {
      ln <- lines[[i]]
      # Some lines may contain trailing commas or whitespace — try robust parsing
      parsed[[i]] <- tryCatch({
        jsonlite::fromJSON(txt = ln, simplifyVector = FALSE)
      }, error = function(e) {
        # try to fix common issue: single quotes or unescaped control characters are hard;
        # we will skip the line but warn
        if (verbose) warning("Skipping invalid JSON line (first 200 chars): ", substring(ln, 1, 200), " ... : ", conditionMessage(e))
        NULL
      })
    }
    # Filter out NULLs
    parsed <- Filter(Negate(is.null), parsed)
    if (length(parsed) == 0) next

    # Convert each parsed JSON object into canonical record
    chunk_records <- lapply(parsed, parse_record)
    rows_accum <- rows_accum + length(chunk_records)
    total_parsed <- total_parsed + length(chunk_records)
    if (verbose) message("Parsed chunk of ", length(chunk_records), " records (total parsed: ", total_parsed, ")")

    # append to all_rows list (we will bind at end)
    all_rows <- c(all_rows, chunk_records)
  }

  if (length(all_rows) == 0) {
    # return an empty tibble with the expected columns
    empty <- as.list(rep(NA, length(target_cols)))
    names(empty) <- target_cols
    return(tibble::as_tibble(empty)[0, , drop = FALSE])
  }

  # Combine into a tibble/data.frame
  if (use_dplyr) {
    df <- dplyr::bind_rows(all_rows)
    df <- tibble::as_tibble(df)
  } else {
    # fallback: attempt base-binding then coerce to tibble
    # this may coerce types more aggressively but should work as fallback
    df <- do.call(rbind, lapply(all_rows, function(rec) {
      # ensure same ordering
      rec[names(all_rows[[1]])]
    }))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df <- tibble::as_tibble(df)
  }

  # Ensure columns appear in the canonical 4CAT order (append any extras at end)
  present <- intersect(target_cols, names(df))
  extras <- setdiff(names(df), target_cols)
  df <- df[, c(present, extras), drop = FALSE]

  if (verbose) message("Completed parsing. Records: ", nrow(df), ". Returning tibble with ", ncol(df), " columns.")

  df
}
