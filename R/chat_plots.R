# How a chart drawn in the sandbox reaches the reader.
#
# The chat serves the REPL's scratch directory over HTTP under a per-session
# prefix, and the model is told to save a PNG there and embed it as a markdown
# image behind that prefix. Two things go wrong. The model refers to the file
# some other way (a bare name, the filesystem path, or a leading slash), which
# resolves locally but on Connect escapes the worker path and 404s. Or the
# model prints the plot instead of saving it: mcp-repl returns the picture to
# the model, which then embeds a file that does not exist. So the reply is
# rewritten as it streams to point every image naming a scratch file at the
# served copy, and the REPL tool is wrapped so a printed plot is written into
# the scratch directory under a name the model is told about.

chat_plot_url <- function(files_prefix, file) paste0(files_prefix, "/", file)

CHAT_IMAGE_PATTERN <- "!\\[([^]]*)\\]\\(([^)[:space:]]+)\\)"

# Every markdown image whose target names a file in `dir` is pointed at the
# served copy. One that meant such a file but names none becomes a note rather
# than a broken icon. Images elsewhere on the web pass.
rewrite_plot_links <- function(text, dir, files_prefix) {
  rewrite_one <- function(image) {
    parts <- regmatches(image, regexec(CHAT_IMAGE_PATTERN, image, perl = TRUE))[[1]]
    alt <- parts[[2]]
    target <- parts[[3]]
    file <- basename(sub("[?#].*$", "", target))
    if (nzchar(file) && file.exists(file.path(dir, file))) {
      return(sprintf("![%s](%s)", alt, chat_plot_url(files_prefix, file)))
    }
    meant_local <- grepl(files_prefix, target, fixed = TRUE) || !grepl("^[A-Za-z][A-Za-z0-9+.-]*:", target)
    if (meant_local) {
      message("chat: the reply shows `", file, "` but no such file is in the REPL directory; the reader gets a note instead")
      return(sprintf("*(The chart `%s` was not saved in the R session, so it cannot be shown.)*", file))
    }
    image
  }
  matches <- gregexpr(CHAT_IMAGE_PATTERN, text, perl = TRUE)
  regmatches(text, matches) <- lapply(regmatches(text, matches), function(images) {
    vapply(images, rewrite_one, character(1), USE.NAMES = FALSE)
  })
  text
}

# A markdown image can arrive split across chunks. Text up to the last image
# that has begun but not closed is ready to send; the rest is held for the
# next chunk. A trailing "!" is held too, since "![" may be about to follow.
split_unfinished_image <- function(text) {
  open <- regexpr("(!\\[[^]]*(\\]\\([^)]*)?|!)$", text, perl = TRUE)
  if (open < 1) return(list(ready = text, held = ""))
  list(ready = substr(text, 1, open - 1), held = substr(text, open, nchar(text)))
}

# The model's stream with image links rewritten as they complete, for
# shinychat::chat_append(). Anything that is not text passes straight through.
#
# A generator's arguments are evaluated lazily, on its first step. A caller
# that writes `stream <- rewrite_plot_stream(stream, ...)` has by then rebound
# `stream` to the wrapper, which would iterate itself and be disabled by coro
# with no useful error. So the arguments are forced here, before the generator
# exists.
rewrite_plot_stream <- function(stream, dir, files_prefix) {
  force(stream); force(dir); force(files_prefix)
  rewrite_plot_generator(stream, dir, files_prefix)
}

rewrite_plot_generator <- coro::async_generator(function(stream, dir, files_prefix) {
  held <- ""
  for (chunk in coro::await_each(stream)) {
    if (is.character(chunk)) {
      parts <- split_unfinished_image(paste0(held, paste(chunk, collapse = "")))
      held <- parts$held
      if (nzchar(parts$ready)) yield(rewrite_plot_links(parts$ready, dir, files_prefix))
    } else {
      if (nzchar(held)) yield(rewrite_plot_links(held, dir, files_prefix))
      held <- ""
      yield(chunk)
    }
  }
  if (nzchar(held)) yield(rewrite_plot_links(held, dir, files_prefix))
})

# mcp-repl returns a printed plot as an inline image, which only the model
# sees. The wrapped tool writes that image into the scratch directory and
# replaces it with the file name and the exact markdown to embed, so a printed
# plot costs no image tokens and can still be shown. The wrapper keeps the
# tool's formals so ellmer can match them to its schema.
capture_repl_plots <- function(tool, dir, files_prefix) {
  inner <- S7::S7_data(tool)
  saved <- 0L
  fun <- function() NULL
  formals(fun) <- formals(inner)
  body(fun) <- quote({
    args <- list()
    for (name in names(formals(sys.function()))) {
      if (!eval(call("missing", as.name(name)))) args[[name]] <- get(name)
    }
    result <- do.call(inner, args)
    if (!is.list(result)) return(result)
    for (i in seq_along(result)) {
      if (S7::S7_inherits(result[[i]], ellmer::ContentImageInline)) {
        saved <<- saved + 1L
        file <- sprintf("plot_printed_%02d.%s", saved, sub("^image/", "", result[[i]]@type))
        writeBin(jsonlite::base64_dec(result[[i]]@data), file.path(dir, file))
        message("chat: a plot the model printed was saved as ", file)
        result[[i]] <- ellmer::ContentText(paste0(
          "[The plot you printed was saved as ", file, ". The reader has not seen it. ",
          "To show it, put ![caption](", chat_plot_url(files_prefix, file), ") in your reply.]"
        ))
      }
    }
    result
  })
  ellmer::tool(
    fun, name = tool@name, description = tool@description,
    arguments = tool@arguments@properties, convert = tool@convert, annotations = tool@annotations
  )
}
