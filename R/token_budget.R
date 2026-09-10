# Token limits for an unattended run.
#
# A REPL on a schedule is an open-ended loop with a bill attached. Two caps and
# one warning: the warning asks the model to finish what it has, the caps stop
# the run regardless. Nothing here retries or second-guesses the counter.
#
# The warning has to travel inside a tool result, because that is the only
# channel back to the model mid-conversation. ellmer's on_tool_result callback
# sees every result but cannot change what the model receives -- its return
# value is discarded -- so the notice is attached by the tools themselves:
# check_token_budget() sets it aside, and every tool passed through
# deliver_budget_notice() appends it to its next reply, once.

token_budget <- function(input_cap, output_cap, warn_at = 0.8, call_cap = Inf) {
  stopifnot(input_cap > 0, output_cap > 0, warn_at > 0, warn_at < 1, call_cap > 0)
  budget <- new.env(parent = emptyenv())
  budget$input_cap <- input_cap
  budget$output_cap <- output_cap
  budget$call_cap <- call_cap
  budget$calls <- 0L
  budget$warn_at <- warn_at
  budget$warned <- FALSE
  budget$pending <- NULL
  budget
}

# `usage` is chat$get_tokens(): one row per completed assistant turn, with
# `input`, `output` and `cached_input` columns. Input is billed on every turn,
# so the sum is what the run costs, not the size of the context. Cached input
# is billed too, at a lower rate, and Anthropic reports it separately from
# `input`; leaving it out would undercount every turn after the first.
check_token_budget <- function(budget, usage) {
  if (is.null(usage) || !nrow(usage)) return(invisible(budget))
  count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  cached <- if ("cached_input" %in% names(usage)) usage$cached_input else 0
  used_in <- sum(usage$input, na.rm = TRUE) + sum(cached, na.rm = TRUE)
  used_out <- sum(usage$output, na.rm = TRUE)

  if (used_in > budget$input_cap || used_out > budget$output_cap) {
    stop(
      "Token cap reached (", count(used_in), " in, ", count(used_out), " out). ",
      "Run aborted.",
      call. = FALSE
    )
  }

  near <- used_in > budget$warn_at * budget$input_cap ||
    used_out > budget$warn_at * budget$output_cap
  if (near && !budget$warned) {
    budget$warned <- TRUE
    budget$pending <- paste0(
      "[SYSTEM NOTICE] You have used ", round(100 * budget$warn_at),
      "% of the token budget (", count(used_in), " of ", count(budget$input_cap),
      " in, ", count(used_out), " of ", count(budget$output_cap), " out). ",
      "Stop exploring. Finish the headline in progress, submit what is ready, ",
      "and call submit_report."
    )
  }
  invisible(budget)
}

# Tokens are only reported per completed turn, so a model making many small
# tool calls would run for a long time before the token caps noticed. This is
# for chat$on_tool_request(), which runs before the tool does and whose errors
# abort the run. It cannot live inside the tool: ellmer catches a tool's error
# and hands it back to the model as a result, which is a nudge, not a stop.
count_tool_call <- function(budget) {
  budget$calls <- budget$calls + 1L
  if (budget$calls > budget$call_cap) {
    stop("Tool call cap reached (", budget$call_cap, " calls). Run aborted.",
      call. = FALSE)
  }
  invisible(budget)
}

# The tool keeps its name, description and argument types; only what it
# returns changes, and only while a notice is waiting.
#
# do.call() with evaluated values rather than inner(...): the tools mcptools
# builds recover their arguments with match.call(), which cannot see through a
# forwarded `...`. This is also how ellmer itself invokes a tool.
deliver_budget_notice <- function(tool, budget) {
  inner <- S7::S7_data(tool)
  S7::S7_data(tool) <- function(...) {
    result <- do.call(inner, list(...))
    notice <- budget$pending
    if (is.null(notice)) return(result)

    if (inherits(result, "ellmer::ContentToolResult")) {
      if (is.character(result@value) && length(result@value) == 1L) {
        result@value <- paste0(result@value, "\n\n", notice)
        budget$pending <- NULL
      }
      return(result)
    }
    if (is.character(result) && length(result) == 1L) {
      budget$pending <- NULL
      return(paste0(result, "\n\n", notice))
    }
    result
  }
  tool
}
