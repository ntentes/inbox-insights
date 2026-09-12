# Who may approve a directive when the feedback app runs on Connect.
#
# Connect hands a Shiny app the viewer's username and groups, nothing about
# their role on the content. Whether they are a collaborator (the owner, or
# given the collaborator role in the access list) has to be asked of the
# Connect API with the key Connect supplies. Anything that stops that question
# being answered denies approval and logs why: an approval tab open to every
# viewer is the failure to avoid. Locally there is no Connect and no login, so
# the app allows all.

connect_api_get <- function(path, ...) {
  server <- sub("/+$", "", Sys.getenv("CONNECT_SERVER"))
  key <- Sys.getenv("CONNECT_API_KEY")
  if (!nzchar(server) || !nzchar(key)) {
    stop("CONNECT_SERVER and CONNECT_API_KEY are not both set.", call. = FALSE)
  }
  httr2::request(paste0(server, "/__api__/v1", path)) |>
    httr2::req_headers(Authorization = paste("Key", key)) |>
    httr2::req_url_query(...) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# The GUID of the content this process is: from Connect's environment, or
# failing that the item of that name owned by the API key's user.
connect_content_guid <- function(content_name, api = connect_api_get) {
  guid <- Sys.getenv("CONNECT_CONTENT_GUID")
  if (nzchar(guid)) return(guid)
  me <- api("/user")
  items <- api("/content", name = content_name, owner_guid = me$guid)
  if (!length(items)) {
    stop("No content named ", content_name, " is owned by ", me$username, ".", call. = FALSE)
  }
  items[[1]]$guid
}

# The owner and every principal holding the owner role in the access list, as
# usernames and group names, since that is what session$user and
# session$groups give a Shiny app to compare against.
connect_collaborators <- function(guid, api = connect_api_get) {
  content <- api(paste0("/content/", guid))
  permissions <- api(paste0("/content/", guid, "/permissions"))
  owners <- Filter(function(p) identical(p$role, "owner"), permissions)
  principal_guids <- function(type) {
    of_type <- Filter(function(p) identical(p$principal_type, type), owners)
    vapply(of_type, `[[`, character(1), "principal_guid")
  }
  user_guids <- unique(c(content$owner_guid, principal_guids("user")))
  group_guids <- unique(principal_guids("group"))
  list(
    users = vapply(user_guids, function(g) api(paste0("/users/", g))$username, character(1), USE.NAMES = FALSE),
    groups = vapply(group_guids, function(g) api(paste0("/groups/", g))$name, character(1), USE.NAMES = FALSE)
  )
}

is_collaborator <- function(user, groups, collaborators) {
  isTRUE(user %in% collaborators$users) || any(groups %in% collaborators$groups)
}

# Whether this user may approve, and if not, why: for the log, not the reader.
feedback_access <- function(user, groups, content_name, api = connect_api_get) {
  result <- tryCatch({
    guid <- connect_content_guid(content_name, api)
    collaborators <- connect_collaborators(guid, api)
    list(allowed = is_collaborator(user, groups %||% character(), collaborators), reason = NULL)
  }, error = function(e) list(allowed = FALSE, reason = conditionMessage(e)))
  if (!result$allowed && is.null(result$reason)) {
    result$reason <- paste(user, "is not a collaborator on this app.")
  }
  result
}
