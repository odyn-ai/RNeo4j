#' @rdname transactions
#' @export
appendCypherToDF = function(transaction, query, ...) UseMethod("appendCypherToDF")

#' @export
appendCypherToDF.transaction = function(transaction, query, ...) {
  stopifnot(is.character(query),
            length(query) == 1)
  
  url = transaction$location
  dots = list(...)
  params = parse_dots(dots)
  
  if(length(params) > 0) {
    fields = list(statements = list(list(statement=query, parameters=params)))
  } else {
    fields = list(statements = list(list(statement=query)))
  }
  
  response = http_request(url, "POST", fields)
  
  if(length(response$errors) > 0) {
    error = response$errors[[1]]
    stop(paste(error['code'], error['message'], sep="\n"))
  }
  
  data = response$results[[1]]$data
  
  if(length(data) == 0) {
    return(invisible())
  }
  
  if("metadata" %in% unlist(lapply(data[[1]], names))) {
    stop("You must query for tabular results when using this function.")
  }
  
  if("length" %in% unlist(lapply(data[[1]], names))) {
    stop("You must query for tabular results when using this function.")
  }
  
  ### Stolen from: http://stackoverflow.com/questions/22870198/is-there-a-more-efficient-way-to-replace-null-with-na-in-a-list
  nullToNA <- function(x) {
    x[sapply(x, is.null)] = NA
    return(x)
  }
  ###
  
  data = do.call(rbind, lapply(data, function(x) x$row))
  data = nullToNA(data)
  options(stringsAsFactors = FALSE)
  df = data.frame(data)
  
  df = unlist_deep(df)
  names(df) = response$results[[1]]$columns
  row.names(df) = NULL
  return(df)
}
