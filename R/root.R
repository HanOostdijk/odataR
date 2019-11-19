
.odataR_options <- new.env()

.onLoad <- function(libname, pkgname) {
  invisible()
}

odataR_root_default = 'https://opendata.cbs.nl'

#' Sets the root for OData data and catalog structure
#'
#' Determines from which structure data and catalog information  will be extracted. If this function is not called
#' the default \code{https://opendata.cbs.nl} (the root for the CBS Statistics Netherlands structures) will be used
#' @param root NULL for the default or the url of the root structure otherwise
#' @param debug Boolean indicating if (for debugging) the generated OData queries are to be printed (default FALSE)
#' @export
#' @examples
#' odataR_set_root()
#' odataR_set_root('https://opendata.cbs.nl')
#' @seealso \code{\link{odataR_get_root}}

odataR_set_root <- function (root=NULL,debug=F) {
  if (is.null(root)) {
    root = odataR_root_default
  }
  # check validity of root
  url = paste0(root, '/ODataCatalog/Tables')
  err_msg='invalid value for root'
  dum= odataR_query(url,odata_query='$count',debug=debug,err_msg=err_msg)
  # in case of error next statemens are not executed
  .odataR_options$root = root
  invisible(root)
}

#' Gets the root for OData data and catalog structure
#'
#' Determines from which structure data and catalog information will be extracted. If the function odataR_set_root
#' is not called yet this will be done first with the default \code{https://opendata.cbs.nl} (the root for the CBS Statistics Netherlands structures).
#' @export
#' @examples
#' odataR_get_root()
#' @seealso \code{\link{odataR_set_root}}
odataR_get_root <- function () {
  root = .odataR_options$root
  if (is.null(root)) {
    root = odataR_set_root()
  }
  invisible(root)
}

#' Gets the root for OData data structure
#'
#' Determines from which structure the data will be extracted. Derived from the common OData data and catalog root: \code{paste0(odataR_get_root(), '/ODataFeed/OData')}
#' @seealso \code{\link{odataR_get_root}} and \code{\link{odataR_get_root_catalog}}
#' @export
odataR_get_root_data <- function () {
  paste0(odataR_get_root(), '/ODataFeed/OData')
}

#' Gets the root for OData catalog structure
#'
#' Determines from which structure the catalog information will be extracted. Derived from the common OData data and catalog root: \code{paste0(odataR_get_root(), '/ODataCatalog/Tables')}.
#'
#' Types of catalog entries available are \code{Featured}, \code{Table_Featured}, \code{Tables}, \code{Themes}, \code{Tables_Themes}
#' @seealso \code{\link{odataR_get_root}} and \code{\link{odataR_get_root_data}}
#' @export
odataR_get_root_catalog <- function (type='Tables') {
  paste0(odataR_get_root(), '/ODataCatalog/', type)
}
