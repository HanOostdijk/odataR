#' odataR: A package for reading OData information.
#'
#' The odataR package contains some functions to obtain information from databases with the OData interface.
#'
#' Located under the root of an OData database are some tables. To obtain the information one should specify the correct root and the name of the table. Because this package was created to access the information of the CBS (Statistics Netherlands) the default root is appropriate for this information. The name of a table (here called table_id) can be found in the documentation of the database. For the CBS you can use the catalog \url{https://opendata.cbs.nl/dataportaal/portal.html} to see the available tables and their metadata. It is also possible to get the catalog information in a data.frame by using the function \code{\link{odataR_get_cat}}.
#'
#' Setting a different root can be done by using the function \code{\link{odataR_set_root}} and querying the root by executing \code{\link{odataR_get_root}}.
#'
#' The other functions in the package are related to actually obtain information from the database. In the CBS database a table name (\code{table_id}) points to a set of sub tables that together provide the information. One 'main' sub table contains the topic data and  dimensions in coded form indicating where the topic data relates to. The other sub tables convey the meaning of the coded dimensions. E.g. a topic field could be the number of unemployed workers and the dimensions could be gender, age-class, province and year.
#'
#' The main function is \code{\link{odataR_get_table}} that extracts the information from the database and decodes the dimensions. This function can also be used to get \code{metadata} about the table. It is possible to use the OData query language.
#'
#' Currently version 3  is used for the CBS data but a conversion to version 4  is taking place at the moment (January 2018)
#'
#' v3 : \url{http://www.odata.org/documentation/odata-version-3-0/odata-version-3-0-core-protocol/}
#'
#' v4 : \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html}
#'
#' For examples of the query language see \code{\link{odataR_get_table}}
#'
#' The description of the function \code{\link{odataR_get_cat}} contains examples to retrieve catalog information. The same query language can be used when that is useful.
#'

#'
#' @name odataR
NULL

#' Retrieve OData (sub) table
#'
#' \code{odataR_query} is a low level function used to retrieve OData data. The high level functions (as e.g.  \code{\link{odataR_get_table}}) convert the user specification to one or more calls to \code{odataR_query} with the appropriate \code{url} and \code{query} . \cr
#' \code{odataR_query2} is similar to \code{odataR_query}. Actually \code{odataR_query2} is called by \code{odataR_query} after the full url is created by concatenating the 'url' and 'query' part. \cr
#' Normally these functions are not directly used by the occasional user. 
#' @name odataR_query
#' @export
#' @examples
#' \dontrun{
#' odataR_get_cat(query='$count',debug=T) # is executed as 
#' odataR_query(
#'    odata_url="https://opendata.cbs.nl/ODataCatalog/Tables",
#'    odata_query ="$count",
#'    )                                   # is executed as 
#' odataR_query2(
#'    odata_url="https://opendata.cbs.nl/ODataCatalog/Tables/$count"
#'    )
#' }

odataR_query <- function (odata_url,odata_query=NULL,
             debug=F,err_msg=NULL,response_only=F) {
  
	# adjust query 
	a_query   = adjust_query(odata_query)
	# print debugging statements
	if (debug) {
		cat(paste0('debug odataR_query url   : ',odata_url,'\n'))
		cat(paste0('debug odataR_query query : ',odata_query,'\n'))
		cat(paste0('debug odataR_query aquery: ',a_query,'\n'))
	}
	url1 = paste0(odata_url, a_query)
	odataR_query2(url1, debug=debug, 
	       err_msg = err_msg, response_only=response_only)
}
#' @param odata_url Character string with the requested url (and in case of \code{odataR_query2} the attached query)
#' @param odata_query Character string with the query to attach to the url (not used in case of \code{odataR_query2}) 
#' @param debug Boolean indicating if the \code{odata_url} and \code{odata_query} argument will be printed. Default \code{FALSE} 
#' @param err_msg Character string with message to display in case of error. Default \code{NULL}
#' @param response_only Boolean indicating that the result of this function should be the response object for the OData call and not the unpacked table. Could be useful for debugging. Default \code{FALSE}   
#' @rdname odataR_query
#' @name odataR_query2
#' @export
#' 
odataR_query2 <- function (odata_url,debug=F, 
            err_msg = NULL,response_only=F) {
  o1=options(error = my_stop,show.error.messages=FALSE)
  on.exit(expr=options(o1))
  if (debug)
    on.exit(
      expr=cat(paste('debug odataR_query2 :',
             numcalls, 'call(s) to \n\t',odata_url, '\n\n')) ,
      add=T)
  err_msg1  <- ifelse(is.null(err_msg), 'Error in OData request',err_msg)
  numcalls = 1
  res1 = httr::GET(odata_url)
  if (response_only == TRUE) {
    return(res1)
  }
  html1 = grepl('html',httr::headers(res1)$`content-type`,fixed=T )
  if (httr::http_error(res1) || html1)
    stop(err_msg1)
  txt1 = httr::content(res1, as = "text",encoding='UTF-8')
  tryCatch(
    res <- jsonlite::fromJSON(txt1),
    error = function(e) stop(err_msg1),
    finally = {}
  )
  if (is.numeric(res))
    # if numeric then return (e.g. in case of $count)
    value = res
  else if (!is.null(res[['odata.error']])) 
    stop(err_msg1)
  else {
    # when no OData error we get actual data in value
    value = res[['value']]
    value = as.data.frame(value)
    # check if there is more data to collect 
    nextLink = res[['odata.nextLink']]
    while (!is.null(nextLink)){
      # retrieve more data, append to earlier data and check again for more data
      res  <- jsonlite::fromJSON(nextLink)
      numcalls = numcalls + 1
      value = rbind(value,as.data.frame(res[['value']]))
      nextLink = res[['odata.nextLink']]
    }
  }
  value
}

my_stop <- function() {
  e <- geterrmessage()
  # e <- strsplit(e,":")[[1]][-1]
  e <- substring(e,regexpr(":", e)+2) 
  cat(e)
}

#' Get table data
#'
#' This is the main function of this package. It retrieves a data table from the data base and all dimensions are decoded (if requested). An (optional) query is executed in the database
#' @param root Character string with root of structure. Default NULL i.e. determined by the last use of \code{odataR_set_root} and if never used \code{'https://opendata.cbs.nl'}
#' @param table_id Character string with identification of table
#' @param query OData query to restrict data returned from structure
#' @param typed Boolean indicating 'TypedDataSet' when T or 'UntypedDataSet' when F 
#' @param decode Boolean indicating if fields are to be decoded (default T)
#' @param keepcode Character string with dimension(s) for which the coded values are kept
#' @return A \code{tibble} (\code{data_frame}) when successful otherwise an empty \code{tibble} or a message
#' @param debug Boolean indicating if (for debugging) the generated OData queries are to be printed (default FALSE)
#' @export
#' @section Remarks:
#' See  \url{http://www.odata.org/documentation/odata-version-3-0/odata-version-3-0-core-protocol/} or \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} (depending on the version of the OData protocol that is used) for details about the query possibilities.
#'
#' \code{$format=json} is forced by the code so do not specify \code{$format=atom} because this will not work
#' @examples
#' \dontrun{
#' df      = odataR_get_table(table_id="82935NED")
#' df      = odataR_get_table(table_id="82935NED",keepcode = "RegioS")
#' df      = odataR_get_table(table_id="82935NED",keepcode = c("RegioS","Perioden"))
#' df      = odataR_get_table(table_id="82935NED",
#'   query  = paste0("?$filter=startswith(RegioS,'NL01')",
#'                   "&$select=RegioS,Perioden,TotaleInvesteringen_1",
#'                   "&$skip=2&$top=3"),
#'   debug=T)
#' }

odataR_get_table <- function(
		root     = NULL,
		table_id = NULL,
		query    = NULL,
		typed    = T,
		decode   = T,
		keepcode = c(),
		debug    = F
		) {
  if (!is.null(root) ) odataR::odataR_set_root(root,debug=debug)
  root     = odataR::odataR_get_root_data()
	bname    = paste0(root, '/', gsub(" ", "", table_id))
	err_msg  = 'No data found for this "table_id" in this catalog'
	subtabt  = odataR_query(bname,debug=debug,err_msg=err_msg)
	if (!('url' %in% names(subtabt))) 
	  stop('Internal error in package "odataR"')
	subtabs  = subtabt$url
	names(subtabs) = subtabt$name
	tds      = ifelse(typed == F, 'UntypedDataSet', 'TypedDataSet')
	df       = odataR_query(subtabs[tds], query,debug=debug,err_msg=NULL)
	if ((decode == F) || !('data.frame' %in% class(df))) return(df)
	err_msg  = paste0('Internal error in package "odataR" :',subtabs['DataProperties'])
	props    = odataR_query(subtabs['DataProperties'],debug=debug,err_msg=err_msg)
	dv       = dplyr::filter(props, Type %in% c('Dimension', 'TimeDimension', 'GeoDimension'))
	dv       = dplyr::filter(dv, Key %in% names(df))
	couple_data(df, dv, subtabs, keepcode,debug)
}

adjust_query <- function (query) {
	# force this as json query and adapt for $count
	query    = ifelse(is.null(query), '', query)
	# remove the '?'
	query = gsub('?','',query,fixed=T)
	# remove '$format=json' however it occurs
	query = remove_clause(query,'\\$format=json')
	# insert '?$format=json' at start of query
	query = paste0('?$format=json',ifelse(nchar(query)>0,'&',''),query)
	# adapt for $count clause
	if (grepl("\\$count",query,ignore.case=T)) {
		# # remove it everwhere
		# query = remove_clause(query,'\\$count')
		# # insert /$count in front of query
		# query = paste0('/$count',query)
		# $count in combination with $query does not seem to work for CBS data
		# $format=json in combination with catalog request does not seem to work for CBS data
		query = '/$count'
	}
	# encode the query
	URLencode(query)
}

remove_clause <- function (query,clause) {
	# remove clause however it occurs in query
	c1 = glue::glue('^{clause}$')
	query = gsub(c1,'',query,perl = T,ignore.case=T)
	c1 = glue::glue('^{clause}&')
	query = gsub(c1,'',query,perl = T,ignore.case=T)
	c1 = glue::glue('&{clause}$')
	query = gsub(c1,'',query,perl = T,ignore.case=T)
	c1 = glue::glue('&{clause}&')
	query = gsub(c1,'&',query,perl = T,ignore.case=T)
	query
}

couple_data <- function(
	df,         # data.frame with coded dimensions (e.g. read by odataR_get_subtable)
	dv,         # character column (Key) with the names of the dimensions
	table_list, # named vector with urls of sub tables
	keepcode,   # dimension(s) for which coded value is kept
	debug       # debugging ?
) {
	tt = df
	for (dim in dv$Key)  {
		if (dim %in% keepcode ) {
			kc = T
		} else {
			kc = F
		}
		tt = couple_data_dim(tt, table_list[dim], # link dimension data
					 keep_code=kc,debug=debug) 
	}
	return(tt)
}

couple_data_dim <- function(tt, dsn, keep_code=F,debug=F) {
	dim  = names(dsn)
	err_msg  = paste0('Internal error in package "odataR" :',dsn)
	tab1 = odataR_query(dsn,debug=debug,err_msg=err_msg)
	tab1 = dplyr::select(tab1,'Key', 'Title')
	tab1 = odataR_rename(tab1,'Title', paste0(dim, '_decode'))
	by1  = c('Key') ; names(by1) = dim
	tt   = dplyr::inner_join(tt, tab1, by = by1)
	tt   = odataR_rename(tt,dim, paste0(dim, '_coded'))
	tt   = odataR_rename(tt,paste0(dim, '_decode'), dim)
	if (keep_code == F) {
		tt = odataR_drop(tt,paste0(dim, '_coded'))
	}
	return(tt)
}

odataR_rename <- function (df,oldname,newname) {
	quo_oldname = dplyr::quo_name(oldname)
	quo_newname = dplyr::quo_name(newname)
	dplyr::rename(df,!!quo_newname := !!quo_oldname)
}

odataR_drop <- function (df,oldname) {
	quo_oldname = dplyr::quo_name(oldname)
	dplyr::select(df,-dplyr::one_of(!!quo_oldname))
}

#' Get catalog information
#'
#' This function can be used to get information from an OData catalog.
#' @param root Character string with root of structure. Default NULL i.e. determined by the last use of \code{odataR_set_root} and if never used \code{'https://opendata.cbs.nl'}
#' @param type Type of catalog information. This type can be one of:
#' \itemize{
#'  \item \code{NULL} : a \code{tibble} with the various types in this catalog \cr
#'  \item \code{'Tables'} or empty : a \code{tibble} with the Tables in this catalog \cr
#'  \item \code{'Featured'} : a \code{tibble} with the Features in this catalog \cr
#'  \item \code{'Table_Featured'} : a \code{tibble} with the Table_Featured in this catalog \cr
#'  \item \code{'Themes'} : a \code{tibble} with the Themes in this catalog \cr
#'  \item \code{'Tables_Themes'} : a \code{tibble} with the Tables_Themes in this catalog \cr
#' }
#' @param query OData query to restrict data returned from the catalog
#' @param debug Boolean indicating if (for debugging) the generated OData queries are to be printed (default FALSE)
#' @return A \code{tibble} (\code{data_frame}) when successful otherwise an empty \code{tibble} or a message
#' @export
#' @section Remarks:
#' See  \url{http://www.odata.org/documentation/odata-version-3-0/odata-version-3-0-core-protocol/} or \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} (depending on the version of the OData protocol that is used) for details about the query possibilities.
#'
#'\code{$format=json} is forced by the code so do not specify \code{$format=atom} because this will not work
#'
#' @examples
#' \dontrun{
#' # all types in catalog:
#' df = odataR_get_cat(type=NULL)
#' # all Tables in catalog:
#' df = odataR_get_cat()
#' # all tables that have a Title starting with 'Vacature':
#' df = odataR_get_cat(query="?$filter=startswith(Title,'Vacature')")
#' # all tables that have a Title containing 'vacature' regardless of case:
#' df = odataR_get_cat(query="?$filter=substringof('vacature',tolower(Title))")
#' # all active tables that have a Title containing 'vacature' regardless of case:
#' df = odataR_get_cat(query="?$filter=substringof('vacature',tolower(Title)) and Frequency ne 'Stopgezet' ")
#' # all Featured entries in catalog:
#' df = odataR_get_cat(type='Featured')
#' # all Featured tables (?) in catalog:
#' df = odataR_get_cat(type='Table_Featured')
#' # all Themes in catalog:
#' df = odataR_get_cat(type='Themes')
#' # all Tables_Themes (?) in catalog:
#' df = odataR_get_cat(type='Tables_Themes')
#' }

odataR_get_cat <- function(
    root=NULL,
    type='Tables',
    query=NULL,
    debug=F) {
	if (is.null(type) || 
			type %in% c('Tables','Featured','Table_Featured','Themes','Table_Themes')) {
		err_msg = 'invalid catalog query'
	} else {
		err_msg = 'invalid catalog type' 
	}
  if (!is.null(root) ) odataR::odataR_set_root(root,debug=debug)
	odataR_query(odataR_get_root_catalog(type),query,debug=debug,err_msg=err_msg)
}

#' Get meta data about table
#'
#' This function can be used to get information about an OData table.
#' @param root Character string with root of structure. Default NULL i.e. determined by the last use of \code{odataR_set_root} and if never used \code{'https://opendata.cbs.nl'}
#' @param table_id Identification of table
#' @param metatype indicates the type of metadata that will be retrieved. Possible values are:
#' \itemize{
#'  \item \code{NULL} or empty : a \code{tibble} with the urls of the subtables will be retrieved \cr
#'  \item \code{'TableInfos'} : a \code{tibble} with one row is retrieved with (background) information about the table \cr
#'  \item \code{'CategoryGroups'} : a \code{tibble} with one row is retrieved with information about the category tables for this table \cr
#'  \item \code{'DataProperties'} : a \code{tibble} is retrieved with information about the columns of the table \cr
#'  \item a dimension name : a \code{tibble} is retrieved with the code information of the dimension (e.g.  \code{Gemeenten} in the examples below). \cr
#' }
#' @param query OData query to restrict data returned from the meta table (do not use in combination with \code{metatype=NULL})
#' @param debug Boolean indicating if (for debugging) the generated OData queries are to be printed (default FALSE)
#' @return A \code{tibble} (\code{data_frame}) when successful otherwise an empty \code{tibble} or a message
#' @export
#' @section Remarks:
#' See  \url{http://www.odata.org/documentation/odata-version-3-0/odata-version-3-0-core-protocol/} or \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} (depending on the version of the OData protocol that is used) for details about the query possibilities.
#'
#'\code{$format=json} is forced by the code so do not specify \code{$format=atom} because this will not work
#'
#' @examples
#' \dontrun{
#' odataR_set_root('http://dataderden.cbs.nl') 
#' # NB statement in previous line is necessary
#' #  otherwise the examples below will not work!
#' df = odataR_get_meta(table_id='45042NED')
#' df = odataR_get_meta(table_id='45042NED',metatype='TableInfos')
#' df = odataR_get_meta(table_id='45042NED',metatype='CategoryGroups')
#' df = odataR_get_meta(table_id='45042NED',metatype='DataProperties')
#' df = odataR_get_meta(table_id='45042NED',metatype='Gemeenten')
#' df = odataR_get_meta(table_id='45042NED',metatype='Verslagsoort')
#' df = odataR_get_meta(table_id='45042NED',metatype='Gemeenten',
#'        query="?$filter=substringof('amstelveen',tolower(Title))",
#'        debug=T)
#' }

odataR_get_meta <- function(
		root     = NULL,
		table_id = NULL,
		metatype = NULL,
		query    = NULL,
		debug    =FALSE
	) {
  o1=options(error = my_stop,show.error.messages=FALSE)
  on.exit(expr=options(o1))
  if (!is.null(root) ) odataR::odataR_set_root(root,debug=debug)
  root     = odataR::odataR_get_root_data()
	bname    = paste0(root, '/', gsub(" ", "", table_id))
	err_msg  = 'No data found for this "table_id" in this catalog'
	subtabt  = odataR_query(bname,debug=debug,err_msg=err_msg)
	if (!('url' %in% names(subtabt))) return(dplyr::data_frame())
	if (is.null(metatype) | length(metatype) == 0)   return(subtabt)
	subtabs  = subtabt$url
	names(subtabs) = subtabt$name
	if (metatype == 'TableInfos') {
	  err_msg  = paste0('Internal error in package "odataR" :',subtabs['TableInfos'])
		return(odataR_query(subtabs['TableInfos'],query,debug=debug,err_msg=err_msg))
	}
	if (metatype == 'CategoryGroups') {
		if (!('CategoryGroups' %in% names(subtabs))) return(dplyr::data_frame())
	  err_msg  = paste0('Internal error in package "odataR" :',subtabs['CategoryGroups'])
		return(odataR_query(subtabs['CategoryGroups'],query,debug=debug,err_msg=err_msg))
	}
	err_msg  = paste0('Internal error in package "odataR" :',subtabs['CategoryGroups'])
	if (metatype == 'DataProperties') {
		return(odataR_query(subtabs['DataProperties'],query,debug=debug,err_msg=err_msg))
	}
	else {
		props    = odataR_query(subtabs['DataProperties'],debug=debug,err_msg=err_msg)
	}
	dv       = dplyr::filter(props, Type %in% c('Dimension', 'TimeDimension', 'GeoDimension'))
	if (metatype %in% dv$Key) {
	  err_msg  = paste0('Internal error in package "odataR" :',subtabs[metatype])
		return(odataR_query(subtabs[metatype],query,debug=debug,err_msg=err_msg))
	} else {
	  stop( paste0('unknown metatype : ',metatype ))
	}
}
