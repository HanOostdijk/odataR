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

#' @export
odataR_query <- function (odata_url) {
	hoqc_chr <- 'Error in fromJSON probably invalid url'
	tryCatch(hoqc_chr  <- jsonlite::fromJSON(odata_url), error = function(e) e, finally = {})
	if (length(hoqc_chr) ==1 && hoqc_chr == 'Error in fromJSON probably invalid url')
		value = paste(hoqc_chr,': ',odata_url)
	else if (is.numeric(hoqc_chr))
		value = hoqc_chr
	else if (is.null(hoqc_chr[['odata.error']])) {
		value = hoqc_chr[['value']]
	  nextLink = hoqc_chr[['odata.nextLink']]
	  if (!is.null(nextLink)) value = dplyr::as_data_frame(value)
	  while (!is.null(nextLink)){
	  	# tryCatch(hoqc_chr  <- fromJSON(nextLink), error = function(e) e, finally = {})
	  	hoqc_chr  <- jsonlite::fromJSON(nextLink)
	  	value = rbind(value,dplyr::as_data_frame(hoqc_chr[['value']]))
	  	nextLink = hoqc_chr[['odata.nextLink']]
	  }
	}
	else
	  value = hoqc_chr[['odata.error']][['msg']]
	return(value)
}

#' Get table data
#'
#' This is the main function of this package. It retrieves a data table from the data base and all dimensions are decoded. An (optional) query is executed in the database
#' @param root Root of data structure
#' @param table_id Identification of table
#' @param query OData query to restrict data returned from structure
#' @param typed Boolean indicating 'TypedDataSet' when T or 'UntypedDataSet' when F'
#' @param keepcode Character string with dimension(s) for which the coded values are kept
#' @return A \code{tibble} (\code{data_frame}) when successful otherwise an empty \code{tibble} or a message
#' @export
#' @section Remarks:
#' See  \url{http://www.odata.org/documentation/odata-version-3-0/odata-version-3-0-core-protocol/} or \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} (depending on the version of the OData protocol that is used) for details about the query possibilities.
#'
#'\code{$format=json} is forced by the code so do not specify \code{$format=atom} because this will not work
#' @examples
#' \dontrun{
#' df      = odataR_get_table(table_id="82935NED")
#' df      = odataR_get_table(table_id="82935NED",keepcode = "RegioS")
#' df      = odataR_get_table(table_id="82935NED",keepcode = c("RegioS","Perioden"))
#' df      = odataR_get_table(table_id="82935NED",
#'   query  = paste0("?$filter=startswith(RegioS,'NL01')",
#'                   "&$select=RegioS,Perioden,TotaleInvesteringen_1",
#'                   "&$skip=2&$top=3") )
#' }

odataR_get_table <- function(
		root     = odataR_get_root_data(),
		table_id = NULL,
		query    = NULL,
		typed    = T,
		keepcode = c() ) {
	tds      = ifelse(typed == F, 'UntypedDataSet', 'TypedDataSet')
	query    = URLencode(ifelse(is.null(query), '', query))
	bname    = paste0(root, '/', gsub(" ", "", table_id))
	subtabt  = odataR_query(paste0(bname, '?$format=json'))
	if (!('url' %in% names(subtabt))) return(dplyr::data_frame())
	subtabs  = subtabt$url
	names(subtabs) = subtabt$name
	url1     = subtabs['DataProperties']
	props    = odataR_query(url1)
	url1     = paste0(subtabs[tds], query)
	df       = odataR_query(url1)
	tv       = dplyr::filter(props, Key %in% names(df))
	dv       = dplyr::select(
		dplyr::filter(tv,Type %in% c('Dimension', 'TimeDimension', 'GeoDimension')),
		'Key')
	tv       = dplyr::select(
		dplyr::filter(tv, Type == 'Topic'),
		'Key')
	couple_data(df, dv, tv, subtabs, keepcode)
}

couple_data <- function(
	df,        # data.frame with coded dimensions (e.g. read by odataR_get_subtable)
	dv,        # character vector with the names of the dimensions
	tv,        # character vector with the names of the topics
	table_list, # named vector with urls of sub tables
	keepcode   # dimension(s) for which coded value is kept
) {
	tt = df
	for (dim in dv$Key)  {
		if (dim %in% keepcode ) {
			kc = T
		} else {
			kc = F
		}
		tt = couple_data_dim(tt, table_list[dim],keep_code=kc) # link dimension data
	}
	return(tt)
}

couple_data_dim <- function(tt, dsn, keep_code=F) {
	dim  = names(dsn)
	tab1 = odataR_query(dsn)
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

odataR_get_cat <- function(type='Tables',query=NULL) {
	query    = URLencode(ifelse(is.null(query),'',query))
	odataR_query(paste0(odataR_get_root_catalog(type),query))
}

#' Get meta data about table
#'
#' This function can be used to get information about an OData table.
#' @param root Root of data structure
#' @param table_id Identification of table
#' @param metatype indicates the type of metadata that will be retrieved. Possible values are:
#' \itemize{
#'  \item \code{NULL} or empty : a \code{tibble} with the urls of the subtables will be retrieved \cr
#'  \item \code{'TableInfos'} : a \code{tibble} with one row is retrieved with (background) information about the table \cr
#'  \item \code{'CategoryGroups'} : a \code{tibble} with one row is retrieved with information about the category tables for this table \cr
#'  \item \code{'DataProperties'} : a \code{tibble} is retrieved with information about the columns of the table \cr
#'  \item a dimension name : a \code{tibble} is retrieved with the code information of the dimension (e.g.  \code{Gemeenten} in the examples below. \cr
#' }
#' @param query OData query to restrict data returned from the meta table
#' @return A \code{tibble} (\code{data_frame}) when successful otherwise an empty \code{tibble} or a message
#' @export
#' @section Remarks:
#' See  \url{http://www.odata.org/documentation/odata-version-3-0/odata-version-3-0-core-protocol/} or \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} (depending on the version of the OData protocol that is used) for details about the query possibilities.
#'
#'\code{$format=json} is forced by the code so do not specify \code{$format=atom} because this will not work
#'
#' @examples
#' \dontrun{
#' odataR_set_root('http://dataderden.cbs.nl') ;
#' df = odataR_get_meta(table_id='45042NED')
#' df = odataR_get_meta(table_id='45042NED',metatype='TableInfos')
#' df = odataR_get_meta(table_id='45042NED',metatype='CategoryGroups')
#' df = odataR_get_meta(table_id='45042NED',metatype='DataProperties')
#' df = odataR_get_meta(table_id='45042NED',metatype='Gemeenten')
#' df = odataR_get_meta(table_id='45042NED',metatype='Verslagsoort')
#' df = odataR_get_meta(table_id='45042NED',metatype='Gemeenten',
#'        query="?$filter=substringof('amstelveen',tolower(Title))")
#' }

odataR_get_meta <- function(
		root     = odataR_get_root_data(),
		table_id = NULL,
		metatype = NULL,
		query    = NULL
	) {
	bname    = paste0(root, '/', gsub(" ", "", table_id))
	query    = URLencode(ifelse(is.null(query),'',query))
	subtabt  = odataR_query(paste0(bname, '?$format=json'))
	if (!('url' %in% names(subtabt))) return(dplyr::data_frame())
	if (is.null(metatype) | length(metatype) == 0)   return(subtabt)
	subtabs  = subtabt$url
	names(subtabs) = subtabt$name
	if (metatype == 'TableInfos') {
		url1     = paste0(subtabs['TableInfos'],query)
		return(odataR_query(url1))
	}
	if (metatype == 'CategoryGroups') {
		if (!('CategoryGroups' %in% names(subtabs))) return(dplyr::data_frame())
		url1     = paste0(subtabs['CategoryGroups'],query)
		return(odataR_query(url1))
	}
	if (metatype == 'DataProperties') {
		url1     = paste0(subtabs['DataProperties'],query)
		props    = odataR_query(url1)
		return(props)
	}
	else {
		url1     = subtabs['DataProperties']
		props    = odataR_query(url1)
	}
	url1     = paste0(subtabs['TypedDataSet'], URLencode('?$top=1'))
	df       = odataR_query(url1)
	tv       = dplyr::filter(props, Key %in% names(df))
	dv       = dplyr::select(
		dplyr::filter(tv,Type %in% c('Dimension', 'TimeDimension', 'GeoDimension')),
		'Key')
	if (metatype %in% dv$Key) {
		url1     = paste0(subtabs[metatype],query)
		return(odataR_query(url1))
	}
	else return(dplyr::data_frame())
}
