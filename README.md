# odataR

This package gives access to information made available with the OData interface.
It is tested on the information from Statistics Netherlands and therefore the root has as default the value https://opendata.cbs.nl .  
Another root can be set with the function **odataR_set_root** .  
The most important function is **odataR_get_table** that downloads for a given table_id the main table and uses its subtables to decode dimensions (as `Region` and `Periods`).  With the function **odataR_get_meta** the meta information of a table can be retrieved. Another important function is **odataR_get_cat** that gives an overview of the available tables in tibble (data_frame) format.
  
## Examples ordered by increasing complexity:

1 `df = odataR_get_table(table_id="82935NED")`  
Convert table `82935NED` to a data.frame with the (two) dimensions (`RegioS` and `Perioden`) decoded  
  
2 `df = odataR_get_table(table_id="82935NED",keepcode = c("RegioS","Perioden"))`  
Same as example1 but the coded version of both `RegioS` and `Perioden` are included  
  
3 `df = odataR_get_table(table_id="82935NED",keepcode = c("RegioS","Perioden"))`  
    `query  = "$filter=startswith(RegioS,'NL01')" )`  
Same as example2 but the OData server will only return rows where the value of RegioS starts with `NL01`  
  
4 `df      = odataR_get_table(table_id="82935NED",`  
    `query  = paste0("$filter=startswith(RegioS,'NL01')",`  
                   `"&$select=RegioS,Perioden,TotaleInvesteringen_1",`   
                   `"&$skip=2&$top=3") )`  
Same as example5 but the OData server will only return the third, fourth and fifth row and the indicated fields (columns) 

### Description of the query language
The `$filter`, `$select`, `$skip` and `$top` commands used in the examples 3 and further are elements of the query language for OData described in the [OData protocol v4](http://docs.oasis-open.org/odata/odata/v4.0/errata02/os/complete/part1-protocol/odata-v4.0-errata02-os-part1-protocol-complete.html) for version 4 and the [OData protocol v3](
http://www.odata.org/documentation/odata-version-3-0/odata-version-3-0-core-protocol/) for version 3. **The website of Statistics Netherlands is in progress of converting from version 3 to 4.**

**NB** the queries specified in odataR should **not** include the `?` that is generated in the OData url.

## Additional information about odataR
The folder *private* contains two PDFs with extra information:  

* *odataR.pdf* is the standard R help information in PDF format that is function oriented
* *vignette_odataR.pdf* describes how to use the package in a 'real world' case
* *Reading OData with MATLAB.pdf* describes how to replay the example in the vignette in the MATLAB world.

## Install odataR

install.packages("devtools")  
library(devtools)  
install_github("HanOostdijk/odataR")

## References
- A introduction to OData :
[Introducing OData](https://msdn.microsoft.com/en-us/data/hh237663.aspx) 
- Details OData :
[OData - the best way to REST](http://www.odata.org/)
- CBS (Statistics Netherlands) OData environment: 
[Handleiding CBS Open Data Services](https://www.cbs.nl/-/media/statline/documenten/handleiding-cbs-opendata-services.pdf?la=nl-nl) (in Dutch)
- CBS (Statistics Netherlands) services:
[Open data](https://www.cbs.nl/nl-nl/onze-diensten/open-data) (in Dutch)

## See also
To my knowledge two other packages concerning OData exist: 

- [OData](https://cran.r-project.org/web/packages/OData)  
- [cbsodataR](https://cran.r-project.org/web/packages/cbsodataR)

