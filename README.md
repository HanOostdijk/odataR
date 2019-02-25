# odataR

This package gives access to information made available with the OData interface.
It is tested on the information from Statistics Netherlands and therefore the root has as default the value https://opendata.cbs.nl .  
Another root can be set with the function **odataR_set_root** .  
The most important function is **odataR_get_table** that downloads for a given table_id the main table and uses its subtables to decode dimensions (as `Region` and `Periods`).  With the function **odataR_get_meta** the meta information of a table can be retrieved. Another important function is **odataR_get_cat** that gives an overview of the available tables in tibble (data_frame) format.
  
## Examples ordered by increasing complexity:

1 `df = odataR_get_cat()`  
Retrieve the contents of the catalog  

2 `df = odataR_get_table(table_id="82935NED")`  
Convert table `82935NED` to a data.frame with all (two) dimensions decoded 

3 `df = odataR_get_meta(table_id="82935NED",metatype="DataProperties")`  
View the information about the fields in this table:  the two dimensions are `RegioS` and `Perioden`  
  
4 `df = odataR_get_table(table_id="82935NED",keepcode = c("RegioS","Perioden"))`  
Same as example2 but the coded version of both `RegioS` and `Perioden` are included  
  
5 `df = odataR_get_table(table_id="82935NED",keepcode = c("RegioS","Perioden"),`  
    `query  = "?$filter=startswith(RegioS,'NL01')" )`  
Same as example4 but the OData server will only return rows where the value of RegioS starts with `NL01`  
  
6 `df      = odataR_get_table(table_id="82935NED",`  
    `query  = paste0("?$filter=startswith(RegioS,'NL01')",`  
                   `"&$select=RegioS,Perioden,TotaleInvesteringen_1",`   
                   `"&$skip=2&$top=3") )`  
Same as example5 but the OData server will only return the third, fourth and fifth row and the indicated fields (columns)

7 `df = odataR_get_cat(query = paste0("?$filter=substringof('bevolking',tolower(ShortDescription))",`
  `"&$select=Identifier,ShortTitle,ShortDescription,RecordCount,ColumnCount"))` 
Find in the catalog the tables with `bevolking` (population) in the `ShortDescription` field and restrict the returned fields to the ones specified with `select`.

### Description of the query language
The `$filter`, `$select`, `$skip` and `$top` commands used in the examples 5 and further are elements of the query language for OData described in the [OData protocol v4](http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html) for version 4 and the 
[OData protocol v3](http://www.odata.org/documentation/odata-version-3-0/odata-version-3-0-core-protocol/) for version 3.  
**The website of Statistics Netherlands is in progress of converting from version 3 to 4. Apart from the change in OData version the structure of the data is slightly changed.** See [New version CBS open data API (CCB)](https://acc-ccb.cbs.nl/OData4/releasenotes.html) for details about the status of the conversion and function `get_table_cbs_odata4` from package [HOQCutil](https://github.com/HanOostdijk/HOQCutil) to read version 4 data.

## Additional information about odataR
The folder *development_scripts* contains three PDFs with extra information:  

* *odataR.pdf* is the standard R help information in PDF format that is function oriented
* *vignette_odataR.pdf* describes how to use the package in a 'real world' case
* *Reading_OData_with_MATLAB.pdf* describes how to replay the example in the vignette in the MATLAB world.

Apart from these PDFs the folder also contains some devtools statements that were used to build the package.

NB this package (based on JSON) replaces [odataR_old](https://github.com/HanOostdijk/odataR_old.git) that was based on XML.

## Install odataR

install.packages("devtools")  
library(devtools)  
install_github("HanOostdijk/odataR",build_vignettes = T) # if ggplot2 already installed or ...
install_github("HanOostdijk/odataR",build_vignettes = F) # if ggplot2 is not installed

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

