
odataR::odataR_set_root('http://dataderden.cbs.nl')

# general tests

test_that("message given incorrect table_id ", {
  expect_error(odataR::odataR_get_meta(table_id='xx'), 
               regexp = 'No data found for this "table_id" in this catalog')
})

# tests for metatype=NULL
table_id='45042NED' 
df1  = odataR_get_meta(table_id=table_id,metatype = NULL) 
df1a = odataR_get_meta(table_id=table_id) 

test_that("metatype NULL gives correct results ", {
  expect_identical(df1, df1a)
  expect_identical(names(df1),c("name", "url"))
  expect_length(df1,2)
  expect_true(all(c("TableInfos","UntypedDataSet","TypedDataSet","DataProperties") %in% df1$name))
})

# tests for metatype='TableInfos'
table_id ='45042NED' 
metatype ='TableInfos'
df2  = odataR_get_meta(table_id=table_id,metatype = metatype) 
df2a  = odataR_get_meta(root='http://dataderden.cbs.nl', table_id=table_id,metatype = metatype,query = '$select=ID,Title,ShortTitle,Identifier',debug=F)

test_that("metatype TableInfo gives correct results ", {
  expect_equal(nrow(df2),1)
  expect_length(df2a,4)
})

# tests for metatype='CategoryGroups'
table_id ='45042NED' 
metatype ='CategoryGroups'
if ('CategoryGroups' %in% df1$name  ) {
  df3  = odataR_get_meta(table_id=table_id,metatype = metatype) 
}

test_that("metatype CategoryGroups gives correct results ", {
  skip_if_not('CategoryGroups' %in% df1$name)
  expect_equal(c("ID","DimensionKey","Title","Description","ParentID"),names(df3))
})

# tests for metatype='DataProperties'
table_id ='45042NED' 
metatype ='DataProperties'
df4  = odataR_get_meta(table_id=table_id,metatype = metatype) 


test_that("metatype DataProperties gives correct results ", {
  expect_length(df4,14)
  expect_equal(c("odata.type", "ID", "Position", "ParentID", "Type", "Key"),names(df4)[1:6])
})

# test for other metatype value (first dimension)
table_id ='45042NED' 
metatype = dplyr::select(dplyr::filter(df4,Type!='Topic'),Key)$Key[1]
df5  = odataR_get_meta(table_id=table_id,metatype = metatype) 

test_that("metatype for a dimension gives correct results ", {
  expect_equal(c("Key", "Title", "Description", "CategoryGroupID"),names(df5))
})

# test for a non-existing metatype
table_id ='45042NED' 
metatype = 'not_existing'

test_that("non-existing metatype gives error", {
  expect_error(odataR_get_meta(table_id=table_id,metatype = metatype),
               regexp = 'unknown metatype : ')
})
