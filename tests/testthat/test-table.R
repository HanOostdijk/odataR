
odataR::odataR_set_root()

# tests for count
df1 = odataR::odataR_get_table(table_id= "82935NED",query='$count') 

test_that("test that count works correctly ", {
  expect_identical(class(df1),'integer')
  expect_true(df1>0)
})

# tests for dimensions and $top
t1  = 2                   
q1  = paste0('$top=',t1)
df2 = odataR::odataR_get_table(table_id= "82935NED",
         query=q1) 

test_that("test that dimension correctly ", {
  expect_equal(dim(df2),c(t1,14))
})

# tests for $select (correct fields)
sel1a = names(df2)[1:4]
sel2a = paste0(sel1a,collapse=',')
q2a   = paste0(q1,'&$select=',sel2a)
df3  = odataR::odataR_get_table(table_id= "82935NED",
          query=q2a) 
# tests for $select (incorrect fields)
sel1b = c(names(df2)[1:4],'xxx') # add incorrect field
sel2b = paste0(sel1b,collapse=',')
q2b   = paste0(q1,'&$select=',sel2b)

test_that("test that $select works correctly ", {
  expect_equal(dim(df3),c(t1,length(sel1a)))
  expect_equal(names(df3),sel1a)
  expect_error(
    odataR::odataR_get_table(table_id= "82935NED",query=q2b), 
    regexp = 'Error in OData request')
})

# tests for keepcode

df1 =  odataR_get_table(table_id="82935NED",query='$top=1',decode=F)  
n1  =  names(df1)[1:4]
q1  =  paste0('$top=1&$select=',paste0(n1,collapse=','))
df2 =  odataR_get_table(table_id="82935NED",query=q1,keepcode = "RegioS") 
n2  = names(df2)
df3 =  odataR_get_table(table_id="82935NED",query=q1) 
n3  = names(df3) 


test_that("test that keepcode works correctly ", {
  expect_equal(setdiff(n2,n1),"RegioS_coded")
  expect_equal(setdiff(n2,n3),"RegioS_coded")
  expect_equal(setdiff(n2,"RegioS_coded"),n3)
  expect_equal(which(n1 %in% 'RegioS'),which(n2 %in% 'RegioS_coded'))

})

