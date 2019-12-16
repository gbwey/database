let x = ./coredb.dhall
let DBSumT = < MS : x.DBMST
             | PG : x.DBPGT
             | MY : x.DBMYT
             | OR : x.DBORT
             | S3 : x.DBS3T >

in {
   , dbsum = DBSumT
   , ms = DBSumT.MS
   , pg = DBSumT.PG
   , my = DBSumT.MY
   , or = DBSumT.OR
   , s3 = DBSumT.S3
   }
