let ms = ./corems.dhall
let pg = ./corepg.dhall
let my = ./coremy.dhall
let or = ./coreor.dhall
let s3 = ./cores3.dhall

let DBSumT = < MS : ms.DBMST
             | PG : pg.DBPGT
             | MY : my.DBMYT
             | OR : or.DBORT
             | S3 : s3.DBS3T >

in {
   , dbsum = DBSumT
   , ms = DBSumT.MS
   , pg = DBSumT.PG
   , my = DBSumT.MY
   , or = DBSumT.OR
   , s3 = DBSumT.S3
   }
