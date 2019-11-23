let MSAuthnT = < Trusted | UserPwd : { User : Text, Password : Text } >
let msauthn = \(user : Text) -> \(pwd : Text) -> MSAuthnT.UserPwd { User = user, Password = pwd }
let OrConT = < TnsName : { driver : Text, tns : Text } | DsnOracle : Text >
let ortns = \(driver : Text) -> \(tns : Text) -> OrConT.TnsName { driver = driver, tns = tns }
let DBMST = { driver : Text, server : Text, db : Text, authn : MSAuthnT }
let DBPGT = { driver : Text, server : Text, db : Text, schema : Optional Text, port : Optional Natural, uid : Text, pwd : Text }
let DBMYT = { driver : Text, server : Text, db : Text, port : Optional Natural, uid : Text, pwd : Text }
let DBORT = { schema : Text, ConnType : OrConT, uid : Text, pwd : Text }
let DBS3T = { driver : Text, schema : Optional Text, fn : Text }
let DBSumT = < MS : DBMST | PG : DBPGT | MY : DBMYT | OR : DBORT | S3 : DBS3T >
in {
   , mstrusted = MSAuthnT.Trusted
   , msauthn = msauthn
   , orconT = OrConT
   , ortns = ortns
   , dbsum = DBSumT
   , ms = DBSumT.MS
   , pg = DBSumT.PG
   , my = DBSumT.MY
   , or = DBSumT.OR
   , s3 = DBSumT.S3
   }
