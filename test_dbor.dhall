let or = ./coreor.dhall
in { ConnType = or.tns "{ordriver}" "ordbq"
   , schema="orschema"
   , uid="oruid"
   , pwd="orpwd"
   , dict = [ or.kv "or1" "aa", or.kv "or2" "bb" ]
   }


