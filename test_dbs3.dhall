let x = ./coredb.dhall
in { driver = "{s3driver}"
   , fn = "s3db"
   , dict = [ x.kv "s31" "aa", x.kv "s32" "bb" ]
   }


