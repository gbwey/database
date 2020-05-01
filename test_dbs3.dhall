let s3 = ./cores3.dhall
in { driver = "{s3driver}"
   , fn = "s3db"
   , dict = [ s3.kv "s31" "aa", s3.kv "s32" "bb" ]
   }


