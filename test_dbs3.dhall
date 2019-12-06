let x = ./coredb.dhall
in x.s3 { driver = "{s3driver}"
        , fn = "s3db"
        , dict = [ x.kv "s31" "aa", x.kv "s32" "bb" ]
        }


