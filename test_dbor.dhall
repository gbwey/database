let x = ./coredb.dhall
in x.or { ConnType = x.ortns "{ordriver}" "ordbq"
        , schema="orschema"
        , uid="oruid"
        , pwd="orpwd"
        , dict = [ x.kv "or1" "aa", x.kv "or2" "bb" ]
        }

