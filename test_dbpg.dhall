let x = ./coredb.dhall
in x.pg { driver = "{pgdriver}"
        , server = "pgserver"
        , db = "pgdb"
        , schema=Some "pgschema"
        , port=None Natural
        , uid="pguid"
        , pwd="pgpwd"
        , dict = [ x.kv "pg1" "aa", x.kv "pg2" "bb" ]
        }


