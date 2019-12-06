let x = ./coredb.dhall
in x.my { driver = "{mydriver}"
        , server = "myserver"
        , db = "mydb"
        , port=None Natural
        , uid="myuid"
        , pwd="mypwd"
        , dict = [ x.kv "my1" "aa", x.kv "my2" "bb" ]
        }


