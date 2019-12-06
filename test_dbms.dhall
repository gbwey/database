let x = ./coredb.dhall
in x.ms { driver = "{msdriver}"
        , server = "msserver"
        , db = "msdb"
        , authn=x.msauthn "msuid" "mspwd"
        , dict = [ x.kv "ms1" "aa", x.kv "ms2" "bb" ]
        }


