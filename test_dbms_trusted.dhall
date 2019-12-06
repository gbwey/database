let x = ./coredb.dhall
in x.ms { driver = "{msdrivert}"
        , server = "msservert"
        , db = "msdbt"
        , authn=x.mstrusted
        , dict = [ x.kv "ms1t" "aat", x.kv "ms2t" "bbt" ]
        }


