let ms = ./corems.dhall
in { driver = "{msdrivert}"
   , server = "msservert"
   , db = "msdbt"
   , authn = ms.trusted
   , dict = [ ms.kv "ms1t" "aat", ms.kv "ms2t" "bbt" ]
   }


