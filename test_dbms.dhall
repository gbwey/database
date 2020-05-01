let ms = ./corems.dhall
in { driver = "{msdriver}"
   , server = "msserver"
   , db = "msdb"
   , authn = ms.authn "msuid" "mspwd"
   , dict = [ ms.kv "ms1" "aa", ms.kv "ms2" "bb" ]
   }


