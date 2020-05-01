let pg = ./corepg.dhall
in { driver = "{pgdriver}"
   , server = "pgserver"
   , db = "pgdb"
   , schema=Some "pgschema"
   , port=None Natural
   , uid="pguid"
   , pwd="pgpwd"
   , dict = [ pg.kv "pg1" "aa", pg.kv "pg2" "bb" ]
   }


