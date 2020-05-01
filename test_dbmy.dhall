let my = ./coremy.dhall
in { driver = "{mydriver}"
   , server = "myserver"
   , db = "mydb"
   , port=None Natural
   , uid="myuid"
   , pwd="mypwd"
   , dict = [ my.kv "my1" "aa", my.kv "my2" "bb" ]
   }


