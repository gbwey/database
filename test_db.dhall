let x = ./coredb.dhall
in x.ms { driver = "fred", server = "asdf", db = "xx", authn=x.mstrusted }

