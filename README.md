Minimal test:

```
$ cd example
$ dune utop
# open Example.Hash;;
# let store = Store.make "my_data.dat";; 
# at_exit (fun () -> Store.save store);;
# let config_width = Store.persistent_ref store "width" A.Typed.int32 0l;;
# Store.persistent_get config_width;;
- : _ = Ok 0l
# Store.persistent_set config_width 10l;;
- : _ = Ok () 
# exit 0 ;;
$ dune utop
# open Example.Hash;;
# let store = Store.make "my_data.dat";; 
# let config_width = Store.persistent_ref store "width" A.Typed.int32 0l;;
# Store.persistent_get config_width;;
- : _ = Ok 10l
```
