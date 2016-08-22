### Dump
This is a very simple database. It only stores one size of block. here is an example of using it.

```
1> V = <<3,2,1,5,6>>.
<<3,2,1,5,6>>
2> Location = dump:put(V, kv).
15992
3> V = dump:get(Location, kv).
<<3,2,1,5,6>>
4>
```

it uses the atom ```kv``` so that you can start more than one dump, and give them different names.
