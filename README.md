### Dump
This is a very simple database. It only stores one size of block. here is an example of using it.

```
1> ID = kv,
1> dump_app:start(normal, [ID]).
ok
{ok,<0.37.0>}
2> V = <<3,2,1,5,6>>.
<<3,2,1,5,6>>
3> Location = dump:put(V, ID).
15992
4> V = dump:get(Location, ID).
<<3,2,1,5,6>>
5>
```

it uses the atom ```kv``` so that you can start more than one dump, and give them different names.
