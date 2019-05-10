### Dump
This is a very simple harddrive database. It only stores one size of block. here is an example of using it.

```
1> ID = database003,
2> dump_sup:start_link(ID, 5, 10000, ram, "temp").
{ok,<0.37.0>}
3> V = <<3,2,1,5,6>>.
<<3,2,1,5,6>>
4> Location = dump:put(V, ID).
15992
5> V = dump:get(Location, ID).
<<3,2,1,5,6>>
6> dump:delete(Location, ID).
ok
7> dump:get(Location, ID).
<<3,2,1,5,6>>
8> Location = dump:put(<<0:40>>, ID).
15992
9> dump:get(Location, ID).
<<0,0,0,0,0>>
```

it uses the atom ```database003``` so that you can start more than one dump, and give them different names.
