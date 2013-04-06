![kakapo](http://cdn1.arkive.org/media/56/5692B0B2-7BCE-40CB-BB19-D48B085CF32E/Presentation.Large/Kakapo-walking.jpg)



### Build & Run

```
$ make rel
```

Start three nodes:

```
$ PORT=8080 _rel/bin/kakapo -config configs/sys1.config -args_file configs/vm1.args
```

```
$ PORT=8081 _rel/bin/kakapo -config configs/sys2.config -args_file configs/vm2.args
```

```
$ PORT=8082 _rel/bin/kakapo -config configs/sys3.config -args_file configs/vm3.args
```

Connect nodes:

```
(kakapo8081@127.0.0.1)1> riak_core:join('kakapo8080@127.0.0.1').
```

```
(kakapo8082@127.0.0.1)1> riak_core:join('kakapo8080@127.0.0.1')
```

Send request to port 8080, 8081 or 8082:

```
$ curl -v localhost:<PORT>/get -H "Host: httpbin.org"
```

