esproto
=====

erlang binding for cloudwu‘s sproto.

> cloudwu‘s sproto github page [sproto](https://github.com/cloudwu/sproto).

### API

dump the *.sproto files in the directory Dir and sub-directories to a beam file, then you should load it to use sproto api.

```erl
sprotodump:dir(Dir, OutDir).

```

* `sproto:encode(TypeName, DataMap)` encodes a erlang map by a type object, and generates a binary message.
* `sproto:decode(TypeName, Message)` decodes a message binary generated by sproto.encode with type.
* `sproto:pack(SprotoMessage)` packs a binary encoded by sproto.encode to reduce the size.
* `sproto:unpack(PackedMessage)` unpacks the binary packed by sproto.pack.

### RPC API

* `sproto:rpc_init()` rpc init work, just is create a ets table.
* `sproto:proto_encode(BasePackage, TypeName, DataMap, Session, UserData)` 
* `sproto:proto_decode(BasePackage, BinaryMessage)` 

Read test.erl and cloudwu‘s sproto github for detail.