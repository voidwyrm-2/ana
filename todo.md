# TODO

## Language Features

- [x] Method operator (analogous to Lua's `:`)
- [ ] Library module importing
- [ ] Local module importing
- [ ] Error Handling

## Stdlib Modules

- [x] `io`
    - [ ] `OpenFile [String path, String mode] -> File`
- [ ] `tables`
    - [x] `Create [] -> Table`
    - [ ] `FromSeq [Seq(Seq(String, Any)) entries] -> Table(Any)`
    - [x] `Get [Table self, String key] -> Any`
    - [x] `Set [Table self, String key, Any value] -> Unit`
    - [ ] `Remove [Table self, String key] -> Unit`
    - [ ] `ToSeq [Table self] -> Seq(Seq(String, Any))`
- [ ] `strings`
    - [ ] `FromSeq [Seq(Int) chars] -> String`
    - [ ] `ToSeq [String self] -> Seq(Int)`
    - [ ] `Split [String self, String sep] -> Seq(String)`
- [ ] `ffi`
    - [ ] `LoadNative [String name] -> FFIModule`
    - [ ] `LoadWasm [String name] -> FFIModule`

## Internal Features

- [ ] Argument count and type verification for stdlib functions
