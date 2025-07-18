# TODO

## Language Features

- [ ] Method operator (analogous to Lua's `:`)
- [ ] Library module importing
- [ ] Local module importing

## Stdlib Modules

- [x] `io`
    - [ ] `OpenFile [String path, String mode] -> File`
- [ ] `table`
    - [ ] `Create [] -> Table`
    - [ ] `FromSeq [Seq(Seq(String, Any)) entries] -> Table(Any)`
    - [ ] `Set [Table self, String key, Any value] -> Unit`
    - [ ] `Get [Table self, String key] -> Any`
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
