# TODO

## Language Features

- [x] Method operator (analogous to Lua's `:`)
- [ ] Error Handling
- [ ] Local module importing
- [ ] `case` statements
- [ ] Library module importing

## Stdlib Modules

- [ ] builtins
    - [ ] `Exit [Int code] -> Unit`
    - [ ] `Error [Any... args] -> Error`
    - [ ] `Len [Any obj] -> Int`
    - [ ] `Get [Any obj, Any index] -> Error|Any`
    - [ ] `Set [Any obj, Any index, Any value] -> Error|Any`
    - [ ] `Reduce [Any obj, Function fun] -> Any`
- [x] `io`
    - [ ] `Out [String... args] -> Unit`
    - [ ] `Outc [Int char] -> Unit`
    - [ ] `Outf [String fmt, Any... args] -> Unit`
    - [ ] `OpenFile [String path, String mode] -> Error|File`
- [ ] `tables`
    - [x] `Create [] -> Table`
    - [ ] `FromSeq [Seq(Seq(String, Any)) entries] -> Table(Any)`
    - [ ] `ToSeq [Table self] -> Seq(Seq(String, Any))`
    - [ ] `Remove [Table self, String key] -> Error|Unit`
- [ ] `strings`
    - [ ] `FromSeq [Seq(Int) chars] -> Error|String`
    - [ ] `ToSeq [String self] -> Seq(Int)`
    - [ ] `Split [String self, String sep] -> Seq(String)`
- [ ] `http`
    - [ ] `Get [String url] -> Error|Request`
    - [ ] `Post [String url, String data] -> Error|Request`
    - [ ] `Put [String url, String data] -> Error|Request`
    - [ ] `Delete [String url] -> Error|Request`
- [ ] `meta`
    - [ ] `Eval [String expr] -> Error|Any`
    - [ ] `Exec [String program] -> Error|Table`
    - [ ] `GetLocals [] -> Seq(Seq(String, Any))`
- [ ] `ffi`
    - [ ] `LoadNative [String name] -> Error|FFIModule`
    - [ ] `LoadWasm [String name] -> Error|FFIModule`

## Internal

- [x] Argument count and type verification for stdlib functions
- [ ] Allow for stdlib modules to be written in ANA

## Ecosystem

- [ ] Documentation
    - [ ] Language Tour
    - [ ] Language Reference
        - [ ] Operator list
        - [ ] Stdlib module list
- [ ] Testing

## Future

> Note: these are in order of importance/difficulty

- [ ] Interfaces/traits/contracts/concepts
- [ ] Compilation to the CLR (.NET)
- [ ] Celia package manager, written in Go
- [ ] Compilation to the BEAM
