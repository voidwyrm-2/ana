# TODO

## Language Features

- [x] Method operator (analogous to Lua's `:`)
- [ ] Error Handling
- [ ] Local module importing
- [ ] `case` statements
- [ ] Library module importing

## Stdlib Modules

- [ ] builtins
    - [ ] `Exit [Int] -> Unit`
    - [ ] `Error [...Any] -> Error`
    - [ ] `Len [Any] -> Int`
    - [ ] `Get [Any, Any] -> Error|Any`
    - [ ] `Set [Any, Any] -> Error|Any`
- [x] `io`
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
- [ ] `ffi`
    - [ ] `LoadNative [String name] -> Error|FFIModule`
    - [ ] `LoadWasm [String name] -> Error|FFIModule`

## Internal

- [x] Argument count and type verification for stdlib functions

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
