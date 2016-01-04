Index your file system so that you can find duplicates.

For now, only works on Unix systems.

# Build

You need OCaml and obuild.

```bash
obuild configure
obuild build
```

# Install

Copy or symbolically link the binary in your `~/bin` directory.

If you want bash completion, run `fsindex --bashcompletion` and follow the instructions.

# Usage

Run `fsindex --help`.

Additionally, you can prevent files/directories from being added to the index by creating a file having the same name as your index file plus `.excl`. E.g. if your index is `~/myindex`, create a file `~/myindex.excl` containing

```
/dev
/proc
/run
/sys
/tmp
```
