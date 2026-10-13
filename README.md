Ethereum Types
==============

Types and utilities used by the [Ethereum Execution Layer Specification (EELS)][eels]. Includes:

- Fixed-size unsigned integers (`U256`, `U64`, etc.)
- Arbitrarily-sized unsigned integers (`Uint`)
- Enumerations (`UintEnum`)
- Fixed-size byte sequences (`Bytes4`, `Bytes8`, etc.)
- Utilities for making/interacting with immutable dataclasses (`slotted_freezable`, `modify`)

[eels]: https://github.com/ethereum/execution-specs

## Quickstart

| What          | How                  |
|---------------|----------------------|
| Static Checks | `hatch fmt --check`  |
| Run Tests     | `hatch test`         |
| Build         | `hatch build`        |
| Build Docs    | `hatch run doc:docc` |

## Compiled Wheels

Releases include wheels compiled with [mypyc] for CPython 3.11 to 3.14 on Linux (x86_64 and aarch64), macOS (arm64) and Windows (x86_64). Every other platform and interpreter, PyPy included, installs the pure-Python wheel, which behaves the same apart from the differences below.

| What                   | How                                                              |
|------------------------|------------------------------------------------------------------|
| Compile locally        | `HATCH_BUILD_HOOK_ENABLE_MYPYC=true uv build --wheel`            |
| Force pure Python      | `pip install --no-binary ethereum-types ethereum-types`          |
| Remove local artifacts | `git clean -fX src` (compiled modules shadow the sources)        |

Compiled numbers differ from the pure-Python ones in a few ways:

- Mixing integer types (`U256(1) + Uint(2)`) raises a `TypeError` naming the expected type instead of `unsupported operand type(s)`.
- In-place operators and comparisons raise a `TypeError` for a foreign operand instead of falling back to its reflected method.
- Instances have no `__dict__` and do not support weak references.

[mypyc]: https://mypyc.readthedocs.io/

