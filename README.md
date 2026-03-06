# SSZ-Lisp

A complete implementation of [Ethereum's SSZ (Simple Serialize)](https://ethereum.github.io/consensus-specs/ssz/simple-serialize/) format in [Coalton](https://coalton-lang.github.io/), a statically-typed functional language that compiles to Common Lisp.

## Features

- **All SSZ types supported**: Basic types (U8-U256, Boolean), Vector, List, Bitvector, Bitlist, Container, Union
- **Full round-trip serialization**: Serialize and deserialize all types
- **Hash tree root computation**: Merkleization with SHA256
- **Type classes**: Extensible `SSZSerialize`, `SSZDeserialize`, and `SSZHashTreeRoot` interfaces
- **Spec-compliant**: Passes 1,760+ official Ethereum consensus spec tests

## Installation

### Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/)
- [Coalton](https://coalton-lang.github.io/)

### Setup

1. Clone the repository into your Quicklisp local-projects directory:

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/your-username/ssz-lisp.git
```

2. Load the system:

```lisp
(ql:quickload "ssz")
```

## Usage

All examples assume the following preamble:

```lisp
(ql:quickload "ssz")
(in-package #:coalton-user)
(named-readtables:in-readtable coalton:coalton)
```

### Basic Types (U8-U64, Boolean)

```lisp
(coalton-toplevel
  ;; Serialize integers (little-endian byte order)
  (define u8-bytes (ssz:serialize-u8 255))           ; => #(255)
  (define u16-bytes (ssz:serialize-u16 1000))        ; => #(232 3)
  (define u32-bytes (ssz:serialize-u32 12345))       ; => #(57 48 0 0)
  (define u64-bytes (ssz:serialize-u64 1000000))     ; => #(64 66 15 0 0 0 0 0)

  ;; Serialize boolean
  (define true-bytes (ssz:serialize-bool True))      ; => #(1)
  (define false-bytes (ssz:serialize-bool False))    ; => #(0)

  ;; Deserialize with error handling
  (define (safe-deserialize-u32 bytes)
    (match (ssz:deserialize-u32 bytes)
      ((Ok val) val)
      ((Err _) 0))))
```

### Large Integers (U128, U256)

```lisp
(coalton-toplevel
  ;; U128: 16 bytes
  (define my-u128 (ssz:u128-make 0 1))  ; high=0, low=1
  (define u128-bytes (ssz:serialize-u128 my-u128))

  ;; U256: 32 bytes (used for hashes, public keys, etc.)
  (define my-u256 (ssz:u256-make 0 0 0 1))  ; four 64-bit words
  (define u256-bytes (ssz:serialize-u256 my-u256))

  ;; Round-trip deserialization
  (define (u256-roundtrip val)
    (match (ssz:deserialize-u256 (ssz:serialize-u256 val))
      ((Ok result) result)
      ((Err _) (ssz:u256-zero)))))
```

### Vectors (Fixed-Length Sequences)

```lisp
(coalton-toplevel
  ;; Create a vector of U32 values
  (define my-vector
    (ssz:ssz-vector-from-list (make-list 100 200 300)))

  ;; Access elements
  (define first-elem (ssz:ssz-vector-ref 0 my-vector))  ; => Some 100
  (define vec-len (ssz:ssz-vector-length my-vector))    ; => 3

  ;; Serialize vector of fixed-size elements
  ;; Each U32 is 4 bytes, so result is 12 bytes
  (define vec-bytes (ssz:ssz-serialize my-vector))

  ;; Deserialize vector
  (define (deserialize-u32-vector count bytes)
    (ssz:deserialize-vector-fixed count 4 ssz:deserialize-u32 bytes)))
```

### Lists (Variable-Length with Max Limit)

```lisp
(coalton-toplevel
  ;; Create a list with max 1000 elements
  (define my-list-result
    (ssz:ssz-list-from-list 1000 (make-list 10 20 30 40 50)))

  ;; Lists are variable-size (includes length in hash tree root)
  (define (use-list)
    (match my-list-result
      ((Ok lst)
       (let ((len (ssz:ssz-list-length lst))          ; => 5
             (max (ssz:ssz-list-max-length lst))      ; => 1000
             (elem (ssz:ssz-list-ref 2 lst)))         ; => Some 30
         len))
      ((Err e) 0)))

  ;; Deserialize list of fixed-size elements
  (define (deserialize-u32-list max-len bytes)
    (ssz:deserialize-list-fixed max-len 4 ssz:deserialize-u32 bytes)))
```

### Bitvector (Fixed-Length Bits)

```lisp
(coalton-toplevel
  ;; Create a bitvector of all zeros
  (define bv-zeros (ssz:ssz-bitvector-zeros 16))

  ;; Access bitvector properties
  (define bv-len (ssz:ssz-bitvector-length bv-zeros))  ; => 16

  ;; Serialize bitvector (16 bits = 2 bytes)
  (define bv-bytes (ssz:serialize-bitvector bv-zeros))

  ;; Deserialize bitvector (must know the bit count)
  (define (deserialize-bv16 bytes)
    (ssz:deserialize-bitvector 16 bytes))

  ;; Hash tree root
  (define bv-root (ssz:hash-tree-root bv-zeros)))
```

### Bitlist (Variable-Length Bits)

```lisp
(coalton-toplevel
  ;; Create an empty bitlist with max 100 bits
  (define bl-empty (ssz:ssz-bitlist-empty 100))

  ;; Access bitlist properties
  (define bl-len (ssz:ssz-bitlist-length bl-empty))      ; => 0
  (define bl-max (ssz:ssz-bitlist-max-length bl-empty))  ; => 100

  ;; Serialize bitlist (includes marker bit)
  (define bl-bytes (ssz:serialize-bitlist bl-empty))

  ;; Deserialize bitlist (must know max length)
  (define (deserialize-bl100 bytes)
    (ssz:deserialize-bitlist 100 bytes))

  ;; Hash tree root
  (define bl-root (ssz:hash-tree-root bl-empty)))
```

### Unions (Tagged Sum Types)

```lisp
(coalton-toplevel
  ;; Create a None union (selector = 0)
  (define none-union (ssz:ssz-union-none Unit))

  ;; Create a Some union with selector and serialized value
  (define (make-some-union)
    (match (ssz:ssz-union-some 1 (ssz:serialize-u32 42))
      ((Ok u) u)
      ((Err _) (ssz:ssz-union-none Unit))))

  ;; Serialize union: selector byte + optional value
  (define (serialize-union-example)
    (ssz:serialize-union (make-some-union)))  ; => #(1 42 0 0 0)

  ;; Deserialize and inspect union
  (define (inspect-union bytes)
    (match (ssz:deserialize-union bytes)
      ((Ok u)
       (if (ssz:ssz-union-is-none u)
           "None"
           "Some value"))
      ((Err _) "Error"))))
```

### Containers (Heterogeneous Structs)

```lisp
(coalton-toplevel
  ;; Define a container with fixed-size fields
  (define-struct SimpleContainer
    (field-a U32)
    (field-b Boolean))

  ;; Serialize using field helpers
  (define (serialize-simple (SimpleContainer a b))
    (ssz:serialize-container
      (make-list
        (ssz:fixed-field 4 (ssz:serialize-u32 a))
        (ssz:fixed-field 1 (ssz:serialize-bool b)))))

  ;; Deserialize container
  (define (deserialize-simple bytes)
    (match (ssz:deserialize-container
             (make-list (Tuple "a" 4) (Tuple "b" 1))  ; field specs
             bytes)
      ((Ok fields)
       ;; Extract fields by name
       (match (Tuple (ssz:get-field "a" fields)
                     (ssz:get-field "b" fields))
         ((Tuple (Some a-bytes) (Some b-bytes))
          (match (Tuple (ssz:deserialize-u32 a-bytes)
                        (ssz:deserialize-bool b-bytes))
            ((Tuple (Ok a) (Ok b))
             (Ok (SimpleContainer a b)))
            (_ (Err "Field deserialization failed"))))
         (_ (Err "Missing fields"))))
      ((Err e) (Err "Container deserialization failed")))))
```

### Container with Variable-Size Fields

```lisp
(coalton-toplevel
  ;; Container mixing fixed and variable-size fields
  (define-struct MixedContainer
    (id U64)              ; 8 bytes fixed
    (data ssz:SSZBitlist)) ; variable-size

  ;; Variable fields use offset encoding
  (define (serialize-mixed (MixedContainer id data))
    (ssz:serialize-container
      (make-list
        (ssz:fixed-field 8 (ssz:serialize-u64 id))
        (ssz:variable-field (ssz:serialize-bitlist data))))))
```

### Hash Tree Root

```lisp
(coalton-toplevel
  ;; Hash tree root for basic types (32-byte result)
  (define u64-root (ssz:hash-tree-root (the U64 42)))
  (define bool-root (ssz:hash-tree-root True))

  ;; Hash tree root for containers (pass list of field roots)
  (declare hash-simple-container (SimpleContainer -> ssz:Bytes))
  (define (hash-simple-container (SimpleContainer a b))
    (ssz:hash-tree-root-container
      (make-list
        (ssz:hash-tree-root a)
        (ssz:hash-tree-root b)))))
```

### Error Handling

```lisp
(coalton-toplevel
  ;; SSZ operations return Result types
  (define (safe-process bytes)
    (match (ssz:deserialize-u32 bytes)
      ((Ok val)
       (format "Got value: ~A" val))
      ((Err (ssz:DeserializationError msg))
       (format "Deserialization failed: ~A" msg))
      ((Err (ssz:SerializationError msg))
       (format "Serialization failed: ~A" msg))
      ((Err (ssz:MerkleizationError msg))
       (format "Merkleization failed: ~A" msg))))

  ;; Chain operations with monadic style
  (define (chain-deserialize bytes)
    (do
      (u32-val <- (ssz:deserialize-u32 (ssz:bytes-slice 0 4 bytes)))
      (bool-val <- (ssz:deserialize-bool (ssz:bytes-slice 4 1 bytes)))
      (pure (Tuple u32-val bool-val)))))
```

### Using Type Classes

```lisp
(coalton-toplevel
  ;; Generic serialization via type classes
  (define (generic-serialize val)
    (ssz:ssz-serialize val))

  ;; Check if type is fixed-size
  (define (process-value val)
    (if (ssz:ssz-fixed-size? val)
        (let ((size (ssz:ssz-fixed-size val)))
          (ssz:ssz-serialize val))
        ;; Variable-size handling
        (ssz:ssz-serialize val)))

  ;; Generic hash tree root
  (define (generic-hash val)
    (ssz:hash-tree-root val)))

## API Reference

### Type Classes

| Class | Methods | Description |
|-------|---------|-------------|
| `SSZSerialize` | `ssz-serialize`, `ssz-fixed-size?`, `ssz-fixed-size` | Serialization to bytes |
| `SSZDeserialize` | `ssz-deserialize` | Deserialization from bytes |
| `SSZHashTreeRoot` | `hash-tree-root` | Merkle tree root computation |

### Basic Type Functions

| Serialize | Deserialize | Type |
|-----------|-------------|------|
| `serialize-u8` | `deserialize-u8` | `U8` |
| `serialize-u16` | `deserialize-u16` | `U16` |
| `serialize-u32` | `deserialize-u32` | `U32` |
| `serialize-u64` | `deserialize-u64` | `U64` |
| `serialize-u128` | `deserialize-u128` | `U128` |
| `serialize-u256` | `deserialize-u256` | `U256` |
| `serialize-bool` | `deserialize-bool` | `Boolean` |

### Composite Types

| Type | Constructor | Serialization |
|------|-------------|---------------|
| `SSZVector` | `ssz-vector-from-list` | Via type class |
| `SSZList` | `ssz-list-from-list` | `serialize-list-variable` |
| `SSZBitvector` | `ssz-bitvector-from-list`, `ssz-bitvector-zeros` | `serialize-bitvector` |
| `SSZBitlist` | `ssz-bitlist-from-list`, `ssz-bitlist-empty` | `serialize-bitlist` |
| `SSZUnion` | `ssz-union-from-value` | `serialize-union` |

### Merkleization

| Function | Description |
|----------|-------------|
| `merkleize` | Build merkle tree from chunks |
| `merkleize-with-limit` | Merkleize with capacity limit |
| `hash-tree-root` | Generic hash tree root via type class |
| `mix-in-length` | Mix length into root (for variable types) |
| `mix-in-selector` | Mix selector into root (for unions) |
| `pack` | Pack values into 32-byte chunks |

## Testing

### Run unit tests

From the REPL:

```lisp
(ql:quickload "ssz/tests")
(ssz-tests/runner:run-tests)
```

Or via ASDF:

```lisp
(asdf:test-system "ssz")
```

From the command line:

```bash
sbcl --non-interactive \
  --eval '(ql:quickload "ssz/tests")' \
  --eval '(ssz-tests/runner:run-tests)'
```

### Run consensus spec tests

The library is tested against official Ethereum consensus spec test vectors (1,760+ tests).

1. Download the test vectors into `refs/`:

```bash
git clone https://github.com/ethereum/consensus-spec-tests refs/consensus-spec-tests
```

2. Generate the spec test data:

```bash
python scripts/extract-spec-tests.py
```

3. Run the spec tests:

```lisp
(ql:quickload "ssz/tests")
(ssz-tests/runner:run-spec-tests)
```

Or via ASDF:

```lisp
(asdf:test-system "ssz/spec-tests")
```

## Benchmarks

### Running Benchmarks

From the REPL:

```lisp
(ql:quickload "ssz/bench")
(ssz-bench:run-all-benchmarks)
```

From the command line:

```bash
sbcl --non-interactive \
  --eval '(ql:quickload "ssz/bench")' \
  --eval '(ssz-bench:run-all-benchmarks)'
```

### Available Benchmark Suites

| Function | Description |
|----------|-------------|
| `run-all-benchmarks` | Run all benchmarks with summary |
| `run-serialization-benchmarks` | Basic type serialization (U8-U256, Boolean) |
| `run-deserialization-benchmarks` | Basic type deserialization |
| `run-merkle-benchmarks` | SHA256, merkleization, hash tree root |
| `run-composite-benchmarks` | Vector, List, Bitvector, Bitlist |
| `run-container-benchmarks` | Container serialize/deserialize/hash |

### Performance Results

Measured on Apple M1, SBCL 2.5.4, 10,000 iterations (100 warmup):

**Basic Type Serialization**

| Operation | Time (µs) | Throughput |
|-----------|-----------|------------|
| serialize-u8 | 0.01 | 83M ops/sec |
| serialize-u32 | 0.02 | 65M ops/sec |
| serialize-u64 | 0.03 | 37M ops/sec |
| serialize-u128 | 0.05 | 21M ops/sec |
| serialize-u256 | 0.08 | 12M ops/sec |

**Deserialization**

| Operation | Time (µs) | Throughput |
|-----------|-----------|------------|
| deserialize-u8 | 0.01 | 70M ops/sec |
| deserialize-u32 | 0.02 | 54M ops/sec |
| deserialize-u64 | 0.03 | 39M ops/sec |
| deserialize-u128 | 0.14 | 7.3M ops/sec |
| deserialize-u256 | 0.28 | 3.6M ops/sec |

**Merkleization**

| Operation | Time (µs) | Throughput |
|-----------|-----------|------------|
| sha256 (32 bytes) | 5.24 | 191K ops/sec |
| sha256 (64 bytes) | 9.60 | 104K ops/sec |
| hash-tree-root (U64) | 0.11 | 9.3M ops/sec |
| merkleize (4 chunks) | 12.51 | 80K ops/sec |

**Composite Types**

| Operation | Time (µs) | Throughput |
|-----------|-----------|------------|
| Vector[U32] serialize (10 elem) | 4.63 | 216K ops/sec |
| Vector[U32] serialize (100 elem) | 145 | 6.9K ops/sec |
| Vector[U32] hash-tree-root (10 elem) | 7.75 | 129K ops/sec |
| Bitvector serialize (256 bits) | 1.27 | 786K ops/sec |
| Container serialize (3 fields) | 0.25 | 4.0M ops/sec |
| Container hash-tree-root (8 fields) | 19.3 | 52K ops/sec |

## Project Structure

```
ssz-lisp/
├── src/
│   ├── package.lisp      # Package definition and exports
│   ├── constants.lisp    # SSZ constants
│   ├── types.lisp        # Core types (Bytes, U128, U256, errors)
│   ├── serialize.lisp    # Basic type serialization
│   ├── deserialize.lisp  # Basic type deserialization
│   ├── merkle.lisp       # SHA256 and merkleization
│   ├── merkle-cache.lisp # Zero hash cache initialization
│   ├── composite.lisp    # Vector, List, Bitvector, Bitlist, Union
│   ├── typeclass.lisp    # Type class instances
│   └── container.lisp    # Container helpers
├── test/
│   ├── package.lisp      # SSZ-TESTS package definition
│   ├── helpers.lisp      # Coalton test helper functions
│   ├── unit-tests.lisp   # Unit test runner
│   └── spec-tests.lisp   # Consensus spec test runner
├── bench/
│   └── benchmarks.lisp   # Performance benchmarks
└── ssz.asd               # ASDF system definition
```

## Dependencies

- [coalton](https://coalton-lang.github.io/) - Statically typed functional programming in Lisp
- [ironclad](https://github.com/sharplispers/ironclad) - Cryptographic library (SHA256)
- [named-readtables](https://github.com/melisgl/named-readtables) - Reader macro management
- [cl-json](https://github.com/sharplispers/cl-json) - JSON parsing (tests only)

## References

- [SSZ Specification](https://ethereum.github.io/consensus-specs/ssz/simple-serialize/)
- [Consensus Specs Repository](https://github.com/ethereum/consensus-specs)
- [Consensus Spec Tests](https://github.com/ethereum/consensus-spec-tests)

## License

MIT
