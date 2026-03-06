;;; SSZ Test Package Definitions

;;; Package for Coalton test helpers
(defpackage #:ssz-tests
  (:documentation "Coalton test helpers for SSZ library")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list))
  (:export
   ;; Vector/List tests
   #:ssz-tests-vector-serialize-length
   #:ssz-tests-vector-fixed-size-p
   #:ssz-tests-vector-fixed-size
   #:ssz-tests-vector-hash-tree-root
   #:ssz-tests-list-serialize-length
   #:ssz-tests-list-fixed-size-p
   #:ssz-tests-list-hash-tree-root
   #:ssz-tests-empty-vector-serialize
   #:ssz-tests-empty-list-serialize
   #:ssz-tests-vector-element-count
   #:ssz-tests-vector-roundtrip
   #:ssz-tests-vector-roundtrip-values
   #:ssz-tests-list-roundtrip
   #:ssz-tests-list-roundtrip-values
   #:ssz-tests-empty-vector-roundtrip
   #:ssz-tests-empty-list-roundtrip
   #:ssz-tests-vector-u8-roundtrip
   ;; Union tests
   #:ssz-tests-union-none-serialize
   #:ssz-tests-union-none-selector
   #:ssz-tests-union-none-is-none
   #:ssz-tests-union-some-serialize-length
   #:ssz-tests-union-some-selector
   #:ssz-tests-union-some-is-none
   #:ssz-tests-union-roundtrip
   #:ssz-tests-union-none-roundtrip
   #:ssz-tests-union-hash-tree-root-length
   #:ssz-tests-union-none-hash-tree-root-length
   #:ssz-tests-union-selector-validation
   #:ssz-tests-union-selector-0-validation
   ;; Basic type roundtrip tests
   #:ssz-tests-u8-roundtrip
   #:ssz-tests-u16-roundtrip
   #:ssz-tests-u32-roundtrip
   #:ssz-tests-u64-roundtrip
   #:ssz-tests-u128-roundtrip
   #:ssz-tests-u256-roundtrip
   #:ssz-tests-bool-true-roundtrip
   #:ssz-tests-bool-false-roundtrip
   #:ssz-tests-u8-max-roundtrip
   #:ssz-tests-u16-max-roundtrip
   #:ssz-tests-u32-max-roundtrip
   #:ssz-tests-u64-max-roundtrip
   ;; Bitvector/Bitlist roundtrip tests
   #:ssz-tests-bitvector-8-roundtrip
   #:ssz-tests-bitvector-64-roundtrip
   #:ssz-tests-bitvector-256-roundtrip
   #:ssz-tests-bitvector-with-bits-roundtrip
   #:ssz-tests-bitlist-empty-roundtrip
   #:ssz-tests-bitlist-with-bits-roundtrip
   ;; Composite type roundtrip tests
   #:ssz-tests-vector-u8-full-roundtrip
   #:ssz-tests-vector-u16-roundtrip
   #:ssz-tests-vector-u64-roundtrip
   #:ssz-tests-vector-bool-roundtrip
   #:ssz-tests-list-u8-roundtrip
   #:ssz-tests-list-u16-roundtrip
   #:ssz-tests-list-u64-roundtrip
   #:ssz-tests-list-bool-roundtrip
   ;; Edge case tests
   #:ssz-tests-vector-single-element
   #:ssz-tests-list-single-element
   #:ssz-tests-bitvector-255-bits
   #:ssz-tests-bitvector-257-bits
   #:ssz-tests-bitvector-single-bit
   #:ssz-tests-bitlist-single-bit
   #:ssz-tests-bitlist-7-bits
   #:ssz-tests-bitlist-8-bits
   #:ssz-tests-pack-31-bytes
   #:ssz-tests-pack-32-bytes
   #:ssz-tests-pack-33-bytes
   #:ssz-tests-pack-64-bytes
   #:ssz-tests-deserialize-u32-truncated
   #:ssz-tests-deserialize-u64-truncated
   #:ssz-tests-deserialize-bool-invalid-2
   #:ssz-tests-deserialize-bool-invalid-255
   #:ssz-tests-bytes-slice-full
   #:ssz-tests-bytes-slice-empty
   #:ssz-tests-zero-hash-depth-0
   #:ssz-tests-zero-hash-depth-1
   #:ssz-tests-merkleize-single-chunk
   #:ssz-tests-merkleize-two-chunks
   ;; Additional coverage tests
   #:ssz-tests-u128-max-roundtrip
   #:ssz-tests-u256-max-roundtrip
   #:ssz-tests-list-from-list-reject
   #:ssz-tests-bitlist-max-reject
   #:ssz-tests-mix-in-selector
   #:ssz-tests-mix-in-selector-differs
   #:ssz-tests-merkleize-with-limit
   #:ssz-tests-bytes-set
   #:ssz-tests-union-value-roundtrip
   #:ssz-tests-large-vector-roundtrip
   #:ssz-tests-large-list-roundtrip
   #:ssz-tests-container-htr-u32
   #:ssz-tests-container-htr-differs))

;;; Package for pure CL test runners
(defpackage #:ssz-tests/runner
  (:documentation "Pure CL test runners for SSZ library")
  (:use #:cl)
  (:export
   #:run-tests
   #:run-spec-tests))
