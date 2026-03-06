;;;; SSZ Basic Usage Examples
;;;; This file demonstrates how to use the SSZ library in Coalton

(defpackage #:ssz-examples
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:ssz #:ssz)
   (#:vec #:coalton-library/vector)
   (#:list #:coalton-library/list)))

(in-package #:ssz-examples)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;; =========================================================================
  ;;; Example 1: Basic Type Serialization & Deserialization
  ;;; =========================================================================

  (define (example-u8-roundtrip)
    "Serialize and deserialize a U8"
    (let ((data (ssz:serialize-u8 255)))
      ;; data is 1 byte: #(255)
      (match (ssz:deserialize-u8 data)
        ((Ok val) val)      ; => 255
        ((Err _) 0))))

  (define (example-u16-roundtrip)
    "Serialize and deserialize a U16 (little-endian)"
    (let ((data (ssz:serialize-u16 258)))
      ;; data is 2 bytes: #(2 1) because 258 = 0x0102 stored little-endian
      (match (ssz:deserialize-u16 data)
        ((Ok val) val)      ; => 258
        ((Err _) 0))))

  (define (example-u32-roundtrip)
    "Serialize and deserialize a U32"
    (let ((data (ssz:serialize-u32 12345)))
      (match (ssz:deserialize-u32 data)
        ((Ok val) val)      ; => 12345
        ((Err _) 0))))

  (define (example-u64-roundtrip)
    "Serialize and deserialize a U64"
    (let ((data (ssz:serialize-u64 9876543210)))
      ;; data is 8 bytes, little-endian
      (match (ssz:deserialize-u64 data)
        ((Ok val) val)      ; => 9876543210
        ((Err _) 0))))

  (define (example-bool-roundtrip)
    "Serialize and deserialize booleans"
    (let ((true-bytes (ssz:serialize-bool True))
          (_false-bytes (ssz:serialize-bool False)))
      ;; true-bytes = #(1), false-bytes = #(0)
      (match (ssz:deserialize-bool true-bytes)
        ((Ok val) val)      ; => True
        ((Err _) False))))

  ;;; =========================================================================
  ;;; Example 2: U128 and U256 (Large Integer Types)
  ;;; =========================================================================

  (define (example-u128)
    "Create, serialize, and deserialize a U128"
    ;; U128 is constructed from two 64-bit words: (low, high)
    (let ((val (ssz:u128-make 12345678 87654321)))
      (let ((data (ssz:serialize-u128 val)))
        ;; data is 16 bytes, little-endian
        (match (ssz:deserialize-u128 data)
          ((Ok result)
           ;; Access individual 64-bit words
           (ssz:u128-word 0 result))  ; => 12345678 (low word)
          ((Err _) 0)))))

  (define (example-u256)
    "Create, serialize, and deserialize a U256"
    ;; U256 is constructed from four 64-bit words
    (let ((val (ssz:u256-make 111 222 333 444)))
      (let ((data (ssz:serialize-u256 val)))
        ;; data is 32 bytes, little-endian
        (match (ssz:deserialize-u256 data)
          ((Ok result)
           ;; Access individual 64-bit words
           (ssz:u256-word 0 result))  ; => 111 (lowest word)
          ((Err _) 0)))))

  (define (example-u256-zero)
    "Use the zero constant"
    (let ((zero (ssz:u256-zero)))
      (ssz:serialize-u256 zero)))  ; => 32 bytes of zeros

  ;;; =========================================================================
  ;;; Example 3: Bytes Utilities
  ;;; =========================================================================

  (define (example-bytes-create)
    "Various ways to create byte vectors"
    ;; Create zero-filled bytes of given length
    (let ((_zeros (ssz:make-bytes 32)))
      ;; Create from a list of byte values
      (let ((data (ssz:bytes-from-list (make-list 1 2 3 4 5))))
        ;; Get length
        (let ((_len (ssz:bytes-length data)))  ; => 5
          ;; Access element (returns Optional)
          (match (ssz:bytes-ref 0 data)
            ((Some b) b)    ; => 1
            ((None) 0))))))

  (define (example-bytes-slice)
    "Slice a byte vector"
    (let ((data (ssz:bytes-from-list (make-list 10 20 30 40 50))))
      ;; bytes-slice offset length data
      (let ((middle (ssz:bytes-slice 1 3 data)))
        ;; middle = #(20 30 40)
        (ssz:bytes-length middle))))  ; => 3

  (define (example-bytes-append)
    "Append two byte vectors"
    (let ((a (ssz:bytes-from-list (make-list 1 2 3)))
          (b (ssz:bytes-from-list (make-list 4 5 6))))
      (let ((combined (ssz:bytes-append a b)))
        ;; combined = #(1 2 3 4 5 6)
        (ssz:bytes-length combined))))  ; => 6

  ;;; =========================================================================
  ;;; Example 4: SSZVector (Fixed-Length Sequence)
  ;;; =========================================================================

  (define (example-vector-create)
    "Create an SSZ Vector"
    ;; SSZVector wraps a List; type annotation needed for element type
    (let ((vec (the (ssz:SSZVector U32)
                    (ssz:SSZVector (make-list 100 200 300)))))
      (ssz:ssz-vector-length vec)))  ; => 3

  (define (example-vector-roundtrip)
    "Serialize and deserialize a Vector[U32]"
    (let ((vec (the (ssz:SSZVector U32)
                    (ssz:SSZVector (make-list 100 200 300)))))
      ;; Serialize using the type class
      (let ((data (ssz:ssz-serialize vec)))
        ;; data = 12 bytes (3 elements * 4 bytes each)
        ;; Deserialize - explicit type annotation is required
        (match (the (ssz:SSZResult (ssz:SSZVector U32))
                    (ssz:ssz-deserialize data))
          ((Ok (ssz:SSZVector elements))
           elements)         ; => (100 200 300)
          ((Err _) Nil)))))

  (define (example-vector-access)
    "Access vector elements"
    (let ((vec (the (ssz:SSZVector U16)
                    (ssz:SSZVector (make-list 10 20 30)))))
      ;; Get elements as list
      (let ((_elems (ssz:ssz-vector-elements vec)))
        ;; Index-based access (returns Optional)
        (match (ssz:ssz-vector-ref 1 vec)
          ((Some val) val)   ; => 20
          ((None) 0)))))

  (define (example-vector-u8)
    "Vector of U8 - serializes to raw bytes"
    (let ((vec (the (ssz:SSZVector U8)
                    (ssz:SSZVector (make-list 0 127 255)))))
      (let ((data (ssz:ssz-serialize vec)))
        ;; data = #(0 127 255) - 3 bytes
        (ssz:bytes-length data))))  ; => 3

  ;;; =========================================================================
  ;;; Example 5: SSZList (Variable-Length Sequence)
  ;;; =========================================================================

  (define (example-list-create)
    "Create an SSZ List with max-length"
    ;; SSZList takes max-length and elements
    (let ((lst (the (ssz:SSZList U32)
                    (ssz:SSZList 100 (make-list 42 99)))))
      ;; Query properties
      (let ((_max (ssz:ssz-list-max-length lst))  ; => 100
            (_len (ssz:ssz-list-length lst)))      ; => 2
        _len)))

  (define (example-list-roundtrip)
    "Serialize and deserialize a List[U64]"
    (let ((lst (the (ssz:SSZList U64)
                    (ssz:SSZList 1000 (make-list 111 222 333)))))
      (let ((data (ssz:ssz-serialize lst)))
        ;; data = 24 bytes (3 elements * 8 bytes each)
        (match (the (ssz:SSZResult (ssz:SSZList U64))
                    (ssz:ssz-deserialize data))
          ((Ok result)
           (ssz:ssz-list-length result))  ; => 3
          ((Err _) 0)))))

  (define (example-list-empty)
    "Empty lists serialize to zero bytes"
    (let ((lst (the (ssz:SSZList U32) (ssz:SSZList 100 Nil))))
      (let ((data (ssz:ssz-serialize lst)))
        (ssz:bytes-length data))))  ; => 0

  ;;; =========================================================================
  ;;; Example 6: Bitvector (Fixed-Length Bit Sequence)
  ;;; =========================================================================

  (define (example-bitvector-zeros)
    "Create a bitvector of all zeros"
    (let ((bv (ssz:ssz-bitvector-zeros 16)))
      ;; Serialize: 16 bits = 2 bytes
      (let ((data (ssz:serialize-bitvector bv)))
        (ssz:bytes-length data))))  ; => 2

  (define (example-bitvector-from-list)
    "Create bitvector from boolean list"
    ;; Bits 0, 2, 7 set => 0b10000101 = 0x85
    (let ((bits (make-list True False True False False False False True)))
      (match (ssz:ssz-bitvector-from-list 8 bits)
        ((Ok bv)
         (ssz:serialize-bitvector bv))
        ((Err _)
         (ssz:make-bytes 0)))))

  (define (example-bitvector-roundtrip)
    "Bitvector serialize/deserialize roundtrip"
    (match (ssz:ssz-bitvector-from-list 8
             (make-list True False True False True True False False))
      ((Ok bv)
       (let ((data (ssz:serialize-bitvector bv)))
         ;; Deserialize requires the bit count
         (match (ssz:deserialize-bitvector 8 data)
           ((Ok result)
            ;; Check individual bits
            (ssz:ssz-bitvector-ref 0 result))  ; => (Some True)
           ((Err _) None))))
      ((Err _) None)))

  ;;; =========================================================================
  ;;; Example 7: Bitlist (Variable-Length Bit Sequence)
  ;;; =========================================================================

  (define (example-bitlist-empty)
    "Create and serialize an empty bitlist"
    (let ((bl (ssz:ssz-bitlist-empty 100)))
      ;; Serializes to 1 byte (just the marker bit)
      (ssz:serialize-bitlist bl)))

  (define (example-bitlist-from-list)
    "Create bitlist from booleans with max-length"
    (let ((bits (make-list True False True True False)))
      (match (ssz:ssz-bitlist-from-list 100 bits)
        ((Ok bl)
         ;; Data bits + marker bit
         ;; Bits: 1,0,1,1,0 + marker at bit 5 => 0b00101101 = 45
         (ssz:serialize-bitlist bl))
        ((Err _)
         (ssz:make-bytes 0)))))

  (define (example-bitlist-roundtrip)
    "Bitlist serialize/deserialize roundtrip"
    (match (ssz:ssz-bitlist-from-list 100
             (make-list True True False True))
      ((Ok bl)
       (let ((data (ssz:serialize-bitlist bl)))
         ;; Deserialize requires the max-length
         (match (ssz:deserialize-bitlist 100 data)
           ((Ok result)
            (ssz:ssz-bitlist-length result))  ; => 4
           ((Err _) 0))))
      ((Err _) 0)))

  ;;; =========================================================================
  ;;; Example 8: SSZUnion (Tagged Union)
  ;;; =========================================================================

  (define (example-union-none)
    "Create a None union (selector 0)"
    (let ((union (ssz:ssz-union-none Unit)))
      ;; Serializes to 1 byte: the selector (0)
      (let ((data (ssz:ssz-serialize union)))
        (ssz:bytes-length data))))  ; => 1

  (define (example-union-some)
    "Create a union with a value"
    ;; ssz-union-from-value serializes the value and wraps it
    ;; Selector must be 1-127
    (match (ssz:ssz-union-from-value 1 (the U32 42))
      ((Ok union)
       ;; Serializes to 5 bytes: selector (1) + U32 (4 bytes)
       (let ((data (ssz:ssz-serialize union)))
         (ssz:bytes-length data)))  ; => 5
      ((Err _) 0)))

  (define (example-union-roundtrip)
    "Union serialize/deserialize roundtrip"
    (match (ssz:ssz-union-from-value 3 (the U32 12345))
      ((Ok union)
       (let ((data (ssz:ssz-serialize union)))
         (match (ssz:ssz-deserialize data)
           ((Ok deserialized)
            ;; Check selector is preserved
            (ssz:ssz-union-selector deserialized))  ; => 3
           ((Err _) 0))))
      ((Err _) 0)))

  (define (example-union-inspect)
    "Inspect union properties"
    (match (ssz:ssz-union-from-value 5 (the U64 999))
      ((Ok union)
       (let ((_sel (ssz:ssz-union-selector union))  ; => 5
             (_is-none (ssz:ssz-union-is-none union))  ; => False
             (_value (ssz:ssz-union-value union)))  ; => (Some <bytes>)
         _sel))
      ((Err _) 0)))

  ;;; =========================================================================
  ;;; Example 9: Container Serialization (Heterogeneous Structs)
  ;;; =========================================================================

  ;; Define a container with all fixed-size fields
  (define-struct SimpleContainer
    "A simple container with uint32 and bool fields"
    (field-a U32)
    (field-b Boolean))

  (declare serialize-simple-container (SimpleContainer -> ssz:Bytes))
  (define (serialize-simple-container (SimpleContainer a b))
    "Serialize SimpleContainer to SSZ bytes"
    ;; Both fields are fixed-size, so no offset encoding needed
    (let ((fields
           (make-list
            (ssz:fixed-field 4 (ssz:serialize-u32 a))
            (ssz:fixed-field 1 (ssz:serialize-bool b)))))
      (ssz:serialize-container fields)))

  (declare deserialize-simple-container (ssz:Bytes -> (ssz:SSZResult SimpleContainer)))
  (define (deserialize-simple-container data)
    "Deserialize SimpleContainer from SSZ bytes"
    ;; Describe the field layout
    (let ((field-types (make-list (ssz:FixedField 4) (ssz:FixedField 1))))
      (match (ssz:deserialize-container field-types data)
        ((Err e) (Err e))
        ((Ok fields)
         ;; Extract each field by index
         (match (ssz:get-field 0 fields)
           ((None) (Err (ssz:DeserializationError "Missing field 0")))
           ((Some field-a-bytes)
            (match (ssz:get-field 1 fields)
              ((None) (Err (ssz:DeserializationError "Missing field 1")))
              ((Some field-b-bytes)
               ;; Deserialize individual fields
               (match (ssz:deserialize-u32 field-a-bytes)
                 ((Err e) (Err e))
                 ((Ok a)
                  (match (ssz:deserialize-bool field-b-bytes)
                    ((Err e) (Err e))
                    ((Ok b) (Ok (SimpleContainer a b))))))))))))))

  (define (example-container-roundtrip)
    "Container serialize/deserialize roundtrip"
    (let ((container (SimpleContainer 42 True)))
      (let ((data (serialize-simple-container container)))
        ;; data = 5 bytes (4 for U32 + 1 for Boolean)
        (match (deserialize-simple-container data)
          ((Ok (SimpleContainer a _b))
           (== a 42))       ; => True
          ((Err _) False)))))

  ;; Container with a variable-size field
  (define-struct MixedContainer
    "Container with fixed and variable-size fields"
    (mc-fixed U64)
    (mc-variable ssz:SSZBitlist))

  (declare serialize-mixed-container (MixedContainer -> ssz:Bytes))
  (define (serialize-mixed-container (MixedContainer fixed var))
    "Serialize MixedContainer: fixed field + offset + variable data"
    (let ((fields
           (make-list
            (ssz:fixed-field 8 (ssz:serialize-u64 fixed))
            (ssz:variable-field (ssz:serialize-bitlist var)))))
      (ssz:serialize-container fields)))

  (declare deserialize-mixed-container (ssz:Bytes -> (ssz:SSZResult MixedContainer)))
  (define (deserialize-mixed-container data)
    "Deserialize MixedContainer from SSZ bytes"
    (let ((field-types (make-list (ssz:FixedField 8) ssz:VariableField)))
      (match (ssz:deserialize-container field-types data)
        ((Err e) (Err e))
        ((Ok fields)
         (match (ssz:get-field 0 fields)
           ((None) (Err (ssz:DeserializationError "Missing field 0")))
           ((Some fixed-bytes)
            (match (ssz:get-field 1 fields)
              ((None) (Err (ssz:DeserializationError "Missing field 1")))
              ((Some var-bytes)
               (match (ssz:deserialize-u64 fixed-bytes)
                 ((Err e) (Err e))
                 ((Ok fixed-val)
                  (match (ssz:deserialize-bitlist 100 var-bytes)
                    ((Err e) (Err e))
                    ((Ok bitlist-val)
                     (Ok (MixedContainer fixed-val bitlist-val))))))))))))))

  ;;; =========================================================================
  ;;; Example 10: Hash Tree Root (Merkleization)
  ;;; =========================================================================

  (define (example-hash-tree-root-basic)
    "Hash tree root of basic types"
    ;; For basic types, the value is serialized and padded to 32 bytes
    (let ((root-u32 (ssz:hash-tree-root (the U32 42)))
          (_root-u64 (ssz:hash-tree-root (the U64 999)))
          (_root-bool (ssz:hash-tree-root True)))
      ;; Each root is 32 bytes
      (ssz:bytes-length root-u32)))  ; => 32

  (define (example-hash-tree-root-composite)
    "Hash tree root of composite types"
    (let ((vec (the (ssz:SSZVector U32)
                    (ssz:SSZVector (make-list 1 2 3)))))
      (let ((root (ssz:hash-tree-root vec)))
        ;; Root is always 32 bytes
        (ssz:bytes-length root))))  ; => 32

  (declare hash-tree-root-simple-container (SimpleContainer -> ssz:Bytes))
  (define (hash-tree-root-simple-container (SimpleContainer a b))
    "Hash tree root of a container = merkleize(field roots)"
    (ssz:hash-tree-root-container
      (make-list
        (ssz:hash-tree-root a)
        (ssz:hash-tree-root b))))

  (define (example-hash-tree-root-deterministic)
    "Same inputs always produce the same root"
    (let ((root1 (ssz:hash-tree-root (the U32 42)))
          (root2 (ssz:hash-tree-root (the U32 42))))
      ;; root1 and root2 are identical
      (== (ssz:bytes-ref 0 root1) (ssz:bytes-ref 0 root2))))  ; => True

  ;;; =========================================================================
  ;;; Example 11: Merkle Utilities
  ;;; =========================================================================

  (define (example-sha256)
    "Compute SHA256 hash"
    (let ((data (ssz:bytes-from-list (make-list 1 2 3 4))))
      (let ((hash (ssz:sha256 data)))
        (ssz:bytes-length hash))))  ; => 32

  (define (example-pack)
    "Pack serialized data into 32-byte chunks"
    ;; 33 bytes of data => 2 chunks (second chunk zero-padded)
    (let ((data (ssz:make-bytes 33)))
      (let ((chunks (ssz:pack data)))
        (list:length chunks))))  ; => 2

  (define (example-merkleize)
    "Merkleize a list of chunks"
    (let ((chunk1 (ssz:zero-chunk))
          (chunk2 (ssz:zero-chunk)))
      ;; merkleize hashes chunks into a binary tree root
      (let ((root (ssz:merkleize (make-list chunk1 chunk2))))
        (ssz:bytes-length root))))  ; => 32

  (define (example-zero-hash)
    "Precomputed zero hashes for merkle tree padding"
    ;; zero-hash(0) = 32 zero bytes
    ;; zero-hash(1) = SHA256(zero-hash(0) || zero-hash(0))
    ;; zero-hash(n) = SHA256(zero-hash(n-1) || zero-hash(n-1))
    (let ((zh0 (ssz:zero-hash 0))
          (_zh1 (ssz:zero-hash 1)))
      ;; zh0 is all zeros, zh1 is not
      (== (ssz:bytes-ref 0 zh0) (Some 0))))  ; => True

  (define (example-mix-in-length)
    "Mix length into a merkle root (used for variable-size types)"
    (let ((root (ssz:zero-chunk)))
      ;; hash(root || length_as_le_bytes)
      (let ((mixed (ssz:mix-in-length root 42)))
        (ssz:bytes-length mixed))))  ; => 32

  (define (example-hash-concat)
    "Hash two chunks together"
    (let ((left (ssz:zero-chunk))
          (right (ssz:zero-chunk)))
      ;; SHA256(left || right)
      (let ((result (ssz:hash-concat left right)))
        (ssz:bytes-length result))))  ; => 32

  ;;; =========================================================================
  ;;; Example 12: Type Class Interface
  ;;; =========================================================================

  (define (example-type-class-serialize)
    "Use the generic ssz-serialize for any SSZ type"
    ;; Works for basic types
    (let ((_bytes-u32 (ssz:ssz-serialize (the U32 42)))
          (_bytes-bool (ssz:ssz-serialize True)))
      ;; Works for composite types
      (let ((_bytes-vec (ssz:ssz-serialize
                          (the (ssz:SSZVector U32)
                               (ssz:SSZVector (make-list 1 2 3))))))
        _bytes-vec)))

  (define (example-type-class-deserialize)
    "Use the generic ssz-deserialize with type annotations"
    (let ((data (ssz:ssz-serialize (the U32 42))))
      ;; The return type annotation tells ssz-deserialize what to produce
      (match (the (ssz:SSZResult U32) (ssz:ssz-deserialize data))
        ((Ok val) val)      ; => 42
        ((Err _) 0))))

  (define (example-type-class-fixed-size)
    "Query whether a type is fixed-size"
    (let ((u32-val (the U32 0))
          (lst-val (the (ssz:SSZList U32) (ssz:SSZList 10 Nil))))
      ;; U32 is fixed-size
      (let ((_is-fixed (ssz:ssz-fixed-size? u32-val))  ; => True
            (_size (ssz:ssz-fixed-size u32-val)))       ; => 4
        ;; SSZList is always variable-size
        (ssz:ssz-fixed-size? lst-val))))  ; => False

  ;;; =========================================================================
  ;;; Example 13: Error Handling Patterns
  ;;; =========================================================================

  (define (example-deserialization-error)
    "Handle deserialization errors gracefully"
    ;; Too few bytes for a U32
    (let ((bad-data (ssz:make-bytes 2)))
      (match (ssz:deserialize-u32 bad-data)
        ((Ok _) "unexpected success")
        ((Err (ssz:DeserializationError msg)) msg)
        ((Err _) "other error"))))

  (define (example-invalid-bool)
    "Boolean only accepts 0 or 1"
    (let ((bad-data (ssz:bytes-from-list (make-list 2))))
      (match (ssz:deserialize-bool bad-data)
        ((Ok _) False)
        ((Err _) True))))   ; => True (error as expected)

  (define (example-bitvector-length-mismatch)
    "Bitvector rejects wrong number of bits"
    ;; Requesting 8-bit bitvector but providing 5 booleans
    (match (ssz:ssz-bitvector-from-list 8
             (make-list True False True False True))
      ((Ok _) False)
      ((Err _) True)))      ; => True (error as expected)

  (define (example-union-invalid-selector)
    "Union rejects selectors > 127"
    (match (ssz:ssz-union-some 128 (ssz:make-bytes 4))
      ((Ok _) False)
      ((Err _) True)))      ; => True (error as expected)
)
