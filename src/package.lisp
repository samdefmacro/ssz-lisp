(defpackage #:ssz
  (:documentation "SSZ (Simple Serialize) - Ethereum's serialization format")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:slice #:coalton-library/slice)
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:bits #:coalton-library/bits)
   (#:list #:coalton-library/list)
   (#:math #:coalton-library/math)
   (#:hash #:coalton-library/hash))
  (:export
   ;; Constants
   #:+bytes-per-chunk+
   #:+bytes-per-length-offset+
   #:+bits-per-byte+

   ;; Result type for SSZ operations
   #:SSZError
   #:SerializationError
   #:DeserializationError
   #:MerkleizationError
   #:SSZResult

   ;; Type class for serializable types
   #:SSZSerialize
   #:ssz-serialize
   #:ssz-fixed-size?
   #:ssz-fixed-size

   #:SSZDeserialize
   #:ssz-deserialize

   #:SSZHashTreeRoot
   #:hash-tree-root

   ;; Byte vector type alias
   #:Bytes
   #:bytes-length
   #:bytes-ref
   #:bytes-append
   #:make-bytes
   #:bytes-from-list
   #:bytes-slice

   ;; Serialization functions
   #:serialize-u8
   #:serialize-u16
   #:serialize-u32
   #:serialize-u64
   #:serialize-u128
   #:serialize-u256
   #:serialize-bool

   ;; Deserialization functions
   #:deserialize-u8
   #:deserialize-u16
   #:deserialize-u32
   #:deserialize-u64
   #:deserialize-u128
   #:deserialize-u256
   #:deserialize-bool

   ;; U256 and U128 types
   #:U256
   #:U128
   #:u256-from-bytes
   #:u128-from-bytes

   ;; Merkleization
   #:Chunk
   #:merkleize
   #:mix-in-length
   #:mix-in-selector
   #:pack
   #:sha256
   #:chunk-count
   #:zero-chunk
   #:zero-hash
   #:hash-concat

   ;; SSZ composite types
   #:SSZVector
   #:ssz-vector-elements
   #:ssz-vector-length
   #:ssz-vector-ref
   #:ssz-vector-from-vec
   #:ssz-vector-from-list
   #:deserialize-vector-fixed
   #:deserialize-vector-variable

   #:SSZList
   #:ssz-list-max-length
   #:ssz-list-elements
   #:ssz-list-length
   #:ssz-list-ref
   #:ssz-list-from-vec
   #:ssz-list-from-list
   #:deserialize-list-fixed
   #:deserialize-list-variable
   #:serialize-list-variable

   #:SSZBitvector
   #:ssz-bitvector-length
   #:ssz-bitvector-bits
   #:ssz-bitvector-ref
   #:ssz-bitvector-from-list
   #:ssz-bitvector-zeros
   #:serialize-bitvector
   #:deserialize-bitvector

   #:SSZBitlist
   #:ssz-bitlist-max-length
   #:ssz-bitlist-length
   #:ssz-bitlist-bits
   #:ssz-bitlist-ref
   #:ssz-bitlist-from-list
   #:ssz-bitlist-empty
   #:serialize-bitlist
   #:deserialize-bitlist

   #:SSZUnion
   #:ssz-union-selector
   #:ssz-union-value
   #:ssz-union-none
   #:ssz-union-is-none
   #:ssz-union-some
   #:ssz-union-from-value
   #:serialize-union
   #:deserialize-union
   #:deserialize-union-value

   ;; Container helpers
   #:FieldType
   #:FixedField
   #:VariableField
   #:SerializedField
   #:serialize-container
   #:deserialize-container
   #:DeserializedFields
   #:get-field
   #:validate-container-offsets
   #:container-fixed-size?
   #:container-fixed-size
   #:fixed-field
   #:variable-field
   #:serialize-fixed-field
   #:serialize-variable-field
   #:hash-tree-root-container

   ;; Additional utilities
   #:bytes-ref-unsafe
   #:bytes-set!
   #:u256-make
   #:u256-word
   #:u256-zero
   #:u128-make
   #:u128-word
   #:u128-zero
   #:zero-chunk
   #:hash-concat
   #:merkleize-with-limit
   #:next-pow-of-two
   #:log2-ceil
   #:zero-hash
   #:pack-bits
   #:chunk-count-bitvector
   #:chunk-count-vector-basic
   #:chunk-count-vector-composite))

(in-package #:ssz)

(named-readtables:in-readtable coalton:coalton)
