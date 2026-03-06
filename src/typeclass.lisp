(in-package #:ssz)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;; Type classes for SSZ serialization

  ;; Type class for SSZ serializable types
  (define-class (SSZSerialize :a)
    "Type class for types that can be SSZ serialized"
    ;; Serialize value to bytes
    (ssz-serialize (:a -> Bytes))
    ;; Is this a fixed-size type?
    (ssz-fixed-size? (:a -> Boolean))
    ;; Get the fixed size (only valid if fixed-size?)
    (ssz-fixed-size (:a -> UFix)))

  ;; Type class for SSZ deserializable types
  (define-class (SSZDeserialize :a)
    "Type class for types that can be SSZ deserialized"
    (ssz-deserialize (Bytes -> (SSZResult :a))))

  ;; Type class for SSZ hash tree root
  (define-class (SSZHashTreeRoot :a)
    "Type class for types that support hash tree root computation"
    (hash-tree-root (:a -> Chunk)))

  ;;; =========================================================================
  ;;; Instances for basic types
  ;;; =========================================================================

  ;; U8
  (define-instance (SSZSerialize U8)
    (define ssz-serialize serialize-u8)
    (define (ssz-fixed-size? _) True)
    (define (ssz-fixed-size _) 1))

  (define-instance (SSZDeserialize U8)
    (define ssz-deserialize deserialize-u8))

  (define-instance (SSZHashTreeRoot U8)
    (define (hash-tree-root val)
      (merkleize (pack (serialize-u8 val)))))

  ;; U16
  (define-instance (SSZSerialize U16)
    (define ssz-serialize serialize-u16)
    (define (ssz-fixed-size? _) True)
    (define (ssz-fixed-size _) 2))

  (define-instance (SSZDeserialize U16)
    (define ssz-deserialize deserialize-u16))

  (define-instance (SSZHashTreeRoot U16)
    (define (hash-tree-root val)
      (merkleize (pack (serialize-u16 val)))))

  ;; U32
  (define-instance (SSZSerialize U32)
    (define ssz-serialize serialize-u32)
    (define (ssz-fixed-size? _) True)
    (define (ssz-fixed-size _) 4))

  (define-instance (SSZDeserialize U32)
    (define ssz-deserialize deserialize-u32))

  (define-instance (SSZHashTreeRoot U32)
    (define (hash-tree-root val)
      (merkleize (pack (serialize-u32 val)))))

  ;; U64
  (define-instance (SSZSerialize U64)
    (define ssz-serialize serialize-u64)
    (define (ssz-fixed-size? _) True)
    (define (ssz-fixed-size _) 8))

  (define-instance (SSZDeserialize U64)
    (define ssz-deserialize deserialize-u64))

  (define-instance (SSZHashTreeRoot U64)
    (define (hash-tree-root val)
      (merkleize (pack (serialize-u64 val)))))

  ;; U128
  (define-instance (SSZSerialize U128)
    (define ssz-serialize serialize-u128)
    (define (ssz-fixed-size? _) True)
    (define (ssz-fixed-size _) 16))

  (define-instance (SSZDeserialize U128)
    (define ssz-deserialize deserialize-u128))

  (define-instance (SSZHashTreeRoot U128)
    (define (hash-tree-root val)
      (merkleize (pack (serialize-u128 val)))))

  ;; U256
  (define-instance (SSZSerialize U256)
    (define ssz-serialize serialize-u256)
    (define (ssz-fixed-size? _) True)
    (define (ssz-fixed-size _) 32))

  (define-instance (SSZDeserialize U256)
    (define ssz-deserialize deserialize-u256))

  (define-instance (SSZHashTreeRoot U256)
    (define (hash-tree-root val)
      (merkleize (pack (serialize-u256 val)))))

  ;; Boolean
  (define-instance (SSZSerialize Boolean)
    (define ssz-serialize serialize-bool)
    (define (ssz-fixed-size? _) True)
    (define (ssz-fixed-size _) 1))

  (define-instance (SSZDeserialize Boolean)
    (define ssz-deserialize deserialize-bool))

  (define-instance (SSZHashTreeRoot Boolean)
    (define (hash-tree-root val)
      (merkleize (pack (serialize-bool val)))))

  ;;; =========================================================================
  ;;; Instances for composite types
  ;;; =========================================================================

  ;; SSZBitvector
  (define-instance (SSZSerialize SSZBitvector)
    (define ssz-serialize serialize-bitvector)
    (define (ssz-fixed-size? _) True)
    (define (ssz-fixed-size bv)
      (let ((len (ssz-bitvector-length bv)))
        (lisp UFix (len) (cl:floor (cl:+ len 7) 8)))))

  (define-instance (SSZHashTreeRoot SSZBitvector)
    (define hash-tree-root hash-tree-root-bitvector))

  ;; SSZBitlist
  (define-instance (SSZSerialize SSZBitlist)
    (define ssz-serialize serialize-bitlist)
    (define (ssz-fixed-size? _) False)
    (define (ssz-fixed-size _) 0))  ; Not applicable for variable-size

  (define-instance (SSZHashTreeRoot SSZBitlist)
    (define hash-tree-root hash-tree-root-bitlist))

  ;; SSZVector - parameterized over element type
  (define-instance ((SSZSerialize :a) => (SSZSerialize (SSZVector :a)))
    (define (ssz-serialize vec)
      "Serialize a vector by concatenating serialized elements"
      (let ((elements (ssz-vector-elements vec)))
        (fold bytes-append (make-bytes 0)
              (map ssz-serialize elements))))

    (define (ssz-fixed-size? vec)
      "Vector is fixed-size if element type is fixed-size"
      (match (ssz-vector-elements vec)
        ((Nil) True)  ; Empty vector is trivially fixed-size
        ((Cons first _) (ssz-fixed-size? first))))

    (define (ssz-fixed-size vec)
      "Fixed size = element_count * element_size"
      (let ((elements (ssz-vector-elements vec)))
        (match elements
          ((Nil) 0)
          ((Cons first _)
           (let ((elem-size (ssz-fixed-size first))
                 (count (ssz-vector-length vec)))
             (* count elem-size)))))))

  (define-instance ((SSZSerialize :a) (SSZHashTreeRoot :a) => (SSZHashTreeRoot (SSZVector :a)))
    (define (hash-tree-root vec)
      "Hash tree root of vector - merkleize element roots"
      (let ((elements (ssz-vector-elements vec)))
        (match elements
          ((Nil) (merkleize (Cons (zero-chunk) Nil)))
          ((Cons first _)
           (if (ssz-fixed-size? first)
               ;; Fixed-size elements: pack serialized data
               (merkleize (pack (ssz-serialize vec)))
               ;; Variable-size elements: merkleize element roots
               (merkleize (map hash-tree-root elements))))))))

  ;; SSZList - parameterized over element type
  (define-instance ((SSZSerialize :a) => (SSZSerialize (SSZList :a)))
    (define (ssz-serialize lst)
      "Serialize a list by concatenating serialized elements (variable-size elements need offsets)"
      (let ((elements (ssz-list-elements lst)))
        (match elements
          ((Nil) (make-bytes 0))
          ((Cons first rest)
           (if (ssz-fixed-size? first)
               ;; Fixed-size elements: simple concatenation
               (fold bytes-append (make-bytes 0)
                     (map ssz-serialize elements))
               ;; Variable-size elements: use offset encoding
               (serialize-list-variable elements))))))

    (define (ssz-fixed-size? _)
      "List is always variable-size"
      False)

    (define (ssz-fixed-size _)
      "Not applicable for variable-size type"
      0))

  (define-instance ((SSZSerialize :a) (SSZHashTreeRoot :a) => (SSZHashTreeRoot (SSZList :a)))
    (define (hash-tree-root lst)
      "Hash tree root of list - merkleize with limit, then mix in length"
      (let ((elements (ssz-list-elements lst))
            (max-len (ssz-list-max-length lst))
            (cur-len (ssz-list-length lst)))
        (match elements
          ((Nil)
           ;; Empty list: merkleize zero chunk with limit, mix in length 0
           (mix-in-length (merkleize-with-limit (Cons (zero-chunk) Nil) (Some max-len)) 0))
          ((Cons first _)
           (let ((root
                  (if (ssz-fixed-size? first)
                      ;; Fixed-size elements: pack serialized data with limit
                      (let ((elem-size (ssz-fixed-size first)))
                        (merkleize-with-limit
                         (pack (ssz-serialize lst))
                         (Some (chunk-count-vector-basic max-len elem-size))))
                      ;; Variable-size elements: merkleize element roots with limit
                      (merkleize-with-limit
                       (map hash-tree-root elements)
                       (Some max-len)))))
             (mix-in-length root cur-len)))))))

  ;; Helper function for serializing lists with variable-size elements
  (declare serialize-list-variable ((SSZSerialize :a) => (List :a) -> Bytes))
  (define (serialize-list-variable elements)
    "Serialize a list of variable-size elements with offset encoding"
    (let ((count (lisp UFix (elements) (cl:length elements))))
      ;; First pass: compute offsets
      ;; Fixed part: 4 bytes per element (offset pointers)
      (let ((fixed-part-size (* count 4)))
        ;; Serialize all elements
        (let ((serialized-elements (map ssz-serialize elements)))
          ;; Compute cumulative sizes
          (let ((sizes (map bytes-length serialized-elements)))
            ;; Build offset list
            (let ((offsets (compute-offsets fixed-part-size sizes)))
              ;; Concatenate offset bytes + data bytes
              (let ((offset-bytes (fold bytes-append (make-bytes 0)
                                        (map serialize-u32 offsets)))
                    (data-bytes (fold bytes-append (make-bytes 0)
                                      serialized-elements)))
                (bytes-append offset-bytes data-bytes))))))))

  ;; Helper to compute offsets from cumulative sizes
  (declare compute-offsets (UFix -> (List UFix) -> (List U32)))
  (define (compute-offsets start-offset sizes)
    "Compute offset list from starting offset and element sizes"
    (lisp (List U32) (start-offset sizes)
      (cl:let ((offsets cl:nil)
               (current-offset start-offset))
        (cl:dolist (size sizes)
          (cl:push current-offset offsets)
          (cl:incf current-offset size))
        (cl:nreverse offsets))))

  ;;; =========================================================================
  ;;; SSZDeserialize instances for composite types
  ;;; =========================================================================

  ;; SSZVector deserialization
  ;; Requires both SSZSerialize (to know element size/fixed-ness) and SSZDeserialize (to deserialize elements)
  (define-instance ((SSZSerialize :a) (SSZDeserialize :a) => (SSZDeserialize (SSZVector :a)))
    (define (ssz-deserialize data)
      "Deserialize a vector - infers element count from data"
      (if (== (bytes-length data) 0)
          ;; Empty data -> empty vector
          (Ok (SSZVector Nil))
          ;; Need to determine if elements are fixed-size
          ;; We use a dummy deserialization to check element properties
          (deserialize-vector-infer data))))

  ;; Helper to deserialize vector by inferring element count
  (declare deserialize-vector-infer
           ((SSZSerialize :a) (SSZDeserialize :a) => Bytes -> (SSZResult (SSZVector :a))))
  (define (deserialize-vector-infer data)
    "Deserialize vector, inferring element count from data"
    ;; Try to deserialize first element to determine if fixed-size
    ;; For fixed-size: count = data-length / elem-size
    ;; For variable-size: count = first-offset / 4
    (let ((data-len (bytes-length data)))
      (if (< data-len 4)
          ;; Less than 4 bytes - try as fixed-size elements (could be multiple U8s, U16s, etc.)
          (deserialize-vector-fixed-infer data)
          ;; Check if this looks like variable-size (first 4 bytes would be an offset)
          (match (deserialize-u32 (bytes-slice 0 4 data))
            ((Err e) (Err e))
            ((Ok first-word)
             (let ((first-word-ufix (lisp UFix (first-word) first-word)))
               ;; Heuristic: if first word is a valid offset (>= 4 and <= data-len and divisible by 4),
               ;; treat as variable-size elements
               (if (and (>= first-word-ufix 4)
                        (<= first-word-ufix data-len)
                        (== (lisp UFix (first-word-ufix) (cl:mod first-word-ufix 4)) 0))
                   ;; Variable-size elements
                   (let ((elem-count (lisp UFix (first-word-ufix) (cl:floor first-word-ufix 4))))
                     (deserialize-vector-variable-tc elem-count data))
                   ;; Fixed-size elements - try to deserialize first element to get size
                   (deserialize-vector-fixed-infer data))))))))

  ;; Deserialize vector of fixed-size elements by inferring size from first element
  (declare deserialize-vector-fixed-infer
           ((SSZSerialize :a) (SSZDeserialize :a) => Bytes -> (SSZResult (SSZVector :a))))
  (define (deserialize-vector-fixed-infer data)
    "Deserialize fixed-size vector by trying progressively larger element sizes"
    ;; Try common element sizes: 1, 2, 4, 8, 16, 32, 48, 96 bytes
    (let ((data-len (bytes-length data)))
      (try-deserialize-with-sizes data data-len (Cons 1 (Cons 2 (Cons 4 (Cons 8
        (Cons 16 (Cons 32 (Cons 48 (Cons 96 Nil)))))))))))

  (declare try-deserialize-with-sizes
           ((SSZSerialize :a) (SSZDeserialize :a) => Bytes -> UFix -> (List UFix) -> (SSZResult (SSZVector :a))))
  (define (try-deserialize-with-sizes data data-len sizes)
    "Try to deserialize with each element size until one works"
    (match sizes
      ((Nil) (Err (DeserializationError "Could not determine element size for vector")))
      ((Cons size rest)
       (if (== (lisp UFix (data-len size) (cl:mod data-len size)) 0)
           ;; Size divides evenly - try it
           (let ((count (lisp UFix (data-len size) (cl:floor data-len size))))
             (match (deserialize-vector-fixed-tc count size data)
               ((Ok vec) (Ok vec))
               ((Err _) (try-deserialize-with-sizes data data-len rest))))
           ;; Doesn't divide evenly - skip
           (try-deserialize-with-sizes data data-len rest)))))

  ;; Helper to deserialize fixed-size vector elements (type class version)
  (declare deserialize-vector-fixed-tc
           ((SSZDeserialize :a) => UFix -> UFix -> Bytes -> (SSZResult (SSZVector :a))))
  (define (deserialize-vector-fixed-tc count elem-size data)
    "Deserialize count fixed-size elements"
    (deserialize-vector-fixed-tc-inner count elem-size data 0 Nil))

  (declare deserialize-vector-fixed-tc-inner
           ((SSZDeserialize :a) => UFix -> UFix -> Bytes -> UFix -> (List :a) -> (SSZResult (SSZVector :a))))
  (define (deserialize-vector-fixed-tc-inner remaining elem-size data offset acc)
    "Helper to deserialize fixed-size vector elements"
    (if (== remaining 0)
        (Ok (SSZVector (list:reverse acc)))
        (let ((elem-bytes (bytes-slice offset elem-size data)))
          (match (ssz-deserialize elem-bytes)
            ((Err e) (Err e))
            ((Ok elem)
             (deserialize-vector-fixed-tc-inner
              (- remaining 1)
              elem-size
              data
              (+ offset elem-size)
              (Cons elem acc)))))))

  ;; Helper to deserialize variable-size vector elements (type class version)
  (declare deserialize-vector-variable-tc
           ((SSZDeserialize :a) => UFix -> Bytes -> (SSZResult (SSZVector :a))))
  (define (deserialize-vector-variable-tc elem-count data)
    "Deserialize variable-size vector using offsets"
    (if (== elem-count 0)
        (Ok (SSZVector Nil))
        (match (parse-variable-offsets elem-count data)
          ((Err e) (Err e))
          ((Ok offsets)
           (deserialize-variable-elements-tc offsets data (bytes-length data) Nil)))))

  (declare deserialize-variable-elements-tc
           ((SSZDeserialize :a) => (List UFix) -> Bytes -> UFix -> (List :a) -> (SSZResult (SSZVector :a))))
  (define (deserialize-variable-elements-tc offsets data total-len acc)
    "Deserialize variable-size elements using offset list"
    (match offsets
      ((Nil) (Ok (SSZVector (list:reverse acc))))
      ((Cons offset rest)
       (let ((end-offset (match rest
                           ((Nil) total-len)
                           ((Cons next-offset _) next-offset))))
         (if (> offset end-offset)
             (Err (DeserializationError "Invalid offset ordering"))
             (let ((elem-bytes (bytes-slice offset (- end-offset offset) data)))
               (match (ssz-deserialize elem-bytes)
                 ((Err e) (Err e))
                 ((Ok elem)
                  (deserialize-variable-elements-tc rest data total-len (Cons elem acc))))))))))

  ;; SSZList deserialization
  ;; Note: max-length cannot be inferred from data, so we use a large default
  (define-instance ((SSZSerialize :a) (SSZDeserialize :a) => (SSZDeserialize (SSZList :a)))
    (define (ssz-deserialize data)
      "Deserialize a list - infers element count from data, uses default max-length"
      ;; First deserialize as a vector, then wrap as list
      (match (ssz-deserialize data)
        ((Err e) (Err e))
        ((Ok (SSZVector elements))
         ;; Use a very large max-length since we can't infer it from data
         ;; The actual count becomes a reasonable upper bound
         (let ((count (lisp UFix (elements) (cl:length elements))))
           (Ok (SSZList (max count 1024) elements)))))))

  ;;; =========================================================================
  ;;; SSZUnion type class instances
  ;;; =========================================================================

  ;; SSZUnion serialization
  (define-instance (SSZSerialize SSZUnion)
    (define ssz-serialize serialize-union)

    (define (ssz-fixed-size? _)
      "Unions are always variable-size"
      False)

    (define (ssz-fixed-size _)
      "Not applicable for variable-size type"
      0))

  ;; SSZUnion deserialization
  (define-instance (SSZDeserialize SSZUnion)
    (define ssz-deserialize deserialize-union))

  ;; SSZUnion hash tree root
  (define-instance (SSZHashTreeRoot SSZUnion)
    (define (hash-tree-root union)
      "Hash tree root of union: mix_in_selector(hash_tree_root(value), selector)"
      (let ((selector (ssz-union-selector union)))
        (if (== selector 0)
            ;; None: mix_in_selector(zero_chunk, 0)
            (mix-in-selector (zero-chunk) 0)
            ;; Some: mix_in_selector(merkleize(pack(value)), selector)
            (match (ssz-union-value union)
              ((None) (mix-in-selector (zero-chunk) selector))
              ((Some value-bytes)
               (mix-in-selector (merkleize (pack value-bytes)) selector)))))))

  ;; Helper to create union from typed value (requires SSZSerialize)
  (declare ssz-union-from-value
           ((SSZSerialize :a) => U8 -> :a -> (SSZResult SSZUnion)))
  (define (ssz-union-from-value selector value)
    "Create a union from a selector and a value (serializes the value)"
    (if (== selector 0)
        (Err (SerializationError "Selector 0 is reserved for None"))
        (if (> selector 127)
            (Err (SerializationError "Selector must be 0-127"))
            (Ok (SSZUnion selector (Some (ssz-serialize value)))))))

  ;; Helper to extract typed value from union (requires SSZDeserialize)
  (declare deserialize-union-value
           ((SSZDeserialize :a) => SSZUnion -> (SSZResult (Optional :a))))
  (define (deserialize-union-value union)
    "Deserialize the value inside a union to its typed form"
    (match (ssz-union-value union)
      ((None) (Ok None))
      ((Some bytes)
       (match (ssz-deserialize bytes)
         ((Err e) (Err e))
         ((Ok val) (Ok (Some val))))))))
