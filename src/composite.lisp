(in-package #:ssz)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;; Composite SSZ Types
  ;;; Vector, List, Bitvector, Bitlist

  ;;; =========================================================================
  ;;; SSZ Vector - Fixed-length homogeneous sequence
  ;;; =========================================================================

  (define-type (SSZVector :a)
    "SSZ Vector - fixed-length sequence of elements"
    (SSZVector (List :a)))

  (declare ssz-vector-elements ((SSZVector :a) -> (List :a)))
  (define (ssz-vector-elements (SSZVector v))
    "Get the underlying list of elements"
    v)

  (declare ssz-vector-length ((SSZVector :a) -> UFix))
  (define (ssz-vector-length (SSZVector v))
    "Get the length of the vector"
    (lisp UFix (v) (cl:length v)))

  (declare ssz-vector-ref (UFix -> (SSZVector :a) -> (Optional :a)))
  (define (ssz-vector-ref idx (SSZVector v))
    "Get element at index"
    (lisp (Optional :a) (idx v)
      (cl:let ((result (cl:nth idx v)))
        (cl:if (cl:and result (cl:< idx (cl:length v)))
               (coalton-prelude:Some result)
               coalton-prelude:None))))

  (declare ssz-vector-from-list ((List :a) -> (SSZVector :a)))
  (define (ssz-vector-from-list lst)
    "Create SSZ vector from list"
    (SSZVector lst))

  ;;; =========================================================================
  ;;; SSZ List - Variable-length homogeneous sequence with max limit
  ;;; =========================================================================

  (define-type (SSZList :a)
    "SSZ List - variable-length sequence with max limit"
    (SSZList UFix (List :a)))  ; (max-length, elements)

  (declare ssz-list-max-length ((SSZList :a) -> UFix))
  (define (ssz-list-max-length (SSZList max _))
    "Get the maximum allowed length"
    max)

  (declare ssz-list-elements ((SSZList :a) -> (List :a)))
  (define (ssz-list-elements (SSZList _ v))
    "Get the underlying list of elements"
    v)

  (declare ssz-list-length ((SSZList :a) -> UFix))
  (define (ssz-list-length (SSZList _ v))
    "Get the current length"
    (lisp UFix (v) (cl:length v)))

  (declare ssz-list-ref (UFix -> (SSZList :a) -> (Optional :a)))
  (define (ssz-list-ref idx (SSZList _ v))
    "Get element at index"
    (lisp (Optional :a) (idx v)
      (cl:if (cl:< idx (cl:length v))
             (coalton-prelude:Some (cl:nth idx v))
             coalton-prelude:None)))

  (declare ssz-list-from-list (UFix -> (List :a) -> (SSZResult (SSZList :a))))
  (define (ssz-list-from-list max-len lst)
    "Create SSZ list from list with max length"
    (if (> (lisp UFix (lst) (cl:length lst)) max-len)
        (Err (SerializationError "List exceeds maximum length"))
        (Ok (SSZList max-len lst))))

  ;;; =========================================================================
  ;;; SSZ Bitvector - Fixed-length bit sequence
  ;;; =========================================================================

  (define-type SSZBitvector
    "SSZ Bitvector - fixed-length bit sequence"
    (SSZBitvector UFix (List Boolean)))  ; (num-bits, bits)

  (declare ssz-bitvector-length (SSZBitvector -> UFix))
  (define (ssz-bitvector-length (SSZBitvector n _))
    "Get the number of bits"
    n)

  (declare ssz-bitvector-bits (SSZBitvector -> (List Boolean)))
  (define (ssz-bitvector-bits (SSZBitvector _ v))
    "Get the underlying bits"
    v)

  (declare ssz-bitvector-ref (UFix -> SSZBitvector -> (Optional Boolean)))
  (define (ssz-bitvector-ref idx (SSZBitvector _ v))
    "Get bit at index"
    (lisp (Optional Boolean) (idx v)
      (cl:if (cl:< idx (cl:length v))
             (coalton-prelude:Some (cl:nth idx v))
             coalton-prelude:None)))

  (declare ssz-bitvector-from-list (UFix -> (List Boolean) -> (SSZResult SSZBitvector)))
  (define (ssz-bitvector-from-list n bits)
    "Create bitvector from boolean list"
    (if (/= (lisp UFix (bits) (cl:length bits)) n)
        (Err (SerializationError "Bitvector length mismatch"))
        (Ok (SSZBitvector n bits))))

  (declare ssz-bitvector-zeros (UFix -> SSZBitvector))
  (define (ssz-bitvector-zeros n)
    "Create a bitvector of n zeros"
    (SSZBitvector n (lisp (List Boolean) (n)
                      (cl:make-list n :initial-element cl:nil))))

  ;;; =========================================================================
  ;;; SSZ Bitlist - Variable-length bit sequence with max limit
  ;;; =========================================================================

  (define-type SSZBitlist
    "SSZ Bitlist - variable-length bit sequence with max limit"
    (SSZBitlist UFix (List Boolean)))  ; (max-bits, bits)

  (declare ssz-bitlist-max-length (SSZBitlist -> UFix))
  (define (ssz-bitlist-max-length (SSZBitlist max _))
    "Get maximum allowed bits"
    max)

  (declare ssz-bitlist-length (SSZBitlist -> UFix))
  (define (ssz-bitlist-length (SSZBitlist _ v))
    "Get current number of bits"
    (lisp UFix (v) (cl:length v)))

  (declare ssz-bitlist-bits (SSZBitlist -> (List Boolean)))
  (define (ssz-bitlist-bits (SSZBitlist _ v))
    "Get the underlying bits"
    v)

  (declare ssz-bitlist-ref (UFix -> SSZBitlist -> (Optional Boolean)))
  (define (ssz-bitlist-ref idx (SSZBitlist _ v))
    "Get bit at index"
    (lisp (Optional Boolean) (idx v)
      (cl:if (cl:< idx (cl:length v))
             (coalton-prelude:Some (cl:nth idx v))
             coalton-prelude:None)))

  (declare ssz-bitlist-from-list (UFix -> (List Boolean) -> (SSZResult SSZBitlist)))
  (define (ssz-bitlist-from-list max-bits bits)
    "Create bitlist from boolean list with max length"
    (if (> (lisp UFix (bits) (cl:length bits)) max-bits)
        (Err (SerializationError "Bitlist exceeds maximum length"))
        (Ok (SSZBitlist max-bits bits))))

  (declare ssz-bitlist-empty (UFix -> SSZBitlist))
  (define (ssz-bitlist-empty max-bits)
    "Create an empty bitlist with given max length"
    (SSZBitlist max-bits Nil))

  ;;; =========================================================================
  ;;; Serialization for composite types
  ;;; =========================================================================

  ;; Serialize Bitvector
  (declare serialize-bitvector (SSZBitvector -> Bytes))
  (define (serialize-bitvector (SSZBitvector n bits))
    "Serialize a bitvector to bytes"
    (lisp Bytes (n bits)
      (cl:let* ((num-bytes (cl:ceiling n 8))
                (result (cl:make-array num-bytes :fill-pointer num-bytes :adjustable cl:t :initial-element 0)))
        (cl:loop :for bit :in bits
                 :for i :from 0
                 :when bit
                 :do (cl:let ((byte-idx (cl:floor i 8))
                              (bit-idx (cl:mod i 8)))
                       (cl:setf (cl:aref result byte-idx)
                                (cl:logior (cl:aref result byte-idx)
                                           (cl:ash 1 bit-idx)))))
        result)))

  ;; Serialize Bitlist (includes length marker bit)
  (declare serialize-bitlist (SSZBitlist -> Bytes))
  (define (serialize-bitlist (SSZBitlist _ bits))
    "Serialize a bitlist to bytes (with length marker bit)"
    (lisp Bytes (bits)
      (cl:let* ((n (cl:length bits))
                (num-bytes (cl:1+ (cl:floor n 8)))
                (result (cl:make-array num-bytes :fill-pointer num-bytes :adjustable cl:t :initial-element 0)))
        ;; Set all the data bits
        (cl:loop :for bit :in bits
                 :for i :from 0
                 :when bit
                 :do (cl:let ((byte-idx (cl:floor i 8))
                              (bit-idx (cl:mod i 8)))
                       (cl:setf (cl:aref result byte-idx)
                                (cl:logior (cl:aref result byte-idx)
                                           (cl:ash 1 bit-idx)))))
        ;; Set the length marker bit at position n
        (cl:let ((marker-byte-idx (cl:floor n 8))
                 (marker-bit-idx (cl:mod n 8)))
          (cl:setf (cl:aref result marker-byte-idx)
                   (cl:logior (cl:aref result marker-byte-idx)
                              (cl:ash 1 marker-bit-idx))))
        result)))

  ;;; =========================================================================
  ;;; Deserialization for composite types
  ;;; =========================================================================

  ;; Deserialize Bitvector
  (declare deserialize-bitvector (UFix -> Bytes -> (SSZResult SSZBitvector)))
  (define (deserialize-bitvector n data)
    "Deserialize a bitvector from bytes"
    (lisp (SSZResult SSZBitvector) (n data)
      (cl:let ((expected-bytes (cl:ceiling n 8)))
        (cl:if (cl:/= (cl:length data) expected-bytes)
               (coalton-prelude:Err (ssz:DeserializationError "Invalid bitvector byte length"))
               ;; Check that unused bits are zero
               (cl:let ((unused-bits (cl:- (cl:* expected-bytes 8) n)))
                 (cl:if (cl:and (cl:> unused-bits 0)
                                (cl:/= (cl:logand (cl:aref data (cl:1- expected-bytes))
                                                  (cl:ash #xFF (cl:- 8 unused-bits)))
                                       0))
                        (coalton-prelude:Err (ssz:DeserializationError "Non-zero padding bits in bitvector"))
                        ;; Extract bits
                        (cl:let ((bits cl:nil))
                          (cl:loop :for i :from (cl:1- n) :downto 0
                                   :for byte-idx := (cl:floor i 8)
                                   :for bit-idx := (cl:mod i 8)
                                   :do (cl:push (cl:logbitp bit-idx (cl:aref data byte-idx)) bits))
                          (coalton-prelude:Ok (ssz:SSZBitvector n bits)))))))))

  ;; Deserialize Bitlist
  (declare deserialize-bitlist (UFix -> Bytes -> (SSZResult SSZBitlist)))
  (define (deserialize-bitlist max-bits data)
    "Deserialize a bitlist from bytes"
    (lisp (SSZResult SSZBitlist) (max-bits data)
      (cl:let ((len (cl:length data)))
        (cl:if (cl:= len 0)
               (coalton-prelude:Err (ssz:DeserializationError "Empty bitlist data"))
               (cl:let ((last-byte (cl:aref data (cl:1- len))))
                 (cl:if (cl:= last-byte 0)
                        (coalton-prelude:Err (ssz:DeserializationError "Invalid bitlist: no marker bit"))
                        (cl:let* ((marker-bit-pos (cl:1- (cl:integer-length last-byte)))
                                  (num-bits (cl:+ (cl:* (cl:1- len) 8) marker-bit-pos)))
                          (cl:if (cl:> num-bits max-bits)
                                 (coalton-prelude:Err (ssz:DeserializationError "Bitlist exceeds max length"))
                                 ;; Extract bits
                                 (cl:let ((bits cl:nil))
                                   (cl:loop :for i :from (cl:1- num-bits) :downto 0
                                            :for byte-idx := (cl:floor i 8)
                                            :for bit-idx := (cl:mod i 8)
                                            :do (cl:push (cl:logbitp bit-idx (cl:aref data byte-idx)) bits))
                                   (coalton-prelude:Ok (ssz:SSZBitlist max-bits bits)))))))))))

  ;;; =========================================================================
  ;;; Hash tree root for composite types
  ;;; =========================================================================

  ;; Hash tree root for bitvector
  (declare hash-tree-root-bitvector (SSZBitvector -> Chunk))
  (define (hash-tree-root-bitvector bv)
    "Compute hash tree root of bitvector"
    (let ((limit (chunk-count-bitvector (ssz-bitvector-length bv))))
      (merkleize-with-limit (pack-bits (ssz-bitvector-bits bv)) (Some limit))))

  ;; Hash tree root for bitlist
  (declare hash-tree-root-bitlist (SSZBitlist -> Chunk))
  (define (hash-tree-root-bitlist bl)
    "Compute hash tree root of bitlist"
    (let ((limit (chunk-count-bitvector (ssz-bitlist-max-length bl)))
          (root (merkleize-with-limit (pack-bits (ssz-bitlist-bits bl)) (Some limit))))
      (mix-in-length root (ssz-bitlist-length bl))))

  ;;; =========================================================================
  ;;; Deserialization for Vector and List
  ;;; =========================================================================

  ;; Deserialize Vector of fixed-size elements
  (declare deserialize-vector-fixed
           (UFix -> UFix -> (Bytes -> (SSZResult :a)) -> Bytes -> (SSZResult (SSZVector :a))))
  (define (deserialize-vector-fixed elem-count elem-size deserialize-elem data)
    "Deserialize a vector of fixed-size elements"
    (let ((expected-size (* elem-count elem-size)))
      (if (/= (bytes-length data) expected-size)
          (Err (DeserializationError "Invalid vector byte length"))
          (deserialize-vector-fixed-inner elem-count elem-size deserialize-elem data 0 Nil))))

  (declare deserialize-vector-fixed-inner
           (UFix -> UFix -> (Bytes -> (SSZResult :a)) -> Bytes -> UFix -> (List :a) -> (SSZResult (SSZVector :a))))
  (define (deserialize-vector-fixed-inner remaining elem-size deserialize-elem data offset acc)
    "Helper to deserialize fixed-size vector elements"
    (if (== remaining 0)
        (Ok (SSZVector (list:reverse acc)))
        (let ((elem-bytes (bytes-slice offset elem-size data)))
          (match (deserialize-elem elem-bytes)
            ((Err e) (Err e))
            ((Ok elem)
             (deserialize-vector-fixed-inner
              (- remaining 1)
              elem-size
              deserialize-elem
              data
              (+ offset elem-size)
              (Cons elem acc)))))))

  ;; Deserialize List of fixed-size elements
  (declare deserialize-list-fixed
           (UFix -> UFix -> (Bytes -> (SSZResult :a)) -> Bytes -> (SSZResult (SSZList :a))))
  (define (deserialize-list-fixed max-len elem-size deserialize-elem data)
    "Deserialize a list of fixed-size elements"
    (let ((data-len (bytes-length data)))
      (if (== data-len 0)
          ;; Empty list
          (Ok (SSZList max-len Nil))
          (if (/= (lisp UFix (data-len elem-size) (cl:mod data-len elem-size)) 0)
              (Err (DeserializationError "List length not multiple of element size"))
              (let ((elem-count (lisp UFix (data-len elem-size) (cl:floor data-len elem-size))))
                (if (> elem-count max-len)
                    (Err (DeserializationError "List exceeds maximum length"))
                    (match (deserialize-vector-fixed elem-count elem-size deserialize-elem data)
                      ((Err e) (Err e))
                      ((Ok (SSZVector elements))
                       (Ok (SSZList max-len elements))))))))))

  ;; Deserialize Vector of variable-size elements
  (declare deserialize-vector-variable
           (UFix -> (Bytes -> (SSZResult :a)) -> Bytes -> (SSZResult (SSZVector :a))))
  (define (deserialize-vector-variable elem-count deserialize-elem data)
    "Deserialize a vector of variable-size elements using offsets"
    (if (== elem-count 0)
        (if (== (bytes-length data) 0)
            (Ok (SSZVector Nil))
            (Err (DeserializationError "Non-empty data for zero-element vector")))
        ;; Parse offsets
        (let ((offsets-result (parse-variable-offsets elem-count data)))
          (match offsets-result
            ((Err e) (Err e))
            ((Ok offsets)
             ;; Extract and deserialize each element
             (deserialize-variable-elements offsets deserialize-elem data (bytes-length data) Nil))))))

  ;; Parse offsets from beginning of data
  (declare parse-variable-offsets (UFix -> Bytes -> (SSZResult (List UFix))))
  (define (parse-variable-offsets count data)
    "Parse count 4-byte offsets from beginning of data"
    (let ((offset-bytes (* count 4)))
      (if (< (bytes-length data) offset-bytes)
          (Err (DeserializationError "Data too short for offsets"))
          (parse-variable-offsets-inner count data 0 Nil))))

  (declare parse-variable-offsets-inner (UFix -> Bytes -> UFix -> (List UFix) -> (SSZResult (List UFix))))
  (define (parse-variable-offsets-inner remaining data pos acc)
    "Helper to parse offset values"
    (if (== remaining 0)
        (Ok (list:reverse acc))
        (match (deserialize-u32 (bytes-slice pos 4 data))
          ((Err e) (Err e))
          ((Ok offset-val)
           (parse-variable-offsets-inner
            (- remaining 1)
            data
            (+ pos 4)
            (Cons (lisp UFix (offset-val) offset-val) acc))))))

  ;; Deserialize elements using offsets
  (declare deserialize-variable-elements
           ((List UFix) -> (Bytes -> (SSZResult :a)) -> Bytes -> UFix -> (List :a) -> (SSZResult (SSZVector :a))))
  (define (deserialize-variable-elements offsets deserialize-elem data total-len acc)
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
               (match (deserialize-elem elem-bytes)
                 ((Err e) (Err e))
                 ((Ok elem)
                  (deserialize-variable-elements rest deserialize-elem data total-len (Cons elem acc))))))))))

  ;; Deserialize List of variable-size elements
  (declare deserialize-list-variable
           (UFix -> (Bytes -> (SSZResult :a)) -> Bytes -> (SSZResult (SSZList :a))))
  (define (deserialize-list-variable max-len deserialize-elem data)
    "Deserialize a list of variable-size elements"
    (if (== (bytes-length data) 0)
        ;; Empty list
        (Ok (SSZList max-len Nil))
        ;; First, we need to determine element count from first offset
        (if (< (bytes-length data) 4)
            (Err (DeserializationError "Data too short for offset"))
            (match (deserialize-u32 (bytes-slice 0 4 data))
              ((Err e) (Err e))
              ((Ok first-offset)
               (let ((first-offset-ufix (lisp UFix (first-offset) first-offset)))
                 (if (== (lisp UFix (first-offset-ufix) (cl:mod first-offset-ufix 4)) 0)
                     (let ((elem-count (lisp UFix (first-offset-ufix) (cl:floor first-offset-ufix 4))))
                       (if (> elem-count max-len)
                           (Err (DeserializationError "List exceeds maximum length"))
                           (match (deserialize-vector-variable elem-count deserialize-elem data)
                             ((Err e) (Err e))
                             ((Ok (SSZVector elements))
                              (Ok (SSZList max-len elements))))))
                     (Err (DeserializationError "Invalid first offset - not aligned")))))))))

  ;;; =========================================================================
  ;;; SSZ Union - Sum type with selector
  ;;; =========================================================================

  ;; SSZ Union stores a selector (0-127) and an optional serialized value
  ;; Selector 0 with None = None type
  ;; Selector N (N > 0) with Some bytes = value of type at index N
  (define-type SSZUnion
    "SSZ Union - tagged union with selector byte"
    (SSZUnion U8 (Optional Bytes)))  ; (selector, optional-serialized-value)

  (declare ssz-union-selector (SSZUnion -> U8))
  (define (ssz-union-selector (SSZUnion sel _))
    "Get the selector (type index) of the union"
    sel)

  (declare ssz-union-value (SSZUnion -> (Optional Bytes)))
  (define (ssz-union-value (SSZUnion _ val))
    "Get the serialized value (None if selector is 0)"
    val)

  (declare ssz-union-none (Unit -> SSZUnion))
  (define (ssz-union-none _)
    "Create a None union (selector 0, no value)"
    (SSZUnion 0 None))

  (declare ssz-union-is-none (SSZUnion -> Boolean))
  (define (ssz-union-is-none (SSZUnion sel _))
    "Check if union is None (selector 0)"
    (== sel 0))

  (declare ssz-union-some (U8 -> Bytes -> (SSZResult SSZUnion)))
  (define (ssz-union-some selector value)
    "Create a union with the given selector and serialized value"
    (cond
      ((== selector 0)
       (Err (SerializationError "Selector 0 is reserved for None - use ssz-union-none")))
      ((> selector 127)
       (Err (SerializationError "Selector must be 0-127")))
      (True
       (Ok (SSZUnion selector (Some value))))))

  ;;; Serialization for Union
  (declare serialize-union (SSZUnion -> Bytes))
  (define (serialize-union (SSZUnion selector maybe-value))
    "Serialize a union: selector byte followed by serialized value"
    (let ((selector-byte (serialize-u8 selector)))
      (match maybe-value
        ((None) selector-byte)  ; Just the selector for None
        ((Some value) (bytes-append selector-byte value)))))

  ;;; Deserialization for Union
  (declare deserialize-union (Bytes -> (SSZResult SSZUnion)))
  (define (deserialize-union data)
    "Deserialize a union from bytes"
    (if (== (bytes-length data) 0)
        (Err (DeserializationError "Empty union data"))
        (match (deserialize-u8 (bytes-slice 0 1 data))
          ((Err e) (Err e))
          ((Ok selector)
           (if (> selector 127)
               (Err (DeserializationError "Invalid union selector (must be 0-127)"))
               (if (== selector 0)
                   ;; None union - should have no additional data
                   (if (> (bytes-length data) 1)
                       (Err (DeserializationError "None union should not have data"))
                       (Ok (SSZUnion 0 None)))
                   ;; Some union - rest is the value
                   (let ((value-bytes (bytes-slice 1 (- (bytes-length data) 1) data)))
                     (Ok (SSZUnion selector (Some value-bytes)))))))))))