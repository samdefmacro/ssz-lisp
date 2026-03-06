(in-package #:ssz)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;; SSZ Container Support
  ;;;
  ;;; Containers are heterogeneous structures with fields that can be
  ;;; fixed-size or variable-size. Serialization uses offset encoding
  ;;; for variable-size fields.
  ;;;
  ;;; Since Coalton has statically typed structs, users define their own
  ;;; container types as structs and implement the SSZ type classes.
  ;;; This module provides helper functions for building container
  ;;; serialization.

  ;;; =========================================================================
  ;;; Field Descriptor - describes a field for serialization
  ;;; =========================================================================

  ;; A field can be either fixed-size (with known size) or variable-size
  (define-type FieldType
    "Describes whether a field is fixed-size or variable-size"
    (FixedField UFix)      ; Fixed-size field with byte size
    (VariableField))       ; Variable-size field (uses offset encoding)

  ;; Serialized field value (bytes) along with its type info
  (define-type SerializedField
    "A serialized field value with its type information"
    (SerializedField FieldType Bytes))

  ;;; =========================================================================
  ;;; Container Serialization Helpers
  ;;; =========================================================================

  ;; Serialize a container given its serialized fields
  (declare serialize-container ((List SerializedField) -> Bytes))
  (define (serialize-container fields)
    "Serialize a container from a list of serialized fields."
    (lisp Bytes (fields)
      ;; Helper to extract ftype and data from a SerializedField struct
      ;; Coalton types are named FieldType/FixedField and FieldType/VariableField
      (cl:flet ((sf-ftype (sf) (cl:slot-value sf '_0))
                (sf-data (sf) (cl:slot-value sf '_1))
                (is-fixed-field (ft) (cl:typep ft 'ssz::FieldType/FixedField))
                (fixed-field-size (ft) (cl:slot-value ft '_0)))
        (cl:let* ((fixed-section-size
                   (cl:loop :for sf :in fields
                            :for ftype := (sf-ftype sf)
                            :for data := (sf-data sf)
                            :sum (cl:if (is-fixed-field ftype)
                                        (cl:length data)
                                        4)))  ; VariableField gets 4-byte offset
                  (result (cl:make-array 0 :fill-pointer 0 :adjustable cl:t))
                  (variable-parts cl:nil)
                  (current-offset fixed-section-size))
          ;; First pass: write fixed parts and collect offsets for variable parts
          (cl:loop :for sf :in fields
                   :for ftype := (sf-ftype sf)
                   :for data := (sf-data sf)
                   :do (cl:if (is-fixed-field ftype)
                              ;; Write fixed field directly
                              (cl:loop :for b :across data :do (cl:vector-push-extend b result))
                              ;; VariableField: write offset to variable data
                              (cl:progn
                                (cl:loop :for i :from 0 :below 4
                                         :do (cl:vector-push-extend (cl:ldb (cl:byte 8 (cl:* i 8)) current-offset) result))
                                ;; Store variable data for later
                                (cl:push data variable-parts)
                                ;; Update offset
                                (cl:incf current-offset (cl:length data)))))
          ;; Second pass: append all variable parts
          (cl:loop :for data :in (cl:nreverse variable-parts)
                   :do (cl:loop :for b :across data :do (cl:vector-push-extend b result)))
          result))))

  ;; Check if a container is fixed-size
  (declare container-fixed-size? ((List FieldType) -> Boolean))
  (define (container-fixed-size? field-types)
    "A container is fixed-size if all its fields are fixed-size"
    (lisp Boolean (field-types)
      (cl:every (cl:lambda (ft) (cl:typep ft 'ssz::FieldType/FixedField)) field-types)))

  ;; Get fixed size of container (only valid if container-fixed-size? is True)
  (declare container-fixed-size ((List FieldType) -> UFix))
  (define (container-fixed-size field-types)
    "Get the fixed size of a container (sum of all field sizes)"
    (fold (fn (acc ft)
            (match ft
              ((FixedField size) (+ acc size))
              ((VariableField) acc)))
          0
          field-types))

  ;;; =========================================================================
  ;;; Container Hash Tree Root Helpers
  ;;; =========================================================================

  ;; Compute hash tree root of container from field roots
  (declare hash-tree-root-container ((List Chunk) -> Chunk))
  (define (hash-tree-root-container field-roots)
    "Compute hash tree root of container from individual field hash tree roots"
    (merkleize field-roots))

  ;;; =========================================================================
  ;;; Convenience Functions for Common Patterns
  ;;; =========================================================================

  ;; Create a SerializedField for a fixed-size value
  (declare fixed-field (UFix -> Bytes -> SerializedField))
  (define (fixed-field size data)
    "Create a serialized fixed-size field"
    (SerializedField (FixedField size) data))

  ;; Create a SerializedField for a variable-size value
  (declare variable-field (Bytes -> SerializedField))
  (define (variable-field data)
    "Create a serialized variable-size field"
    (SerializedField VariableField data))

  ;; Helper to serialize a fixed-size field using type class
  (declare serialize-fixed-field (SSZSerialize :a => :a -> SerializedField))
  (define (serialize-fixed-field val)
    "Serialize a fixed-size field"
    (fixed-field (ssz-fixed-size val) (ssz-serialize val)))

  ;; Helper to serialize a variable-size field using type class
  (declare serialize-variable-field (SSZSerialize :a => :a -> SerializedField))
  (define (serialize-variable-field val)
    "Serialize a variable-size field"
    (variable-field (ssz-serialize val)))

  ;;; =========================================================================
  ;;; Container Deserialization Helpers
  ;;; =========================================================================

  ;; Result of deserializing a container: list of byte slices for each field
  (define-type-alias DeserializedFields (List Bytes))

  ;; Parse offsets from the fixed section
  (declare parse-container-offsets
           ((List FieldType) -> Bytes -> (SSZResult (Tuple (List Bytes) (List U32)))))
  (define (parse-container-offsets field-types data)
    "Parse fixed fields and collect offsets from container data.
     Returns (fixed-field-bytes, variable-field-offsets)"
    (lisp (SSZResult (Tuple (List Bytes) (List U32))) (field-types data)
      (cl:flet ((is-fixed-field (ft) (cl:typep ft 'ssz::FieldType/FixedField))
                (fixed-field-size (ft) (cl:slot-value ft '_0)))
        (cl:let ((pos 0)
                 (fixed-parts cl:nil)
                 (offsets cl:nil)
                 (data-len (cl:length data)))
          (cl:handler-case
              (cl:progn
                (cl:loop :for ft :in field-types
                         :do (cl:if (is-fixed-field ft)
                                    ;; FixedField
                                    (cl:let ((size (fixed-field-size ft)))
                                      (cl:when (cl:> (cl:+ pos size) data-len)
                                        (cl:error "Not enough data for fixed field"))
                                      (cl:let ((field-data (cl:make-array size :fill-pointer size :adjustable cl:t)))
                                        (cl:loop :for i :from 0 :below size
                                                 :do (cl:setf (cl:aref field-data i) (cl:aref data (cl:+ pos i))))
                                        (cl:push field-data fixed-parts))
                                      (cl:incf pos size))
                                    ;; VariableField: Read 4-byte offset
                                    (cl:progn
                                      (cl:when (cl:> (cl:+ pos 4) data-len)
                                        (cl:error "Not enough data for offset"))
                                      (cl:let ((offset (cl:logior
                                                        (cl:aref data pos)
                                                        (cl:ash (cl:aref data (cl:+ pos 1)) 8)
                                                        (cl:ash (cl:aref data (cl:+ pos 2)) 16)
                                                        (cl:ash (cl:aref data (cl:+ pos 3)) 24))))
                                        (cl:push offset offsets)
                                        (cl:push cl:nil fixed-parts))  ; Placeholder for variable field
                                      (cl:incf pos 4))))
                (coalton-prelude:Ok
                 (coalton-prelude:Tuple (cl:nreverse fixed-parts) (cl:nreverse offsets))))
            (cl:error (e)
              (coalton-prelude:Err (ssz:DeserializationError (cl:format cl:nil "~A" e)))))))))

  ;; Extract variable-size field data using offsets
  (declare extract-variable-fields
           ((List U32) -> UFix -> Bytes -> (SSZResult (List Bytes))))
  (define (extract-variable-fields offsets total-len data)
    "Extract variable-size fields from data using offsets.
     offsets: list of starting offsets for variable fields
     total-len: total length of the data
     data: the full serialized data"
    (lisp (SSZResult (List Bytes)) (offsets total-len data)
      (cl:let ((results cl:nil)
               (num-offsets (cl:length offsets)))
        (cl:handler-case
            (cl:progn
              (cl:loop :for i :from 0 :below num-offsets
                       :for start := (cl:nth i offsets)
                       :for end := (cl:if (cl:< i (cl:1- num-offsets))
                                          (cl:nth (cl:1+ i) offsets)
                                          total-len)
                       :do (cl:progn
                             ;; Validate offset ordering
                             (cl:when (cl:> start end)
                               (cl:error "Offsets out of order"))
                             (cl:when (cl:> end total-len)
                               (cl:error "Offset exceeds data length"))
                             (cl:let* ((size (cl:- end start))
                                       (field-data (cl:make-array size :fill-pointer size :adjustable cl:t)))
                               (cl:loop :for j :from 0 :below size
                                        :do (cl:setf (cl:aref field-data j)
                                                     (cl:aref data (cl:+ start j))))
                               (cl:push field-data results))))
              (coalton-prelude:Ok (cl:nreverse results)))
          (cl:error (e)
            (coalton-prelude:Err (ssz:DeserializationError (cl:format cl:nil "~A" e))))))))

  ;; Helper to merge fixed and variable field data (defined before deserialize-container)
  (declare merge-fixed-variable-fields
           ((List FieldType) -> (List Bytes) -> (List Bytes) -> (List Bytes)))
  (define (merge-fixed-variable-fields field-types fixed-parts var-parts)
    "Merge fixed field data with variable field data based on field types"
    (lisp (List Bytes) (field-types fixed-parts var-parts)
      (cl:flet ((is-fixed-field (ft) (cl:typep ft 'ssz::FieldType/FixedField)))
        (cl:let ((result cl:nil)
                 (var-idx 0))
          (cl:loop :for ft :in field-types
                   :for fixed-data :in fixed-parts
                   :do (cl:if (is-fixed-field ft)
                              (cl:push fixed-data result)
                              (cl:progn
                                (cl:push (cl:nth var-idx var-parts) result)
                                (cl:incf var-idx))))
          (cl:nreverse result)))))

  ;; Main container deserialization function
  (declare deserialize-container
           ((List FieldType) -> Bytes -> (SSZResult DeserializedFields)))
  (define (deserialize-container field-types data)
    "Deserialize a container into a list of byte slices for each field.

     The field-types list describes each field:
     - (FixedField n): Fixed-size field of n bytes
     - VariableField: Variable-size field (offset encoded)

     Returns a list of Bytes, one for each field in order.
     For fixed-size fields, the bytes are the field data.
     For variable-size fields, the bytes are the field data (without offset).

     The caller is responsible for deserializing each field's bytes
     using the appropriate type-specific deserializer."
    (match (parse-container-offsets field-types data)
      ((Err e) (Err e))
      ((Ok (Tuple fixed-parts offsets))
       (match offsets
         ((Nil)
          ;; All fixed fields, just return them
          (Ok fixed-parts))
         (_
          ;; Has variable fields, need to extract them
          (match (extract-variable-fields offsets (bytes-length data) data)
            ((Err e) (Err e))
            ((Ok var-parts)
             ;; Merge fixed and variable parts
             (Ok (merge-fixed-variable-fields field-types fixed-parts var-parts)))))))))

  ;; Convenience: get nth field from deserialized fields
  (declare get-field (UFix -> DeserializedFields -> (Optional Bytes)))
  (define (get-field idx fields)
    "Get the nth field from deserialized container fields"
    (lisp (Optional Bytes) (idx fields)
      (cl:if (cl:< idx (cl:length fields))
             (coalton-prelude:Some (cl:nth idx fields))
             coalton-prelude:None)))

  ;; Validate that offsets are well-formed
  (declare validate-container-offsets
           ((List U32) -> UFix -> UFix -> (SSZResult Unit)))
  (define (validate-container-offsets offsets fixed-section-size total-len)
    "Validate that container offsets are well-formed.
     - First offset must equal fixed-section-size
     - Offsets must be in non-decreasing order
     - Last offset must not exceed total-len"
    (lisp (SSZResult Unit) (offsets fixed-section-size total-len)
      (cl:if (cl:null offsets)
             (coalton-prelude:Ok coalton:Unit)
             (cl:let ((first-offset (cl:first offsets)))
               (cl:cond
                 ((cl:/= first-offset fixed-section-size)
                  (coalton-prelude:Err
                   (ssz:DeserializationError
                    (cl:format cl:nil "First offset ~A does not match fixed section size ~A"
                               first-offset fixed-section-size))))
                 (cl:t
                  (cl:let ((order-err
                            (cl:loop :for (a b) :on offsets
                                     :while b
                                     :when (cl:> a b)
                                     :do (cl:return
                                           (coalton-prelude:Err
                                            (ssz:DeserializationError "Offsets are not in order"))))))
                    (cl:if order-err
                           order-err
                           (cl:let ((last-offset (cl:car (cl:last offsets))))
                             (cl:if (cl:> last-offset total-len)
                                    (coalton-prelude:Err
                                     (ssz:DeserializationError "Last offset exceeds data length"))
                                    (coalton-prelude:Ok coalton:Unit))))))))))))
