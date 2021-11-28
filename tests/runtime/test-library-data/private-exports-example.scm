(define-library* (test amap)
  (import (scheme base))
  (export alist->amap
          amap->alist
	  amap-args
          amap-clean!
          amap-clear!
	  amap-comparator
          amap-contains?
          amap-copy
          amap-count
          amap-delete!
          amap-difference!
          amap-empty-copy
          amap-empty?
          amap-entries
          amap-find
          amap-fold
          amap-for-each
	  amap-implementation-name
          amap-intern!
          amap-intersection!
          amap-keys
          amap-map
          amap-map!
          amap-map->list
          amap-mutable?
          amap-pop!
          amap-prune!
          amap-ref
          amap-ref/default
          amap-set!
          amap-size
          amap-unfold
          amap-union!
          amap-update!
          amap-update!/default
          amap-values
          amap-xor!
          amap=?
          amap?
          make-amap)
  (begin
    (define alist->amap)
    (define amap->alist)
    (define amap-args)
    (define amap-clean!)
    (define amap-clear!)
    (define amap-comparator)
    (define amap-contains?)
    (define amap-copy)
    (define amap-count)
    (define amap-delete!)
    (define amap-difference!)
    (define amap-empty-copy)
    (define amap-empty?)
    (define amap-entries)
    (define amap-find)
    (define amap-fold)
    (define amap-for-each)
    (define amap-implementation-name)
    (define amap-intern!)
    (define amap-intersection!)
    (define amap-keys)
    (define amap-map)
    (define amap-map!)
    (define amap-map->list)
    (define amap-mutable?)
    (define amap-pop!)
    (define amap-prune!)
    (define amap-ref)
    (define amap-ref/default)
    (define amap-set!)
    (define amap-size)
    (define amap-unfold)
    (define amap-union!)
    (define amap-update!)
    (define amap-update!/default)
    (define amap-values)
    (define amap-xor!)
    (define amap=?)
    (define amap?)
    (define make-amap)))

(define-library* (test amap impl)
  (import (scheme base))
  (export all-amap-args
	  amap-implementation-names
	  amap-implementation-supported-args
	  amap-implementation-supports-args?
	  amap-implementation-supports-comparator?
          define-amap-implementation
          define-amap-implementation-selector
	  make-amap-implementation)
  (export-to (test amap)
             select-impl)
  (begin
    (define all-amap-args)
    (define amap-implementation-names)
    (define amap-implementation-supported-args)
    (define amap-implementation-supports-args?)
    (define amap-implementation-supports-comparator?)
    (define define-amap-implementation)
    (define define-amap-implementation-selector)
    (define make-amap-implementation)
    (define select-impl)))