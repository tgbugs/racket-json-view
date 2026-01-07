#lang racket/gui

(require mrlib/hierlist
         "json-hierlist-item-mixin.rkt"
         "node-data.rkt")

(provide json-hierlist%)

(define json-hierlist%
  (class hierarchical-list%
    (init-field [on-item-select values])
    (define node-cache (make-hash))
    (define unexpanded (make-hash))
    (field [root #f])
    
    (define/private (new-item-node parent)
      (send parent new-item json-hierlist-item-mixin))

    (define/private (new-list-node parent parent-path value kind style)
      (define node (send parent new-list json-hierlist-item-mixin))
      (define path (reverse (cons value parent-path)))
      (send* node
        (insert-styled-text style (~a " " value))
        (user-data (node-data kind value #f path)))
      (hash-set! node-cache path node)
      node)

    (define/private (atom? value)
      (not (or (hash? value) (list? value))))

    (define/private (get-value-type value)
      (cond
        ((hash? value) 'hash)
        ((list? value) 'list)
        (else 'value)))

    (define/private (create-key-value-tree parent parent-path key value kind style [once 0])
      (if (atom? value)
          (let ((node (new-item-node parent))
                (path (reverse (cons key parent-path))))
            (send* node
              (insert-styled-text style (~a " " key))
              (insert-styled-text 'index " : ")
              (insert-value value)
              (user-data (node-data kind key value path)))
            (hash-set! node-cache path node))
          (let ((node (new-list-node parent parent-path key kind style))
                (path (cons key parent-path)))
            (if (< once 1)
                (hash-set! unexpanded path value)
                (create-tree value node path (sub1 once))))))

    (define/private (create-tree jsexpr parent path [once 1])
      (cond
        ((hash? jsexpr)
         (for (((key value) (in-hash jsexpr)))
              (create-key-value-tree parent path key value (get-value-type value) 'key once)))
        ((list? jsexpr)
         (for (((value index) (in-indexed jsexpr)))
              (create-key-value-tree parent path index value (get-value-type value) 'index once)))
        (else
         (let ((node (new-item-node parent))
               (path (reverse (cons jsexpr path))))
           (send* node
             (insert-value jsexpr)
             (user-data (node-data 'value jsexpr jsexpr path)))
           (hash-set! node-cache path node)))))

    (define/override (on-item-opened item)
      (let ([items (send item get-items)])
        (when (and (memq (node-data-type (send item user-data)) '(hash list))
                   (null? items))
          (let* ([nd (send item user-data)]
                 [ndp (reverse (node-data-path nd))]
                 [hr (hash-ref unexpanded ndp #f)])
            (when hr
              (create-tree hr item ndp))))))

    (define/override (on-select item)
      (when item
        (on-item-select (send item user-data))))

    (define/private (get-json-helper node)
      (define data (send node user-data))
      (define type (node-data-type data))
      (case type
        ((hash)
         (for/hasheq ((item (send node get-items)))
                     (values
                      (node-data-name (send item user-data))
                      (get-json-helper item))))
        ((list)
         (for/list ((item (send node get-items)))
                   (get-json-helper item)))
        ((value)
         (node-data-value
          (if (eq? node root) ; hack to handle the case where the tree is a single value
              (send (car (send node get-items)) user-data)
              data)))))

    (define/public (get-json)
      (unless root
        (error "no JSON loaded"))
      (get-json-helper root))

    (define/public (set-json! jsexpr)
      (set! root (new-list-node this '() "object" (get-value-type jsexpr) 'index))
      (create-tree jsexpr root '("object")))

    (define/public (select-path path)
      (define node (hash-ref node-cache path))
      (send this select node))

    (super-new)))
