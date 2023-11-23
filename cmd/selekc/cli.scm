#!/usr/bin/guile3.0 \
-e main -s
!#
(use-modules (ice-9 regex)
             (ice-9 textual-ports))
(define (kubeconfig? str)
  ;; test if the string contains KUBECONFIG
  ;; (kubeconfig? "KUBECONFIG_PROD=/path/to/thing")
  ;; KUBECONFIG_PROD=/path/to/thing
  (if (string-match "KUBECONFIG" str)
      (let ((res str))
        res)
      #f))

;; TODO make a hash map 
(define (create-menu opts)
  (let ((menu-ls '()))
    (define (iter-create-menu opts pos res)
      ;; add a numeric prefix and display a list of options
      
      (if (null? opts)
          (menu-ls)
          (begin
            (assoc-set! menu-ls pos (car opts))
            (iter-create-menu (cdr opts) (+ pos 1) menu-ls))))
  (iter-create-menu opts 1 menu-ls)))


(define (main args)
   (map (lambda (arg) (display arg) (display " "))
       (cdr args))
  (newline)
  (let ([kconfig-opts (filter kubeconfig? (environ))])
    (if kconfig-opts
        (begin  ;; this is the main io loop
          (display-menu kconfig-opts)))
  (newline))

;; tests
(define test-env-kubeconfig
  (list "PATH=/k/y/z" "KUBECONFIG_PROD=/somewhere/config.yaml"))

(filter kubeconfig? '("KUBECONFIG_PROD" "NOTHING"))

(define t
  (let ((res (lambda ()(filter even? '(1 2 3 4)))))
    res))

(t)


;; scratch

(define getinput
  (let ((res (get-char (current-input-port))))
      (display res)))


alist
