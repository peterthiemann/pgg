;;; ,config

(define-interface expander-interface
  (export expand-form
	  expand-stuff
	  expand
	  make-usage
	  usage-reference-count
	  usage-operator-count
	  usage-assignment-count
	  note-reference!
	  note-operator!
	  note-assignment!
	  free-top-level-variables))

(define-structure expander expander-interface
  (open scheme-level-2
	syntactic packages scan meta-types reconstruction
	define-record-types
	util signals tables fluids strong
	features)  ; string-hash
  (files ("../../../soft/scheme48-0.44/scheme/opt" expand)
         ("../../../soft/scheme48-0.44/scheme/opt" sort)))

(define-structure flattener (export flatten-form)
  (open scheme syntactic packages scan expander)
  (files ("../../../soft/scheme48-0.44/scheme/opt" flatten)))

;;; ,user
;;; ,load-package flattener
;;; ,open scan
;;; (set-standard-optimizers! 'flat-environments)
