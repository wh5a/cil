;; Based on "Debugging support" in the manual, http://manju.cs.berkeley.edu/cil/cil015.html
;; ALT-x cil-debug
;; cilly --ocamldebug -c hello.c
(defvar ocamldebug-history nil)
(defun cil-debug (command-line)
  "Run camldebug on program FILE in buffer *camldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for camldebug.  If you wish to change this, use
the camldebug commands `cd DIR' and `directory'."
  (interactive
   (list (read-from-minibuffer "Run ocamldebug (like this): "
                               (if (consp ocamldebug-history)
                                   (car ocamldebug-history)
                                 "cilly --ocamldebug ")
                               nil
                               nil
                               '(ocamldebug-history . 1))))
  (require 'camldebug)
  (camldebug-numeric-arg 1)
  (pop-to-buffer (concat "*camldebug*"))
  (setq words (split-string command-line)) 
  (message "Current directory is %s" default-directory)
  (apply 'make-comint (cons "camldebug"
                            (cons (car words)
                                  (cons nil (cdr words)))))
  (set-process-filter (get-buffer-process (current-buffer))
                      'camldebug-filter)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        'camldebug-sentinel)
  (camldebug-mode)
  (camldebug-set-buffer))

;; cil-debug is only useful when debugging CIL itself, but not when using CIL as a lib.

;; The following HACK doesn't work well
;; We send commands to the camldebug buffer to extend the source paths.
;; (defvar cil-dir "/home/wh5a/cil/")
;; (defvar cil-srcs '( "src" "src/frontc" "src/ext" "src/ext/pta"
;;                     "ocamlutil"
;;                     "obj/x86_LINUX"
;;                     "ext" ))
;; ;; Extend this variable for any paths you want to add
;; (defvar ocaml-srcs
;;   (mapcar (lambda (s)
;;             (concat cil-dir s)) cil-srcs))

;; (defun camldebug-add-dir ()
;;   (interactive)
;;   (mapc (lambda (s)
;;           (comint-send-string current-camldebug-buffer
;;                               (concat "directory " s "\n")))
;;         ocaml-srcs))

;; ;; Implement this with hook instead of advice
;; (defadvice camldebug (after camldebug-add-dir)
;;   "Add the paths stored in ocaml-srcs to ocamldebug's search path"
;;   (mapc (lambda (s)
;;           (comint-send-string current-camldebug-buffer
;;                               (concat "directory " s "\n")))
;;         ocaml-srcs))

;; ;(ad-activate 'camldebug)


;; This is a KISS method by just sourcing an external file.
(defvar camldebug-source "/home/wh5a/cil/tips/camldebug.cmd")
(defun camldebug-send-cmd ()
  "Source the commands."
  (interactive)
  (comint-send-string current-camldebug-buffer
                      (concat "source " camldebug-source "\n")))
;; Remove this hook if not working with CIL
(add-hook 'camldebug-mode-hook 'camldebug-send-cmd)
