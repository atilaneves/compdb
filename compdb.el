;;; compdb.el --- Calls CMake to find out include paths and other compiler flags -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs "24.1"))
;; Keywords: languages
;; URL: http://github.com/atilaneves/compdb

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package runs CMake and sets variables for IDE-like functionality
;; provided by other packages such as:
;; On the fly syntax checks with flycheck
;; auto-completion using auto-complete-clang or company-clang
;; Jump to definition and refactoring with rtags
;; These other packages must be installed for the functionality to work

;;; Usage:

;; (compdb-setup)
;;
;; If compdb-flags-c or compdb-flags-c++ are set, they will be added to ac-clang-flags.
;; These variables should be set. Particularly, they should contain the system include paths.
;;
;;; Code:

(require 'json)

(declare-function rtags-call-rc "rtags")

(defvar compdb-flags-c
  nil
  "The C compiler flags to use.  Should have -I flags for system includes.")

(defvar compdb-flags-c++
  nil
  "The C++ compiler flags to use.  Should have -I flags for system includes.")

(defvar compdb-dir
  nil
  "The build directory to run CMake in.  If nil, runs in a temp dir.")

(defvar compdb-compile-command
  nil
  "The command to use to compile the project.  Can also include running tests.")

;;; The buffers to set variables for
(defvar compdb--src-buffers nil)
(defvar compdb--hdr-buffers nil)

(defcustom compdb-rdm-executable
  "rdm"
  "Location of rdm executable."
  :group 'rtags
  :type 'file)

(defcustom compdb-rc-executable
  "rc"
  "Location of rc executable."
  :group 'rtags
  :type 'file)

(defconst compdb-rdm-buffer-name "*rdm*" "The rdm buffer name.")

(defun compdb--mode-hook()
  "Function to add to a major mode hook"
  (add-hook 'find-file-hook #'compdb--maybe-run-cmake nil 'local)
  (when (and (featurep 'rtags) (compdb--locate-cmakelists))
    (compdb-maybe-start-rdm)))

;;;###autoload
(defun compdb-setup ()
  "Set up the Emacs hooks for working with CMake projects."
  (add-hook 'c-mode-hook #'compdb--mode-hook)
  (add-hook 'c++-mode-hook #'compdb--mode-hook)

  ;; When creating a file in Emacs, run CMake again to pick it up
  (add-hook 'before-save-hook (lambda ()
                                (when (and (compdb--is-src-file (buffer-file-name))
                                           (not (file-readable-p (buffer-file-name))))
                                  (add-hook 'after-save-hook 'compdb--new-file-saved nil 'local)))))

(defun compdb--new-file-saved ()
  "Run CMake to pick up newly created files."
  (compdb-run-cmake)
  (remove-hook 'after-save-hook 'compdb--new-file-saved 'local))

(defun compdb--maybe-run-cmake ()
  "Run CMake if the compilation database json file is not found."
  (if (compdb--need-to-run-cmake)
      (compdb-run-cmake)
    (progn
      (compdb--add-file-to-buffer-list)
      (compdb--on-cmake-finished))))

(defun compdb--add-file-to-buffer-list ()
  "Add buffer to the appropriate list for when CMake finishes running."
  (if (compdb--is-src-file buffer-file-name)
      (add-to-list 'compdb--src-buffers (current-buffer))
    (add-to-list 'compdb--hdr-buffers (current-buffer))))

(defun compdb--comp-db-file-name ()
  "The name of the compilation database file."
  (expand-file-name "compile_commands.json" (compdb--get-dir)))

(defun compdb--need-to-run-cmake ()
  "If CMake needs to be run or not."
  (and (not (get-process "cmake")) ; don't run if already running
       (not (file-exists-p (compdb--comp-db-file-name))))) ; no need if the file exists

;;;###autoload
(defun compdb-run-cmake ()
  "Run CMake and set compiler flags for auto-completion and flycheck.
This works by calling cmake in a temporary directory
and parsing the json file deposited there with the compiler
flags."
  (interactive)
  (when (file-readable-p (buffer-file-name)) ; new files need not apply
    (let ((project-dir (compdb--locate-cmakelists)))
      (when project-dir ; no point if it's not a CMake project
        ;; register this buffer to be either a header or source file
        ;; waiting for results
        (compdb--add-file-to-buffer-list)

        (let ((default-directory (compdb--get-dir)))
          (compdb--run-cmake-impl project-dir (compdb--get-dir))
          (compdb--register-callback))))))

(defun compdb--message (str &rest vars)
  "Output a message with STR and formatted by VARS."
  (message (apply #'format (concat "compdb: " str) vars)))

(defun compdb--register-callback ()
  "Register callback for when CMake finishes running."
  (set-process-sentinel (get-process "cmake")
                        (lambda (_process _event)
                          (compdb--message "Finished running CMake")
                          (compdb--on-cmake-finished))))

(defun compdb--on-cmake-finished ()
  "Set compiler flags for all buffers that requested it."
  (let* ((json (json-read-file (compdb--comp-db-file-name)))
         (set-flags (lambda (x) (compdb--set-flags-for-file json x))))
    (mapc set-flags compdb--src-buffers)
    (mapc set-flags compdb--hdr-buffers)
    (setq compdb--src-buffers nil compdb--hdr-buffers nil)
    (compdb--run-rc)))


(defun compdb--run-rc ()
  "Run rc to add definitions to the rtags daemon."
  (when (and (featurep 'rtags) (get-process "rdm"))
    (with-current-buffer (get-buffer compdb-rdm-buffer-name)
      (rtags-call-rc "-J" (compdb--get-dir)))))

(defun compdb--set-flags-for-file (json buffer)
  "Set the compiler flags from JSON for BUFFER visiting file FILE-NAME."
  (compdb--message "Setting flags for file %s" (buffer-file-name buffer))
  (let* ((file-params (compdb--file-params json (buffer-file-name buffer)))
         (commands (mapcar (lambda (x) (compdb--get-file-param 'command x)) json))
         (src-flags (compdb--params-to-src-flags file-params))
         (hdr-flags (compdb--commands-to-hdr-flags commands))
         (src-includes (compdb--params-to-src-includes file-params))
         (hdr-includes (compdb--commands-to-hdr-includes commands))
         (sys-includes (compdb--params-to-sys-includes file-params))
         )
    ;; set flags for all source files that registered
    (when src-flags (compdb-set-compiler-flags buffer src-flags src-includes sys-includes))
    (when hdr-flags (compdb-set-compiler-flags buffer hdr-flags hdr-includes sys-includes))))


(defun compdb-set-compiler-flags (buffer flags includes sys-includes)
  "Set ac-clang and flycheck variables for BUFFER from FLAGS, INCLUDES and SYS-INCLUDES."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer

      (when (featurep 'auto-complete-clang)
        (make-local-variable 'ac-clang-flags)
        (setq ac-clang-flags (compdb--get-compiler-flags flags)))

      (when (featurep 'company)
        (make-local-variable 'company-clang-arguments)
        (setq company-clang-arguments (compdb--get-compiler-flags flags)))

      (when (featurep 'flycheck)
        (make-local-variable 'flycheck-clang-include-path)
        (setq flycheck-clang-include-path (append sys-includes (compdb--flags-to-include-paths flags)))

        (make-local-variable 'flycheck-clang-definitions)
        (setq flycheck-clang-definitions
              (append (compdb--get-existing-definitions) (compdb--flags-to-defines flags)))

        (make-local-variable 'flycheck-cppcheck-include-path)
        (setq flycheck-cppcheck-include-path (append sys-includes (compdb--flags-to-include-paths flags)))

        (setq flycheck-clang-includes includes)
        (flycheck-clear)
        (run-at-time "0.5 sec" nil 'flycheck-buffer)))))

(defun compdb-delete-file ()
  "Remove file connected to current buffer and kill buffer, then run CMake."
  (interactive)
  (if compdb-dir
      (let ((filename (buffer-file-name))
            (buffer (current-buffer))
            (name (buffer-name)))
        (if (not (and filename (file-exists-p filename)))
            (error "Buffer '%s' is not visiting a file!" name)
          (when (yes-or-no-p "Are you sure you want to remove this file? ")
            (delete-file filename)
            (kill-buffer buffer)
            (let ((project-dir (compdb--locate-cmakelists)))
              (when project-dir (compdb--run-cmake-impl project-dir compdb-dir))
              (compdb--message "File '%s' successfully removed" filename)))))
    (error "Not possible to delete a file without setting compdb-dir")))


(defun compdb--run-cmake-impl (project-dir cmake-dir)
  "Run the CMake process for PROJECT-DIR in CMAKE-DIR."
  (when project-dir
    (let ((default-directory cmake-dir))
      (compdb--message "Running cmake for src path %s in build path %s" project-dir cmake-dir)
      (start-process "cmake" "*cmake*" "cmake" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-dir))))


(defun compdb--get-dir ()
  "Return the directory name to run CMake in."
  (when (not compdb-dir) (setq compdb-dir (make-temp-file "cmake" t)))
  (file-name-as-directory compdb-dir))


(defun compdb--ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))


(defun compdb--is-src-file (string)
  "Test if STRING is a source file or not."
  (or (compdb--ends-with string ".c")
      (compdb--ends-with string ".cpp")
      (compdb--ends-with string ".C")
      (compdb--ends-with string ".cxx")
      (compdb--ends-with string ".cc")))


(defun compdb--filter (pred seq)
  "Apply PRED to filter SEQ."
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) seq)))


(defun compdb--filter-params (file-params filter-func)
  "Filter FILE-PARAMS with FILTER-FUNC."
  ;; The compilation database is a json array of json objects
  ;; Each object is a file with directory, file and command fields
  ;; Depending on FILTER-FUNC, it maps file names to desired compiler flags
  ;; An example would be -I include flags
  (let* ((command (compdb--get-file-param 'command file-params))
         (args (split-string command " +"))
         (flags (funcall filter-func args)))
    (mapconcat 'identity flags " ")))


(defun compdb--args-to-include-and-define-flags (args)
  "Filters a list of compiler command ARGS to yield only includes and defines."
  (let ((case-fold-search)) ;; case sensitive matching
    (compdb--filter (lambda (x) (string-match "^-[ID].+\\b" x)) args)))

(defun compdb--params-to-src-flags (file-params &optional filter-func)
  "Source compiler flags for FILE-PARAMS using FILTER-FUNC."
  (if (not file-params) nil
    (let* ((filter-func (or filter-func #'compdb--args-to-include-and-define-flags))
           (value (compdb--filter-params file-params filter-func))
           (flags-string (if value value nil)))
      (if flags-string (split-string flags-string " +") nil))))


(defun compdb--commands-to-hdr-flags (commands)
  "Header compiler flags from COMMANDS."
  (let ((args (compdb--flatten (mapcar (lambda (x) (split-string x " +")) commands))))
    (delete-dups (compdb--args-to-include-and-define-flags args))))

(defun compdb--params-to-src-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (compdb--flags-to-includes (compdb--params-to-src-flags file-params 'identity)))


(defun compdb--params-to-sys-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (compdb--flags-to-sys-includes (compdb--params-to-src-flags file-params 'identity)))


(defun compdb--commands-to-hdr-includes (commands)
  "Header `-include` flags from COMMANDS."
  (let ((args (compdb--flatten (mapcar (lambda (x) (split-string x " +")) commands))))
    (delete-dups (compdb--flags-to-includes args))))


(defun compdb--flatten (lst)
  "Flatten LST."
  (apply 'append lst))


(defun compdb--flags-to-include-paths (flags)
  "From FLAGS (a list of flags) to a list of include paths."
  (compdb--to-simple-flags flags "-I"))


(defun compdb--flags-to-defines (flags)
  "From FLAGS (a list of flags) to a list of defines."
  (compdb--to-simple-flags flags "-D"))


(defun compdb--flags-to-includes (flags)
  "From FLAGS (a list of flags) to a list of includes."
  (let ((includes nil))
    (while (member "-include" flags)
      (setq flags (cdr (member "-include" flags)))
      (when flags (setq includes (cons (car flags) includes))))
    includes))

(defun compdb--flags-to-sys-includes (flags)
  "From FLAGS (a list of flags) to a list of isystem includes."
  (let ((sysincludes nil))
    (while (member "-isystem" flags)
      (setq flags (cdr (member "-isystem" flags)))
      (when flags (setq sysincludes (cons (car flags) sysincludes))))
    sysincludes))


(defun compdb--to-simple-flags (flags flag)
  "A list of either directories or defines from FLAGS depending on FLAG."
  (let* ((include-flags (compdb--filter
                         (lambda (x)
                           (let ((match (string-match flag x)))
                             (and match (zerop match))))
                         flags)))
    (mapcar (lambda (x) (replace-regexp-in-string flag "" x)) include-flags)))


(defun compdb--get-compiler-flags (flags)
  "Use FLAGS to return all compiler flags including existing ones."
  (append (compdb--get-existing-compiler-flags) flags))

(defun compdb--get-existing-compiler-flags ()
  "Return existing ac-clang flags for this mode, if set."
  (if (eq major-mode 'c++-mode)
      (compdb--symbol-value 'compdb-flags-c++)
    (compdb--symbol-value 'compdb-flags-c)))

(defun compdb--get-existing-definitions ()
  "Return existing compiler defines, if set."
  (compdb--symbol-value 'compdb-definitions))


(defun compdb--symbol-value (sym)
  "Return the value of SYM if bound, nil if not."
  (if (boundp sym) (symbol-value sym) nil))


(defun compdb--locate-cmakelists ()
  "Find the topmost CMakeLists.txt file."
  (compdb--locate-cmakelists-impl default-directory nil))


(defun compdb--locate-cmakelists-impl (dir last-found)
  "Find the topmost CMakeLists.txt from DIR using LAST-FOUND as a 'plan B'."
  (let ((new-dir (locate-dominating-file dir "CMakeLists.txt")))
    (if new-dir
        (compdb--locate-cmakelists-impl (expand-file-name ".." new-dir) new-dir)
      last-found)))

(defun compdb--string-to-json (json-str)
  "Tranform JSON-STR into an opaque json object."
  (json-read-from-string json-str))


(defun compdb--file-params (json file-name)
  "Get parameters from a JSON object for FILE-NAME."
  (compdb--find-in-vector (lambda (x) (equal (compdb--get-file-param 'file x) file-name)) json))


(defun compdb--find-in-vector (pred vec)
  "Find the 1st element satisfying PRED in VEC."
  (let ((i 0)
        (max (length vec))
        (found nil))
    (while (and (not found) (< i max))
      (if (funcall pred (elt vec i))
          (setq found t)
        (setq i (1+ i)))
      )
    (if found (elt vec i) nil)))

(defun compdb--get-file-param (key obj)
  "Get the value for KEY in OBJ."
  (cdr (assoc key obj)))

;;;###autoload
(defun compdb-compile ()
  "Compile the project."
  (interactive)
  (if compdb-dir
      (compile (compdb--get-compile-command compdb-dir))
    (let ((command (read-from-minibuffer "Compiler command: " compile-command)))
      (compile command)))
  (compdb--run-rc))


(defun compdb--get-compile-command (dir)
  "Return the compile command to use for DIR."
  (cond (compdb-compile-command compdb-compile-command)
        ((file-exists-p (expand-file-name "build.ninja" dir)) (concat "ninja -C " dir))
        ((file-exists-p (expand-file-name "Makefile" dir)) (concat "make -C " dir))
        (t nil)))


;;;###autoload
(defun compdb-maybe-start-rdm ()
  "Start the rdm (rtags) server."
  (when (featurep 'rtags)
    (unless (get-process "rdm")
      (let ((buf (get-buffer-create compdb-rdm-buffer-name)))
        (with-current-buffer buf (start-process "rdm" (current-buffer)
                                                compdb-rdm-executable))))))


(provide 'compdb)
;;; compdb.el ends here
