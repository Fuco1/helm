;;; helm-org.el --- Helm for org headlines and keywords completion -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'org)
(require 'org-archive)
(require 'dash)

(defgroup helm-org nil
  "Org related functions for helm."
  :group 'helm)

(defcustom helm-org-headings-fontify nil
  "Fontify org buffers before parsing them.
This reflect fontification in helm-buffer when non--nil.
NOTE: This will be slow on large org buffers."
  :group 'helm-org
  :type 'boolean)

;;; Org headings
;;
;;
(defun helm-org-goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (org-show-entry))

(cl-defun helm-source-org-headings-for-files (filenames
                                              &optional (min-depth 1) (max-depth 8))
  (helm-build-sync-source "Org Headings"
    :candidates (helm-org-get-candidates filenames min-depth max-depth)
    :action '(("Go to line" . helm-org-goto-marker)
              ("Refile to this heading" . helm-org-heading-refile)
              ("Insert link to this heading"
               . helm-org-insert-link-to-heading-at-marker))))

(defun helm-org-insert-link-to-heading-at-marker (marker)
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker))
    (let ((heading-name (nth 4 (org-heading-components)))
          (file-name buffer-file-name))
      (message heading-name)
      (message file-name)
      (with-helm-current-buffer
        (org-insert-link
         file-name (concat "file:" file-name "::*" heading-name))))))

(defun helm-org-heading-refile (marker)
  (with-helm-current-buffer
    (org-cut-subtree))
  (let ((target-level (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (org-current-level))))
    (helm-org-goto-marker marker)
    (org-end-of-subtree t t)
    (org-paste-subtree (+ target-level 1))))

(defun helm-org-get-candidates (filenames min-depth max-depth)
  (apply #'append
   (mapcar (lambda (filename)
             (helm-get-org-candidates-in-file
              filename min-depth max-depth helm-org-headings-fontify))
           filenames)))

(defun helm-get-org-candidates-in-file (filename min-depth max-depth
                                        &optional fontify)
  (with-current-buffer (find-file-noselect filename)
    (and fontify (jit-lock-fontify-now))
    (let ((match-fn (if fontify 'match-string 'match-string-no-properties)))
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (re-search-forward org-complex-heading-regexp nil t)
              if (let ((num-stars (length (match-string-no-properties 1))))
                   (and (>= num-stars min-depth) (<= num-stars max-depth)))
              collect `(,(funcall match-fn 0) . ,(point-marker)))))))

;;;###autoload
(defun helm-org-agenda-files-headings ()
  (interactive)
  (helm :sources (helm-source-org-headings-for-files (org-agenda-files))
        :candidate-number-limit 99999
        :buffer "*helm org headings*"))

;;;###autoload
(defun helm-org-in-buffer-headings ()
  (interactive)
  (helm :sources (helm-source-org-headings-for-files
                  (list (buffer-file-name (current-buffer))))
        :candidate-number-limit 99999
        :buffer "*helm org inbuffer*"))

;;; Org search
;;
;;

(defun helm-org-goto-char (candidate)
  (-let (((point buffer) candidate))
    (switch-to-buffer buffer)
    (goto-char point)
    (org-show-context)
    (org-show-entry)))

(defun helm-org-show-top-heading (point)
  (switch-to-buffer helm-current-buffer)
  (goto-char point)
  (my-org-show-parent-context))

(defun helm-org-search-get-candidates-in-file ()
  (let* ((archive-file (with-current-buffer helm-current-buffer
                         (org-extract-archive-file
                          (org-get-local-archive-location))))
         (buffers (list
                   helm-current-buffer
                   (or (find-buffer-visiting archive-file)
                       (find-file-noselect archive-file)))))
    (-mapcat #'helm-org-search--get-candidates-in-file buffers)))

(defun helm-org-search--get-candidates-in-file (buffer)
  ;; TODO: add support for multiple buffers
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (outline-next-heading)
      (let ((words (split-string helm-pattern))
            (match t)
            todo tags search headline re)
        (mapc
         (lambda (word)
           (cond
            ((and (> (length word) 1) (= ?! (aref word 0))) (push (substring word 1) todo))
            ((and (> (length word) 1) (= ?: (aref word 0))) (push (substring word 1) tags))
            ((and (> (length word) 1) (= ?* (aref word 0))) (push (substring word 1) headline))
            (t (push word search))))
         words)
        (setq search (nreverse search))
        (while match
          (let (tags-at)
            (when (and
                   ;; sideffect search!
                   (or (not search) (setq match (re-search-forward (car search) nil t)))
                   (or (not (> (length search) 1))
                       (let ((limit (save-excursion (or (outline-next-heading) (point-max)))))
                         (every (lambda (p)
                                  (org-back-to-heading t)
                                  (re-search-forward p limit t))
                                (cdr search))))
                   (let ((header (when (or todo headline) (org-heading-components))))
                     (and
                      (or (not todo) (every (lambda (m) (string-match-p m (or (nth 2 header) ""))) todo))
                      (or (not tags) (progn (setq tags-at (org-get-tags-at)) (every (lambda (m) (org-match-any-p m tags-at)) tags)))
                      (or (not headline) (every (lambda (m) (string-match-p m (nth 4 header))) headline)))))
              (push (cons
                     ;; TODO: ulozit samostatne header/tags/context, nakonci to spojit tak, aby to bolo zarovnane
                     (save-excursion
                       (org-back-to-heading t)
                       (let* ((lb (line-beginning-position))
                              (le (line-end-position))
                              (heading (progn
                                         (font-lock-fontify-region lb le)
                                         (looking-at org-todo-line-tags-regexp)
                                         (buffer-substring lb (or (match-beginning 4) le)))))
                         (concat
                          (replace-regexp-in-string
                           "[[:space:]]+" " "
                           (concat
                            heading
                            (if (equal buffer helm-current-buffer) " " " [ARCHIVED] ")
                            ;; add all tags, including inherited
                            (if (or tags-at (setq tags-at (org-get-tags-at)))
                                (propertize (concat ":" (mapconcat 'identity tags-at ":") ":")
                                            'face (get-text-property 0 'face heading))
                              "")))
                          ;; only display context if the match isn't in
                          ;; the headline itself and we made a search
                          (if (or (not (and search match)) (and (<= lb match) (<= match le))) ""
                            (progn
                              (goto-char match)
                              (concat "    " (substring (thing-at-point 'line) 0 -1)))))))
                     (list (point) buffer))
                    re)))
          (setq match (and match (outline-next-heading))))
        (nreverse re)))))

(defvar helm-org-source-search
  '((name . "Org search")
    ;; (init . (lambda ()
    ;;           (setq helm-org-search-pattern nil)
    ;;           (helm-candidate-buffer helm-current-buffer)))
    ;; (candidates-in-buffer)
    (candidates . helm-org-search-get-candidates-in-file)
    (volatile . t)
    (match (lambda (_) t))
    ;; (search helm-org-search-forward)
    ;; (get-line . helm-org-search-get-line)
    ;; cache the parsed pattern, so we don't have to recompute it each
    ;; time we search forward for a new match
    ;; (pattern-transformer helm-org-search-parse-pattern-and-cache)
    ;; TODO: add persistent-action
    ;; (persistent-action . helm-regexp-persistent-action)
    (persistent-help . "Show this line")
    (nomark)
    (no-matchplugin)
    (requires-pattern . 2)
    (mode-line . "Press TAB to select action.")
    ;; TODO: add action to clock in the current header
    (action ("Go to line" . helm-org-goto-char)
            ("Show top parent" . helm-org-show-top-heading)
            ("Refile to this heading" . helm-org-heading-refile)
            ("Insert link to this heading"
             . helm-org-insert-link-to-heading-at-marker))))

;;;###autoload
(defun helm-org-in-buffer-search ()
  (interactive)
  (helm :sources 'helm-org-source-search
        :candidate-number-limit 99999
        :buffer "*helm org search*"))

(provide 'helm-org)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-org.el ends here
