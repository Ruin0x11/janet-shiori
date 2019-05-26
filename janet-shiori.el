;;; janet-shiori.el --- Janet-SHIORI interface over SSTP

;; Copyright (C) 2019 Ruin0x11

;; Author:  <ipickering2@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Client for conversing with Janet-SHIORI.

;;; Code:

(require 'eval-sexp-fu nil t)

(defvar janet-shiori-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-:"  'janet-shiori-eval-expression)
    (define-key map "\M-\C-x"  'janet-shiori-eval-defun)
    (define-key map "\C-x\C-e" 'janet-shiori-eval-last-sexp)
    (define-key map "\C-c\C-e" 'janet-shiori-eval-defun)
    (define-key map "\C-c\C-r" 'janet-shiori-eval-region)
    (define-key map "\C-c\C-n" 'janet-shiori-eval-form-and-next)
    (define-key map "\C-c\C-p" 'janet-shiori-eval-paragraph)
    (define-key map "\C-c\C-b" 'janet-shiori-eval-buffer)
    (define-key map "\C-c\C-s" 'janet-shiori-connect)
    ;(define-key map "\C-c\C-d" 'janet-shiori-describe-sym)
    ;(define-key map "\C-c\C-f" 'janet-shiori-show-function-documentation)
    ;(define-key map "\C-c\C-v" 'janet-shiori-show-variable-documentation)
    map))

;;;###autoload
(define-minor-mode janet-shiori-minor-mode
  "Minor mode for interacting with Janet-SHIORI.

The following commands are available:

\\{janet-shiori-minor-mode-map}"
  :lighter " Janet-SHIORI" :keymap janet-shiori-minor-mode-map)

(defun janet-shiori--make-tcp-connection (host port)
  (make-network-process :name "Janet-SHIORI"
                        :buffer "*Janet-SHIORI*"
                        :host host
                        :service port
                        :nowait t
                        :coding 'utf-8))

(defun janet-shiori--escape-str (str)
  (replace-regexp-in-string "\n" "\\\\n"
                            (replace-regexp-in-string "\r" "\\\\r"
                                                      (replace-regexp-in-string "\\\\" "\\\\\\\\" str))))
(janet-shiori--escape-str "\n \n \\n")

(defun janet-shiori--send (str)
  (let ((proc (janet-shiori--make-tcp-connection "127.0.0.1" 9821))
        (req (format
              "NOTIFY SSTP/1.0\r\nSender: %s\r\nEvent: OnJanetEval\r\nReference0: %s\r\nCharset: UTF-8\r\n\r\n"
              (janet-shiori--escape-str (version))
              (janet-shiori--escape-str str))))
    (comint-send-string proc req)))

(defun janet-shiori--send-region (start end)
  (janet-shiori--send (buffer-substring start end)))

(defun janet-shiori-eval-paragraph ()
  "Send the current paragraph to the Janet-SHIORI process."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (janet-shiori-eval-region (point) (mark))))

(defun janet-shiori-eval-expression (exp)
  "Send the expression EXP to the Janet-SHIORI process."
  (interactive "sEval: \n")
  (janet-shiori--send exp))

(defun janet-shiori-eval-region (start end)
  "Send the current region to the Janet-SHIORI process."
  (interactive "r\n")
  (janet-shiori--send (buffer-substring start end)))

(defun janet-shiori-do-defun (do-region)
  "Send the current defun to the Janet-SHIORI process."
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f")
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
        (funcall do-region (point) end))))

(defun janet-shiori-eval-defun ()
  "Send the current defun to the Janet-SHIORI process."
  (interactive)
  (janet-shiori-do-defun 'janet-shiori-eval-region))

(defun janet-shiori-eval-last-sexp ()
  "Send the previous sexp to the Janet-SHIORI process."
  (interactive)
  (janet-shiori-eval-region (save-excursion (backward-sexp) (point)) (point)))

(defun janet-shiori-eval-form-and-next ()
  "Send the previous sexp to the Janet-SHIORI process and move to the next one."
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (janet-shiori-eval-last-sexp)
  (forward-sexp))

(defun janet-shiori-eval-buffer ()
  (interactive)
  (janet-shiori-eval-region (point-min) (point-max)))

(defun janet-shiori--initialize-esf ()
  (when (boundp 'eval-sexp-fu)
    (define-eval-sexp-fu-flash-command janet-shiori-eval-last-sexp
      (eval-sexp-fu-flash (when (ignore-errors (preceding-sexp))
                            (with-esf-end-of-sexp
                              (bounds-of-thing-at-point 'sexp)))))
    (define-eval-sexp-fu-flash-command janet-shiori-eval-defun
      (eval-sexp-fu-flash (when (ignore-errors (preceding-sexp))
                            (save-excursion
                              (end-of-defun)
                              (beginning-of-defun)
                              (bounds-of-thing-at-point 'sexp)))))))

(janet-shiori--initialize-esf)


(provide 'janet-shiori)
;;; janet-shiori.el ends here
