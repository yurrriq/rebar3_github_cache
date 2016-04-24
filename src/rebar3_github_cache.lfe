(defmodule rebar3_github_cache
  ;; TODO: write docstring
  (behaviour provider)
  ;; Provider callbacks
  ;; (export (init 1) (do 1) (format_error 1))
  (export all)     ; The compiler doesn't like do/1, so we (export all) instead.
  ;; Lispy alias
  (import (rename rebar_state ((add_resource 2) add-resource))))

(defun init (state)
  (let ((resource #(github rebar_github_resource)))
    `#(ok ,(add-resource state resource))))

(defun do (state)
  `#(ok ,state))

(defun format_error (reason)
  (io_lib:format "~p" `[,reason]))
