;;; protagentic-pkg.el --- Package definition for Protagentic -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Package definition file for MELPA and other package repositories.

;;; Code:

(define-package "protagentic" "0.1.0"
  "Structured feature planning for Emacs"
  '((emacs "26.1"))
  :keywords '("tools" "project" "planning" "workflow")
  :url "https://github.com/kgthegreat/protagentic"
  :maintainer '("Kumar Gaurav" . "kgthegreat@gmail.com")
  :authors '(("Kumar Gaurav" . "kgthegreat@gmail.com")))

;;; protagentic-pkg.el ends here