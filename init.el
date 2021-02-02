;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp")

(setq exec-path (parse-colon-path (getenv "PATH")))

;; Include any packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(require 'magit)
(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'whitespace)
(require 'scss-mode)
(require 'emmet-mode)
(require 'vue-mode)
(require 'php-mode)
(require 'web-mode)
(require 'company)
(require 'multiple-cursors)
(require 'google-translate)
(require 'ace-link)
(require 'eww)
(require 'xah-lookup)
(require 'flycheck)

;; Default directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-directory "~/")
(setq command-line-default-directory "~/")

;; Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テーマ格納ディレクトリのパス追加
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "theme")))

;; テーマ選択
(load-theme 'gnupack-dark t)

;; Scroll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スクロール時のカーソル位置を維持（有効：t、無効：nil）
(setq scroll-preserve-screen-position t)

;; スクロール開始の残り行数
(setq scroll-margin 0)

;; スクロール時の行数
(setq scroll-conservatively 10000)

;; スクロール時の行数（scroll-marginに影響せず）
(setq scroll-step 0)

;; 画面スクロール時の重複表示する行数
(setq next-screen-context-lines 1)

;; キー入力中の画面更新を抑止（有効：t、無効：nil）
(setq redisplay-dont-pause t)

;; recenter-top-bottomのポジション
(setq recenter-positions '(middle top bottom))

;; 横スクロール開始の残り列数
(setq hscroll-margin 1)

;; 横スクロール時の列数
(setq hscroll-step 1)

;; スクロールダウン
(global-set-key (kbd "C-z") 'scroll-down)

;; バッファの最後までスクロールダウン
(defadvice scroll-down (around scroll-down activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        )
    (if (< bgn-num (window-height))
        (goto-char (point-min))
      ad-do-it) ))

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        (end-num nil)
        )
    (save-excursion
      (goto-char (point-max))
      (setq end-num (+ 1 (count-lines (point-min) (point))))
      )
    (if (< (- (- end-num bgn-num) (window-height)) 0)
        (goto-char (point-max))
      ad-do-it) ))

;; Char code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq default-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; Font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil :family "Menlo" :height 120)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))
(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN*" . 1.2))

;; Parentheses highlight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode t)
;;(setq show-paren-delay 0)
;;(setq show-paren-style 'mixed)

;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun switch-tab ()
  "Switch 'indent-tabs-mode'"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)))
;;(setq-default inent-tabs-mode nil)
;;(custom-set-variables '(tab-width 4))

(defun semicolon-ret ()
  (interactive)
  (insert ";")
  (newline-and-indent))

(defun brace-ret-brace ()
  (interactive)
  (insert "{") (newline-and-indent)
  (newline-and-indent)
  (insert "}") (indent-for-tab-command)
  (newline-and-indent) (newline-and-indent)
  (previous-line) (previous-line) (previous-line)
  (indent-for-tab-command))

;; Window controll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<right>") 'windmove-right)

(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1)) c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key (kbd "\C-c\C-r") 'window-resizer)

;; HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setting-html-tab ()
  (interactive)
  (let* ((tab-width 3)
         (hook `(lambda ()
                  (custom-set-variables
                   '(tab-width ,tab-width)
                   '(indent-tabs-mode t)
                   '(sgml-basic-offset ,tab-width)))))

    (remove-hook 'html-mode-hook hook)
    (add-hook 'html-mode-hook hook t)))
 
(defun setting-html-space (width &optional tab-width)
  (interactive "nWidth:")
  (let* ((tab-width (if tab-width tab-width 4))
         (hook `(lambda ()
            (setq tab-width, tab-width)
            (setq indent-tabs-mode nil))))
 
    (custom-set-variables
     `(sgml-basic-offset, width))
 
    (remove-hook 'html-mode-hook hook)
    (add-hook 'html-mode-hook hook t)))

;; CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq css-indent-level 2)
(setq css-indent-offset 2)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; js2-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq my-js-mode-indent-num 2)
            (setq js2-basic-offset my-js-mode-indent-num)
            (setq js-switch-indent-offset my-js-mode-indent-num)))

;; scss-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil)))
(add-hook 'scss-mode-hook
          '(lambda() (scss-custom)))

;; emmet-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'vue-mode-hook 'emmet-mode)
(add-hook 'markdown-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(setq emmet-self-closing-tag-style "")
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil))
(define-key emmet-mode-keymap (kbd "C-;") 'emmet-expand-line)

;; php-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "php-mode")
(setq php-mode-force-pear t)
;;(set-face-foreground 'font-lock-constant-face  "brack")

(add-hook 'php-mode-hook
          '(lambda ()
             (c-set-offset 'arglist-intro '+)
             (c-set-offset 'arglist-close 0)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(gnupack-dark))
 '(custom-safe-themes
   '("1d1c7afb6cbb5a8a8fb7eb157a4aaf06805215521c2ab841bd2c4a310ce3781e" default))
 '(package-selected-packages
   '(js2-mode flycheck php-mode vue-mode multiple-cursors web-mode helm company ht f dash magit s markdown-mode emmet-mode sass-mode xah-lookup scss-mode google-translate ace-link)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 0)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-tag-auto-close-style 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq whitespace-style '(fase
                         trailing
                         tabs
;;                         empty
                         space-mark
                         tab-mark))

(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(global-whitespace-mode 1)

;; company-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'company
  (global-company-mode)

  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-auto-expand t)
  (setq company-transformers '(company-sort-by-backend-importance))

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer)
  (global-set-key (kbd "C-M-i") 'company-complete)

  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "#ccc")

  (set-face-attribute 'company-tooltip-common nil
                      :foreground "white" :background "#666")

  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "white" :background "#666")

  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "#333")

  (set-face-attribute 'company-scrollbar-fg nil
                      :background "#eee")

  (set-face-attribute 'company-scrollbar-bg nil
                      :background "#333"))

;; multiple-cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-e") 'mc/edit-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-S-n") 'mc/skip-to-next-like-this)
(global-set-key (kbd "M-S-p") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "M-s") 'mc/mark-all-like-this)
(global-set-key (kbd "M-d") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "M-t") 'mc/mark-sgml-tag-pair)

;; google-translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun google-translate-enja-or-jaen (&optional string)
  "Translate words in region or current position. Can also specify query with C-u"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (thing-at-point 'word))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" "[:ascii:]’“”–")
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))

(global-set-key (kbd "\C-c t") 'google-translate-enja-or-jaen)

;; Fix error of "Failed to search TKK"
(defun google-translate--get-b-d1 ()
    ;; TKK='427110.1469889687'
  (list 427110 1469889687))

;; eww
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))

(setq eww-search-prefix "http://www.google.co.jp/search?gl=jp&hl=ja&q=")

;; (defun eww-mode-hook--rename-buffer ()
;;   "Rename eww browser's buffer so sites open in new page."
;;   (rename-buffer "eww" t))
;; (add-hook 'eww-mode-hook 'eww-mode-hook--rename-buffer)

(defun eww-search (term)
  (interactive "sSearch terms: ")
  (setq eww-hl-search-word term)
  (eww-browse-url (concat eww-search-prefix term)))

(add-hook 'eww-after-render-hook (lambda ()
                                   (highlight-regexp eww-hl-search-word)
                                   (setq eww-hl-search-word nil)))

(defun eww-disable-images ()
  "eww で画像表示させない"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))
(defun eww-enable-images ()
  "eww で画像表示させる"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))
(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))
;; はじめから非表示
(defun eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'shr-put-image-alt))
(add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)

;; ace-link
(eval-after-load 'eww '(define-key eww-mode-map "f" 'ace-link-eww))
(ace-link-setup-default)

;; xah-lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq xah-lookup-browser-function 'eww)

;; Wikipedia
(defun xah-lookup-wikipedia (&optional @word)
  "Lookup current word or text selection in Wikipedia.
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   @word
   (get 'xah-lookup-wikipedia 'xah-lookup-url )
   (get 'xah-lookup-wikipedia 'xah-lookup-browser-function )))

(put 'xah-lookup-wikipedia 'xah-lookup-url "http://ja.wikipedia.org/wiki/word02051")
(put 'xah-lookup-wikipedia 'xah-lookup-browser-function xah-lookup-browser-function)

;; Weblio
(defun xah-lookup-weblio (&optional @word)
  "Lookup current word or text selection in URL `http://www.weblio.jp/'.
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   @word
   (get 'xah-lookup-weblio 'xah-lookup-url )
   (get 'xah-lookup-weblio 'xah-lookup-browser-function )))

(put 'xah-lookup-weblio 'xah-lookup-url "http://www.weblio.jp/content/word02051")
(put 'xah-lookup-weblio 'xah-lookup-browser-function xah-lookup-browser-function)

;; alc
(defun xah-lookup-alc (&optional @word)
  "Lookup current word or text selection in URL `https://eow.alc.co.jp/'.
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   @word
   (get 'xah-lookup-alc 'xah-lookup-url )
   (get 'xah-lookup-alc 'xah-lookup-browser-function )))

(put 'xah-lookup-alc 'xah-lookup-url "https://eow.alc.co.jp/search?q=word02051")
(put 'xah-lookup-alc 'xah-lookup-browser-function xah-lookup-browser-function)

;; PHP ref
(defun xah-lookup-php (&optional @word)
  "Lookup current word or text selection in PHP ref.
Version 2017-02-09"
  (interactive)
  (xah-lookup-word-on-internet
   @word
   (get 'xah-lookup-php 'xah-lookup-url )
   (get 'xah-lookup-php 'xah-lookup-browser-function )))

(put 'xah-lookup-php 'xah-lookup-url "http://php.net/word02051")
(put 'xah-lookup-php 'xah-lookup-browser-function xah-lookup-browser-function)

;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-flycheck-mode)
(flycheck-define-checker textlint
  "A linter for Markdown."
  :command ("textlint" "--format" "unix" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode))
 
(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq flycheck-checker 'textlint)
             (flycheck-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
(setq ring-bell-function 'ignore)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(setq use-dialog-box nil)
(setq vc-make-backup-files nil)
(setq scroll-conservatively 1)
(setq comint-scroll-show-maximum-output t)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-j") 'newline-and-indent)
(delete-selection-mode t)
(global-linum-mode t)
