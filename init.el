;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テーマ格納ディレクトリのパス追加
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "theme")))

;; テーマ選択
(load-theme 'gnupack-dark t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scroll
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 文字コード
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq default-coding-system 'utf-8)
(set-clipboard-coding-system 'sjis)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スペース可視化
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (progn
;;   (require 'whitespace)
;;   (setq whitespace-style
;;         '(
;;           face ; faceで可視化
;;           trailing ; 行末
;;           tabs ; タブ
;;           spaces ; スペース
;;           space-mark ; 表示のマッピング
;;           tab-mark
;;           ))
;;   (setq whitespace-display-mappings
;;         '(
;;           (space-mark ?\u3000 [?\u2423])
;;           (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
;;           ))
;;   (setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
;;   (setq whitespace-space-regexp "\\(\u3000+\\)")
;;   (set-face-attribute 'whitespace-trailing nil
;;                       :foreground "RoyalBlue4"
;;                       :background "RoyalBlue4"
;;                       :underline nil)
;;   (set-face-attribute 'whitespace-tab nil
;;                       :foreground "yellow4"
;;                       :background "yellow4"
;;                       :underline nil)
;;   (set-face-attribute 'whitespace-space nil
;;                       :foreground "gray40"
;;                       :background "gray20"
;;                       :underline nil)
;;   (global-whitespace-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 括弧
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode t)
;;(setq show-paren-delay 0)
;;(setq show-paren-style 'mixed)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; インデント
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window操作
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
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq css-indent-level 2)
(setq css-indent-offset 2)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/vendor")

;; (require 'scss-mode)
;; (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; (defun scss-custom ()
;;   "scss-mode-hook"
;;   (and
;;    (set (make-local-variable 'css-indent-offset) 2)
;;    (set (make-local-variable 'scss-compile-at-save) nil)))
;; (add-hook 'scss-mode-hook
;;           '(lambda() (scss-custom)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "php-mode")
(require 'php-mode)
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
 '(custom-enabled-themes (quote (gnupack-dark)))
 '(custom-safe-themes
   (quote
    ("1d1c7afb6cbb5a8a8fb7eb157a4aaf06805215521c2ab841bd2c4a310ce3781e" default)))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (sass-mode xah-lookup scss-mode php-mode org-trello google-translate ace-link))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; google-translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'google-translate)
(defvar google-translate-english-chars "[:ascii:]’“”–"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (save-excursion
                 (let (s)
                   (forward-char 1)
                   (backward-sentence)
                   (setq s (point))
                   (forward-sentence)
                   (buffer-substring s (point)))))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))
(global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(require 'ace-link)
(eval-after-load 'eww '(define-key eww-mode-map "f" 'ace-link-eww))
(ace-link-setup-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xah-lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eww)
(setq xah-lookup-browser-function 'eww)
(require 'xah-lookup)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trello
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-trello)
