;; -*- coding: utf-8; lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs init.el
;; Compile to .elc use the below command.
;; emacs --batch -f batch-byte-compile init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 基本設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (prog1 "General settings"
    (require 'cli-lib nil t)
    (setq default-directory "~/")
    (setq command-line-default-directory "~/")
    (setq byte-compile-warnings '(not cl-function obsolete))

    (when (or load-file-name byte-compile-current-file)
      (setq user-emacs-directory
            (expand-file-name
             (file-name-directory (or load-file-name byte-compile-current-file)))))

    (defconst my:d:pkg:elpa
      (expand-file-name "pkg/elpa" user-emacs-directory))
    (defconst my:d:pkg:elget
      (expand-file-name "pkg/el-get" user-emacs-directory))
    (defconst my:d:tmp
      (expand-file-name "tmp/" user-emacs-directory))
    (defconst my:d:var
      (expand-file-name "var/" user-emacs-directory))
    (defconst my:d:themes
      (expand-file-name "themes/" user-emacs-directory))
    (defconst my:d:lisp
      (expand-file-name "site-lisp/" user-emacs-directory))
    (defconst my:d:backup
      (expand-file-name "backup/" user-emacs-directory))

    (dolist (my:d '(my:d:pkg:elpa
                    my:d:pkg:elget
                    my:d:tmp
                    my:d:var
                    my:d:themes
                    my:d:lisp
                    my:d:backup))
      (lambda ()
        (unless (file-directory-p my:d)
          (make-directory my:d t))))

    (custom-set-variables
     '(package-archives '(("org" . "https://orgmode.org/elpa/")
                          ("gnu" . "https://elpa.gnu.org/packages/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("melpa-stable" . "https://stable.melpa.org/packages/")))
     '(package-gnupghome-dir (expand-file-name ".gnupg" (getenv "HOME")))
     '(package-user-dir my:d:pkg:elpa)
     '(package-enable-at-startup t)
     '(gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

    (package-initialize)

    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf t))

    (leaf leaf-keywords
      :ensure t
      :init
      (leaf hydra :ensure t)
      (leaf blackout :ensure t)
      (leaf el-get :ensure t
        :preface (defconst el-get-dir my:d:pkg:elget)
        :custom ((el-get-notify . 'message)
                 (el-get-git-shallow-clone . t)))
      :config (leaf-keywords-init))))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf exec-path-from-shell
  :ensure t
  :defun (exec-path-from-shell-initialize)
  :config (exec-path-from-shell-initialize))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf use-package :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf cus-edit
  :preface
  (setq custom-file (expand-file-name "custom.el" my:d:tmp))
  :custom
  `((custom-file . ,(expand-file-name "custom.el" my:d:tmp)))
  :hook
  `((kill-emacs-hook . (lambda ()
                         (if (file-exists-p custom-file)
                             (delete-file custom-file))))))

(leaf cus-start
  :load-path my:d:lisp
  :custom ((user-full-name . "Takama Okazaki")
           (user-mail-address . "akamaru.tkm@gmail.com")
           (user-login-name . "Yap-house")

           ;; ビープ音を鳴らさない
           (ring-bell-function . 'ignore)

           ;; メニューバーを消す
           (menu-bar-mode . nil)

           ;; ツールバーを消す
           (tool-bar-mode . nil)

           ;; sgml-mode インデント
           (sgml-basic-offset . 2)

           ;; タブ幅
           (tab-width . 4)

           ;; インデントにタブを使わない
           (indent-tabs-mode . nil)

           ;; バッファ画面外文字の切り詰め表示（有効：t、無効：nil）
           (truncate-lines . t))

  :config
  ;; .elc、.elの新しい方を読み込む
  (when (boundp 'load-prefer-newer)
    (setq load-prefer-newer t))

  ;; yes or no を y or n に
  (defalias 'yes-or-no-p 'y-or-n-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド変更
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf-keys (("M-h" . help-for-help)
            ("C-h" . backward-delete-char)
            ("C-/" . undo)
            ("C-c M-l" . toggle-truncate-lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp Utilityの読み込み
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf f :ensure t)
(leaf s :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 選択範囲の上書き
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf delete-selection
  :doc "Delete selection if you insert."
  :global-minor-mode delete-selection-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カーソルの点滅
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf disable-blink-cursor
  :doc "Disable blink-cursor."
  :config (blink-cursor-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; electric-indent-modeの無効化
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf disable-electric-indent
  :doc "Disable electric-indent-mode."
  :config (electric-indent-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 括弧のハイライト
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf paren
  :doc "Hightlight matching paren."
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; バッファ自動更新
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf autorevert
  :doc "Revert buffer when file on disk change."
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 現在行のハイライト
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf highlight-line
  :doc "Highlight with current line."
  :global-minor-mode global-hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 行番号設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf line-number-settings
  :config
  (if (version<= "26.0.5" emacs-version)
      (progn
        (global-display-line-numbers-mode t)
        (custom-set-variables '(display-line-numbers-width-start t)))
    (progn
      (line-number-mode t)
      (column-number-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; モードラインのファイル名表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf uniquify
  :custom
  '((uniquify-buffer-name-style . 'forward)
    (uniquify-buffer-name-style . 'post-forward-angle-brackets)
    (uniquify-ignore-buffers-re . "*[^*]+*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 言語設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf cp5022x
  :ensure t
  :require t
  :config
  (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201 'katakana-jisx0201 'iso-8859-1 'unicode)
  (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932))

(leaf language-setting
  :custom
  `(;; google-ime-skkの場所
    (skk-server-prog . "/Users/takama/.rbenv/shims/google-ime-skk")
    ;; 辞書サーバが起動していなかったときに Emacs からプロセスを立ち上げる
    (skk-server-inhibit-startup-server . nil)
    ;; サーバー機能を利用
    (skk-server-host . "localhost")
    ;; ポートはgoogle-ime-skk
    (skk-server-portnum . 55100)
    ;; 複数 skk 辞書を共有
    (skk-share-private-jisyo . t))

  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)

  ;; cp932エンコードの表記変更
  (coding-system-put 'cp932 :mnemonic ?P)
  (coding-system-put 'cp932-dos :mnemonic ?P)
  (coding-system-put 'cp932-unix :mnemonic ?P)
  (coding-system-put 'cp932-mac :mnemonic ?P)

  ;; UTF-8エンコードの表記変更
  (coding-system-put 'utf-8 :mnemonic ?U)
  (coding-system-put 'utf-8-with-signature :mnemonic ?u)

  ;; 改行コードの表記追加
  (setq eol-mnemonic-dos       ":Dos ")
  (setq eol-mnemonic-mac       ":Mac ")
  (setq eol-mnemonic-unix      ":Unx ")
  (setq eol-mnemonic-undecided ":??? ")
  (global-set-key (kbd "C-\\") 'toggle-input-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フォント設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf font-settings
  :config
  ;; デフォルト フォント
  (set-face-attribute 'default nil :family "Ricty" :height 140)

  ;; プロポーショナル フォント
  (set-face-attribute 'variable-pitch nil :family "Ricty" :height 140)

  ;; 等幅フォント
  (set-face-attribute 'fixed-pitch nil :family "Ricty" :height 140)

  ;; ツールチップ表示フォント
  (set-face-attribute 'tooltip nil :family "Ricty" :height 140)

  ;; フォントサイズ調整
  (global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
  (global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
  (global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
  (global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))
  (global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 絵文字
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode)
  :bind
  ("C-x e" . emojify-insert-emoji))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 起動画面
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf startup
  :custom
  ((inhibit-startup-screen . t)
   (inhibit-startup-message . t)
   (inhibit-startup-echo-area-message . t)
   (inhibit-scratch-message . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スクリーン設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf screen-settings
  :setq ((initial-frame-alist . default-frame-alist)
         (frame-title-format quote ("emacs " emacs-version (buffer-file-name " - %f"))))
  :config
  (setq default-frame-alist
        (append
         '((width . 125)
           (height . 48)
           (left . 10)
           (top . 10)
           (line-spacing . 0)
           (left-fringe . 10)
           (right-fringe . 11)
           (menu-bar-lines . 1)
           (tool-bar-lines . 1)
           (vertical-scroll-bars . 1)
           (scroll-bar-width . 17)
           (cursor-type . box)
           (alpha . 100))
         default-frame-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スクロール設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf scroll-settings
  :custom
  (;; スクロールバー表示
   (scroll-bar-mode . nil)

   ;; スクロール時のカーソル位置を維持（有効：t、無効：nil）
   (scroll-preserve-screen-position . t)

   ;; スクロール開始の残り行数
   (scroll-margin . 0)

   ;; スクロール時の行数
   (scroll-conservatively . 10000)

   ;; スクロール時の行数（scroll-marginに影響せず）
   (scroll-step . 0)

   ;; 画面スクロール時の重複表示する行数
   (next-screen-context-lines . 1)

   ;; 横スクロール開始の残り列数
   (hscroll-margin . 1)

   ;; 横スクロール時の列数
   (hscroll-step . 1)

   ;; キー入力中の画面更新を抑止（有効：t、無効：nil）
   (redisplay-dont-pause . t)

   ;; recenter-top-bottomのポジション
   (recenter-positions . '(middle top bottom)))

  :init
  ;; バッファの最後までスクロールダウン
  (defadvice scroll-down (around scroll-down activate compile)
    (interactive)
    (let ((bgn-num (+ 1
                      (count-lines
                       (point-min)
                       (point)))))
      (if (< bgn-num
             (window-height))
          (goto-char (point-min))
        ad-do-it)))

  ;; バッファの先頭までスクロールアップ
  (defadvice scroll-up (around scroll-up activate compile)
    (interactive)
    (let ((bgn-num (+ 1
                      (count-lines
                       (point-min)
                       (point))))
          (end-num nil))
      (save-excursion
        (goto-char (point-max))
        (setq end-num (+ 1
                         (count-lines
                          (point-min)
                          (point)))))
      (if (<
           (-
            (- end-num bgn-num)
            (window-height))
           0)
          (goto-char (point-max))
        ad-do-it))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ウィンドウ制御
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf window-resizer
  :require t
  :bind
  ("C-c C-c C-r" . window-resizer)
  ("M-RET" . toggle-frame-fullscreen)
  ("C-S-h" . windmove-left)
  ("C-S-j" . windmove-down)
  ("C-S-k" . windmove-up)
  ("C-S-l" . windmove-right))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自動保存設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf auto-save-settings
  :custom
  `(;; 自動バックアップの実行有無
    (make-backup-files . t)

    ;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
    (backup-directory-alist . '(("." . ,my:d:backup)))

    ;; バックアップファイルへの番号付与
    (version-control . t)

    ;; 最新バックアップファイルの保持数
    (kept-new-versions . 3)

    ;; 最古バックアップファイルの保持数
    (kept-old-versions . 1)

    ;; バックアップファイル削除の実行有無
    (delete-old-versions . t)

    ;; 編集中ファイルのバックアップ（有効：t、無効：nil）
    (auto-save-list-file-name . nil)
    (auto-save-list-file-prefix . nil)

    ;; 編集中ファイルのバックアップ間隔（秒）
    (auto-save-timeout . 3)

    ;; 編集中ファイルのバックアップ間隔（打鍵）
    (auto-save-interval . 100)

    ;; 編集中ファイル（##）の格納ディレクトリ
    (auto-save-file-name-transforms . '((".*" ,my:d:tmp t)))

    ;; ロックファイルを生成（有効：t、無効：nil）
    (create-lockfiles . nil)))

;; その他各種ファイル格納ディレクトリ指定
(leaf *change-default-file-location
  :custom
  `((url-configuration-directory . ,(expand-file-name "url" my:d:tmp))
    (nsm-settings-file . ,(expand-file-name "nsm.data" my:d:tmp))
    (bookmark-default-file . ,(expand-file-name "bookmarks" my:d:var))
    (eshell-directory-name . ,(expand-file-name "eshell" my:d:tmp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GC設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf gc-settings
  :custom
  (;; GCの閾値を変更
   '(gc-cons-threshold . (* gc-cons-threshold 1.0))

   ;; GC実行時に通知
   (garbage-collection-messages . t))
  :config
  ;; Emacsがアイドル状態のとき、60秒ごとにGCを実行
  (run-with-idle-timer 60.0 t #'garbage-collect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitespace - 空白の強調表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf whitespace
  :ensure t
  :commands whitespace-mode
  :bind ("C-c W" . whitespace-cleanup)
  :custom ((whitespace-style . '(face
                                 trailing
                                 tabs
                                 spaces
                                 empty
                                 space-mark
                                 tab-mark))
           (whitespace-display-mappings . '((space-mark ?\u3000 [?\u25a1])
                                            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
           (whitespace-space-regexp . "\\(\u3000+\\)")
           (whitespace-global-modes . '(emacs-lisp-mode shell-script-mode sh-mode python-mode org-mode))
           (global-whitespace-mode . t))

  :config
  (set-face-attribute 'whitespace-trailing nil
                      :background "Black"
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background "Black"
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background "Black"
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background "Black"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hiwin - 未選択ウィンドウの色を変える
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (leaf hiwin :ensure t :init (hiwin-activate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; リモートファイル編集
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf tramp
  :defvar tramp-persistency-file-name
  :preface
  (custom-set-variables '(tramp-persistency-file-name (expand-file-name "tramp" my:d:tmp)))

  :custom
  `((tramp-default-method . "ssh")
    (tramp-persistency-file-name . ,(expand-file-name "tramp" my:d:tmp))
    (tramp-completion-reread-directory-timeout . nil))

  :hook
  (kill-emacs-hook
   . (lambda ()
       (if (file-exists-p tramp-persistency-file-name)
           (delete-file tramp-persistency-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf server
  :require t
  :when window-system
  :config
  (if (and (fboundp 'server-running-p)
           (not (server-running-p)))
      (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs機能補完パッケージ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t

  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf company
  :ensure t
  :defvar company-active-map
  :global-minor-mode global-company-mode
  :custom
  ((company-idle-delay . 0)
   (company-minimum-prefix-length . 1)
   (company-selection-wrap-around . t)
   (company-auto-expand . t)
   (company-transformers . '(company-sort-by-backend-importance)))

  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer)
  (global-set-key (kbd "C-M-i") 'company-complete)

  ;; 未選択項目
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "#ccc")

  ;; 未選択項目&一致文字
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "white" :background "#666")

  ;; 選択項目
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "white" :background "#666")

  ;; 選択項目&一致文字
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "#333")

  ;; スクロールバー
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "#eee")

  ;; スクロールバー背景
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "#333"))

(leaf company-web
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-jade)
  (add-to-list 'company-backends 'company-web-slim))

(leaf company-box
  :ensure t
  :after (company all-the-icons)
  :hook ((company-mode . company-box-mode))
  :custom
  ((company-box-icons-alist . 'company-box-icons-all-the-icons)
   (company-box-doc-enable . nil)))

(leaf all-the-icons
  :ensure t
  :custom
  (all-the-icons-scale-factor . 1.0))

(leaf projectile
  :ensure t
  :defun projectile-mode
  :bind ((projectile-mode-map
          ("s-p" . projectile-command-map))
         (projectile-mode-map
          ("C-c p" . projectile-command-map)))
  :require t
  :custom `((projectile-known-projects-file . ,(expand-file-name "projectile-bookmarks.eld" my:d:var)))
  :config
  (projectile-mode 1))

(leaf multiple-cursors
  :ensure t
  :bind (("M-e" . mc/edit-lines)
         ("M-n" . mc/mark-next-like-this)
         ("M-S-n" . mc/skip-to-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("M-S-p" . mc/skip-to-previous-like-this)
         ("M-s" . mc/mark-all-like-this)
         ("M-d" . mc/mark-all-like-this-dwim)
         ("M-t" . mc/mark-sgml-tag-pair)))

(leaf treemacs
  :ensure t
  :defvar
  (winum-keymap
   treemacs-deferred-git-apply-delay
   treemacs-python-executable
   treemacs-collapse-dirs
   treemacs-directory-name-transformer
   treemacs-display-in-side-window
   treemacs-eldoc-display
   treemacs-file-event-delay
   treemacs-last-period-regex-value
   treemacs-file-extension-regex
   treemacs-file-follow-delay
   treemacs-file-name-transformer
   treemacs-follow-after-init
   treemacs-git-command-pipe
   treemacs-goto-tag-strategy
   treemacs-indentation
   treemacs-indentation-string
   treemacs-is-never-other-window
   treemacs-max-git-entries
   treemacs-missing-project-action
   treemacs-move-forward-on-expand
   treemacs-no-png-images
   treemacs-no-delete-other-windows
   treemacs-project-follow-cleanup
   treemacs-persist-file
   treemacs-position
   treemacs-read-string-input
   treemacs-recenter-distance
   treemacs-recenter-after-file-follow
   treemacs-recenter-after-tag-follow
   treemacs-recenter-after-project-jump
   treemacs-recenter-after-project-expand
   treemacs-show-cursor
   treemacs-show-hidden-files
   treemacs-silent-filewatch
   treemacs-silent-refresh
   treemacs-sorting
   treemacs-space-between-root-nodes
   treemacs-tag-follow-cleanup
   treemacs-tag-follow-delay
   treemacs-user-mode-line-format
   treemacs-user-header-line-format
   treemacs-width
   treemacs-workspace-switch-cleanup)

  :defun
  (treemacs-follow-mode
   treemacs-filewatch-mode
   treemacs-fringe-indicator-mode
   treemacs-git-mode
   treemacs-icons-dired-mode
   treemacs-set-scope-type)

  :bind (("M-0" . treemacs-select-window)
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t t" . treemacs)
         ("C-x t B" . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag))
  :config
  (with-eval-after-load 'winum
    (define-key winum-keymap
      (kbd "M-0")
      #'treemacs-select-window))

  (with-eval-after-load 'treemacs
    (progn
      (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
            treemacs-deferred-git-apply-delay 0.5
            treemacs-directory-name-transformer #'identity
            treemacs-display-in-side-window t
            treemacs-eldoc-display t
            treemacs-file-event-delay 5000
            treemacs-file-extension-regex treemacs-last-period-regex-value
            treemacs-file-follow-delay 0.2
            treemacs-file-name-transformer #'identity
            treemacs-follow-after-init t
            treemacs-git-command-pipe ""
            treemacs-goto-tag-strategy 'refetch-index
            treemacs-indentation 2
            treemacs-indentation-string " "
            treemacs-is-never-other-window nil
            treemacs-max-git-entries 5000
            treemacs-missing-project-action 'ask
            treemacs-move-forward-on-expand nil
            treemacs-no-png-images nil
            treemacs-no-delete-other-windows t
            treemacs-project-follow-cleanup nil
            treemacs-persist-file (expand-file-name ".cache/treemacs-persist" my:d:var)
            treemacs-position 'left
            treemacs-read-string-input 'from-child-frame
            treemacs-recenter-distance 0.1
            treemacs-recenter-after-file-follow nil
            treemacs-recenter-after-tag-follow nil
            treemacs-recenter-after-project-jump 'always
            treemacs-recenter-after-project-expand 'on-distance
            treemacs-show-cursor nil
            treemacs-show-hidden-files t
            treemacs-silent-filewatch nil
            treemacs-silent-refresh nil
            treemacs-sorting 'alphabetic-asc
            treemacs-space-between-root-nodes t
            treemacs-tag-follow-cleanup t
            treemacs-tag-follow-delay 1.5
            treemacs-user-mode-line-format nil
            treemacs-user-header-line-format nil
            treemacs-width 35
            treemacs-workspace-switch-cleanup nil)
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode 'always)
      (pcase (cons
              (not (null (executable-find "git")))
              (not (null treemacs-python-executable)))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple))))))

(leaf treemacs-evil
  :ensure t
  :config
  (with-eval-after-load 'evil
    (eval-after-load 'treemacs
      '(require 'treemacs-evil nil nil))))

(leaf treemacs-projectile
  :ensure t
  :config
  (with-eval-after-load 'projectile
    (eval-after-load 'treemacs
      '(require 'treemacs-projectile nil nil))))

(leaf treemacs-icons-dired
  :ensure t
  :config
  (with-eval-after-load 'dired
    (eval-after-load 'treemacs
      '(progn
         (require 'treemacs-icons-dired nil nil)
         (treemacs-icons-dired-mode)
         t))))

(leaf treemacs-magit
  :ensure t
  :config
  (with-eval-after-load 'magit
    (eval-after-load 'treemacs
      '(require 'treemacs-magit nil nil))))

(leaf treemacs-persp
  :ensure t
  :config
  (with-eval-after-load 'persp-mode
    (eval-after-load 'treemacs
      '(progn
         (require 'treemacs-persp nil nil)
         (treemacs-set-scope-type 'Perspectives)
         t))))

(leaf yasnippet
  :ensure t
  :blackout yas-minor-mode
  :defun (yatemplate-fill-alist
          company-mode/backend-with-yas)
  :custom ((yas-indent-line . 'fixed)
           (yas-global-mode . t))

  :bind ((yas-keymap
          ("<tab>" . nil))
         (yas-minor-mode-map
          ("C-c y i" . yas-insert-snippet)
          ("C-c y n" . yas-new-snippet)
          ("C-c y v" . yas-visit-snippet-file)
          ("C-c y l" . yas-describe-tables)
          ("C-c y g" . yas-reload-all)))
  :config
  (leaf yasnippet-snippets :ensure t)

  (leaf yatemplate
    :ensure t
    :config
    (yatemplate-fill-alist))

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (defun set-yas-as-company-backend ()
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
  :hook
  ((company-mode-hook . set-yas-as-company-backend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf calendar
  :custom
  ((mark-holidays-in-calendar . t)
   (calendar-week-start-day . 0)))

(leaf japanese-holidays
  :after calendar
  :ensure t
  :custom
  ((calendar-mark-holidays-flag . t)
   (japanese-holiday-weekend . '(0 6))
   (japanese-holiday-weekend-marker . '(holiday nil nil nil nil nil japanese-holiday-saturday))
   (calendar-holidays . (append japanese-holidays holiday-local-holidays holiday-other-holidays)))

  :hook
  ((calendar-today-visible-hook . 'japanese-holiday-mark-weekend)
   (calendar-today-invisible-hook . 'japanese-holiday-mark-weekend)
   (calendar-today-visible-hook . 'calendar-mark-today)))

(leaf org-icalendar
  :custom
  ((org-agenda-include-diary . t)
   (org-icalendar-combined-description . "OrgModeスケジュール出力")
   (org-icalendar-timezone . "Asia/Tokyo")
   (org-icalendar-include-todo . t)
   (org-icalendar-use-scheduled . '(event-if-todo))
   (org-icalendar-use-deadline . '(event-if-todo))))

(leaf calfw
  :ensure t
  :custom-face
  ((cfw:face-day-title quote '((t :background "grey10")))
   (cfw:face-default-content quote '((t :foreground "green2")))
   (cfw:face-header quote '((t (:foreground "maroon2" :weight bold))))
   (cfw:face-holiday quote '((t :background "grey10" :foreground "purple" :weight bold)))
   (cfw:face-regions quote '((t :foreground "cyan")))
   (cfw:face-saturday quote '((t :foreground "blue" :weight bold)))
   (cfw:face-select quote '((t :background "blue4")))
   (cfw:face-sunday quote '((t :foreground "red" :weight bold)))
   (cfw:face-title quote '((t (:foreground "darkgoldenrod3" :weight bold :height 2.0 :inherit variable-pitch))))
   (cfw:face-today quote '((t :foreground: "cyan" :weight bold)))
   (cfw:face-today-title quote '((t :background "red4" :weight bold)))))

(leaf calfw-org :ensure t)
(leaf calfw-ical :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google翻訳
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf google-translate
  :ensure t
  :require t
  :config
  (defvar google-translate--english-chars "[:ascii:]’“”–")
  (defvar google-translate-translation-directions-alist)

  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

  (setq google-translate-translation-directions-alist
        '(("en" . "ja") ("ja" . "en")))

  (global-set-key (kbd "C-c C-t") 'google-translate-smooth-translate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WEB制作
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf web-mode
  :ensure t
  :defvar
  (web-mode-markup-indent-offset
   web-mode-css-indent-offset
   web-mode-code-indent-offset
   web-mode-style-padding
   web-mode-script-padding
   web-mode-block-padding
   web-mode-enable-auto-indentation
   web-mode-enable-current-element-highlight
   web-mode-enable-current-column-highlight
   web-mode-enable-auto-pairing
   web-mode-tag-auto-close-style
   web-mode-engines-alist
   web-mode-enable-engine-detection
   web-mode-content-types-alist)

  :preface
  (defun my-web-mode-hook nil
    "Hooks for Web mode."
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
    (setq web-mode-tag-auto-close-style 2)
    (setq web-mode-enable-engine-detection t))

  :setq
  (web-mode-engines-alist . '(("php" . "\\.phtml")
                              ("blade" . "\\.blade\\.")
                              ("jsx" . "\\.[jt]s\\'")))
  (web-mode-content-types-alist . '(("jsx" . "\\.[jt]s")))

  :mode (("\\.html$" . web-mode)
         ("\\.ejs$" . web-mode)
         ("\\.[agj]sp$" . web-mode)
         ("\\.as[cp]x$" . web-mode)
         ("\\.erb$" . web-mode)
         ("\\.mustache$" . web-mode)
         ("\\.vue$" . web-mode)
         ("\\.[jt]sx$" . web-mode)
         ("\\.[jt]s$" . web-mode)
         ("\\.phtml$" . web-mode)
         ("\\.tpl\\.php$" . web-mode))

  :hook ((web-mode-hook . my-web-mode-hook))

  :custom-face
  ((web-mode-current-element-highlight-face quote ((t (:background "#3e3c36" :underline (:color "#bbcc66" :style line)))))
   (web-mode-doctype-face quote ((t (:inherit web-mode-block-delimiter-face))))
   (web-mode-html-attr-equal-face quote ((t (:foreground "white"))))
   (web-mode-html-attr-name-face quote ((t (:inherit font-lock-variable-name-face))))
   (web-mode-html-tag-face quote ((t (:inherit font-lock-function-name-face))))))

(leaf emmet-mode
  :ensure t
  :defvar emmet-indentation emmet-snippets emmet-preferences
  :custom ((emmet-self-closing-tag-style . ""))
  :bind ((emmet-mode-keymap
          ("C-;" . emmet-expand-line)))
  :hook ((sgml-mode-hook . emmet-mode)
         (web-mode-hook . emmet-mode)
         (markdown-mode-hook . emmet-mode)
         (css-mode-hook . emmet-mode)
         (xml-mode-hook . emmet-mode))
  :config
  (add-hook 'emmet-mode-hook (lambda nil (setq emmet-indentation 2)))
  (with-eval-after-load '"emmet-mode"
    (define-key emmet-mode-keymap (kbd "C-j") nil)

    (puthash "img"
             (let ((tbl (make-hash-table :test 'equal)))
               (puthash "block" nil tbl)
               (puthash "defaultAttr"
                        (let ((tbl (make-hash-table :test 'equal)))
                          (puthash "src" "" tbl)
                          (puthash "alt" "" tbl)
                          tbl)
                        tbl)
               (puthash "selfClosing" t tbl)
               tbl)
             (gethash "tags" (gethash "html" emmet-preferences)))

    (puthash "!!!" "<!DOCTYPE html>" (gethash "snippets" (gethash "html" emmet-snippets)))
    (puthash "doc" "html>(head>meta[charset=utf-8]+title{Document})+body" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:4s" "!!!4s+doc4[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:4t" "!!!4t+doc4[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:5" "!!!+doc[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:xml" "html[xmlns=http://www.w3.org/1999/xhtml]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:xs" "!!!xs+doc4[xmlns=http://www.w3.org/1999/xhtml xml:lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))

    (puthash "my:doc"
             (concat
              "html>(head>meta[charset=utf-8]+title{Document})+body>div.site-wrapper>header.site-header>h1.site-title{Document}^nav.site-navigation+main.site-body+footer.site-footer>p.site-copyright{&copy;"
              (format-time-string "%Y" (current-time))
              " Yap-house.}")
             (gethash "aliases" (gethash "html" emmet-snippets)))

    (puthash "my:!" "!!!+my:doc" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "my:bc" "div.site-breadcrumbs>ol.list>li.item>a" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "my:toc" "div.page-toc>ul.list>li.item>a[href=#]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "my:cols" "div.mdl-columns>div.item*2" (gethash "aliases" (gethash "html" emmet-snippets)))))

(leaf vue-mode :ensure t)

(leaf css-mode
  :defvar css-indent-offset
  :config
  (add-hook 'css-mode-hook
            '(lambda nil
               (setq tab-width 2
                     css-indent-offset 2
                     indent-tabs-mode nil))))

(leaf scss-mode
  :ensure t
  :defvar
  (css-indent-offset
   scss-compile-at-save)
  :preface
  (defun scss-custom nil
    "scss-mode-hook"
    (and
     (set (make-local-variable 'css-indent-offset) 2)
     (set (make-local-variable 'scss-compile-at-save) nil)))

  :mode (("\\.scss$" . scss-mode))
  :config
  (add-hook 'scss-mode-hook
            '(lambda nil (scss-custom))))

;; (leaf js2-mode
;;   :ensure t
;;   :mode (("\\.js$" . js2-mode))
;;   :config
;;   (add-hook 'js2-mode-hook
;;             '(lambda nil
;;                (setq js2-basic-offset 2
;;                      tab-width 2
;;                      indent-tabs-mode nil
;;                      js2-cleanup-whitespace nil))))

(leaf js-doc
  :ensure t
  :defvar js-doc-mail-address
  :custom
  ((js-doc-license . "The MIT License")
   (js-doc-mail-address . "akamaru.tkm@gmail.com"))

  :config
  (customize-set-variable 'js-doc-author
                          (format "takama okazaki <%s>" js-doc-mail-address))
  (add-hook 'js2-mode-hook
            '(lambda nil
               (local-set-key "C-ci" 'js-doc-insert-function-doc)
               (local-set-key "@" 'js-doc-insert-tag))))

(leaf php-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook
            '(lambda nil
               (setq tab-width 2)
               (setq indent-tabs-mode nil)
               (setq c-basic-offset 2))))

(leaf highlight-indent-guides
  :ensure t
  :blackout t
  :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))
  :custom (
           (highlight-indent-guides-method . 'character)
           (highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . ?\|)))

(leaf rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode-hook . rainbow-delimiters-mode)))

(leaf py-isort :ensure t)

(leaf elpy
  :ensure t
  :defun (elpy-enable)
  :init
  (elpy-enable)

  :config
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
  (remove-hook 'elpy-modules 'elpy-module-flymake)

  :custom
  (flycheck-python-flake8-executable . "flake8")

  :bind (elpy-mode-map
         ("C-c C-r f" . elpy-format-code))
  :hook ((elpy-mode-hook . flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf markdown-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf visual-basic-mode
  :require t
  :commands visual-basic-mode
  :config
  (setq auto-mode-alist (append
                         '(("\\.vb$\\|\\.bas$\\|\\.frm$\\|\\.cls$" . visual-basic-mode))
                         auto-mode-alist))
  (add-hook 'visual-basic-mode-hook
            '(lambda nil
               (setq tab-width 2
                     visual-basic-mode-indent 2
                     indent-tabs-mode nil))))

(leaf vbasense
  :ensure t
  :defvar
  (vbasense-popup-help-key
   vbasense-jump-to-definition-key)

  :defun vbasense-config-default
  :setq ((vbasense-popup-help-key . "C-:")
         (vbasense-jump-to-definition-key . "C->"))
  :config
  (vbasense-config-default)
  (add-hook 'vbasense-hook
            '(lambda nil
               (setq c-basic-offset 2
                     tab-width 2
                     indent-tabs-mode nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf sql
  :require t
  :defvar sql-indent-offset
  :defun sql-set-product
  :init
  (leaf sql-indent :ensure t)

  :setq ((sql-indent-offset . 2))
  :config
  (eval-after-load "sql"
    (load-library "sql-indent"))

  (sql-set-product "mysql"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANSI escape sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf ansi-color
  :ensure t
  :defun ansi-color-apply-on-region
  :preface
  (defun display-ansi-colors nil
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       (point-min)
       (point-max))))

  :config
  (add-hook 'compilation-filter-hook
            '(lambda nil
               (ansi-color-apply-on-region
                (point-min)
                (point-max)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf org-mode
  :defvar (org-startup-truncated
           org-image-actual-width
           org-export-latex-coding-system
           org-latex-pdf-process
           org-latex-default-class
           org-latex-with-hyperref
           org-latex-classes
           org-latex-packages-alist)
  :setq
  ((org-startup-truncated . nil)
   (org-image-actual-width . nil)
   (org-latex-pdf-process . '("platex %f" "dvipdfmx %b.dvi"))
   (org-latex-default-class . "jsarticle")
   (org-latex-with-hyperref . nil)
   (org-export-latex-coding-system . 'utf-8)
   (org-latex-classes . nil)
   (org-latex-packages-alist . '(("AUTO" "inputenc" nil)
                                 ("T1" "fontenc" t)
                                 ("dvipdfmx" "graphicx" t)
                                 ("dvipdfmx" "color" t))))

  :config
  (add-to-list 'org-latex-classes
               '("jsarticle"
                 "\\documentclass[a4j]{jsarticle}
[NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height . 15)))

(leaf doom-themes
  :require t
  :defvar (doom-themes-enable-bold doom-themes-enable-italic doom-themes-treemacs-theme)
  :setq
  ((doom-themes-enable-bold . t)
   (doom-themes-enable-italic . t)
   (doom-themes-treemacs-theme . "doom-colors"))
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(provide 'init)
;;; init.el ends here
