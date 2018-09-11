;; Emacs package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;language
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
;;(set-language-environment "UTF-8")

;; load-pathに追加
(add-to-list 'load-path "~/.emacs.d/mylisp")




;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)

;;backupfileを作らない ~ and ##
(setq make-backup-files nil)
(setq auto-save-default nil)

;;改行時の自動インデントを無効
(electric-indent-mode -1)

;;popupwindow
(require 'popup)
(require 'popup-select-window)
;;(global-set-key "\C-xo" 'popup-select-window)
(define-key global-map(kbd "C-x C-o")'popup-select-window)

;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;; ssh
;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
(require 'tramp)
(setq tramp-default-method "ssh")
;;https://qiita.com/miyakou1982/items/d05e1ce07ad632c94720
;;(add-to-list 'tramp-default-proxies-alist
;;             '(nil "\\`root\\'" "/ssh:%h:"))
;;(add-to-list 'tramp-default-proxies-alist
;;             '("localhost" nil nil))
;;(add-to-list 'tramp-default-proxies-alist
;;             '((regexp-quote (system-name)) nil nil))


;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;;  キーバインド
;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

;;ctrl-h
(keyboard-translate ?\C-h ?\C-?)
;;Ctrl+y
;;(define-key global-map(kbd "C-z")'undo)
(define-key global-map(kbd "C-z")'scroll-down-command)
;;(define-key global-map(kbd "\C-m")'scroll-down-command)
;;(define-key global-map(kbd "\C-c")'copy-region-as-kill)
;;(define-key global-map(kbd "C-v")'yank)

;; reload buffer
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;; window関連
;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

;; 対応するカッコを強調表示
(show-paren-mode t)

;; 時間を表示
(display-time)

;;行数を表示
(global-linum-mode t)

;; 列数を表示する
(column-number-mode t)


;;バッファの終端を明示
(setq-default indicate-empty-lines t)

;;選択したところをハイライト
(setq transient-mark-mode t)

;; 自動で色を付ける設定
(global-font-lock-mode t)

;;アクティブバッファの色を変える
(when (require 'hiwin nil t)
  (hiwin-activate)                            ;; hiwin-modeを有効化
  (set-face-background 'hiwin-face "gray10"))  ;; 非アクティブバッファの背景色を設定
;; アクティブウィンドウ／非アクティブウィンドウ（alphaの値で透明度を指定）
;;(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))


;;カーソル行のハイライト
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "OliveDrab1"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;; (setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)

;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;;auto-complet
;;install
;; auto-complete
;; fuzzy
;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
(require 'auto-complete)
(require 'auto-complete-config)
(require 'fuzzy) ;; fuzzy search (heaby)
(setq ac-use-fuzzy t)
(global-auto-complete-mode t)
(ac-config-default)
(setq ac-delay 0) ;; 補完候補表示までの時間
(setq ac-auto-show-menu 0.05) ;; ヒント表示までの時間
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)
(setq ac-menu-height 25) ;; ちょっと大きめにとりましょう！
(setq ac-auto-start 2) ;; 個人的には3でもいいかも
(setq ac-ignore-case t)
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
;(global-set-key ac-complete-mode-map "\M-TAB" 'ac-next)
;(define-key ac-complete-mode-map "\C-n" 'ac-next)
;(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; ac-source（要するにどうやって補完候補を選ぶか）
(setq-default ac-sources 'ac-source-words-in-same-mode-buffers)
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))



;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;;ac-php
;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
(add-hook 'php-mode-hook '(lambda ()
                           (auto-complete-mode t)
                           (require 'ac-php)
                           (setq ac-sources  '(ac-source-php ) )
                           (yas-global-mode 1)

                           (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
                           (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
                           ))

;;(require 'cl)
;;(add-hook 'php-mode-hook
;            '(lambda ()
;               (auto-complete-mode t)
;               (require 'ac-php)
;               (setq ac-sources '(ac-source-php ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;               (yas-global-mode 1)
;               (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
;               (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
;               ))


;//////////////////////////////////////////////
;;flycheck
;//////////////////////////////////////////////
(add-hook 'after-init-hook 'global-flycheck-mode)

(when (require 'flycheck nil 'noerror)
  (custom-set-variables
   ;; エラーをポップアップで表示
   '(flycheck-display-errors-function
     (lambda (errors)
       (let ((messages (mapcar #'flycheck-error-message errors)))
         (popup-tip (mapconcat 'identity messages "\n")))))
   '(flycheck-display-errors-delay 0.5))
  (define-key flycheck-mode-map (kbd "C-M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-M-p") 'flycheck-previous-error)
  (add-hook 'c-mode-common-hook 'flycheck-mode))

;; 改行コードを表示する
;;(setq eol-mnemonic-dos "(CRLF)")
;;(setq eol-mnemonic-mac "(CR)")
;;(setq eol-mnemonic-unix "(LF)")

;;ウインドウのアルファ化
;;(add-to-list 'default-frame-alist '(alpha . 50))

;;Window
(if window-system
    (progn
      ;; 文字の色を設定します。
;;      (add-to-list 'default-frame-alist '(foreground-color . "VioletRed1"))
      ;; 背景色を設定します。
;;      (add-to-list 'default-frame-alist '(background-color . "black"))
      ;; カーソルの色を設定します。
;;      (add-to-list 'default-frame-alist '(cursor-color . "snow"))
      ;; マウスポインタの色を設定します。
;      (add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
      ;; モードラインの文字の色を設定します。
;      (set-face-foreground 'modeline "white")
      ;; モードラインの背景色を設定します。
;      (set-face-background 'modeline "MediumPurple2")
      ;; 選択中のリージョンの色を設定します。
;      (set-face-background 'region "LightSteelBlue1")
      ;; モードライン（アクティブでないバッファ）の文字色を設定します。
;      (set-face-foreground 'mode-line-inactive "gray30")
      ;; モードライン（アクティブでないバッファ）の背景色を設定します。
;      (set-face-background 'mode-line-inactive "gray85")
      ))



;; 空白などの可視化
;; whitespace
;;
;;(require 'whitespace)
;;(setq whitespace-style '(face           ; faceで可視化
;;                         trailing       ; 行末
;;                         tabs           ; タブ
;;;;                         empty          ; 先頭/末尾の空行
;;                         space-mark     ; 表示のマッピング
;;                         tab-mark
;;                         ))
;;(setq whitespace-display-mappings
;;      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
;;(global-whitespace-mode 1)

;; 空白文字を強制表示
;;part1
;;(require 'whitespace)
;;;; see whitespace.el for more details
;;(setq whitespace-style '(face tabs tab-mark spaces space-mark))
;;(setq whitespace-display-mappings
;;      '((space-mark ?\u3000 [?\u25a1])
;;        ;; WARNING: the mapping below has a problem.
;;        ;; When a TAB occupies exactly one column, it will display the
;;        ;; character ?\xBB at that column followed by a TAB which goes to
;;        ;; the next TAB column.
;;        ;; If this is a problem for you, please, comment the line below.
;;        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])))
;;(setq whitespace-space-regexp "\\(\u3000+\\)")
;;(set-face-foreground 'whitespace-tab "#adff2f")
;;(set-face-background 'whitespace-tab 'nil)
;;(set-face-underline  'whitespace-tab t)
;;(set-face-foreground 'whitespace-space "#7cfc00")
;;(set-face-background 'whitespace-space 'nil)
;;(set-face-bold-p 'whitespace-space t)
;;(global-whitespace-mode 1)
;;(global-set-key (kbd "C-x w") 'global-whitespace-mode)
;;part2
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")
;; タブの色
(set-face-foreground 'whitespace-tab "DarkRed")
(set-face-underline  'whitespace-tab t)
(set-face-background 'whitespace-tab nil)

;;setting#3
;;(progn
;;  (require 'whitespace)
;;  (setq whitespace-style
;;        '(
;;          face ; faceで可視化
;;          trailing ; 行末
;;          tabs ; タブ
;;          spaces ; スペース
;;          space-mark ; 表示のマッピング
;;          tab-mark
;;          ))
;;  (setq whitespace-display-mappings
;;        '(
;;          (space-mark ?\u3000 [?\u2423])
;;          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
;;          ))
;;  (setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
;;  (setq whitespace-space-regexp "\\(\u3000+\\)")
;;  (set-face-attribute 'whitespace-trailing nil
;;                      :foreground "RoyalBlue4"
;;                      :background "RoyalBlue4"
;;                      :underline nil)
;;  (set-face-attribute 'whitespace-tab nil
;;                      :foreground "yellow4"
;;                      :background "yellow4"
;;                      :underline nil)
;;  (set-face-attribute 'whitespace-space nil
;;                      :foreground "gray40"
;;                      :background "gray20"
;;                      :underline nil)
;;  (global-whitespace-mode t)
;;  )


;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;; 日本語入力設定定
;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

;;part1
;; (set-language-environment "UTF-8") ;; UTF-8 でも問題ないので適宜コメントアウトしてください
;;(setq default-input-method "W32-IME")
;;(setq-default w32-ime-mode-line-state-indicator "[--]")
;;(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
;;(w32-ime-initialize)
;;part2
;;(global-set-key [kanji] 'toggle-input-method)
;;(add-hook 'w32-ime-on-hook (function (lambda () (set-cursor-color "#ff0000"))))
;;(add-hook 'w32-ime-off-hook (function (lambda () (set-cursor-color "#0000ff"))))




;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;; 各パッケージ毎の設定
;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/


;;*******************************//gtags

(require 'gtags)
;;キーバインドの設定
(global-set-key "\M-t" 'gtags-find-tag)
(global-set-key "\M-r" 'gtags-find-rtag)
(global-set-key "\M-s" 'gtags-find-symbol)
;;(symbol-set-key "\C-t" 'gtags-pop-stack)
;;(define-key global-map(kbd "C-x t")'gtags-pop-stack)
;;ファイル保存時にタグの更新をする
(defun my-c-mode-update-gtags ()
  (let* ((file (buffer-file-name (current-buffer)))
     (dir (directory-file-name (file-name-directory file))))
    (when (executable-find "global")
      (start-process "gtags-update" nil
             "global" "-uv"))))
(add-hook 'after-save-hook
      'my-c-mode-update-gtags)


;;*******************************//neotree
;;(add-to-list 'load-path "必要なパス指定")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;*******************************//editorconfig
;;editorconfig
(editorconfig-mode 1)

;;*******************************//dired関連
;;(require 'dired-subtree)
;;; iを置き換え
;;(define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
;;; org-modeのようにTABで折り畳む
;;(define-key dired-mode-map (kbd "<tab>") 'dired-subtree-remove)
;;; C-x n nでsubtreeにナローイング
;;(define-key dired-mode-map (kbd "C-x n n") 'dired-subtree-narrow)

;;; ファイル名以外の情報を(と)で隠したり表示したり
;;(require 'dired-details)
;;(dired-details-install)
;;(setq dired-details-hidden-string "")
;;(setq dired-details-hide-link-targets nil)
;;(setq dired-details-initially-hide nil)

;;; dired-subtreeをdired-detailsに対応させる
;;(defun dired-subtree-after-insert-hook--dired-details ()
;;  (dired-details-delete-overlays)
;;  (dired-details-activate))
;;(add-hook 'dired-subtree-after-insert-hook
;;          'dired-subtree-after-insert-hook--dired-details)

;; find-dired対応
;;(defadvice find-dired-sentinel (after dired-details (proc state) activate)
;;  (ignore-errors
;;    (with-current-buffer (process-buffer proc)
;;      (dired-details-activate))))
;; (progn (ad-disable-advice 'find-dired-sentinel 'after 'dired-details) (ad-update 'find-dired-sentinel))


;;*******************************//php-mode
(require 'php-mode)
(add-to-list 'auto-mode-alist
	     '("\\.php$" . php-mode)) ;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-display-errors-function
   (lambda
     (errors)
     (let
	 ((messages
	   (mapcar
	    (function flycheck-error-message)
	    errors)))
       (popup-tip
	(mapconcat
	 (quote identity)
	 messages "
")))))
 '(package-selected-packages
   (quote
    (undo-tree undohist flycheck ac-php fuzzy auto-complete dired-subtree dired-toggle ssh popup web-mode php-mode neotree editorconfig))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;*******************************// web-mode
(require 'web-mode)
;; 拡張子の設定
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

;*******************************//;undo-tre
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(put 'upcase-region 'disabled nil)

;*******************************//;undo-hist
(require 'undohist)
(undohist-initialize)
;;; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files
      '("~/tmp/.undohist" "COMMIT_EDITMSG"))
