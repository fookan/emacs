;; melpa;
;;;  ******************************* // package-list

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)


;;; ******************************* // 基本設定

;; load-pathに追加
(add-to-list 'load-path "~/.emacs.d/mylisp")

;;; 環境を日本語、UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; バックアップファイルを作成させない
(setq make-backup-files nil)

;;backupfileを作らない ~ and ##
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 終了時にオートセーブファイルを削除する
;;(setq delete-auto-save-files t)

;; タブにスペースを使用する
;(setq-default tab-width 4 indent-tabs-mode nil)

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; 複数ウィンドウを禁止する
(setq ns-pop-up-frames nil)

;; ウィンドウを透明にする
;; アクティブウィンドウ／非アクティブウィンドウ（alphaの値で透明度を指定）
;;(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))

;; メニューバーを消す
(menu-bar-mode -1)

;; ツールバーを消す
(tool-bar-mode -1)

;; 時間を表示
(display-time)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)

;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;; 1行あたり８０文字こえたら色を付ける
;(setq fill-column 180)
;(global-font-lock-mode t)

;; カーソル行をハイライトする
(global-hl-line-mode t)

;; 対応する括弧を光らせる
;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 選択範囲を光らせる
(transient-mark-mode t)

;; 画面右端で折り返さない
(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)

;; 現在の関数名をモードラインに表示
(which-function-mode 1)

;; バッファの最後でnewlineで新規行を追加するのを禁止する
;(setq next-line-add-newlines nil)

;; スペース、タブなどを可視化する
;(global-whitespace-mode 1)
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")
;; タブの色
(set-face-foreground 'whitespace-tab "DarkRed")
(set-face-underline  'whitespace-tab t)
(set-face-background 'whitespace-tab nil)

;;バッファの終端を明示
;(setq-default indicate-empty-lines t)

;; スクロールは１行ごとに
(setq scroll-conservatively 1)

;;改行時の自動インデントを無効
(electric-indent-mode -1)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)

;; シフト＋矢印で範囲選択
;;(setq pc-select-selection-keys-only t)
;;(pc-selection-mode 1)

;;; dired設定
(require 'dired-x)

;; "yes or no" の選択を "y or n" にする
;;(fset 'yes-or-no-p 'y-or-n-p)

;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)


;;; ******************************* // キーの設定

;; Macのキーバインドを使う
;;(mac-key-mode 1)

;; Macのoptionをメタキーにする
(setq mac-option-modifier 'meta)

;;バックスペース ctrl-h
(keyboard-translate ?\C-h ?\C-?)

;; C-kで行全体を削除する
(setq kill-whole-line t)

;;ページ戻し
(define-key global-map(kbd "C-z")'scroll-down-command)

;; \C-\の日本語入力の設定を無効にする
(define-key global-map "\C-\\" nil)

;; reload buffer
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)


;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)
;; RET 標準の dired-find-file では dired バッファが複数作られるので
;; dired-find-alternate-file を代わりに使う
;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
;(defun dired-open-in-accordance-with-situation ()
;  (interactive)
;  (let ((file (dired-get-filename)))
;    (if (file-directory-p file)
;        (dired-find-alternate-file)
;      (dired-find-file))))
;(define-key dired-mode-map (kbd "RET") 'dired-open-in-accordance-with-situation)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "a") 'dired-find-file)


;;; **************************************** // package

;; --- editor-config
(editorconfig-mode 1)

;; --- popup
(require 'popup)
(require 'popup-select-window)
(define-key global-map(kbd "C-x C-o")'popup-select-window)

;; -------- flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; -------- auato-complete
(require 'auto-complete)
(require 'auto-complete-config)
;(require 'fuzzy) ;; fuzzy search (heaby)
(setq ac-use-fuzzy t)
(global-auto-complete-mode t)
(ac-config-default)
(setq ac-delay 0) ;; 補完候補表示までの時間
(setq ac-auto-show-menu 0.5) ;; ヒント表示までの時間
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)
(setq ac-menu-height 25) ;; ちょっと大きめにとりましょう！
(setq ac-auto-start 2) ;; 個人的には3でもいいかも
(setq ac-ignore-case t)
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
;(global-set-key ac-complete-mode-map "\M-TAB" 'ac-next)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; undo-list
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;; --- undo-tree:
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(put 'upcase-region 'disabled nil)


;; --- undo-hist:
(require 'undohist)
(undohist-initialize)

; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files '("~/tmp/.undohist" "COMMIT_EDITMSG"))

;; --- gtags
(require 'gtags)
;;キーバインドの設定
(setq gtags-mode-hook
    '(lambda ()
        (local-set-key "\M-t" 'gtags-find-tag)    ;関数へジャンプ
        (local-set-key "\M-r" 'gtags-find-rtag)   ;関数の参照元へジャンプ
        (local-set-key "\M-s" 'gtags-find-symbol) ;変数の定義元/参照先へジャンプ
        (local-set-key "\M-q" 'gtags-pop-stack)   ;前のバッファに戻る
        ))
;;(global-set-key "\M-t" 'gtags-find-tag)
;;(global-set-key "\M-r" 'gtags-find-rtag)
;;(global-set-key "\M-s" 'gtags-find-symbol)
;;(symbol-set-key "\C-t" 'gtags-pop-stack)
;;(define-key global-map(kbd "C-x t")'gtags-pop-stack)
;;ファイル保存時にタグの更新をする
(defun my-update-gtags ()
  (let* ((file (buffer-file-name (current-buffer)))
     (dir (directory-file-name (file-name-directory file))))
    (when (executable-find "global")
      (start-process "gtags-update" nil
             "global" "-uv"))))

;;(add-hook 'after-save-hook
;;      'my-update-gtags)

;;(add-hook 'after-save-hook
;;    'ac-php-remake-tags-all)

;呼び出すモード
;(add-hook 'php-mode-hook 'gtags-mode)

;; php関連
; 関数名補完設定
(add-hook 'php-mode-hook '(lambda ()
                           (auto-complete-mode t)
                           (require 'ac-php)
                           (setq ac-sources  '(ac-source-php ) )
                           (yas-global-mode 1)
                           (gtags-mode t)
                           (add-hook 'after-save-hook 'ac-php-remake-tags-all)
                           (add-hook 'after-save-hook 'my-update-gtags)
;                           (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
;                           (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
                           ))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
      (quote
          (web-mode undohist undo-tree magit flycheck editorconfig ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;*******************************// web-mode
(require 'web-mode)
;; 拡張子の設定
;;(add-to-list 'auto-mode-alist '("\\.php$"     . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.tpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp?$"     . web-mode))
