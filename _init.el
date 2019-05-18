;;; ------------------------------基本設定
;; ロードパスの設定
(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/mylisp"
                   "~/.emacs.d/packages")
                 load-path))

;; Emacs package system
;(require 'package)
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;(package-initialize)

;;******************************* //基本設定

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

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)

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
(global-font-lock-mode t)

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
(setq next-line-add-newlines nil)

;; スペース、タブなどを可視化する
(global-whitespace-mode 1)

;;バッファの終端を明示
(setq-default indicate-empty-lines t)

;; スクロールは１行ごとに
(setq scroll-conservatively 1)

;;改行時の自動インデントを無効
(electric-indent-mode -1)


;;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)

;; シフト＋矢印で範囲選択
;;(setq pc-select-selection-keys-only t)
;;(pc-selection-mode 1)

;;; dired設定
;;(require 'dired-x)

;; "yes or no" の選択を "y or n" にする
;;(fset 'yes-or-no-p 'y-or-n-p)

;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)

;;******************************* //キーの設定
;; Macのキーバインドを使う
;;(mac-key-mode 1)

;; Macのoptionをメタキーにする
;;(setq mac-option-modifier 'meta)

;;バックスペース ctrl-h
(keyboard-translate ?\C-h ?\C-?)

;; C-kで行全体を削除する
(setq kill-whole-line t)

;;ページ戻し
;;(define-key global-map(kbd "C-z")'scroll-down-command)

;; \C-\の日本語入力の設定を無効にする
(define-key global-map "\C-\\" nil)

;; reload buffer
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)



;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;; 各パッケージの設定
;;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/


;;******************************* //popupwindow
;(require 'popup)
;(require 'popup-select-window)
;(define-key global-map(kbd "C-x C-o")'popup-select-window)


;;******************************* //auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
;(ac-set-trigger-key "TAB")
;(setq ac-use-menu-map t)
;(setq ac-menu-height 25) ;; ちょっと大きめにとりましょう！
;(setq ac-auto-start 2) ;; 個人的には3でもいいかも
;(setq ac-ignore-case t)
;(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
;;(global-set-key ac-complete-mode-map "\M-TAB" 'ac-next)
;(define-key ac-complete-mode-map "\C-n" 'ac-next)
;(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; ac-source（要するにどうやって補完候補を選ぶか）
;(setq-default ac-sources 'ac-source-words-in-same-mode-buffers)
;(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))


;;******************************* //flycheck
; 構文エラーチェック設定
(add-hook 'after-init-hook #'global-flycheck-mode)


;;******************************* //php-mode
;; 関数名補完設定
(add-hook 'php-mode-hook '(lambda ()
                           (auto-complete-mode t)
                           (require 'ac-php)
                           (setq ac-sources  '(ac-source-php ) )
                           (yas-global-mode 1)

                           (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
                           (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
                           ))

;;*******************************//;undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(put 'upcase-region 'disabled nil)


;;*******************************//;undo-hist
(require 'undohist)
(undohist-initialize)
;;; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files
      '("~/tmp/.undohist" "COMMIT_EDITMSG"))
(custom-set-variables
 '(package-selected-packages
      (quote
          (web-mode undohist undo-tree neotree magit fuzzy flycheck editorconfig dired-toggle dired-subtree ac-php))))
(custom-set-faces
 )
