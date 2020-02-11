;;; require
(require 'package)

;;;package
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; 環境を日本語、UTF-8にする
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; shellの環境変数を引き継ぐ
(exec-path-from-shell-initialize)
;(setq exec-path (parse-colon-path (getenv "PATH")))
;(setq eshell-path-env (getenv "PATH"))

;; バックアップファイルを作成させない
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; 複数ウィンドウを禁止する
(setq ns-pop-up-frames nil)

;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; ツールバーを消す
(tool-bar-mode -1)

; メニューを消す
(menu-bar-mode -1)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)

;; 時間を表示
(display-time)

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;; カーソル行をハイライトする
(global-hl-line-mode t)

;;改行時の自動インデントを無効
(electric-indent-mode -1)

;; タブにスペースを使用する
(setq-default tab-width 4 indent-tabs-mode nil)

;; スペース、タブなどを可視化する
;(global-whitespace-mode 1)

;;バッファの終端を明示
;(setq-default indicate-empty-lines t)

;; 選択範囲を光らせる
(transient-mark-mode t)

;; 対応する括弧を光らせる
;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;; スクロールは１行ごとに
(setq scroll-conservatively 1)

;;バックスペース ctrl-h
(keyboard-translate ?\C-h ?\C-?)

;; C-kで行全体を削除する
(setq kill-whole-line t)

;; C-c l 改行のとぐる
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; ういんどう切替
(define-key global-map (kbd "<C-tab>") 'other-window)

;; reload buffer
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive) (revert-buffer t t))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
