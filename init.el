;;; init.el:
;;; prepare:
;; gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40

;;; package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; 環境を日本語、UTF-8にする
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; shellの環境変数を引き継ぐ
; (setq exec-path (parse-colon-path (getenv "PATH")))
; (setq eshell-path-env (getenv "PATH"))

; バックアップファイルとオートセーブファイの保存場所
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*",(expand-file-name "~/.emacs.d/backups/") t)))

;; バックアップファイルを作成させない場合
; (setq make-backup-files nil)

;; オートセーブフィルを作る間隔
(setq auto-save-timeout 15)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; 複数ウィンドウを禁止する
(setq ns-pop-up-frames nil)

;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; ツールバーを消す
; (tool-bar-mode -1)

; メニューを消す
(menu-bar-mode -1)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)

;; 日付と時刻を表示
(setq display-time-string-forms
 '((format "%s/%s(%s)%s:%s"
		 month day dayname
		 24-hours minutes
   )))
(setq display-time-day-and-date t)
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
(global-whitespace-mode 1)

;;バッファの終端を明示
(setq-default indicate-empty-lines t)

;; カッコの自動挿入
(electric-pair-mode 1)

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

;; mark set
(define-key global-map (kbd "C-SPC") 'set-mark-command)
(define-key global-map (kbd "C-@") 'set-mark-command)

;; 日本語入力
;; tips
; C-\で標準のKKCになる
; SPACEで変換モードに入る。変換モード中に以下の操作ができる
;  C-i 選択範囲をつめる。
;  C-o 選択範囲を広げる
;  l 候補一覧をページ送り
; Kを押すとカタカナに変換する

;; ctrl-shift-sapceで言語切替できるようにする。
; (global-set-key (kbd "C-S-SPC") 'toggle-input-method)

;; color-theme-sanityinc-tomorrow-theme
; (when (require 'sanityinc-tomorrow-bright-theme nil t)
;  (load-theme 'sanityinc-tomorrow-bright t))

;; font
; フォントをインストール
; fc-list でフォントを確認
; (set-fontset-font nil 'unicode "TakaoGothic")
; (set-face-attribute 'default nil :height 130)





