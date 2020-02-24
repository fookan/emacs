;;; init.el:
;;; prepare:
;; gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40

;;; package-history:
;; auto-complete
;; undohist
;; undo-tree
;; ddskk
;; color-theme-sanityinc-tomorrow
;; flycheck
;; add-node-modules-path
;; fuzzy

;;; package
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; 環境を日本語、UTF-8にする
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; shellの環境変数を引き継ぐ
(setq exec-path (parse-colon-path (getenv "PATH")))
(setq eshell-path-env (getenv "PATH"))

; バックアップファイルとオートセーブファイの保存場所
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*",(expand-file-name "~/.emacs.d/backups/") t)))

;; バックアップファイルを作成させない場合
;(setq make-backup-files nil)

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

;; for mbac ¥markを\にする
(when (eq system-type 'darwin)
  (define-key global-map [?¥] [?\\])
  (define-key local-function-key-map [?\C-¥] [?\C-\\])
  ; (define-key local-function-key-map [?\M-¥] [?\M-\\])
  ; (define-key local-function-key-map [?\C-\M-¥] [?\C-\M-\\])
  ; (define-key global-map (kdb "C-SPC") 'set-mark-command)
  (setq mac-right-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))

;; auto-complete
(when (require 'auto-complete-config nil t)
  (require 'fuzzy) ;; fuzzy search (heaby)
  (setq ac-use-fuzzy t)
  ; (define-key ac-mode-map (kbd "<tab>") 'auto-complete)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  (global-auto-complete-mode t)
  (ac-config-default)
  (setq ac-delay 0) ;; 補完候補表示までの時間
  (setq ac-auto-show-menu 0.05) ;; ヒント表示までの時間
  (setq ac-user-menu-map t)
  (setq ac-menu-height 25) ;; ちょっと大きめにとりましょう！
  (setq ac-auto-start 3) ;; 個人的には3でもいいかも
  (setq ac-ignore-case nil))

;; undohist
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-tree
;; install: gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40
(when (require 'undo-tree nil t)
  (define-key global-map (kbd "M-/") 'undo-tree-redo)
   (global-undo-tree-mode))

;; skk
;(when (require 'skk nil t)
;; (global-set-key (kbd "C-x j") 'skk-auto-fill-mode) ;;良い感じに改行を自動入力してくれる機能
;  (setq default-input-method "japanese-skk")         ;;emacs上での日本語入力にskkをつかう
;  (require 'skk-study)
; 変換候補の表示位置
;  (setq skk-show-candidates-always-pop-to-buffer t)
; 候補表示件数を2列に
;  (setq skk-henkan-show-candidates-rows 2)
; Enterで改行しない
;  (setq skk-egg-like-newline t)
; カタカナを変換候補に入れる
;  (setq skk-search-katakana 'jisx0201-kana))

;; color-theme-sanityinc-tomorrow-theme
(when (require 'sanityinc-tomorrow-bright-theme nil t)
  (load-theme 'sanityinc-tomorrow-bright t))

;; font
; フォントをインストール
; fc-list でフォントを確認
(set-fontset-font nil 'unicode "TakaoGothic")
(set-face-attribute 'default nil :height 130)

;; window
(set-frame-size (selected-frame) 130 40)
(set-frame-position (selected-frame) 0 0)

;; flycheck
(when (require 'flycheck nil t)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; js-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(flycheck-add-mode 'javascript-eslint 'js-mode)
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
(add-hook 'js-mode-hook 'flycheck-mode)
(add-hook 'js-mode-hook #'add-node-modules-path)

;;; init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (fuzzy add-node-modules-path flycheck undo-tree undohist color-theme-sanityinc-tomorrow auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here

