;;; require
(require 'package)

;;;package
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)


(require 'web-mode)
(require 'flycheck)

;;; 環境を日本語、UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)


;; バックアップファイルを作成させない
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;;;
;; ツールバーを消す
;(tool-bar-mode -1)

; メニューを消す
(menu-bar-mode -1)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)

;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;; カーソル行をハイライトする
(global-hl-line-mode t)

;;改行時の自動インデントを無効
(electric-indent-mode -1)

;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)

;; Macのoptionをメタキーにする
(setq mac-option-modifier 'meta)

;;バックスペース ctrl-h
(keyboard-translate ?\C-h ?\C-?)

;; C-kで行全体を削除する
(setq kill-whole-line t)

;;; major-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; eslint 用の linter を登録
(flycheck-add-mode 'javascript-eslint 'web-mode)
(add-hook 'web-mode-hook 'flycheck-mode)
(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook #'add-node-modules-path))

;(global-flycheck-mode)

;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-popup-tip add-node-modules-path web-mode flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'flycheck-tip)
;(define-key your-prog-map (kbd "C-c C-n") 'flycheck-tip-cycle)
;(define-key your-prog-map (kbd "C-c C-n") 'flycheck-tip-cycle-reverse)
