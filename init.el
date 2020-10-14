;;; init.el:
;;; prepare:
;; gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40

;;; package-history:
;; mozc
;;  sudo apt install emacs-mozc-bin
;; auto-complete
;; undohist
;; undo-tree
;; ddskk
;; color-theme-sanityinc-tomorrow
;; flycheck
;; add-node-modules-path
;; fuzzy
;; company
;; proectile

;; projectile
; gitのfolder構成に管理できるのでgit initしてぷろじぇくとをつくる 

;;; package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; mozc
(require 'mozc) ; mozcの読み込み
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc") ; IMEをjapanes-mozcに
(prefer-coding-system 'utf-8)

;;; 環境を日本語、UTF-8にする
;; (set-language-environment "Japanese")
;; (prefer-coding-system 'utf-8)

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
;(global-whitespace-mode 1)

;;バッファの終端を明示
;(setq-default indicate-empty-lines t)

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

;; for mac ¥markを\にする
;; (when (eq system-type 'darwin)
;;   ; (define-key global-map [?¥] [?\\]) ; ￥マークで\を入力する場合はこちら
;;   (define-key global-map [?\A-¥] [?\\]) ; option-￥マークで\を入力する場合はこちら
;;   (define-key local-function-key-map [?\C-¥] [?\C-\\])
;;   ; (define-key local-function-key-map [?\M-¥] [?\M-\\])
;;   ; (define-key local-function-key-map [?\C-\M-¥] [?\C-\M-\\])
;;   ; (define-key global-map (kbd "C-SPC") 'set-mark-command)
;;   (setq mac-right-command-modifier 'meta)
;;   (setq mac-option-modifier 'alt)
;;   (setq mac-command-modifier 'meta))
;; macのデフォルトのIMEを使う
;; (when (eq system-type 'darwin)
;;   (setq default-input-method "mac-input-source"))


;; mark set
(define-key global-map (kbd "C-SPC") 'set-mark-command)
(define-key global-map (kbd "C-@") 'set-mark-command)

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

;; 日本語入力
;; tips
; C-\で標準のKKCになる
; SPACEで変換モードに入る。変換モード中に以下の操作ができる
;  C-i 選択範囲をつめる。
;  C-o 選択範囲を広げる
;  l 候補一覧をページ送り
; Kを押すとカタカナに変換する

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


;; ctrl-shift-sapceで言語切替できるようにする。
(global-set-key (kbd "C-S-SPC") 'toggle-input-method)

;; color-theme-sanityinc-tomorrow-theme
(when (require 'sanityinc-tomorrow-bright-theme nil t)
  (load-theme 'sanityinc-tomorrow-bright t))

;; font
; フォントをインストール
; fc-list でフォントを確認
; (set-fontset-font nil 'unicode "TakaoGothic")
; (set-face-attribute 'default nil :height 130)

;; window
; (set-frame-size (selected-frame) 130 40)
; (set-frame-position (selected-frame) 0 0)

;; flycheck
(when (require 'flycheck nil t)
;  (require 'flycheck-pos-tip)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; flycheckのメッセージをtipsで表示する
;(eval-after-load 'flycheck
;  '(custom-set-variables
;   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; js-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(flycheck-add-mode 'javascript-eslint 'js-mode)
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
(add-hook 'js-mode-hook 'flycheck-mode)
(add-hook 'js-mode-hook #'add-node-modules-path)


;; mew メールを読むだけ設定
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(setq mew-mailbox-type 'mbox)
(setq mew-mbox-command "incm")
(setq mew-mbox-command-arg "-u -d ~/Mail")

;; wanderlust メールを読むだけ設定
;; .foldersが必要
;(setq elmo-maildir-folder-path "~/Mail/")
;(setq wl-default-folder "+new")

;; company
(when (require 'company nil t)
  (setq company-auto-expand t) ;; 1個目を自動的に補完
  (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (setq company-idle-delay 0) ; 遅延なしにすぐ表示
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)

  (global-set-key (kbd "C-M-i") 'company-complete)
  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
  (define-key company-active-map (kbd "<tab>") 'company-indent-or-complete-common)
  ; (define-key company-active-map (kbd "C-h") nil) ;; C-hはバックスペース割当のため無効化
  ; (define-key company-active-map (kbd "C-S-h") 'company-show-doc-buffer) ;; ドキュメント表示はC-Shift-h
  
  ;; ;; 未選択項目
  ;; (set-face-attribute 'company-tooltip nil
  ;;                     :foreground "#36c6b0" :background "#244f36")
  ;; ;; 未選択項目&一致文字
  ;; (set-face-attribute 'company-tooltip-common nil
  ;;                     :foreground "white" :background "#244f36")
  ;; ;; 選択項目
  ;; (set-face-attribute 'company-tooltip-selection nil
  ;;                     :foreground "#a1ffcd" :background "#007771")
  ;; ;; 選択項目&一致文字
  ;; (set-face-attribute 'company-tooltip-common-selection nil
  ;;                     :foreground "white" :background "#007771")
  ;; ;; スクロールバー
  ;; (set-face-attribute 'company-scrollbar-fg nil
  ;;                     :background "#4cd0c1")
  ;; ;; スクロールバー背景
  ;; (set-face-attribute 'company-scrollbar-bg nil
  ;;                     :background "#002b37")
  ;; color settings
  (set-face-attribute 'company-tooltip nil
		              :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
		              :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
		              :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
		              :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
		              :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
		              :background "grey60")
  (set-face-attribute 'company-scrollbar-bg nil
		              :background "gray40")

  ; tab
  ; (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
  ; 候補が１つの場合はそれを選択する．
 ; 候補が複数の場合，挿入可能なprefixがあれば挿入し，なければcompany-select-nextする
  ;; (defun company--insert-candidate2 (candidate)
  ;;   (when (> (length candidate) 0)
  ;;     (setq candidate (substring-no-properties candidate))
  ;;     (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
  ;;         (insert (company-strip-prefix candidate))
  ;;       (if (equal company-prefix candidate)
  ;;           (company-select-next)
  ;;         (delete-region (- (point) (length company-prefix)) (point))
  ;;         (insert candidate))
  ;;       )))

  ;; (defun company-complete-common2 ()
  ;;   (interactive)
  ;;   (when (company-manual-begin)
  ;;     (if (and (not (cdr company-candidates))
  ;;              (equal company-common (car company-candidates)))
  ;;         (company-complete-selection)
  ;;       (company--insert-candidate2 company-common))))

  ;; (define-key company-active-map [tab] 'company-complete-common2)
  )

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)


;;; init.el
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(package-selected-packages
   (quote
    (wanderlust web-mode flycheck-pos-tip yasnippet spinner lsp-java mozc projectile company ## mew fuzzy add-node-modules-path flycheck undo-tree undohist color-theme-sanityinc-tomorrow auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here

