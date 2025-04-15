;;; fontaine.el --- Set font configurations using presets -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/fontaine
;; Version: 3.0.1
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Fontaine lets the user specify presets of font configurations and set
;; them on demand on graphical Emacs frames.  The user option
;; `fontaine-presets' holds all such presets.
;;
;; Consult the manual for all the available features.  And remember
;; that Fonts, Ornaments, and Neat Typography Are Irrelevant in
;; Non-graphical Emacs (FONTAINE).

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup fontaine ()
  "Set font configurations using presets."
  :group 'font)

;; NOTE 2025-01-06: This is what `use-package' does with its own
;; theme, so it is probably the right approach for us too.
(eval-and-compile
  ;; Declare a synthetic theme for :custom variables.
  ;; Necessary in order to avoid having those variables saved by custom.el.
  (deftheme fontaine "Special theme for Fontaine fonts."))

(enable-theme 'fontaine)
;; Remove the synthetic fontaine theme from the enabled themes, so
;; iterating over them to "disable all themes" won't disable it.
(setq custom-enabled-themes (remq 'fontaine custom-enabled-themes))


(defvar fontaine-weights
  '( thin ultralight extralight light semilight regular medium
     semibold bold heavy extrabold ultrabold)
  "List of font weights.")

(defvar fontaine-slants
  '(normal italic oblique reverse-italic reverse-oblique)
  "List of font slants.")

(defvar fontaine-widths
  '( ultra-condensed
     extra-condensed
     condensed
     semi-condensed demi-condensed
     normal medium regular
     semi-expanded demi-expanded
     expanded
     extra-expanded
     ultra-expanded)
  "List of font widths.")

(defconst fontaine--weights-widget
  '(choice :tag "Font weight (must be supported by the typeface)"
           (const :tag "Normal" normal)
           (const :tag "Regular (same as normal)" regular)
           (const :tag "Thin" thin)
           (const :tag "Ultra-light" ultralight)
           (const :tag "Extra-light" extralight)
           (const :tag "Light" light)
           (const :tag "Semi-light" semilight)
           (const :tag "Medium" medium)
           (const :tag "Semi-bold" semibold)
           (const :tag "Bold" bold)
           (const :tag "Extra-bold" extrabold)
           (const :tag "Ultra-bold" ultrabold)
           (const :tag "Use fallback value" nil))
  "Widget with font weights for `fontaine-presets'.")

(defconst fontaine--slants-widget
  '(choice :tag "Font slant (must be supported by the typeface)"
           (const italic)
           (const oblique)
           (const normal)
           (const reverse-italic)
           (const reverse-oblique)
           (const :tag "Use fallback value" nil))
  "Widget with font slants for `fontaine-presets'.")

(defconst fontaine--widths-widget
  '(choice :tag "Font width (must be supported by the typeface)"
           (const ultra-condensed)
           (const extra-condensed)
           (const condensed)
           (const semi-condensed)
           (const :tag "Alias for semi-condensed" demi-condensed)
           (const normal)
           (const :tag "Alias for normal" medium)
           (const :tag "Alias for normal" regular)
           (const semi-expanded)
           (const :tag "Alias for semi-expanded" demi-expanded)
           (const expanded)
           (const extra-expanded)
           (const ultra-expanded)
           (const :tag "Use fallback value" nil))
  "Widget with font weights for `fontaine-presets'.")

(defconst fontaine-faces
  '( default fixed-pitch fixed-pitch-serif variable-pitch
     mode-line-active mode-line-inactive header-line
     line-number tab-bar tab-line
     bold italic)
  "List of faces with relevant font attributes.")

(defun fontaine--get-face-widget (face)
  "Define `fontaine-presets' properties for FACE as a widget."
  (list
   `((const :tag ,(format "%s font family" face) ,(intern (format ":%s-family" face))) string)
   `((const :tag ,(format "%s weight" face) ,(intern (format ":%s-weight" face))) ,fontaine--weights-widget)
   `((const :tag ,(format "%s slant" face) ,(intern (format ":%s-slant" face))) ,fontaine--slants-widget)
   `((const :tag ,(format "%s width" face) ,(intern (format ":%s-width" face))) ,fontaine--widths-widget)
   `((const :tag ,(format "%s height" face) ,(intern (format ":%s-height" face))) float)))

(defcustom fontaine-presets
  '((regular
     :default-height 100)
    (large
     :default-weight semilight
     :default-height 140
     :bold-weight extrabold)
    (t
     ;; I keep all properties for didactic purposes, but most can be
     ;; omitted.  See the fontaine manual for the technicalities:
     ;; <https://protesilaos.com/emacs/fontaine>.
     :default-family "Monospace"
     :default-weight regular
     :default-slant normal
     :default-width normal
     :default-height 100

     :fixed-pitch-family "Monospace"
     :fixed-pitch-weight nil
     :fixed-pitch-slant nil
     :fixed-pitch-width nil
     :fixed-pitch-height 1.0

     :fixed-pitch-serif-family nil
     :fixed-pitch-serif-weight nil
     :fixed-pitch-serif-slant nil
     :fixed-pitch-serif-width nil
     :fixed-pitch-serif-height 1.0

     :variable-pitch-family "Sans"
     :variable-pitch-weight nil
     :variable-pitch-slant nil
     :variable-pitch-width nil
     :variable-pitch-height 1.0

     :mode-line-active-family nil
     :mode-line-active-weight nil
     :mode-line-active-slant nil
     :mode-line-active-width nil
     :mode-line-active-height 1.0

     :mode-line-inactive-family nil
     :mode-line-inactive-weight nil
     :mode-line-inactive-slant nil
     :mode-line-inactive-width nil
     :mode-line-inactive-height 1.0

     :header-line-family nil
     :header-line-weight nil
     :header-line-slant nil
     :header-line-width nil
     :header-line-height 1.0

     :line-number-family nil
     :line-number-weight nil
     :line-number-slant nil
     :line-number-width nil
     :line-number-height 1.0

     :tab-bar-family nil
     :tab-bar-weight nil
     :tab-bar-slant nil
     :tab-bar-width nil
     :tab-bar-height 1.0

     :tab-line-family nil
     :tab-line-weight nil
     :tab-line-slant nil
     :tab-line-width nil
     :tab-line-height 1.0

     :bold-family nil
     :bold-slant nil
     :bold-weight bold
     :bold-width nil
     :bold-height 1.0

     :italic-family nil
     :italic-weight nil
     :italic-slant italic
     :italic-width nil
     :italic-height 1.0

     :line-spacing nil))
  "Alist of desired typographic properties.

The car of each cell is an arbitrary symbol that identifies
and/or describes the set of properties (e.g. small, reading).

A preset whose car is t is treated as the default option.  This
makes it easier to specify multiple presets without duplicating
their properties.  The other presets beside t act as overrides of
the defaults and, as such, need only consist of the properties
that change from the default.  See the default value of this
variable for how that is done.

The cdr is a plist that specifies the typographic properties of
the faces listed in `fontaine-faces'.  It also covers the
`line-spacing' variable.

For each face, Fontaine reads keywords that describe its font
family, font weight, font slant, and font height.  The name of
those keywords is composed from the name of the face plus th
specifier.  For example, with the `default' face, we have
`:default-family', `:default-height', `:default-weight',
`:default-slant', and `:default-width'.

The properties in more detail:

- The font family is a string that refers to the name of the
  font.

- The font weight is an unquoted symbol among `fontaine-weights'.

- The font slant is an unquoted symbol among `fontaine-slants'.

- The font width is an unquoted symbol among `fontaine-widths'.

- The font height is a floating point (like 1.0) which is
  interpreted as a multiple of the default font height.  An
  exception to this is for the `default' face (i.e. the
  `:default-height'), which needs to be a natural number.

- The `:line-spacing' specifies the value of the `line-spacing'
  variable.

- The `:inherit' contains the name of another named preset.  This
  tells the relevant Fontaine functions to get the properties of
  that preset and blend them with those of the current one.  The
  properties of the current preset take precedence over those of
  the inherited one, thus overriding them.  In practice, this is
  a way to have something like an extra-large preset copy the
  large preset and then only modify its individual properties.
  Remember that all named presets fall back to the preset whose
  name is t: the `:inherit' is not a substitute for that generic
  fallback but rather an extra method of specifying font
  configuration presets.

Use the desired preset with the command `fontaine-set-preset'.

For detailed configuration: Info node `(fontaine) Shared and
implicit fallback values for presets'.

Caveats or further notes:

- On a Windows system, setting a `default' weight other than
  `regular' or `normal' will not work.  This is a limitation with
  Emacs on that system.

- All the properties for `bold' and `italic' will only have a
  noticeable effect if the active theme does not hardcode a
  weight and a slant, but instead inherits the relevant
  face (such as the `modus-themes', `ef-themes',
  `standard-themes').

- Fontaine does not [yet] support Emacs' fontsets for other
  scripts or character sets (e.g. Emoji).  Read the documentation
  in the Info node `(emacs) Modifying Fontsets'."
  :type `(alist
          :value-type
          (plist :options
                 (((const :tag "Default font family" :default-family) string)
                  ((const :tag "Default weight" :default-weight) ,fontaine--weights-widget)
                  ((const :tag "Default slant" :default-slant) ,fontaine--slants-widget)
                  ((const :tag "Default width" :default-width) ,fontaine--widths-widget)
                  ((const :tag "Default height" :default-height) natnum)

                  ,@(mapcan
                     (lambda (face)
                       (fontaine--get-face-widget face))
                     (delq 'default fontaine-faces))

                  ((const :tag "Line spacing" :line-spacing) ,(get 'line-spacing 'custom-type))
                  ((const :tag "Inherit another preset" :inherit) symbol
                   ;; FIXME 2024-02-21: Is this correct?  It does not seem to work...
                   :match (lambda (_widget value)
                            (memq value (delq t (mapcar #'car fontaine-presets))))))
                 :key-type symbol))
  :package-version '(fontaine . "2.0.0")
  :group 'fontaine
  :link '(info-link "(fontaine) Shared and implicit fallback values for presets"))

(defcustom fontaine-latest-state-file
  (locate-user-emacs-file "fontaine-latest-state.eld")
  "File to save the latest value of `fontaine-set-preset'.
Saving is done by the `fontaine-store-latest-preset' function,
which should be assigned to a hook (e.g. `kill-emacs-hook').

This is then used to restore the last value with the function
`fontaine-restore-latest-preset'."
  :type 'file
  :package-version '(fontaine . "0.1.0")
  :group 'fontaine)

(make-obsolete-variable 'fontaine-font-families nil "2.0.0")

(defcustom fontaine-set-preset-hook nil
  "Hook that runs after setting fonts with `fontaine-set-preset'."
  :type 'hook
  :package-version '(fontaine . "2.0.0")
  :group 'fontaine)

;;;; Apply preset configurations

(defun fontaine--preset-p (preset)
  "Return non-nil if PRESET is one of the named `fontaine-presets'."
  (let ((presets (mapcar #'car fontaine-presets)))
    (memq preset presets)))

(defun fontaine--get-inherit-name (preset)
  "Get the `:inherit' value of PRESET."
  (when-let* ((inherit (plist-get (alist-get preset fontaine-presets) :inherit))
              (fontaine--preset-p inherit))
    inherit))

(defconst fontaine-generic-face-families
  '(:default-family "Monospace"
    :fixed-pitch-family "Monospace"
    :fixed-pitch-serif-family "Monospace"
    :variable-pitch-family "Sans")
  "Preset with generic font families for internal use.")

(defun fontaine--get-preset-properties (preset)
  "Return list of properties for PRESET in `fontaine-presets'."
  (let ((presets fontaine-presets))
    (append (alist-get preset presets)
            (when-let* ((inherit (fontaine--get-inherit-name preset)))
              (alist-get inherit presets))
            (or (alist-get t presets)
                fontaine-generic-face-families))))

(defun fontaine--get-preset-property (preset property)
  "Get PRESET's PROPERTY."
  (plist-get (fontaine--get-preset-properties preset) property))

(defun fontaine--get-property (face attribute properties)
  "Get the fontaine property of FACE with ATTRIBUTE in PROPERTIES."
  (plist-get properties (intern (format ":%s-%s" face attribute))))

(defun fontaine--get-face-spec (preset face)
  "Set font properties taken from PRESET to FACE."
  (let* ((properties (fontaine--get-preset-properties preset))
         (family (or (fontaine--get-property face "family" properties)
                     (and (eq face 'fixed-pitch)
                          (fontaine--get-property 'default "family" properties))))
         (weight (fontaine--get-property face "weight" properties))
         (slant (fontaine--get-property face "slant" properties))
         (height (fontaine--get-property face "height" properties))
         (width (fontaine--get-property face "width" properties)))
    (when (or family weight slant height width)
      `(,face
        ((((type graphic))
          ,@(when family (list :family family))
          ,@(when weight (list :weight weight))
          ,@(when slant (list :slant slant))
          ,@(when height (list :height height))
          ,@(when width (list :width width))))))))

(defun fontaine--set-faces (preset)
  "Set all `fontaine-faces' according to PRESET."
  (let ((custom--inhibit-theme-enable nil)
        (faces (mapcar
                (lambda (face)
                  (fontaine--get-face-spec preset face))
                fontaine-faces)))
    (apply 'custom-theme-set-faces 'fontaine faces)
    (setq-default line-spacing (fontaine--get-preset-property preset :line-spacing))))

(make-obsolete 'fontaine--font-display-hist nil "3.0.0")

(defun fontaine-not-t-p (symbol)
  "Return non-nil if SYMBOL is `symbolp' and not t."
  (and (symbolp symbol)
       (not (eq symbol t))))

(defun fontaine--presets-no-fallback ()
  "Return list of `fontaine-presets', minus the fallback value."
  (seq-filter
   (lambda (preset)
     (fontaine-not-t-p (car preset)))
   fontaine-presets))

(defun fontaine--get-preset-symbols ()
  "Return list of the `car' of each element in `fontain-presets'."
  (delq nil
        (mapcar
         (lambda (element)
           (when-let* ((first (car element))
                       ((fontaine-not-t-p first)))
             first))
         fontaine-presets)))

(defun fontaine--get-preset-symbols-as-strings ()
  "Convert `fontaine--get-preset-symbols' return value to list of string."
  (mapcar #'symbol-name (fontaine--get-preset-symbols)))

(defvar fontaine-current-preset nil
  "Current font set in `fontaine-presets'.
This is the preset last used by `fontaine-set-preset'.")

(defun fontaine--get-first-non-current-preset (history)
  "Return the first element of HISTORY which is not `fontaine-current-preset'.
Only consider elements that are still part of the `fontaine-presets',
per `fontaine--get-preset-symbols'."
  (catch 'first
    (dolist (element history)
      (when (symbolp element)
        (setq element (symbol-name element)))
      (when (and (not (string= element fontaine-current-preset))
                 (member element (fontaine--get-preset-symbols-as-strings)))
        (throw 'first element)))))

(defvar fontaine-preset-history nil
  "Minibuffer history of `fontaine-preset-prompt'.")

(defun fontaine-preset-prompt (&optional prompt-text)
  "Prompt for preset among `fontaine-presets'.
With optional PROMPT-TEXT, use it instead of the generic prompt."
  (let ((default (fontaine--get-first-non-current-preset fontaine-preset-history)))
    (intern
     (completing-read
      (format-prompt (or prompt-text "Apply font configurations from PRESET") default)
      (fontaine--presets-no-fallback)
      nil t nil 'fontaine-preset-history default))))

;;;###autoload
(defun fontaine-set-preset (preset)
  "Set font configurations specified in PRESET.
PRESET is a symbol that represents the car of a list in
`fontaine-presets'.  When called interactively, prompt for
PRESET.

Set `fontaine-current-preset' to PRESET.  Call
`fontaine-set-preset-hook' as a final step after setting the PRESET."
  (interactive (list (fontaine-preset-prompt)))
  (cond
   ((and (not (daemonp)) (not window-system))
    (display-warning 'fontaine "Cannot use Fontaine in a terminal emulator; try the Emacs GUI"))
   ((fontaine--preset-p preset)
    (fontaine--set-faces preset)
    (setq fontaine-current-preset preset)
    (run-hooks 'fontaine-set-preset-hook))
   (t
    (display-warning 'fontaine (format-message "The preset `%s' is not among the `fontaine-presets'" preset))
    nil)))

(make-obsolete 'fontaine-apply-current-preset nil "3.0.0")

;;;###autoload
(defun fontaine-toggle-preset ()
  "Toggle between the last two known Fontaine presets.
These are the presets that were set with `fontaine-set-preset'.  If
there are no two selected presets, then prompt the user to set a preset.

As a final step, call the `fontaine-set-preset-hook'."
  (interactive)
  (if-let* ((preset (or (intern (fontaine--get-first-non-current-preset fontaine-preset-history))
                        (fontaine-preset-prompt "No previous preset to toggle; select PRESET"))))
      (fontaine-set-preset preset)
    (error "Could not find a Fontaine preset to toggle")))

;;;; Store and restore preset

(make-obsolete 'fontaine--preset-history nil "3.0.0")

;;;###autoload
(defun fontaine-store-latest-preset ()
  "Write latest state to `fontaine-latest-state-file'.
Can be assigned to `kill-emacs-hook'."
  (when-let* ((latest (car fontaine-preset-history))
              ((not (member latest '("nil" "t")))))
    (with-temp-file fontaine-latest-state-file
      (insert ";; Auto-generated file; don't edit -*- mode: "
              (if (<= 28 emacs-major-version)
                  "lisp-data"
                "emacs-lisp")
              " -*-\n")
      (pp (intern latest) (current-buffer)))))

(defvar fontaine-recovered-preset nil
  "Recovered value of latest stored font preset.")

;;;###autoload
(defun fontaine-restore-latest-preset ()
  "Restore latest preset set by `fontaine-set-preset'.
The value is stored in `fontaine-latest-state-file'."
  (when-let* ((file fontaine-latest-state-file)
              ((file-exists-p file)))
    (setq fontaine-recovered-preset
          (unless (zerop
                   (or (file-attribute-size (file-attributes file))
                       0))
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))

;;;###autoload
(define-minor-mode fontaine-mode
  "Persist Fontaine presets.
Arrange to store and restore the current Fontaine preset when
closing and restarting Emacs."
  :global t
  (if fontaine-mode
      (progn
        (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
        (add-hook 'fontaine-set-preset-hook #'fontaine-store-latest-preset))
    (remove-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
    (remove-hook 'fontaine-set-preset-hook #'fontaine-store-latest-preset)))

(provide 'fontaine)
;;; fontaine.el ends here
