;;; fontaine.el --- Set font configurations using presets -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/fontaine
;; Version: 2.0.0
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
     :default-height 100

     :fixed-pitch-family nil
     :fixed-pitch-weight nil
     :fixed-pitch-slant nil
     :fixed-pitch-height 1.0

     :fixed-pitch-serif-family nil
     :fixed-pitch-serif-weight nil
     :fixed-pitch-serif-slant nil
     :fixed-pitch-serif-height 1.0

     :variable-pitch-family "Sans"
     :variable-pitch-weight nil
     :variable-pitch-slant nil
     :variable-pitch-height 1.0

     :mode-line-active-family nil
     :mode-line-active-weight nil
     :mode-line-active-slant nil
     :mode-line-active-height 1.0

     :mode-line-inactive-family nil
     :mode-line-inactive-weight nil
     :mode-line-inactive-slant nil
     :mode-line-inactive-height 1.0

     :header-line-family nil
     :header-line-weight nil
     :header-line-slant nil
     :header-line-height 1.0

     :line-number-family nil
     :line-number-weight nil
     :line-number-slant nil
     :line-number-height 1.0

     :tab-bar-family nil
     :tab-bar-weight nil
     :tab-bar-slant nil
     :tab-bar-height 1.0

     :tab-line-family nil
     :tab-line-weight nil
     :tab-line-slant nil
     :tab-line-height 1.0

     :bold-family nil
     :bold-slant nil
     :bold-weight bold
     :bold-height 1.0

     :italic-family nil
     :italic-weight nil
     :italic-slant italic
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

;;;; General utilities

(defun fontaine--frame (frame)
  "Return FRAME for `internal-set-lisp-face-attribute'."
  (cond
   ((framep frame) frame)
   (frame nil)
   (t 0)))

(defun fontaine--set-face-attributes (face family &optional weight slant height width frame)
  "Set FACE font to FAMILY, with optional WEIGHT, SLANT, HEIGHT, WIDTH, FRAME."
  (let ((frames (fontaine--frame frame)))
    ;; ;; Read this: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45920>
    ;; ;; Hence why the following fails.  Keeping it for posterity...
    ;; (set-face-attribute face nil :family family :weight weight :height height)
    (when (and weight
               (symbolp weight)
               (eq (face-attribute face :weight) weight)
               (stringp family))
      (internal-set-lisp-face-attribute face :family family frames))
    (when (and weight (symbolp weight))
      (internal-set-lisp-face-attribute face :weight weight frames))
    (when (and width (symbolp width))
      (internal-set-lisp-face-attribute face :width width frames))
    (when (and slant (symbolp slant))
      (internal-set-lisp-face-attribute face :slant slant frames))
    (when (stringp family)
      (internal-set-lisp-face-attribute face :family family frames))
    (when (and (numberp height) (not (zerop height)))
      (internal-set-lisp-face-attribute face :height height frames))))

;;;; Apply preset configurations

(defun fontaine--preset-p (preset)
  "Return non-nil if PRESET is one of the named `fontaine-presets'."
  (let ((presets (mapcar #'car fontaine-presets)))
    (memq preset presets)))

(defun fontaine--get-inherit-name (preset)
  "Get the `:inherit' value of PRESET."
  (when-let ((inherit (plist-get (alist-get preset fontaine-presets) :inherit))
             (fontaine--preset-p inherit))
    inherit))

(defun fontaine--get-preset-properties (preset)
  "Return list of properties for PRESET in `fontaine-presets'."
  (let ((presets fontaine-presets))
    (append (alist-get preset presets)
            (when-let ((inherit (fontaine--get-inherit-name preset)))
              (alist-get inherit presets))
            (alist-get t presets))))

(defun fontaine--get-preset-property (preset property)
  "Get PRESET's PROPERTY."
  (plist-get (fontaine--get-preset-properties preset) property))

(defun fontaine--set-face (preset face &optional frame)
  "Set font properties taken from PRESET to FACE in optional FRAME.
If FRAME is nil, apply the effect to all frames."
  (let ((properties (fontaine--get-preset-properties preset)))
    (fontaine--set-face-attributes
     face
     (plist-get properties (intern (format ":%s-family" face)))
     (plist-get properties (intern (format ":%s-weight" face)))
     (plist-get properties (intern (format ":%s-slant" face)))
     (plist-get properties (intern (format ":%s-height" face)))
     (plist-get properties (intern (format ":%s-width" face)))
     frame)))

(defun fontaine--set-faces (preset frame)
  "Set all `fontaine-faces' according to PRESET in FRAME."
  (mapc
   (lambda (face)
     (fontaine--set-face preset face frame))
   fontaine-faces)
  (setq-default line-spacing (fontaine--get-preset-property preset :line-spacing)))

(defvar fontaine--font-display-hist '()
  "History of inputs for display-related font associations.")

(defun fontaine--presets-no-fallback ()
  "Return list of `fontaine-presets', minus the fallback value."
  (delq
   nil
   (mapcar (lambda (symbol)
             (unless (eq (car symbol) t)
               symbol))
           fontaine-presets)))

(defun fontaine--set-fonts-prompt ()
  "Prompt for font set (used by `fontaine-set-fonts')."
  (let* ((def (nth 1 fontaine--font-display-hist))
         (prompt (if def
                     (format "Apply font configurations from PRESET [%s]: " def)
                   "Apply font configurations from PRESET: ")))
    (intern
     (completing-read
      prompt
      (fontaine--presets-no-fallback)
      nil t nil 'fontaine--font-display-hist def))))

(defvar fontaine-current-preset nil
  "Current font set in `fontaine-presets'.
This is the preset last used by `fontaine-set-preset'.  Also see
the command `fontaine-apply-current-preset'.")

;;;###autoload
(defun fontaine-set-preset (preset &optional frame)
  "Set font configurations specified in PRESET.
PRESET is a symbol that represents the car of a list in
`fontaine-presets'.  When called interactively, prompt for
PRESET.g

Unless optional FRAME argument is supplied, apply the change to
all frames.  If FRAME satisfies `framep', then make the changes
affect only it.  If FRAME is non-nil, interpret it as the current
frame and apply the effects to it.

When called interactively with a universal prefix
argument (\\[universal-argument]), FRAME is interpreted as
non-nil.

Set `fontaine-current-preset' to PRESET.  Also see the command
`fontaine-apply-current-preset'.

Call `fontaine-set-preset-hook' as a final step."
  (interactive (list (fontaine--set-fonts-prompt) current-prefix-arg))
  (if (and (not (daemonp)) (not window-system))
      (user-error "Cannot use this in a terminal emulator; try the Emacs GUI")
    (fontaine--set-faces preset frame)
    (setq fontaine-current-preset preset)
    (unless frame
      (add-to-history 'fontaine--preset-history (format "%s" preset)))
    (run-hooks 'fontaine-set-preset-hook)))

;;;###autoload
(defun fontaine-apply-current-preset (&rest _)
  "Use `fontaine-set-preset' on `fontaine-current-preset'.
The value of `fontaine-current-preset' must be one of the keys in
`fontaine-presets'.

Re-applying the current preset is useful when a new theme is
loaded which overrides certain font families.  For example, if
the theme defines the `bold' face without a `:family', loading
that theme will make `bold' use the `default' family, even if the
`fontaine-presets' are configured to have different families
between the two.  In such a case, applying the current preset at
the post `load-theme' phase (e.g. via a hook) ensures that font
configurations remain consistent.

Some themes that provide hooks of this sort are the
`modus-themes', `ef-themes', `standard-themes' (all by
Protesilaos).  Alternatively, Emacs 29 provides the special
`enable-theme-functions' hook, which passes a THEME argument
which this function ignores"
  (interactive)
  (if-let ((current fontaine-current-preset)
           ((fontaine--preset-p current)))
      (fontaine-set-preset current)
    (user-error "The `fontaine-current-preset' is not among `fontaine-presets'")))

;;;; Store and restore preset

(defvar fontaine--preset-history '()
  "Minibuffer history of preset configurations.")

;;;###autoload
(defun fontaine-store-latest-preset ()
  "Write latest cursor state to `fontaine-latest-state-file'.
Can be assigned to `kill-emacs-hook'."
  (when-let ((hist fontaine--preset-history)
             (latest (car hist))
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
  (when-let ((file fontaine-latest-state-file)
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
closing and restarting Emacs.  Also, do it for theme switching,
if the Emacs version is 29 or higher.

[ Note that in older versions of Emacs we do not have a hook that
  is called at the post-theme-load phase.  Users can do this by
  installing an advice.  Read the Info node `(fontaine)
  Theme-agnostic hook before Emacs 29'.  ]"
  :global t
  (if fontaine-mode
      (progn
        (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
        (add-hook 'fontaine-set-preset-hook #'fontaine-store-latest-preset)
        (add-hook 'enable-theme-functions #'fontaine-apply-current-preset))
    (remove-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
    (remove-hook 'fontaine-set-preset-hook #'fontaine-store-latest-preset)
    (remove-hook 'enable-theme-functions #'fontaine-apply-current-preset)))

(provide 'fontaine)
;;; fontaine.el ends here
