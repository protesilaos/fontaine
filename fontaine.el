;;; fontaine.el --- Set font configurations using presets -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/fontaine
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

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

(defvar fontaine-font-weights
  '( thin ultralight extralight light semilight regular medium
     semibold bold heavy extrabold ultrabold)
  "List of font weights.")

(defvar fontaine-font-slants
  '(normal italic oblique reverse-italic reverse-oblique)
  "List of font slants.")

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

(defconst fontaine-faces
  '( default fixed-pitch variable-pitch
     mode-line-active mode-line-inactive
     line-number tab-bar tab-line
     bold italic)
  "List of faces with relevant font attributes.")

(defcustom fontaine-presets
  '((regular
     :default-height 100)
    (large
     :default-weight semilight
     :default-height 140
     :bold-weight extrabold)
    (t
     ;; I keep all properties for didactic purposes, but most can be
     ;; omitted.
     :default-family "Monospace"
     :default-weight regular
     :default-height 100

     :fixed-pitch-family nil ; falls back to :default-family
     :fixed-pitch-weight nil ; falls back to :default-weight
     :fixed-pitch-height 1.0

     :fixed-pitch-serif-family nil ; falls back to :default-family
     :fixed-pitch-serif-weight nil ; falls back to :default-weight
     :fixed-pitch-serif-height 1.0

     :variable-pitch-family "Sans"
     :variable-pitch-weight nil
     :variable-pitch-height 1.0

     :mode-line-active-family nil ; falls back to :default-family
     :mode-line-active-weight nil ; falls back to :default-weight
     :mode-line-active-height 1.0

     :mode-line-inactive-family nil ; falls back to :default-family
     :mode-line-inactive-weight nil ; falls back to :default-weight
     :mode-line-inactive-height 1.0

     :header-line-family nil ; falls back to :default-family
     :header-line-weight nil ; falls back to :default-weight
     :header-line-height 1.0

     :line-number-family nil ; falls back to :default-family
     :line-number-weight nil ; falls back to :default-weight
     :line-number-height 1.0

     :tab-bar-family nil ; falls back to :default-family
     :tab-bar-weight nil ; falls back to :default-weight
     :tab-bar-height 1.0

     :tab-line-family nil ; falls back to :default-family
     :tab-line-weight nil ; falls back to :default-weight
     :tab-line-height 1.0

     :bold-family nil ; use whatever the underlying face has
     :bold-weight bold
     :italic-family nil
     :italic-slant italic
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
the faces `default', `fixed-pitch', `variable-pitch', `bold', and
`italic'.  It also covers the `line-spacing' variable.

The properties in detail:

- `:default-family' is the family of the `default' face.  If not
  specified, it falls back to Monospace.

- `:default-weight' is the weight of the `default' face.  The
  fallback value is `normal'.  Available weights are `normal' or
  `regular', `thin', `ultralight', `extralight', `light',
  `semilight', `medium', `semibold', `extrabold', `ultrabold' and
  must be supported by the underlying typeface.

- `:default-height' is the height of the `default' face.  The
  fallback value is 100 (the height is 10x the point size).

- `:fixed-pitch-family', `:fixed-pitch-weight',
  `:fixed-pitch-height' apply to the `fixed-pitch' face.  Their
  fallback values are `:default-family', `:default-weight', and
  1.0, respectively.

- `:fixed-pitch-serif-family', `:fixed-pitch-serif-weight',
  `:fixed-pitch-serif-height' apply to the `fixed-pitch-serif'
  face.  Their fallback values are `:default-family',
  `:default-weight', and 1.0, respectively.

- The `:variable-pitch-family', `:variable-pitch-weight', and
  `:variable-pitch-height' apply to the `variable-pitch' face.
  They all fall back to the respective default values, as
  described above.

- The `:mode-line-active-family', `:mode-line-active-weight', and
  `:mode-line-active-height' apply to the `mode-line' and
  `mode-line-active' faces.  They all fall back to the respective
  default values, as described above.

- The `:mode-line-inactive-family', `:mode-line-inactive-weight',
  and `:mode-line-inactive-height' apply to the
  `mode-line-inactive' face.  They all fall back to the
  respective default values, as described above.

- The `:header-line-family', `:header-line-weight', and
  `:header-line-height' apply to the `header-line' face.  They
  all fall back to the respective default values, as described
  above.

- The `:line-number-family', `:line-number-weight', and
  `:line-number-height' apply to the `line-number' face.  They
  all fall back to the respective default values, as described
  above.

- The `:tab-bar-family', `:tab-bar-weight', and `:tab-bar-height'
  apply to the `tab-bar' face.  They all fall back to the
  respective default values, as described above.

- The `:tab-line-family', `:tab-line-weight', and
  `:tab-line-height' apply to the `tab-line' face.  They all fall
  back to the respective default values, as described above.

- The `:bold-family' and `:italic-family' are the font families
  of the `bold' and `italic' faces, respectively.  Only set them
  if you want to override that of the underlying face.

- The `:bold-weight' specifies the weight of the `bold' face.
  Its fallback value is bold, meaning the weight, not the face.
  For more, refer to the value of `fontaine-font-weights'.  The
  font family must support the given weight.

- The `:italic-slant' specifies the slant of the `italic' face.
  Its fallback value is italic, in reference to the slant, not
  the face.  Acceptable values are those included in the value of
  `fontaine-font-slants' and must be supported by the underlying
  typeface

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
  face (such as the `modus-themes').

- A height attribute for anything other than the `default' face
  must be set to a floating point, which is understood as a
  multiple of the default height (this allows all faces to scale
  harmoniously).  The `:default-height' always is a natural
  number.

- Fontaine does not [yet] support Emacs' fontsets for other
  scripts or character sets (e.g. Emoji).  Read the documentation
  in the Info node `(emacs) Modifying Fontsets'."
  :type `(alist
          :value-type
          (plist :options
                 (((const :tag "Default font family" :default-family) string)
                  ((const :tag "Default weight" :default-weight) ,fontaine--weights-widget)
                  ((const :tag "Default height" :default-height) natnum)

                  ((const :tag "Fixed pitch font family" :fixed-pitch-family) string)
                  ((const :tag "Fixed pitch regular weight" :fixed-pitch-weight) ,fontaine--weights-widget)
                  ((const :tag "Fixed pitch height" :fixed-pitch-height) float)

                  ((const :tag "Fixed pitch serif font family" :fixed-pitch-serif-family) string)
                  ((const :tag "Fixed pitch serif regular weight" :fixed-pitch-serif-weight) ,fontaine--weights-widget)
                  ((const :tag "Fixed pitch serif height" :fixed-pitch-serif-height) float)

                  ((const :tag "Variable pitch font family" :variable-pitch-family) string)
                  ((const :tag "Variable pitch regular weight" :variable-pitch-weight) ,fontaine--weights-widget)
                  ((const :tag "Variable pitch height" :variable-pitch-height) float)

                  ((const :tag "Active mode line font family" :mode-line-active-family) string)
                  ((const :tag "Active mode line regular weight" :mode-line-active-weight) ,fontaine--weights-widget)
                  ((const :tag "Active mode line height" :mode-line-active-height) float)

                  ((const :tag "Inactive mode line font family" :mode-line-inactive-family) string)
                  ((const :tag "Inactive mode line regular weight" :mode-line-inactive-weight) ,fontaine--weights-widget)
                  ((const :tag "Inactive mode line height" :mode-line-inactive-height) float)

                  ((const :tag "Header line font family" :header-line-family) string)
                  ((const :tag "Header line regular weight" :header-line-weight) ,fontaine--weights-widget)
                  ((const :tag "Header line height" :header-line-height) float)

                  ((const :tag "Line number font family" :line-number-family) string)
                  ((const :tag "Line number regular weight" :line-number-weight) ,fontaine--weights-widget)
                  ((const :tag "Line number height" :line-number-height) float)

                  ((const :tag "Tab bar font family" :tab-bar-family) string)
                  ((const :tag "Tab bar regular weight" :tab-bar-weight) ,fontaine--weights-widget)
                  ((const :tag "Tab bar height" :tab-bar-height) float)

                  ((const :tag "Tab line font family" :tab-line-family) string)
                  ((const :tag "Tab line regular weight" :tab-line-weight) ,fontaine--weights-widget)
                  ((const :tag "Tab line height" :tab-line-height) float)

                  ((const :tag "Font family of the `bold' face" :bold-family) string)
                  ((const :tag "Weight for the `bold' face" :bold-weight) ,fontaine--weights-widget)

                  ((const :tag "Font family of the `italic' face" :italic-family) string)
                  ((const :tag "Slant for the `italic' face" :italic-slant)
                   (choice
                    (const italic)
                    (const oblique)
                    (const normal)
                    (const reverse-italic)
                    (const reverse-oblique)))

                  ((const :tag "Line spacing" :line-spacing) ,(get 'line-spacing 'custom-type))
                  ;; FIXME 2023-01-19: Adding the (choice
                  ;; ,@(fontaine--inheritable-presets-widget)) instead
                  ;; of `symbol' does not have the desired effect
                  ;; because it does not re-read `fontaine-presets'.
                  ((const :tag "Inherit another preset" :inherit) symbol)))
          :key-type symbol)
  :package-version '(fontaine . "1.1.0")
  :group 'fontaine
  :link '(info-link "(fontaine) Shared and implicit fallback values for presets"))

;; ;; See FIXME above in `fontaine-presets' :type.
;; ;;
;; (defun fontaine--inheritable-presets-widget ()
;;   "Return widget with choice among named presets."
;;   (mapcar (lambda (s)
;;             (list 'const s))
;;           (delq t (mapcar #'car fontaine-presets))))

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

(defcustom fontaine-font-families nil
  "An alist of preferred font families.

The expected value of this option is a triplet of cons cells
where the car is `default', `fixed-pitch', or `variable-pitch'
and the cdr is a list of strings that reference font family
names.  For example:

    (setq fontaine-font-families
          \\='((default \"Iosevka Comfy\" \"Hack\" \"Roboto Mono\")
            (fixed-pitch \"Mononoki\" \"Source Code Pro\" \"Fira Code\")
            (variable-pitch \"Noto Sans\" \"Roboto\" \"FiraGO\")))


This is used at the minibuffer prompt while using the command
`fontaine-set-face-font' to prompt for a font family.  When this
user option is nil, that prompt will try to find all relevant
fonts installed on the system, which might not always be
reliable (depending on the Emacs build and the environment it
runs in).

If only the `default' is nil and the others are specified, the
command `fontaine-set-face-font' will produce results that
combine the other two lists."
  :type '(set
          (cons :tag "Default font families"
                (const default)
                (repeat string))
          (cons :tag "Fixed pitch font families"
                (const fixed-pitch)
                (repeat string))
          (cons :tag "Variable pitch font families"
                (const variable-pitch)
                (repeat string)))
  :package-version '(fontaine . "0.2.0")
  :group 'fontaine)

(defcustom fontaine-set-preset-hook nil
  "Hook that runs after setting fonts with `fontaine-set-preset'."
  :type 'hook
  :package-version '(fontaine . "1.1.0")
  :group 'fontaine)

;;;; General utilities

(defun fontaine--frame (frame)
  "Return FRAME for `internal-set-lisp-face-attribute'."
  (cond
   ((framep frame) frame)
   (frame nil)
   (t 0)))

(defun fontaine--set-face-attributes (face family &optional weight slant height frame)
  "Set FACE font to FAMILY, with optional WEIGHT, SLANT, HEIGHT, FRAME."
  (let ((family (cond
                 ((and (eq face 'variable-pitch)
                       (or (eq family 'unspecified)
                           (null family)))
                  "Sans")
                 (family family)
                 (t "Monospace")))
        (height (cond
                 ((and (eq face 'default)
                       (or (eq height 'unspecified)
                           (null height)))
                  100)
                 (height height)
                 (t 1.0)))
        (weight (cond
                 ((and (eq face 'bold)
                       (or (eq weight 'unspecified)
                           (null weight)))
                  'bold)
                 (weight weight)
                 (t 'normal)))
        (slant (cond
                 ((and (eq face 'italic)
                       (or (eq slant 'unspecified)
                           (null slant)))
                  'italic)
                 (slant slant)
                 (t 'normal)))
        (frames (fontaine--frame frame)))
    ;; ;; Read this: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45920>
    ;; ;; Hence why the following fails.  Keeping it for posterity...
    ;; (set-face-attribute face nil :family family :weight weight :height height)
    (if (eq (face-attribute face :weight) weight)
        (internal-set-lisp-face-attribute face :family family frames)
      (internal-set-lisp-face-attribute face :weight weight frames)
      (internal-set-lisp-face-attribute face :slant slant frames)
      (internal-set-lisp-face-attribute face :family family frames)
      (internal-set-lisp-face-attribute face :weight weight frames)
      (internal-set-lisp-face-attribute face :slant slant frames))
    (internal-set-lisp-face-attribute face :height height frames)))

;;;; Apply preset configurations

(defun fontaine--preset-p (preset)
  "Return non-nil if PRESET is one of the named `fontaine-presets'."
  (let ((presets (delq t (mapcar #'car fontaine-presets))))
    (memq preset presets)))

(defun fontaine--get-inherit-name (preset)
  "Get the `:inherit' value of PRESET."
  (when-let* ((inherit (plist-get (alist-get preset fontaine-presets) :inherit))
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
     (or (plist-get properties (intern (format ":%s-family" face))) 'unspecified)
     (or (plist-get properties (intern (format ":%s-weight" face))) 'unspecified)
     (or (plist-get properties (intern (format ":%s-height" face))) 'unspecified)
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
           ((alist-get current fontaine-presets)))
      (fontaine-set-preset current)
    (user-error "The `fontaine-current-preset' is not among `fontaine-presets'")))

;;;; Store and restore preset

(defvar fontaine--preset-history '()
  "Minibuffer history of preset configurations.")

;;;###autoload
(defun fontaine-store-latest-preset ()
  "Write latest cursor state to `fontaine-latest-state-file'.
Can be assigned to `kill-emacs-hook'."
  (when-let ((hist fontaine--preset-history))
    (with-temp-file fontaine-latest-state-file
      (insert ";; Auto-generated file; don't edit -*- mode: "
              (if (<= 28 emacs-major-version)
                  "lisp-data"
                "emacs-lisp")
              " -*-\n")
      (pp (intern (car hist)) (current-buffer)))))

(defvar fontaine-recovered-preset nil
  "Recovered value of latest store cursor preset.")

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

(provide 'fontaine)
;;; fontaine.el ends here
