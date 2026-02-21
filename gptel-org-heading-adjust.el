;;; gptel-org-heading-adjust.el --- Adjust LLM response headings to org context -*- lexical-binding: t; -*-

;; Author: Ryan
;; Description: Adjusts markdown-to-org converted headlines to nest properly
;;              under the current org heading level.

;;; Commentary:

;; When gptel responses are inserted under an org heading (e.g., ** level 2),
;; the LLM's markdown headings (# ## ###) get converted to org (* ** ***).
;; This results in headings that don't nest properly under the parent.
;;
;; This package adjusts the headline levels so they nest correctly:
;; - If you're under a ** (level 2) heading
;; - LLM's # becomes *** (level 3, not *)
;; - LLM's ## becomes **** (level 4, not **)
;; - etc.
;;
;; The adjustment happens after each streaming chunk is inserted,
;; using text properties to track which headlines have been adjusted.

;;; Code:

(require 'org)
(require 'gptel)

(defvar gptel-org-heading-adjust-enable t
  "When non-nil, adjust LLM response headings to fit org context.")

(defvar-local gptel-org-heading-adjust--extra-stars 0
  "Number of extra stars to add to response headlines.
Captured when a request is sent, based on current org heading level.")

(defvar-local gptel-org-heading-adjust--response-start nil
  "Marker for the start of the current response.
Used to limit the search region for headline adjustment.")

;;; Core adjustment logic

(defun gptel-org-heading-adjust--adjust-headlines ()
  "Adjust org headlines in the current gptel response region.
Adds extra stars to headlines that haven't been adjusted yet.
Uses the `gptel-heading-adjusted' text property to track state."
  (when (and gptel-org-heading-adjust-enable
             (> gptel-org-heading-adjust--extra-stars 0)
             gptel-org-heading-adjust--response-start
             (marker-position gptel-org-heading-adjust--response-start))
    (save-excursion
      ;;(message "Starting for match")
      (goto-char gptel-org-heading-adjust--response-start)
      ;;(message (format "Starting my search at %s" gptel-org-heading-adjust--response-start))
      (while (re-search-forward "^\\(\\*+\\) " nil t)
        ;; Only adjust if within gptel response and not already adjusted
        (let ((star-start (match-beginning 1))
              (star-end (match-end 1)))
          ;;(message (format "Maybe found star at %s %s" star-start star-end))
          (when (and (get-text-property star-start 'gptel)
                     (not (get-text-property star-start 'gptel-heading-adjusted)))
            ;; Add extra stars
            ;;(message (format "Adding extra stars! starting %s ending %s" star-start star-end))
            (goto-char star-end)
            (insert (make-string gptel-org-heading-adjust--extra-stars ?*))
            ;; Mark as adjusted (put property on the first star)
            (put-text-property star-start (1+ star-start)
                               'gptel-heading-adjusted t)))))))

;;; Hook into gptel

(defun gptel-org-heading-adjust--capture-context (&rest _)
  "Capture org heading level before sending request."
  (when (and gptel-org-heading-adjust-enable
             (derived-mode-p 'org-mode))
    (setq gptel-org-heading-adjust--extra-stars
          (or (org-current-level) 0))
    ;; Mark response start position (will be updated by gptel)
    ;;(message (format "We are noting we are in extra stars level %s starting at %s" gptel-org-heading-adjust--extra-stars (point)))
    (setq gptel-org-heading-adjust--response-start
          (copy-marker (point) nil))))

(defun gptel-org-heading-adjust--on-stream ()
  "Hook function to adjust headlines after each stream chunk."
  (when (derived-mode-p 'org-mode)
    (gptel-org-heading-adjust--adjust-headlines)))

(defun gptel-org-heading-adjust--on-response-end (start _end)
  "Clean up after response is complete.
START and _END are the response region bounds."
  (when (and (derived-mode-p 'org-mode)
             gptel-org-heading-adjust--response-start)
    ;; Final adjustment pass
    (gptel-org-heading-adjust--adjust-headlines)
    ;; Clear the marker
    (set-marker gptel-org-heading-adjust--response-start nil)
    (setq gptel-org-heading-adjust--response-start nil)))

;;; Setup and teardown

;;;###autoload
(defun gptel-org-heading-adjust-mode-enable ()
  "Enable automatic org heading level adjustment for gptel responses."
  (interactive)
  (add-hook 'gptel-post-stream-hook #'gptel-org-heading-adjust--on-stream)
  (add-hook 'gptel-post-response-functions #'gptel-org-heading-adjust--on-response-end)
  (advice-add 'gptel-send :before #'gptel-org-heading-adjust--capture-context)
  (setq gptel-org-heading-adjust-enable t)
  (message "gptel org heading adjustment enabled"))

(defun gptel-org-heading-adjust-mode-disable ()
  "Disable automatic org heading level adjustment for gptel responses."
  (interactive)
  (remove-hook 'gptel-post-stream-hook #'gptel-org-heading-adjust--on-stream)
  (remove-hook 'gptel-post-response-functions #'gptel-org-heading-adjust--on-response-end)
  (advice-remove 'gptel-send #'gptel-org-heading-adjust--capture-context)
  (setq gptel-org-heading-adjust-enable nil)
  (message "gptel org heading adjustment disabled"))

(provide 'gptel-org-heading-adjust)
;;; gptel-org-heading-adjust.el ends here
