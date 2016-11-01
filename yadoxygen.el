;;; yadoxygen --- Doxygen support for Emacs via yasnippet templates

;;; Commentary:

;;; Code:
(require 'semantic)
(require 'yasnippet)

;;;###autoload
(defun yadoxygen-document-function ()
  "Generate and expand a yasnippet template for function documentation."
  (interactive)
  (let ((semantic-enabled semantic-mode)
        (snippet nil))
    ;; temporarily enable semantic-mode so we can parse the file if not already
    ;; enabled
    (unless semantic-enabled
      (semantic-mode 1)
      (semantic-fetch-tags))
    (save-excursion
      ;; find the next function tag
      (let ((tag (senator-next-tag)))
        (while (or (null tag)
                   (not (semantic-tag-of-class-p tag 'function)))
          (setq tag (senator-next-tag)))
        (let ((name (semantic-tag-name tag))
              (arguments (plist-get (semantic-tag-attributes tag) :arguments))
              (return-name (plist-get (semantic-tag-attributes tag) :type)))
          ;; santize arguments and return name
          (if (and (= 1 (length arguments))
                   (string-equal "" (caar arguments)))
              (setq arguments nil))
          (if (listp return-name)
              (setq return-name (car return-name)))
          (if (string-equal "void" return-name)
              (setq return-name nil))
          (let ((args (if arguments
                          (concat
                           (mapconcat
                            (lambda (arg)
                              (let ((name (first arg))
                                    (type (first (plist-get (third arg) :type))))
                                (format " * @param %s ${A %s}\n"
                                        name type)))
                            arguments
                            ""))
                        nil))
                (ret (if return-name
                         (format " * @return ${%s}\n" return-name)
                       nil)))
            (setq snippet
                  (format
                   (mapconcat #'identity
                              (remove-if #'null (list "/**\n * @brief ${%s}\n"
                                                      (concat args ret)
                                                      " * ${Detailed description}\n"
                                                      " */\n"))
                              " *\n")
                   name))))))
    ;; disable semantic again if we turned it on above
    (unless semantic-enabled
      (semantic-mode 0))
    (when snippet
      (yas-expand-snippet snippet))))

(provide 'yadoxygen)
;;; yadoxygen.el ends here
