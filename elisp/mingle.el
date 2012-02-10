(require 'url)
(require 'xml-parse)

(defcustom mingle-root-url "mingle.local"
  "*Base url for your mingle instance"
  :type 'string
  :group 'mingle)

(defcustom mingle-username ""
  "*Username for your mingle instance"
  :type 'string
  :group 'mingle)

(defcustom mingle-password ""
  "*Password for your mingle instance"
  :type 'string
  :group 'mingle)

(defvar mingle-project "packmanager"
  "project you want to interact with")

(defun mingle-card ()
  "display a mingle card by id"
  (interactive)
  (let* ((card-number (read-from-minibuffer "Card: "))
         (card-url (mingle-url (concat "cards/" card-number ".xml"))))
    (mingle-received-card (url-retrieve-synchronously card-url))))

(defun mingle-url (endpoint)
  "construct a mingle url for a request"
  (concat "http://" mingle-username ":" mingle-password "@"
          mingle-root-url "/api/v2/projects/" mingle-project "/" endpoint))

(defun mingle-received-card (buffer)
  "Display results"
  (with-current-buffer buffer
    (let* ((card (mingle-parse-card))
           (desc (mingle-card-description card))
           (owner (mingle-card-owner card))
           (designer (mingle-card-designer card))
           (type (mingle-card-type card))
           (dev (mingle-card-dev-owner card))
           (name (mingle-card-name card))
           (priority (mingle-card-priority card))
           (branch (mingle-card-branch card))
           (status (mingle-card-status card)))

      (mingle-display (concat
                       (mingle-field "Title" name)
                       (mingle-field "Type" type)
                       (mingle-field "Priority" priority)
                       (mingle-field "Owner" owner)
                       (mingle-field "Dev Owner" dev)
                       (mingle-field "Designer" designer)
                       (mingle-field "Branch" branch)
                       (mingle-field "Status" status)

                       (mingle-title "Description")
                       desc)))))

(defun mingle-field (label value)
  (concat
   (mingle-underline (concat label ": " value) ?-)))

(defun mingle-title (str)
  (concat
   "\n" (mingle-underline str ?=) "\n"))

(defun mingle-underline (str char)
  "underlines a string"
  (concat str "\n"
          (make-string (length str) char) "\n"))

(defun mingle-parse-card ()
  (mingle-start-of-card)
  (car
   (xml-parse-region (point) (point-max) nil nil nil)))

(defun mingle-display (str)
  (with-output-to-temp-buffer "*mingle*"
    (princ str)))

(defun mingle-card-status (card)
  (caddr (mingle-property-value card "Card Status")))

(defun mingle-card-branch (card)
  (caddr (mingle-property-value card "Branch")))

(defun mingle-card-priority (card)
  (caddr (mingle-property-value card "Priority")))

(defun mingle-card-description (card)
  (caddr (assoc 'description card)))

(defun mingle-card-name (card)
  (caddr (assoc 'name card)))

(defun mingle-card-owner (card)
  (let ((name (assoc 'name (mingle-property-value card "Owner"))))
    (if (consp name)
        (caddr name))))

(defun mingle-card-type (card)
  (let ((name (assoc 'name (assoc 'card_type card))))
    (if (consp name)
        (caddr name))))

(defun mingle-card-dev-owner (card)
  (let ((name (assoc 'name (mingle-property-value card "Dev Owner"))))
    (if (consp name)
        (caddr name))))

(defun mingle-card-designer (card)
  (let ((name (assoc 'name (mingle-property-value card "Designer"))))
    (if (consp name)
        (caddr name))))

(defun mingle-start-of-card ()
  (re-search-forward "<card>")
  (beginning-of-line-text))

(defun mingle-property-value (card name)
  "get the value for a given property"
  (assoc 'value (mingle-property card name)))

(defun mingle-property (card name)
  "find the property value for name"
  (let ((props (assoc 'properties card))
        (property-with-name-p (lambda (prop)
                                (if (consp prop)
                                    (equal name (caddr (assoc 'name prop)))))))

    (find-if property-with-name-p props)))

(provide 'mingle)
