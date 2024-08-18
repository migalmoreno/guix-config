(define-module (migalmoreno users vega mail)
  #:use-module (migalmoreno utils)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features mail))

(define gnus-topic-alist
  '(("Personal"
     "nnmaildir+personal:inbox"
     "nnmaildir+personal:drafts"
     "nnmaildir+personal:sent"
     "nnmaildir+personal:spam"
     "nnmaildir+personal:trash")
    ("Bug Trackers"
     "nntp+gwene:gmane.comp.gnu.guix.bugs"
     "nntp+gwene:gmane.comp.gnu.guix.patches")
    ("News"
     "nntp+gwene:gwene.rs.lobste"
     "nntp+gwene:gwene.org.hnrss.newest.points"
     "nntp+gwene:gwene.net.lwn.headlines.newrss")
    ("Inbox")
    ("Gnus")))

(define gnus-topic-topology
  '(("Gnus" visible)
    (("Inbox" visible)
     (("Personal" visible nil)))
    (("Bug Trackers" visible nil))
    (("News" visible nil))))

(define gnus-group-parameters
  '(("^nnmaildir"
     (gcc-self . "nnmaildir+personal:sent")
     (display . 10000))
    ("^nntp"
     (display . 1000))))

(define gnus-posting-styles
  `((".*"
     (cc ,%default-email))
    ((header "cc" ".*@debbugs.gnu.org")
     (To rde-gnus-get-article-participants)
     (cc nil))
    ((header "to" ".*@lists.sr.ht")
     (To rde-gnus-get-article-participants)
     (cc ,%default-email))
    ("^nntp.+:"
     (To rde-gnus-get-article-participants)
     (cc ,%default-email))))

(define-public features
  (list
   (feature-mail-settings
    #:mail-accounts
    (list
     (mail-account
      (id 'personal)
      (type 'gandi)
      (fqda %default-email)
      (pass-cmd "pass show mail/personal")))
    #:mail-directory-fn
    (const (string-append (getenv "XDG_STATE_HOME") "/mail")))
   (feature-isync)
   (feature-goimapnotify
    #:notify? #t)
   (feature-emacs-gnus
    #:topic-alist gnus-topic-alist
    #:topic-topology gnus-topic-topology
    #:message-archive-method '(nnmaildir "personal")
    #:message-archive-group '((".*" "sent"))
    #:group-parameters gnus-group-parameters
    #:posting-styles gnus-posting-styles)
   (feature-emacs-message)
   (feature-emacs-org-mime
    #:html-element-styles
    '(("pre" . "color: #E6E1Dc; background-color: #232323; padding: 0.5em;")
      ("blockquote" . "border-left: 2px solid gray; padding-left: 4px;")))
   (feature-msmtp)
   (feature-emacs-debbugs)
   (feature-emacs-ebdb
    #:ebdb-sources (list "~/documents/contacts")
    #:ebdb-popup-size 0.2)))
