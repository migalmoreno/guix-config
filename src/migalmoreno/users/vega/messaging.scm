(define-module (migalmoreno users vega messaging)
  #:use-module (migalmoreno utils)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features matrix)
  #:use-module (rde features messaging))

(define-public features
  (list
   (feature-matrix-settings
    #:homeserver "https://pantalaimon.conses.eu"
    #:matrix-accounts
    (list
     (matrix-account
      (id "@sloan:conses.eu")
      (server "matrix.conses.eu"))))
   (feature-emacs-ement)
   (feature-slack-settings
    #:slack-accounts
    (list
     (slack-account
      (workspace "clojurians")
      (nick %default-username)
      (cookie? #t))))
   (feature-emacs-slack)
   (feature-emacs-telega)))
