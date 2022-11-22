(define-module (conses packages gnome)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:))

;; TODO: sofia_sip
;; TODO: callaudiod
(define-public calls
  (package
    (name "calls")
    (version "43.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/calls")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1dqjfpayycz8spxm0wapas494vsdf0crfq2hyacyif0q4scsy42p"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs
     (list
      vala
      pkg-config
      desktop-file-utils
      gtk-doc
      docbook-xsl
      docbook-xml))
    (inputs
     (list
      modem-manager
      libhandy
      evolution-data-server
      folks
      gsound
      gom
      gstreamer
      gst-plugins-bad
      gst-plugins-base
      gst-plugins-good
      gst-plugins-ugly
      feedbackd
      gtk+
      libpeas))
    (home-page "https://gitlab.gnome.org/GNOME/calls")
    (synopsis "A phone dialer and caller handler")
    (description "GNOME Calls is a phone dialer and call handler.")
    (license license:gpl3+)))
