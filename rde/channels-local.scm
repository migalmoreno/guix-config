(list (channel
       (name 'rde)
       (url "file:///home/vega/src/guile/rde")
       (branch "base-to-upstream")
       (introduction
        (make-channel-introduction
         "257cebd587b66e4d865b3537a9a88cccd7107c95"
         (openpgp-fingerprint
          "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
       (name 'guix)
       (url "file:///home/vega/src/guile/guix")
       (branch "base-to-upstream"))
      (channel
       (name 'nonguix)
       (url "file:///home/vega/src/guile/nonguix")
       (branch "base-to-upstream")))
