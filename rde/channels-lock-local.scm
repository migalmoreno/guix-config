(list (channel
        (name 'nonguix)
        (url "file:///home/vega/src/guile/nonguix")
        (branch "base-to-upstream")
        (commit
          "026b5b5c47f606161113398cda710a7c85164937"))
      (channel
        (name 'guix)
        (url "file:///home/vega/src/guile/guix")
        (branch "base-to-upstream")
        (commit
          "5c9a6ec115f866c7f3bd30653527a88a9702342b"))
      (channel
        (name 'rde)
        (url "file:///home/vega/src/guile/rde")
        (branch "base-to-upstream")
        (commit
          "81a9dc0055aece8d1914b7278430e256dad3fda2")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))
