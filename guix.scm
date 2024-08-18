(use-modules (farg provider)
             (migalmoreno config)
             (migalmoreno dispatcher)
             (migalmoreno themes))

(dispatcher
 (list
  (config
   (host (host (name "cygnus")
               (features (@ (migalmoreno hosts cygnus) features))))
   (users (list (user
                 (name "deneb")
                 (features (@ (migalmoreno users deneb) features)))))
   (machine (@ (migalmoreno machines cygnus) machines)))
  (config
   (host (host (name "lyra")
               (features (@ (migalmoreno hosts lyra) features))))
   (users (list (user
                 (name "vega")
                 (features
                  (farg:theme-provider
                   dark-theme
                   (@ (migalmoreno users vega) features)))))))
  (config
   (host (host (name "live")
               (features (@ (migalmoreno hosts live) features)))))))
