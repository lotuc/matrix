(ns demo.window-location
  (:require
   [tiltontec.matrix.api :as mx]))

(defn make-window-location [& kvs]
  (apply mx/make :js-window/location
         :on-quiesce (fn [c]
                       (js/window.navigation.removeEventListener
                        "navigate" (mx/mget c :listener)))
         :listener (mx/cFonce
                    (let [cb (fn [e]
                               (let [u (js/URL. (.-url (.-destination e)))]
                                 (mx/mset! me :href (.-href u))
                                 (mx/mset! me :origin (.-origin u))
                                 (mx/mset! me :host (.-host u))
                                 (mx/mset! me :hostname (.-hostname u))
                                 (mx/mset! me :port (.-port u))
                                 (mx/mset! me :protocol (.-protocol u))
                                 (mx/mset! me :pathname (.-pathname u))
                                 (mx/mset! me :search (.-search u))
                                 (mx/mset! me :hash (.-hash u))))]
                      (js/window.navigation.addEventListener "navigate" cb)
                      cb))
         :hash (mx/cI js/window.location.hash)
         :host (mx/cI js/window.location.host)
         :hostname (mx/cI js/window.location.hostname)
         :href (mx/cI js/window.location.href)
         :origin (mx/cI js/window.location.origin)
         :pathname (mx/cI js/window.location.pathname)
         :port (mx/cI js/window.location.port)
         :protocol (mx/cI js/window.location.protocol)
         :search (mx/cI js/window.location.search)
         kvs))
