(ns matrixrn.app
  (:require
    [tiltontec.model.core :as md]
    [expo.root :as expo-root]
    ["react" :as r]
    ["react-native" :as rn]
    [matrixrn.matrixrn :as mxn]
    ;[matrixrn.demo.simple :as demo]
    ;[matrixrn.demo.navi :as demo]
    ;[matrixrn.demo.flatlist :as demo]
    ;[matrixrn.demo.http :as demo]
    ;[matrixrn.demo.tutorial.main]
    ; Pick one ^^^ for next line...vvv
    [matrixrn.demo.tutorial.main :as demo]))

(defn matrix-build! []
  (reset! mxn/ssdict {})
  (reset! md/matrix (demo/demo)))

(defn                                                       ;; ^:dev/after-load
  start []
  (let [app-matrix (matrix-build!)
        root-mx (md/mget app-matrix :rx-dom)
        ;; _ (prn :root-mmmmmmx!!!! root-mx)
        root-element (md/mget root-mx :react-element)]
    (prn :root-elt root-element)
    (expo-root/render-root
      (fn [] (md/mget root-mx :react-element)))))

(defn init []
  (start))
