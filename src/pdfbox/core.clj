(ns pdfbox.core
  (:require [clojure.contrib.trace :as trace])
  (:import [org.apache.pdfbox.pdmodel PDDocument PDPage]
           [org.apache.pdfbox.pdmodel.edit PDPageContentStream]
           [org.apache.pdfbox.pdmodel.font PDType1Font]))




(def *pdf* nil)

(defmacro prog1 [first-expr & body]
  `(let [res# ~first-expr]
     ~@body
     res#))

(defmacro do-pdf [& body]
  `(binding [~'*pdf* (atom {:document (PDDocument.)})]
     (prog1
         (do
           ~@body))))

(defn document []
  (:document @*pdf*))

(defn add-page! []
  (let [page (PDPage.)]
    (.addPage (document) page)
    (swap! *pdf* assoc :current-page page)))

(defn current-page []
  (:current-page @*pdf*))

(comment

  (do-pdf
    (add-page!)
    (with-open [content-stream (PDPageContentStream. (document) (current-page))]
      (.beginText content-stream)
      (.setFont content-stream PDType1Font/HELVETICA_BOLD 12)
      (.moveTextPositionByAmount content-stream 100 700)
      (.drawString content-stream "Hello World")
      (.endText content-stream))
    (save "test2.pdf"))

  (let [doc (PDDocument.)
        page (PDPage.)]
    (.addPage doc page)
    (with-open [content-stream (PDPageContentStream. doc page)]
      (.beginText content-stream)
      (.setFont content-stream PDType1Font/HELVETICA_BOLD 12)
      (.moveTextPositionByAmount content-stream 100 700)
      (.drawString content-stream "Hello World")
      (.endText content-stream))
    (.save doc "test.pdf")
    (.close doc))

  )