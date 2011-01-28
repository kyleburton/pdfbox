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
  `(binding [~'*pdf* (atom {:document (PDDocument.)
                            :text-mode? (atom false)
                            :finalizers []})]
     (prog1
         (do
           ~@body)
       (fire-finalizers!))))

(defn document []
  (:document @*pdf*))

(defn add-page! []
  (let [page (PDPage.)]
    (.addPage (document) page)
    (swap! *pdf* assoc :current-page page)))

(defn current-page []
  (:current-page @*pdf*))

(defn ensure-page []
  (if (not (current-page))
    (add-page!)))

(defn- fire-finalizers! []
  (doseq [f (:finalizers @*pdf*)]
    (f))
  (swap! *pdf* assoc :finalizers []))

(defn- register-finalizer [f]
  (swap! *pdf* assoc :finalizers (cons f (:finalizers @*pdf*))))

(defn add-content-stream! []
  (ensure-page)
  (let [cs (PDPageContentStream. (document)
                                 (current-page))]
    (register-finalizer (fn [] (.close cs)))
    (swap! *pdf* assoc :content-stream cs)))

(defn content-stream []
  (:content-stream @*pdf*))

(defn- text-mode? []
  @(:text-mode? @*pdf*))

(defn save [file]
  (fire-finalizers!)
  (.save (document) file))

(defn- close-text []
  (when (text-mode?)
    (.endText (content-stream))
    (reset! (:text-mode? @*pdf*) false)))

(defn- begin-text []
  (try
    (.beginText (content-stream))
    (register-finalizer close-text)
    (reset! (:text-mode? @*pdf*) true)
    (catch java.io.IOException e
      :ok)))

(defn write-text [text]
  (begin-text)
  (.setFont (content-stream) PDType1Font/HELVETICA_BOLD 12)
  (.moveTextPositionByAmount (content-stream) 100 700)
  (.drawString (content-stream) text))

(comment

    (do-pdf
      (add-content-stream!)
      (write-text "some stuff, closed")
      ;;(.endText (content-stream))
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