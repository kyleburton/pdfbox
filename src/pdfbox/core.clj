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
                            :font PDType1Font/HELVETICA_BOLD
                            :font-size 12
                            :finalizers []})]
     (prog1
         (do
           ~@body)
       (fire-finalizers!))))

(defn font [& [f]]
  (if f
    (swap! *pdf* assoc :font f))
  (:font @*pdf*))

(defn font-size [& [s]]
  (if s
    (swap! *pdf* assoc :font-size s))
  (:font-size @*pdf*))

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

(defn ensure-content-stream []
  (if (not (content-stream))
    (add-content-stream!)))

(defn move-text-position [p1 p2]
  (ensure-content-stream)
  (begin-text)
  (.moveTextPositionByAmount (content-stream) p1 p2))

(defn write-line [text]
  (ensure-content-stream)
  (begin-text)
  (.setFont (content-stream) (font) (font-size))
  (.drawString (content-stream) text)
  (move-text-position 0 (- (font-height (font))))
)

;; height*fontSize*1.05f;
(defn font-height [fnt]
  (* 1.05     ;; factor
     (font-size)
     (/ (-> fnt .getFontDescriptor .getFontBoundingBox .getHeight)
        1000)))

(comment
  (font-height PDType1Font/HELVETICA_BOLD)

    (do-pdf
      (move-text-position 100 700)
      (write-line "some stuff, top")
      (font-size 24)
      (write-line "some stuff, bottom")
      (save "test2.pdf"))

    ;; (font.getStringWidth( lineWithNextWord )/1000) * fontSize;

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