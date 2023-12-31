(ns org-project.core
  (:require [clojure.string :as string]
            [org-project.utils :as u]))

(def org-file (slurp "/home/thawes/org-files/Projects/naurrnen-website/src/index.org"))
(def vulwin (slurp "/home/thawes/org-files/Projects/naurrnen-website/src/Nations/Vulwin Horde.org"))

(declare parse-content)
(declare handle-string)

(def document (atom []))

(defn handle-title
  [block]
  )

(defn handle-section
  [block]
  (let [section (u/split-line block)
        section-data (u/section-data (first section))
        content-with-props (u/get-properties (second section))
        section-class (if (:html_headline_class (:props content-with-props))
                        {:class (:html_headline_class (:props content-with-props))}
                        "")]
    (into
     []
     (remove
      nil?
      [:div {:class (str "section-" (:level section-data))
             :id (u/sanitize-for-id (:string section-data))}
       (when (:category (:props content-with-props))
         (str "<-- CATEGORY: " (:category (:props content-with-props)) " -->"))
       [(keyword (str "h" (:level section-data))) section-class (:string section-data)]
       (parse-content (:content content-with-props))]))))

(defn handle-image
  ([object]
   (let [image-data object]
     (into [] (remove nil?
                      [:figure [:img (:attr_html image-data) (:img image-data) ]
                       (when (:caption image-data)
                         [:figcaption {:class "caption"} (:caption image-data)])])))))

(defn handle-directive
  [block]
  (let [directive-index (u/re-index-of #"\n[^#\+]" block)
        directives (u/filter-directives (subs block 0 directive-index))
        rest-block (subs block (inc directive-index))
        next-block (subs rest-block 0 (string/index-of rest-block "\n"))]
    ;; (parse-content (string/trim (subs rest-block (string/index-of rest-block "\n"))))
    (cond (u/image-link? next-block) [(handle-image (assoc directives :img (-> next-block
                                                                               (string/replace #"\]" "")
                                                                               (string/replace #"\[" "")
                                                                               (string/trim))))
                                      (parse-content (string/trim (subs rest-block (string/index-of rest-block "\n"))))])
    ))

(defn handle-string
  [block]
  (mapv (fn [line] (vec (cons :p (u/parse-paragraph line))))
        (filter not-empty (string/split block #"\n"))))

(defn handle-quote
  [block]
  (let [[quote remaining] (u/handle-block :begin_quote block)
        quoted-paragraphs (string/split quote #"\n")]
    [[:blockquote (mapv handle-string quoted-paragraphs)] (handle-string remaining)]))

(defn handle-default
  [block]
  (str "<!-- " block " -->"))

(defn parse-content
  [block]
  (case (u/key-prefix block)
    :title (handle-title block)
    :section (handle-section block)
    :directive (handle-directive block)
    :image (handle-image {:img block})
    :begin_quote (handle-quote block)
    :string (handle-string block)
    (handle-default block)))

(defn harness [block]
block)

(defn hiccupify
[document]
(let [sections (u/split-by-headers document)]
  (map parse-content sections)))
