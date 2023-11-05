(ns org-project.core
  (:require [clojure.string :as string]))

(defn split-by-headers [text]
  (let [pattern #"(?<=\n)(?=\*+ )"]
    (->> (clojure.string/split text pattern)
         (remove clojure.string/blank?)
         (map #(string/trim %)))))

;; (def org-file (parse (slurp "/home/thawes/org-files/Projects/naurrnen-website/src/index.org")))
(def org-file (split-by-headers (slurp "/home/thawes/org-files/Projects/naurrnen-website/src/index.org")))

(defn key-prefix [line]
  (let [index (string/index-of line " ")
        eol (string/index-of line "\n")]
    (cond  (and (nil? eol) (nil? index)) (keyword (string/lower-case line))
           (or (and (nil? index) eol) (and (and index eol) (> index eol))) (keyword (string/lower-case (subs line 0 eol)))
           :else (keyword (string/lower-case (subs line 0 index))))))

(defn remove-prefix [line]
  (let [index (string/index-of line " ")]
    (if (nil? index)
      line
      (subs line (inc index)))))

(defn split-line [line]
  (let [index (string/index-of line "\n")]
    (if index
      (list (subs line 0 index) (subs line (inc index))))))

(defn orig-get-properties [line]
  (let [begin ":PROPERTIES:"
        end ":END:"
        start-index (+ (count begin) (string/index-of line begin))
        end-index (string/index-of line end)
        content-index (+ (count end) end-index)]
    (when (> start-index end-index)
      (throw (Throwable. "MALFORMED PROPERTIES")))
    (let [props-string (string/trim (subs line start-index end-index))
          props (-> props-string
                    (string/split #"\n")
                    ((fn [x]
                       (map #(string/split % #" ") x)))
                    ((fn [x]
                       (map (fn [y] [(-> (first y)
                                         string/lower-case
                                         (string/replace #":" "")
                                         keyword) (second y)]) x))))]
      (assoc (into (sorted-map) props) :content (string/trim (subs line content-index))))))

(defn parse-property-line [line]
  (let [parts (string/split line #" ")]
    {(keyword (string/replace (string/lower-case (first parts)) #":" ""))
     (second parts)}))

(defn extract-properties [props-string]
  (->> (string/split props-string #"\n")
       (map parse-property-line)
       (into [])))

(defn get-properties [line]
  (let [begin ":PROPERTIES:"
        end ":END:"
        start-index (when-let [index (string/index-of line begin)]
                      (+ index (count begin)))
        end-index (string/index-of line end)]
    (if (or (nil? start-index) (nil? end-index) (> start-index end-index))
      {:content line} ; Return the original line if no properties are found or if they're malformed.
      (let [props-string (string/trim (subs line start-index end-index))
            content (string/trim (subs line (+ end-index (count end))))]
        (assoc {:props (extract-properties props-string)} :content content)))))

(defmulti handle-properties
  (fn [props]
    (first (keys props))))

(defmethod handle-properties :html_headline_class [data]
  {:class (first (vals data))})

(defmethod handle-properties :default
  [data]
  (str "<!-- " data " -->"))

(defmulti hiccupify key-prefix)

(defmethod hiccupify :* [block]
  (let [section (split-line block)
        heading (remove-prefix (first section))]
    [:div {:class "section-1" :id heading} [:h1 heading] (hiccupify (second section))]))

(defmethod hiccupify :** [block]
  (let [section (split-line block)
        heading (remove-prefix (first section))]
    [:div {:class "section-1" :id heading} [:h2 heading] (hiccupify (second section))]))

(defmethod hiccupify :*** [block]
  (let [section (split-line block)
        heading (remove-prefix (first section))]
    [:div {:class "section-1" :id heading} [:h3 heading] (hiccupify (second section))]))


(defmethod hiccupify :default [block]
  [:p block])
