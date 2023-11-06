(ns org-project.core
  (:require [clojure.string :as string]))

(defn split-by-headers
  "Splits a document fed in as a single string on org-mode headers."
  [text]
  (let [pattern #"(?<=\n)(?=\*+ )"]
    (->> (string/split text pattern)
         (remove string/blank?)
         (map #(string/trim %)))))

;; (def org-file (parse (slurp "/home/thawes/org-files/Projects/naurrnen-website/src/index.org")))
(def org-file (split-by-headers (slurp "/home/thawes/org-files/Projects/naurrnen-website/src/index.org")))

(defn key-prefix
  "Determines the keyword for org-mode directives or special block beginnings."
  [line]
  (let [trimmed (clojure.string/trim line)
        section-pattern #"^(\*+)\s.*" ; Match only asterisks at the beginning
        directive-pattern #"^\#\+([a-zA-Z_]+):.*"
        block-begin-pattern #"^\#\+(begin_[a-z_]+)"]
    (cond
      (re-find section-pattern trimmed)
      (->> trimmed
           (re-find section-pattern)
           second ; Get the group of asterisks
           clojure.string/lower-case
           keyword)

      (re-find directive-pattern trimmed)
      (->> trimmed
           (re-find directive-pattern)
           second ; Get the group with the directive name
           clojure.string/lower-case
           keyword)

      (re-find block-begin-pattern trimmed)
      (->> trimmed
           (re-find block-begin-pattern)
           second
           clojure.string/lower-case
           keyword)

      :else :string)))

(defn remove-prefix
  "Removed the prefix from the string and return the result. This is usually org-mode that needs to still be parsed."
  [line]
  (-> line
      string/trim
      (string/replace #"\#\+[a-zA-Z]+_?[a-zA-Z]+:"  "")
      (string/replace #"\*+" "")
      string/trim))


(defn split-line
  "Splits a line on the first line-feed"
  [line]
  (let [index (string/index-of line "\n")]
    (if index
      (list (subs line 0 index) (subs line (inc index))))))

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

(defn handle-block
  "Function for handling org blocks."
  [directive block]
  (let [begin (str "#+" (name directive))
        end (str "#+" (string/replace (name directive) #"begin" "end"))
        begin-index (string/index-of block begin)
        end-index (string/index-of block end)]
    {directive (string/trim (subs block (+ (count begin) begin-index)
                                  end-index))
     :block (string/trim (subs block (+ (count end) end-index)))}))

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
        heading (remove-prefix (first section))
        content-with-props (get-properties (second section))]
    [:div {:class "section-1" :id heading} [:h1 heading]
     ]))

(defmethod hiccupify :** [block]
  (let [section (split-line block)
        heading (remove-prefix (first section))]
    [:div {:class "section-1" :id heading} [:h2 heading] (hiccupify (second section))]))

(defmethod hiccupify :*** [block]
  (let [section (split-line block)
        heading (remove-prefix (first section))]
    [:div {:class "section-1" :id heading} [:h3 heading] (hiccupify (second section))]))

(defmethod hiccupify :properties [block]
  (handle-properties block))

(defmethod hiccupify :begin_quote
  [block]
  (handle-block :begin_quote block))

(defmethod hiccupify :string [block]
  [:p block])

(defmethod hiccupify :default
  [block]
  (str "<!-- " block "-->"))
