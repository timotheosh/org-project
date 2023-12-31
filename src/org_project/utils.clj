(ns org-project.utils
  (:require [clojure.string :as string]))

(defn split-by-headers
  "Splits a document fed in as a single string on org-mode headers."
  [text]
  (let [pattern #"(?<=\n)(?=\*+ )"]
    (->> (string/split text pattern)
         (remove string/blank?)
         (map #(string/trim %)))))

(defn re-index-of [regex s]
  (let [m (re-matcher regex s)]
    (when (re-find m)
      (.start m))))

(defn re-indexes-of [regex s]
  (let [m (re-matcher regex s)]
    (when (re-find m)
      {:start (.start m)
       :end (.end m)})))

(defn key-prefix
  "Determines the keyword for org-mode directives or special block beginnings."
  [line]
  (when (not-empty line)
    (let [trimmed (clojure.string/trim line)
          title-pattern #"^\#\+(title|TITLE):.*"
          section-pattern #"^(\*+)\s.*" ; Match only asterisks at the beginning
          directive-pattern #"^\#\+([a-zA-Z_]+):.*"
          block-begin-pattern #"^\#\+(BEGIN_[A-Z_]+|begin_[a-z_]+)"]
      (cond
        (re-find title-pattern trimmed)
        :title

        (re-find section-pattern trimmed)
        :section

        (re-find directive-pattern trimmed)
        :directive

        (= line "image")
        :image

        (re-find block-begin-pattern trimmed)
        (->> trimmed
             (re-find block-begin-pattern)
             second
             clojure.string/lower-case
             keyword)

        :else :string))))

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

(defn section-data
  "Returns data on a section line."
  [section-line]
  (let [index (string/index-of section-line " ")
        level (count (string/trim (subs section-line 0 index)))
        section-string (string/trim (subs section-line (inc index)))]
    {:level level
     :string section-string}))

(defn sanitize-for-id [title]
  (-> title
      (clojure.string/lower-case)
      (clojure.string/replace #"&" "and")
      (clojure.string/replace #"[^\w\s-]" "") ; remove special chars
      (clojure.string/replace #"\s+" "_")))   ; replace spaces with underscores

(defn extract-directive-block [input]
  (re-find #"(?s)(\#\+.*?\n)*\[\[.*?\]\](.*?\n\n)" input))


(defn parse-property-line
  "Simple transform a property naem into a clojure keyword with its value."
  [line]
  (let [parts (string/split line #" ")]
    {(keyword (string/replace (string/lower-case (first parts)) #":" ""))
     (second parts)}))

(defn extract-properties
  "Extracts the properties from a block-string."
  [props-string]
  (->> (string/split props-string #"\n")
       (map parse-property-line)
       (into {})))

(defn get-properties
  "Main function for parsing properties from a line"
  [line]
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
  "Function for handling org blocks. Returns a vector of two values, first is the block content, the second is the rest of the string following the block content."
  [directive block]
  (let [begin (str "#+" (name directive))
        end (str "#+" (string/replace (name directive) #"begin" "end"))
        begin-index (string/index-of (string/lower-case block) begin)
        end-index (string/index-of (string/lower-case block) end)]
    [(string/trim (subs block (+ (count begin) begin-index)
                        end-index))
     (string/trim (subs block (+ (count end) end-index)))]))

(defn image-link?
[block]
(re-find #"\[\[.*?\.(png|jpg|webp)\]\]" block))

(defn link?
[block]
(re-find #"\[\[.*?\]\[.*?\]\]" block))

(defn parse-attr-string
"This parses an #+ATTR_HTML: line for hiccup.
  ChatGPT generated this function (after I suggested a fitting strategy). Be very afraid!
  this:
  #+HTML_ATTR: :id SomeID :alt This is a picture of a door :width 100%
  looks deceptively similar to a Clojure map, but is not easily converted into a representative map.
  We have to split on keywords, not spaces."
[attr-string]
(let [attr-parts (-> attr-string
                     (clojure.string/split #":"))  ;; Split the string on ':'
      attr-map (reduce (fn [attrs-map part]
                         (let [parts (clojure.string/split part #" " 2) ;; Each string is prefixed with it's keyword.
                               key (keyword (first parts))
                               value (second parts)]
                           (if (nil? (get attrs-map key))
                             (assoc attrs-map key value)
                             (update attrs-map key #(str % " :" (clojure.string/trim part))))))
                       {}
                       (rest attr-parts))]
  (into {} (map (fn [[k v]] [k (clojure.string/trim v)]) attr-map))))

(defn filters-for-directives [directive]
(let [the-key (first directive)]
  (cond (= the-key :caption) directive
        (= the-key :attr_html) [the-key (parse-attr-string (second directive))])))

(defn filter-directives
"Filters directives that we do support."
[block]
(let [parse-line (fn [line]
                   (when-let [[_ key value] (re-matches #"\A#\+(.*?):\s*(.*)\z" line)]
                     [(keyword key) value]))
      dir-map (into [] (comp (map parse-line) (remove nil?)) (string/split block #"\n"))]
  (into {} (map filters-for-directives dir-map))))

(defn handle-link
"Special formatting for org links."
  [link]
  (-> link
      (string/replace #"^file:" "")
      (string/replace #"\.org$" ".html")
      (string/replace #"\.org::[\*]+" ".html#")))

(defn parse-paragraph
  "This handles the minor descrepencies within a normal paragraph. Results are recursive for handling nested directives."
  [text]
  (loop [remaining text
         result []]
    (cond
      (re-find #"\[\[(.*?)\]\[(.*?)\]\]" remaining)
      (let [[_ before url label after] (re-find #"(.*?)\[\[(.*?)\]\[(.*?)\]\](.*)" remaining)]
        (recur after (conj result before [:a {:href (handle-link url)} (parse-paragraph label)])))

      (re-find #"\*(.*?)\*" remaining)
      (let [[_ before bold after] (re-find #"(.*?)\*(.*?)\*(.*)" remaining)]
        (recur after (conj result before [:strong (parse-paragraph bold)])))

      (re-find #"/(.*?)/" remaining)
      (let [[_ before italic after] (re-find #"(.*?)\/(.*?)\/(.*)" remaining)]
        (recur after (conj result before [:em (parse-paragraph italic)])))

      (re-find #"~(.*?)~" remaining)
      (let [[_ before literal after] (re-find #"(.*?)~(.*?)~(.*)" remaining)]
        (recur after (conj result before [:code (parse-paragraph literal)])))

      (re-find #"_(.*?)_" remaining)
      (let [[_ before underscore after] (re-find #"(.*?)_(.*?)_(.*)" remaining)]
        (recur after (conj result before [:span {:style "text-decoration: underline;"} (parse-paragraph underscore)])))

      :else
      (conj (into [] (remove empty? result)) remaining))))
