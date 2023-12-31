(ns org-project.utils-test
  (:require [clojure.test :refer :all]
            [org-project.utils :refer :all]))

(deftest test-heirachical-split
  (testing ""
    (is (=  (split-by-headers "#+title: Welcome\n\n* Section 1\n\nNullam libero mauris.\n** Subsection 1\nOnsequat quis, varius et, dictum id, arcu.\n* Section 2\nSed id ligula quis est convallis tempor.")
            '("#+title: Welcome"
              "* Section 1\n\nNullam libero mauris."
              "** Subsection 1\nOnsequat quis, varius et, dictum id, arcu."
              "* Section 2\nSed id ligula quis est convallis tempor.")))))

(deftest test-key-prefix
  (testing "org-mode directives"
    (is (= (key-prefix "#+title: My Document") :title))
    (is (= (key-prefix "#+TITLE: My Document") :title)) ; case insensitivity
    (is (= (key-prefix "#+results:") :directive)))

  (testing "section headings"
    (is (= (key-prefix "* First level heading") :section))
    (is (= (key-prefix "** Second level heading") :section))
    (is (= (key-prefix "*** Third level heading") :section)))

  (testing "lines that are not directives or headings"
    (is (= (key-prefix "Just some text.") :string))
    (is (= (key-prefix "Another line of text") :string))
    (is (= (key-prefix "# Not a directive") :string))
    (is (= (key-prefix "****Not a section because of the missing space") :string)))

  (testing "special block beginnings"
    (is (= (key-prefix "#+begin_quote\n") :begin_quote))
    (is (= (key-prefix "#+begin_example\n") :begin_example))
    (is (= (key-prefix "#+begin_src clojure\n") :begin_src)))

  (testing "empty lines"
    (is (= (key-prefix "") :string))
    (is (= (key-prefix " ") :string)))

  (testing "lines with only whitespace before directive or heading"
    (is (= (key-prefix "  #+options: toc:nil") :directive))
    (is (= (key-prefix "   * Whitespace before heading") :section))))

(deftest test-remove-prefix
  (testing "org directives"
    (is (= (remove-prefix "#+attr_html: :id pic-banner :alt Bellgrald") ":id pic-banner :alt Bellgrald"))
    (is (= (remove-prefix "#+HTML: <button type=\"button\" class=\"collapsible\">") "<button type=\"button\" class=\"collapsible\">"))
    (is (= (remove-prefix "#+ATTR_HTML: :id pic-banner :alt Vaabhath") ":id pic-banner :alt Vaabhath")))
  (testing "section headers"
    (is (= (remove-prefix "* Section 1") "Section 1"))
    (is (= (remove-prefix "**** Section 4") "Section 4"))
    (is (= (remove-prefix "*** Section 3\nLorem Ipso") "Section 3\nLorem Ipso"))))

(deftest test-split-line
  (testing "splitting lines only once"
    (is (= (split-line "* Section 1\nLorem Ipso.\nNam a sapien.") '("* Section 1" "Lorem Ipso.\nNam a sapien.")))))

(deftest test-section-data
  (testing "get section data"
    (is (= (section-data "***   Welcome to Org-Mode") {:level 3 :string "Welcome to Org-Mode"}))))

(deftest test-sanitize-for-id
  (testing "sanitize-for-id"
    (is (= (sanitize-for-id "Welcome to Naurrnen & Stuff") "welcome_to_naurrnen_and_stuff"))
    (is (= (sanitize-for-id "Laurië Citime") "lauri_citime"))))

(deftest test-parse-property-line
  (testing "testing parse-property-line"
    (is (= (parse-property-line ":HTML_HEADLINE_CLASS: absent") {:html_headline_class "absent"}))
    (is (= (parse-property-line ":category: Places") {:category "Places"}))))

(deftest test-extract-properties
  (testing "testing extract properties"
    (is (= (extract-properties ":HTML_HEADLINE_CLASS: absent")
           {:html_headline_class "absent"}))
    (is (= (extract-properties ":HTML_HEADLINE_CLASS: absent\n:CATEGORY: Places")
           {:html_headline_class "absent" :category "Places"}))))

(deftest test-get-properties
  (testing "test get-properties"
    (is (= (get-properties ":PROPERTIES:\n:HTML_HEADLINE_CLASS: absent\n:END:\n#+caption: Main port of the city")
           {:props {:html_headline_class "absent"},
            :content "#+caption: Main port of the city"}))))

(deftest test-handle-blocks
  (testing "test handle-blocks"
    (is (= (handle-block :begin_quote "#+begin_quote\nPower corrupts?\n--Lord Acton\n#+end_quote\nAliquam posuere.  ")
           ["Power corrupts?\n--Lord Acton", "Aliquam posuere."]))
    (is (= (handle-block :begin_quote "#+BEGIN_QUOTE\nTo be or not to be?\n--Shakespeare\n#+END_QUOTE\nDonec posuere augue in quam.  ")
           ["To be or not to be?\n--Shakespeare", "Donec posuere augue in quam."]))))
