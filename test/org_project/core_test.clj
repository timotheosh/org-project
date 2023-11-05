(ns org-project.core-test
  (:require [clojure.test :refer :all]
            [org-project.core :refer :all]))

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
    (is (= (key-prefix "#+results:") :results)))

  (testing "section headings"
    (is (= (key-prefix "* First level heading") :*))
    (is (= (key-prefix "** Second level heading") :**))
    (is (= (key-prefix "*** Third level heading") :***)))

  (testing "lines that are not directives or headings"
    (is (= (key-prefix "Just some text.") :string))
    (is (= (key-prefix "Another line of text") :string))
    (is (= (key-prefix "# Not a directive") :string))
    (is (= (key-prefix "****Not a section because of the missing space") :string)))

  (testing "empty lines"
    (is (= (key-prefix "") :string))
    (is (= (key-prefix " ") :string)))

  (testing "lines with only whitespace before directive or heading"
    (is (= (key-prefix "  #+options: toc:nil") :options))
    (is (= (key-prefix "   * Whitespace before heading") :*))))
