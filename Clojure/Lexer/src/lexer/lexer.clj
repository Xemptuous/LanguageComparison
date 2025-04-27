(ns lexer.lexer
  (:require [lexer.token :refer [->Token singleTokenMap doubleTokenMap keywordMap]]
            [clojure.string :as str]))

(defrecord Lexer [input char curr peek])

(defn advance [lexer]
  (let [p (:peek lexer)
        ch (if (< p (count (:input lexer)))
             (nth (:input lexer) p)
             nil)]
    (->Lexer (:input lexer) ch p (inc p))))

(defn newLexer [input]
  (advance (->Lexer input nil 0 0)))

(defn isLetter [c]
  (when c (or (Character/isLetterOrDigit c) (= c \_))))

(defn isDigit [c]
  (when c (Character/isDigit c)))

(defn isWhitespace [c]
  (when c (<= (int c) 32)))

(defn readWhile [lexer pred]
  (loop [acc [] l lexer]
    (let [c (:char l)]
      (if (and c (pred c))
        (recur (conj acc c) (advance l))
        [(apply str acc) l]))))

(defn readIdentifier [lexer]
  (readWhile lexer isLetter))

(defn readNumber [lexer]
  (readWhile lexer #(or (isDigit %) (= % \.))))

(defn readString [lexer]
  (readWhile lexer #(not= % \")))

(defn readChar [lexer]
  (readWhile lexer #(not= % \')))

(defn readComment [lexer]
  (readWhile lexer #(not (contains? #{0, 10, 12, 13} (int %)))))

(defn nextToken [lexer]
  (let [{:keys [input curr char]} lexer
        ds (str/join "" (take 2 (drop curr input)))]
    (cond
      (isWhitespace char) (nextToken (advance lexer))

      ;; Early return for comments
      (= ds "//")
      (let [[literal new-lexer] (readComment lexer)]
        [(->Token :comment literal) (advance new-lexer)])

      ;; Double toks
      (contains? doubleTokenMap ds)
      (let [token-type (get doubleTokenMap ds)]
        [(->Token token-type ds) (advance (advance lexer))])

      ;; String literal
      (= char \")
      (let [[literal new-lexer] (readString (advance lexer))]
        [(->Token :string literal) (advance new-lexer)])

      ;; Char literal
      (= char \')
      (let [[literal new-lexer] (readChar (advance lexer))]
        [(->Token :char literal) (advance new-lexer)])

      ;; Single toks
      (contains? singleTokenMap char)
      (let [token-type (get singleTokenMap char)]
        [(->Token token-type (str char)) (advance lexer)])

      ;; a-zA-Z
      (isLetter char)
      (let [[ident new-lexer] (readIdentifier lexer)
            token-type (get keywordMap ident)]
        (if (nil? token-type)
          [(->Token :identifier ident) new-lexer]
          [(->Token token-type ident) new-lexer]))

      ;; 0-9
      (isDigit char)
      (let [[num new-lexer] (readNumber lexer)
            num-decimal (count (filter #{\.} num))
            token-type (case num-decimal 0 :number 1 :float :illegal)]
        [(->Token token-type num) new-lexer])

      ;; otherwise
      (nil? char) [(->Token :eof "eof") lexer]
      :else [(->Token :illegal (str char)) (advance lexer)])))

(defn parseWhile [lexer]
  (let [[tok new-lexer] (nextToken lexer)]
    (println tok)
    (cond
      (= (:type tok) :eof) :ok
      :else (recur new-lexer))))
