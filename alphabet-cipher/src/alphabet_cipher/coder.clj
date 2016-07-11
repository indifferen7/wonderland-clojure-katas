(ns alphabet-cipher.coder)

; normal alphabet (i.e. a-z)
(def alphabet (map char (range 97 123)))

(defn shift-alphabet
  "Returns the alphabet with the provided
  char used as the start of the alphabet.
  Any leading characters are appended to
  the end of the resulting char array."
  [c]
  (flatten
    (cons
      (map char (range (int c) 123))
      (map char (range 97 (int c))))))

(defn char->index
  "Returns the index of char c in the
  provided colleciton of chars in xs."
  [c xs]
  (loop [current xs i 0]
    (if (= (first current) c)
      i
      (recur (rest current) (inc i)))))

(defn repeat-keyword
  "Repeats the characters in the keyword n times."
  [keyword n]
  (loop [acc []]
    (if (>= (count acc) n)
      (apply str (first (split-at n (clojure.string/join acc))))
      (recur (conj acc keyword)))))

(defn zip
  "Simply zips the two collections together."
  [xs ys] (map vector xs ys))

(defn encode [keyword message]
  (let [repeated-keyword (repeat-keyword keyword (count message))]
    (apply str
           (for [[c1 c2] (zip repeated-keyword message)]
             (nth (shift-alphabet c1) (char->index c2 alphabet))))))

(defn decode [keyword message]
  (let [repeated-keyword (repeat-keyword keyword (count message))]
    (apply str
           (for [[c1 c2] (zip repeated-keyword message)]
             (nth alphabet (char->index c2 (shift-alphabet c1)))))))

(defn extract-keyword
  "Extracts the keyword based on the provided
  cipher and message."
  [cipher message]
  (apply str
         (for [[c1 c2] (zip cipher message)]
           (nth alphabet (char->index c1 (shift-alphabet c2))))))

(defn equals-repeated?
  "This function repeats the provided candidate
  so that the string equals the length of the
  keyword. If candidate equals keyword, the function
  returns true."
  [candidate keyword]
  (= (repeat-keyword candidate (count keyword)) keyword))

(defn decipher [cipher message]
  (let [keyword (extract-keyword cipher message)]
    (loop [remaining keyword
           acc []]
      (let [candidate (apply str acc)]
        (if
          (or
            (empty? remaining)
            (equals-repeated? candidate keyword))
          candidate
          (recur (rest remaining) (conj acc (first remaining))))))))

