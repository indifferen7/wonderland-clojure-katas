(ns alphabet-cipher.coder)

; A vector containing the characters a to z
(def a->z (map char (range 97 123)))

(defn shift-a->z
  "Shifts a->z so that it starts with the character c."
  [c]
  (let [ix (.indexOf a->z c)]
    (concat (drop ix a->z) (take ix a->z))))

(defn encode-char
  "Encodes a character."
  [keyword-char message-char]
  (let [shifted (shift-a->z keyword-char)]
    (nth shifted (.indexOf a->z message-char))))

(defn encode
  "Encodes the message with the provided keyword."
  [keyword message]
  (->> (map vector (cycle keyword) message)
       (reduce
         (fn [acc [keyword-char message-char]]
           (conj acc (encode-char keyword-char message-char)))
         [])
       (apply str)))

(defn decode-char
  "Decodes a message character."
  [keyword-char message-char]
  (let [shifted (shift-a->z keyword-char)]
    (nth a->z (.indexOf shifted message-char))))

(defn decode
  "Decodes the message with the provided keyword."
  [keyword message]
  (->> (map vector (cycle keyword) message)
       (reduce
         (fn [acc [keyword-char message-char]]
           (conj acc (decode-char keyword-char message-char)))
         [])
       (apply str)))

(defn lengthen
  "Lengthens the provided string s with n characters by repeating it."
  [s n]
  (->> (cycle s)
       (take n)))

(defn resolve-keyword
  "Resolves the keyword of repeated characters in coll."
  [coll]
  (let [length (count coll)]
    (reduce
      (fn [candidate char]
        (if (= (lengthen candidate length) coll)
          (reduced candidate)
          (conj candidate char)))
      []
      coll)))

(defn decipher [cipher message]
  "Extracts the keyword from the cipher and the message."
  (->> (map vector cipher message)
       (reduce
         (fn [acc [cipher-char message-char]]
           (conj acc (decode-char message-char cipher-char)))
         [])
       (resolve-keyword)
       (apply str)))