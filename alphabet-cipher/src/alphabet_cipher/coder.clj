(ns alphabet-cipher.coder)

(def alphabet (map char (range 97 123)))

(def alphabet_alphabet (vec (concat alphabet alphabet)))

(defn char_to_int [ch]
  (- (int ch) 97))

(defn int_to_char [integer]
  (char (+ integer 97)))

(defn lookup_encode [x, y]
  (let [x_offset (char_to_int x) ]
    (get (subvec alphabet_alphabet x_offset (+ x_offset 26)) (char_to_int y))))

(defn lookup_decode [y, x]
  (let [x_offset (char_to_int x) ]
    (int_to_char (.indexOf (subvec alphabet_alphabet x_offset (+ x_offset 26)) y))))

(defn encode_helper [keyword message offset code_size coder]
  (if (empty? message)
    '()
    (cons (coder (first message) (get keyword offset))
        (encode_helper
          keyword
          (rest message)
          (mod (+ offset 1) code_size)
          code_size
          coder))))

(defn code [keyword message]
  (fn [look_upper]
    (clojure.string/join "" (encode_helper keyword (seq message) 0 (count keyword) look_upper))))

(defn encode [keyword message]
  ((code keyword message) lookup_encode))

(defn decode [keyword message]
  ((code keyword message) lookup_decode))

(defn decipher-helper [decoded_message coded_message]
  (if (empty? decoded_message)
    '()
    (cons (lookup_decode (first coded_message) (first decoded_message) )
          (decipher-helper (rest decoded_message) (rest coded_message)))))

(defn firstRep [text offset]
  (cond
    (> (+ offset offset) (count text))
      []
    (=  (subvec text 0 offset)
        (subvec text offset (+ offset offset)))
      (subvec text 0 offset)
    :else
      (firstRep text (+ 1 offset))))

(defn decipher [coded_message decoded_message]
  (clojure.string/join ""
    (firstRep (vec (decipher-helper decoded_message coded_message)) 1)))
