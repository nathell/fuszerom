(ns fuszerom.dictionary
  (:require
    [clojure.java.io :as io]
    [nio.core :as nio]))

(defn slurp-bytes [f]
  (let [file (io/file f)]
    (if-not (.isFile file)
      (throw (ex-info "Not a file" {:file f}))
      (let [len (.length file)
            arr (make-array Byte/TYPE len)]
        (with-open [stream (io/input-stream file)]
          (.read stream arr 0 len))
        arr))))

(defn read-null-terminated-string [buf]
  (String. (into-array Byte/TYPE (take-while (comp not zero?) (repeatedly #(.get buf))))))

(defn read-tag [buf]
  [(Short/toUnsignedInt (.getShort buf))
   (read-null-terminated-string buf)])

(defn read-tags [buf]
  (let [num (Short/toUnsignedInt (.getShort buf))
        tags (repeatedly num #(read-tag buf))]
    (into {} tags)))

(defn read-separators-list [buf]
  (let [num (Short/toUnsignedInt (.getShort buf))]
    (vec (repeatedly num #(.getInt buf)))))

(defn read-segrules-fsa-options [buf]
  (let [num (Byte/toUnsignedLong (.get buf))]
    (into {} (repeatedly num #(vector (read-null-terminated-string buf)
                                      (read-null-terminated-string buf))))))

(defn read-segrules-fsa [buf]
  (let [size (.getInt buf)]
    ;; TODO: deserialize the FSA
    (.position buf (+ (.position buf) size))
    size))

(defn read-segrules-fsa-entry [buf]
  [(read-segrules-fsa-options buf)
   (read-segrules-fsa buf)])

(defn read-segrules-fsa-map [buf]
  (let [num (Byte/toUnsignedLong (.get buf))]
    (into {} (repeatedly num #(read-segrules-fsa-entry buf)))))

(def magic-number (unchecked-int 0x8fc2bc1b))
(def version-number 21)
(def version-number-offset 4)
(def implementation-number-offset 5)
(def fsa-data-size-offset 6)
(def fsa-data-offset 10)

(defn load-dictionary [filename]
  (let [bytes (slurp-bytes filename)
        buffer (nio/byte-buffer bytes)
        _ (when-not (and (= (.getInt buffer) magic-number)
                         (= (.get buffer) version-number))
            (throw (ex-info "Not a dictionary file" {:filename filename})))
        _ (.position buffer fsa-data-size-offset)
        fsa-data-size (.getInt buffer)
        epilogue-offset (+ fsa-data-offset fsa-data-size)
        _ (.position buffer epilogue-offset)
        a1-size (.getInt buffer) ; FIXME: proper name
        id (read-null-terminated-string buffer)
        copyright (read-null-terminated-string buffer)
        tagset-id (read-null-terminated-string buffer)
        tags (read-tags buffer)
        names (read-tags buffer)
        labels (read-tags buffer)
        _ (.position buffer (+ epilogue-offset a1-size 4))
        separators-list (read-separators-list buffer)
        segrules-fsa-map (read-segrules-fsa-map buffer)
        _ (.position buffer implementation-number-offset)
        impl-num (.get buffer)
        _ (.position buffer fsa-data-offset)
        short-labels (vec (take 257 (repeatedly #(.get buffer))))]
    {:buffer buffer
     :short-labels short-labels
     :id id
     :copyright copyright
     :tagset-id tagset-id
     :tags tags
     :names names
     :labels labels
     :separators-list separators-list
     :segrules-fsa-map segrules-fsa-map
     :impl-num impl-num}))

(defn follow [{:keys }])
