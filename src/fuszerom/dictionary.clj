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

(defn read-segrules-initial-transitions [arr]
  (let [buf (nio/byte-buffer arr)
        _ (.position buf 1)
        num-transitions (.get buf)]
    (into {}
          (repeatedly num-transitions (fn []
                                        (let [index (.get buf)
                                              shift (.get buf)
                                              offset (.getShort buf)
                                              b1 (aget arr offset)
                                              b2 (aget arr (inc offset))
                                              accepting (bit-test b1 0)
                                              weak (bit-test b1 1)
                                              sink (zero? b2)
                                              failed (and sink (not accepting))]
                                          [index {:shift (not= shift 0)
                                                  :offset offset
                                                  :accepting accepting
                                                  :weak weak
                                                  :sink sink
                                                  :failed failed}]))))))

(defn read-segrules-fsa [buf]
  (let [size (.getInt buf)
        arr (byte-array size)]
    ;; TODO: deserialize the FSA more fully
    (.get buf arr)
    {:image arr
     :initial-transitions (read-segrules-initial-transitions arr)}))

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

(defn read-bytes [buf size]
  (loop [res 0 left size]
    (if (zero? left)
      res
      (let [x (bit-and (.get buf) 255)]
        (recur (+ (bit-shift-left res 8) x) (dec left))))))

(defn read-cfsa1-transition-data [buf]
  (let [b1 (.get buf)
        offset-size (bit-and b1 3)
        short-label (bit-and (bit-shift-right b1 2) 63)
        label (when (zero? short-label)
                (.get buf))]
    {:short-label short-label, :label label, :offset-size offset-size, :offset (read-bytes buf offset-size)}))

(defn read-cfsa1-all-transitions [buf]
  (let [count (.get buf)]
    (repeatedly count (partial read-cfsa1-transition-data buf))))

(defn read-fsa [buffer]
  (.position buffer fsa-data-size-offset)
  (let [fsa-data-size (.getInt buffer)
        arr (byte-array fsa-data-size)]
    (.get buffer arr)
    arr))

(defn load-dictionary [filename]
  (let [bytes (slurp-bytes filename)
        buffer (nio/byte-buffer bytes)
        _ (when-not (and (= (.getInt buffer) magic-number)
                         (= (.get buffer) version-number))
            (throw (ex-info "Not a dictionary file" {:filename filename})))
        fsa (read-fsa buffer)
        epilogue-offset (.position buffer)
        a1-size (.getInt buffer) ; FIXME: proper name
        id (read-null-terminated-string buffer)
        copyright (read-null-terminated-string buffer)
        tagset-id (read-null-terminated-string buffer)
        tags (read-tags buffer)
        names (read-tags buffer)
        labels (read-tags buffer)
        _ (.position buffer (+ epilogue-offset a1-size 4))
        separators-list (read-separators-list buffer)
        segrules-fsa-map (read-segrules-fsa-map buffer)]
    {:id id
     :copyright copyright
     :tagset-id tagset-id
     :tags tags
     :names names
     :labels labels
     :separators-list separators-list
     :fsa fsa
     :segrules-fsa-map segrules-fsa-map}))

(comment
  (def dict (load-dictionary "../vendor/Morfeusz/dict/sgjp-a.dict"))
  (def fbuf (nio/byte-buffer (:fsa dict)))
  (.position fbuf 257)
  (read-cfsa1-all-transitions fbuf))
