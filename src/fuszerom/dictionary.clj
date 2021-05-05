(ns fuszerom.dictionary
  (:require
   [clojure.java.io :as io]
   [nio.core :as nio])
  (:import
   [java.nio ByteBuffer]))

(defn slurp-bytes [f]
  (let [file (io/file f)]
    (if-not (.isFile file)
      (throw (ex-info "Not a file" {:file f}))
      (let [len (.length file)
            arr (make-array Byte/TYPE len)]
        (with-open [stream (io/input-stream file)]
          (.read stream arr 0 len))
        arr))))

(defn read-null-terminated-string [^ByteBuffer buf]
  (String. (into-array Byte/TYPE (take-while (comp not zero?) (repeatedly #(.get buf))))))

(defn read-tag [^ByteBuffer buf]
  [(Short/toUnsignedInt (.getShort buf))
   (read-null-terminated-string buf)])

(defn read-tags [^ByteBuffer buf]
  (let [num (Short/toUnsignedInt (.getShort buf))
        tags (repeatedly num #(read-tag buf))]
    (into {} tags)))

(defn read-separators-list [^ByteBuffer buf]
  (let [num (Short/toUnsignedInt (.getShort buf))]
    (vec (repeatedly num #(.getInt buf)))))

(defn read-segrules-fsa-options [^ByteBuffer buf]
  (let [num (Byte/toUnsignedLong (.get buf))]
    (into {} (repeatedly num #(vector (read-null-terminated-string buf)
                                      (read-null-terminated-string buf))))))

(defmacro with-marked-position [buf & body]
  `(let [buf# ~buf
         saved-pos# (.position buf#)]
     (try
       ~@body
       (finally
         (.position buf# saved-pos#)))))

(defn slice [^ByteBuffer buf ^long size]
  (let [res (.slice buf)]
    (.limit res size)
    res))

(defn segrules-entry [^ByteBuffer buf]
  (let [segnum (.get buf)
        shift (.get buf)
        offset (Short/toUnsignedInt (.getShort buf))
        [b1 b2] (with-marked-position buf
                  (.position buf offset)
                  [(.get buf) (.get buf)])
        accepting (bit-test b1 0)
        weak (bit-test b1 1)
        sink (zero? b2)
        failed (and sink (not accepting))]
    {:segnum segnum
     :shift (not (zero? shift))
     :offset offset
     :accepting accepting
     :weak weak
     :sink sink
     :failed failed}))

(defn segrules-entries [^ByteBuffer buf ^long offset]
  (.position buf (inc offset))
  (let [num-entries (Byte/toUnsignedLong (.get buf))]
    (vec (repeatedly num-entries #(segrules-entry buf)))))

(defn read-segrules-fsa [^ByteBuffer buf]
  (let [size (.getInt buf)
        image (slice buf size)]
    (.position buf (+ (.position buf) size))
    ;; TODO: deserialize the FSA more fully
    {:image image
     :initial-transitions (->> (segrules-entries image 0)
                               (map (juxt :segnum identity))
                               (into {}))}))

(defn read-segrules-fsa-entry [^ByteBuffer buf]
  [(read-segrules-fsa-options buf)
   (read-segrules-fsa buf)])

(defn read-segrules-fsa-map [^ByteBuffer buf]
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
  (read-cfsa1-all-transitions fbuf)
  (def buf (-> dict :segrules-fsa-map (get {"aggl" "strict", "praet" "split"}) :image))
  (segrules-entries buf 8340))
