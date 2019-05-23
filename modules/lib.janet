(defn min-by [f & args]
  (reduce (fn [res i] (min res (f i))) (f (first args)) args))

(defn zip [& rest]
  (let [res @[]
        count (min-by length (splice rest))]
    (loop [i :range [0 count]]
          (array/push res (map (fn [j] (get j i)) rest)))
    res))

(defn kvpairs [tbl]
  (partition 2 (kvs tbl)))

(defn escape-str [str]
  (-> str
      (string/replace "\n" "\\n")))

(defn safe-eval-string [str]
  (let [trunc (fn [s] (string/slice s 0 (min (length s) 256)))]
    (try
     [:success (trunc (eval-string str))]
     ([err fib] [:failure (trunc err)]))))
