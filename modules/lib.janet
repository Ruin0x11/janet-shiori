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

(defn unescape-str [str]
  (->> str
      (string/replace-all "\\n" "\n")
      (string/replace-all "\\r" "\r")))

(defn escape-str [str]
  (->> str
      (string/replace-all "\n" "\\n")
      (string/replace-all "\r" "\\r")))

(defn my-eval-string
  "Evaluates a string in the specified environment."
  [str env]
  (var state (string str))
  (defn chunks [buf _]
    (def ret state)
    (set state nil)
    (when ret
      (buffer/push-string buf str)
      (buffer/push-string buf "\n")))
  (var returnval nil)
  (run-context {:env env :chunks chunks
                :on-compile-error (fn [msg errf &]
                                    (error (string "compile error: " msg)))
                :on-parse-error (fn [p x]
                                  (error (string "parse error: " (parser/error p))))
                :fiber-flags :i
                :on-status (fn [f val]
                             (if-not (= (fiber/status f) :dead)
                               (error val))
                             (set returnval val))
                :source "eval-string"})
  returnval)

(var env (make-env))

(defn safe-eval-string [str]
  (let [trunc (fn [s] (escape-str (string/slice s 0 (min (length s) 256))))]
    (pp (keys module/cache))
    (try
     [:success (trunc (string
                       (my-eval-string
                        (unescape-str str)
                        env)))]
     ([err fib] [:failure (trunc err)]))))
