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

(defn to-table [arr]
  (def tbl @{})
  (each x arr (put tbl x true))
  tbl)

(defn in? [set i]
  (if (not (indexed? set))
      nil
      (let [kvs (flatten (map (fn [a] [a true]) set))]
        (get (table (splice kvs)) i))))

(defn symbol= [a b]
  (= (symbol a) b))

(def- escapes @{13 "\\r"
                10 "\\n"
                92 "\\\\"})

(def- unescapes @{114 13
                  110 10
                   92 92})

(defn escape-str
  [str]
  (def buf @"")
  (var seen false)
  (loop [byte :in str]
        (if (= 92 byte)
            (buffer/push-string buf "\\\\")
          (if-let [rep (get escapes byte)]
              (buffer/push-string buf rep)
            (buffer/push-byte buf byte))))
  (when seen
    (buffer/push-byte buf 92))
  (string buf))

(defn unescape-str
  [str]
  (def buf @"")
  (var seen false)
  (loop [byte :in str]
        (if seen
            (do
                (set seen false)
                (if-let [rep (get unescapes byte)]
                    (buffer/push-byte buf rep)
                  (do
                      (buffer/push-byte buf 92)
                      (buffer/push-byte buf byte))))
          (if (= byte 92)
              (set seen true)
            (buffer/push-byte buf byte))))
  (when seen
    (buffer/push-byte buf 92))
  (string buf))


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
  (let [trunc (fn [s] (escape-str (string/slice s 0 (min (length s) 1024))))]
    (try
     (let [val (my-eval-string (unescape-str str) env)
           ret (if (string? val) (trunc val) val)]
       [:success ret])
     ([err fib] [:failure (trunc err)]))))
