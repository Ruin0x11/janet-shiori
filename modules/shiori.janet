(import "lib" :prefix "")

(def- valid-ops {"GET" true "POST" true "NOTIFY" true})

(def handlers @{})
(var state {})

(def- statuses {200 "OK"
                204 "No Content"
                311 "Not Enough"
                312 "Advice"
                400 "Bad Request"
                500 "Internal Server Error"})

(defn- concat-opts [opts]
  (if (empty? opts)
      ""
    (string
     (string/join
      (map (fn [i] (string/join @[(string (get i 0))
                                  ": "
                                  (string (get i 1))]))
           (kvpairs opts))
      "\n")
     "\n")))

(defn- make-opts [result]
  (if (dictionary? result)
      (let [value (get result :value)]
        (if (dictionary? value)
            value
          {"Value" value}))
    {"Value" result}))

(defn- parse-request [req]
  (unless (string/has-suffix? "\n\n" req) (error "No trailing newline"))
  (let [sp (string/split "\n" req)
           head (get sp 0)
           kvs (map (fn [i] (string/split ": " i))
                    (array/slice sp 1 (- (length sp) 1)))
           opts (table/to-struct
                 (reduce (fn [t i] (put t (get i 0) (get i 1))) @{} kvs))]
    (if (= "GET Version SHIORI/2.6")
        {:op "GET Version" :ver "2.6" :opts opts} # hack
      (let [[op s] (string/split " " head)
            [shiori ver] (string/split "/" s)]
        (unless (= ver "3.0") (error "Bad version"))
        (unless (= shiori "SHIORI") (error "Bad request"))
        (unless (get opts "ID") (error "No ID"))
        (unless (get valid-ops op) (error "Bad operation"))
        {:op op :ver ver :opts opts}))))

(defn- make-status [result]
  (let [found
        (if (dictionary? result)
            (get result :status)
          200)]
    (if (get statuses found) found 400)))

(defn- make-response [result]
  (let [the-status (make-status result)
        message (get statuses the-status)
        body (-> result
                 make-opts
                 concat-opts)]
    (string/format "SHIORI/3.0 %d %s\nCharset: UTF-8\n%s\n" the-status message body)))

(defn- bad-request [err]
  (make-response {:status 500 :value err}))

(defn trigger [event & opts]
  (default opts @{})
  (if-let [f (get handlers event)]
      (apply f opts [(or state {})])
    @{:status 204 :value "No handler"}))

(defn- trigger* [event opts]
  (let [result (trigger event opts)]
    (match (type result)
           :string (make-response result)
           :struct (make-response result)
           :table (make-response result)
           :tuple
           (do
               (set state (get result 1))
               (make-response (get result 0)))
           (bad-request "Bad handler"))))

(def- log (file/open "shiori.log" :w))

(defn dolog [str]
  (when log (file/write log str))
  str)

(defn request
  "Parses REQ, a string, and returns a string with the response.

Main entry point from SHIORI server library. This function must be
bound as it is called from C."
  [req]
  (dolog
   (try
    (do
        (dolog req)
        (let [{:opts opts} (parse-request req)
              id (or (opts "ID") (opts "Event"))]
          (trigger* id opts)))
    ([err fib]
     (debug/stacktrace fib err)
     (bad-request err)))))

(defmacro register-handler
  "Register a function for the SHIORI event EVENT that runs BODY.
The function takes the arguments [OPTS STATE] and returns either
RESULT or a tuple [RESULT STATE]. If RESULT is a string, return a 200
with the string as 'Value'. If RESULT is a dictionary, use :status as
the status and return :value, a string or dictionary."
  [event & body]
  (with-syms [$the-func $handlers]
    ~(let [,$the-func (fn [opts state] ,;body)
           ,$handlers (((module/cache "shiori") (symbol :handlers)) :value)]
       (put ,$handlers ,event ,$the-func))))

(defmacro- register-handler-internal [event & body]
  (with-syms [$the-func]
             ~(let [,$the-func (fn [opts state] ,;body)]
                (put handlers ,event ,$the-func))))

(var version "0.0.1")
(var name "janet-shiori")
(var craftman "Ruin0x11")
(var craftmanw "ルイン")

(register-handler-internal "version" version)
(register-handler-internal "name" name)
(register-handler-internal "craftman" craftman)
(register-handler-internal "craftmanw" craftmanw)

(defn get-top-sym [code]
  (let [parts (string/split " " code)]
    (if (> (length parts) 0)
        (string/triml (get parts 0) "(")
      "")))

(register-handler-internal
 "OnJanetEval"
 (let [code (get opts "Reference0")
            head (get-top-sym code)
            [stat res] (safe-eval-string code)
            event (if (= stat :failure) "OnJanetEvalFailure" "OnJanetEvalSuccess")]
   (trigger event @{"Reference0" res "Reference1" head})))