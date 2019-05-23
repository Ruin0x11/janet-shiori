(import "lib" :prefix "")

(def- valid-ops {"GET" true "POST" true})

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
        [op s] (string/split " " head )
        [shiori ver] (string/split "/" s)
        kvs (map (fn [i] (string/split ": " i))
                 (array/slice sp 1 (- (length sp) 1)))
        opts (table/to-struct
              (reduce (fn [t i] (put t (get i 0) (get i 1))) @{} kvs))]
    (unless (= ver "3.0") (error "Bad version"))
    (unless (= shiori "SHIORI") (error "Bad request"))
    (unless (get opts "ID") (error "No ID"))
    (unless (get valid-ops op) (error "Bad operation"))
    {:op op :ver ver :opts opts}))

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
    (string/format "SHIORI/3.0\n%d %s\nEncoding: UTF-8\n%s\n" the-status message body)))

(defn- bad-request [err]
  (make-response {:status 400 :value err}))

(defn request
  "Parses REQ, a string, and returns a string with the response.

Main entry point from SHIORI server library. This function must be
bound as it is called from C."
  [req]
  (try
   (do
       (let [{:opts opts} (parse-request req)
             {"ID" id} opts]
         (if-let [f (get handlers id)]
             (let [result (apply f opts [(or state {})])]
               (match (type result)
                      :string (make-response result)
                      :tuple
                      (do
                          (set state (get result 1))
                          (make-response (get result 0)))
                      (bad-request "Bad handler")))
          (bad-request (string/format "No handler for %s" id)))))
   ([err fib]
    (debug/stacktrace fib err)
    (bad-request err))))

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
