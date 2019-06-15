(import "lib" :prefix "")

(def- valid-ops {"GET" :get "POST" :post "NOTIFY" :notify})

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
            op-kw (get valid-ops op)
            [shiori ver] (string/split "/" s)]
        (unless (= ver "3.0") (error "Bad version"))
        (unless (= shiori "SHIORI") (error "Bad request"))
        (unless (get opts "ID") (error "No ID"))
        (unless op-kw (error "Bad operation"))
        {:op op-kw :ver ver :opts opts}))))

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

(defn trigger [event &opt opts status]
  (default opts @{})
  (default status @{})
  (if-let [for-event (get handlers event)
           val (some (fn [f] (apply f opts status [(or state {})])) for-event)]
      val
    @{:status 204 :value "No handler"}))

(defn- trigger* [event opts status]
  (let [result (trigger event opts status)]
    (match (type result)
           :string (make-response result)
           :struct (make-response result)
           :table (make-response result)
           :tuple
           (do
               (set state (get result 1))
               (make-response (get result 0)))
           (bad-request "Bad handler"))))

(defn register-handler
  "Register a function for the SHIORI event EVENT that runs FUNC.
The function takes the arguments [OPTS STATE] and returns either
RESULT or a tuple [RESULT STATE]. If RESULT is a string, return a 200
with the string as 'Value'. If RESULT is a dictionary, use :status as
the status and return :value, a string or dictionary."
  [event func]
  (let [handlers (((module/cache "shiori") (symbol :handlers)) :value)]
    (unless (get handlers event)
      (put handlers event @[]))
    (array/insert (get handlers event) 0 func)))

(defn clear-handlers
  "Clears all event handlers for EVENT."
  [event]
  (put handlers event @[]))

(defn set-handler
  "Like `register-handler`, but clears the handlers table for EVENT first. "
  [event func]
  (clear-handlers event)
  (array/insert (get handlers event) 0 func))

(defn- get-nested
  [map keys]
  (var ret map)
  (var going true)
  (def len (length keys))
  (for i 0 len
    (let [k (keyword (get keys i))]
      (set ret
        (cond
          (not (dictionary? ret))
          nil

          (== i (- len 1))
          (get ret k)

          (let [v (get ret k)]
            (if (dictionary? v)
              v
              nil))))))
  ret)

(defn- get-env-binding [env name]
  (get (or (get env (symbol name)) @{}) :value))

(def config
  (if (os/stat "config.janet")
    (get-env-binding (dofile "config.janet") :config)
    @{}))

(defn get-config-value
  "Gets a value in the config from a string like 'my.nested.value'."
  [key]
  (get-nested config (string/split "." key)))

(defn- get-top-sym [code]
  (let [parts (string/split " " code)]
    (if (> (length parts) 0)
        (string/triml (get parts 0) "(")
      "")))

(defn- on-janet-eval [opts status state]
  (let [code (get opts "Reference0")
        head (get-top-sym code)
        [stat res] (safe-eval-string code)
        event (if (= stat :failure) "OnJanetEvalFailure" "OnJanetEvalSuccess")]
    (trigger event @{"Reference0" res "Reference1" head} status)))

(set-handler "OnJanetEval" on-janet-eval)


(defn on-choice [choice func]
  (register-handler "OnChoiceSelectEx"
    (fn [opts status state]
      (when (= (get opts "Reference1") choice)
        (func opts status state)))))

#
#
# Request
#
#

(def- log (file/open "shiori.log" :w))

(defn dolog [str]
  (when log (file/write log str))
  str)

(var on-request-callbacks @[])

(defn on-request [cb]
  (array/concat on-request-callbacks cb))

(defn clear-on-request [cb]
  (set on-request-callbacks @[]))

(defn parse-status [status]
  (if (not status)
      @{}
    (let [res (string/split "," status)]
      (if (and (= (length res) 1) (= (first res) ""))
          @{}
        (to-table res)))))

(defn request
  "Parses REQ, a string, and returns a string with the response.

Main entry point from SHIORI server library. This function must be
bound as it is called from C."
  [req]
  (dolog
   (try
    (do
        (dolog req)
        (let [{:op op :opts opts} (parse-request req)
              id (or (opts "ID") (opts "Event"))
              status (parse-status (opts "Status"))]
          (each cb on-request-callbacks
                (apply cb id opts status [state]))
          (match op
            :get
            (trigger* id opts status)

            :post
            (trigger* id opts status)

            :notify
            (trigger* id opts status))))
    ([err fib]
     (debug/stacktrace fib err)
     (bad-request err)))))
