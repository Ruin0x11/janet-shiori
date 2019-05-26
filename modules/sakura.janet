(defn in? [set i]
  (if (not (indexed? set))
      nil
      (let [kvs (flatten (map (fn [a] [a true]) set))]
        (get (table (splice kvs)) i))))

(defn match-id [id]
  (match id
         :sakura 0
         :kero 1
         x x))

(defn symbol= [a b]
  (= (symbol a) b))

(defn arg-list [val &opt slice-idx end-idx]
  (default slice-idx 1)
  (default end-idx (length val))
  (let [sl (tuple/slice val slice-idx end-idx)]
    (if (nil? (first sl))
        ""
      (string ","
              (string/join (map (fn [i] (string i)) sl) ",")
              )))
  )

(defn id-list [val]
  (string/join
   (map
    (fn [i] (string (match-id i)))
    (tuple/slice val 1))
   ","))

(defn option-list [val &opt slice-idx]
  (default slice-idx 1)
  (let [sl (tuple/slice val slice-idx)]
    (if (nil? (first sl))
        ""
      (string ","
              (string/join
               (map (fn [i]
                        (cond
                               (tuple? i)
                               (let [[l u r d] i]
                                 (string/format "--clipping=%d %d %d %d"
                                         l u r d))
                               (string "--option=" i))
                        )
                    sl)
               ",")))))

(defn pos [arg]
  (match arg
         @[sym val]
      (cond
       (symbol= :em sym) (string/format "%dem" val)
       (symbol= :% sym) (string/format "%d%%" val)
       (symbol= :@ sym) (string/format "@%d" val)
       (error (string "Bad cursor pos " sym)))
    x (string x)))

(defn dehyphen [o]
  (string/replace-all "-" "" (string o)))

(defn parse-font-height [v]
  (cond
   (number? v)
   (string v)

   (= :default v)
   "default"

   (tuple? v)
   (cond
    (symbol= :% (first v))
    (string/format "%d%%" (get v 1))

    (symbol= :+ (first v))
    (string/format "+%d" (get v 1))

    (error (string "Bad font height " v)))
   (error (string "Bad font height " v))))

(defn parse-color [v]
  (match v
         @[r g b]
         (string/format "%d,%d,%d" r g b)

       x (cond
          (and (tuple? v) (symbol= :% (first v)))
          (let [[r g b] (get v 1)]
            (string/format "%d%%,%d%%,%d%%" r g b))

          (keyword? x)
          (string x)

          (and (string? x) (string/has-prefix? "#" x))
          x

          (error ("Bad color" x)))))

(defn parse-font-val-val [sym v]
  (cond
   (and (= :name sym) (string? v))
   (string v)

   (= :height sym)
   (parse-font-height v)

   (in? [:color :shadowcolor :anchor.font.color] sym)
   (parse-color v)

   (in? [:outline :bold :italic :strike :underline :sub :sup] sym)
   (match v
          true "true"
          false "false"
          :default "default"
          x (error (string "Bad font value " x)))
   (error (string "Bad font value " v))))

(defn parse-font-val [tup]
  (let [[k v] tup
        sym (keyword k)]
    (string/format "\\f[%s,%s]" (string k) (parse-font-val-val sym v))))

(defn parse-font [arr]
  (def buf @[])
  (if (= [:default] arr)
      "\\f[default]"
    (do
        (each t arr
              (array/concat buf (parse-font-val t)))
        (string/join buf))))

(def- recurse-sentinel :recurse)

(defn flat [i]
  (def arr @[])
  (each x i
        (if (and (tuple? x) (= recurse-sentinel) (first x))
            (each x (tuple/slice x 1) (array/concat arr x))
          (array/concat arr x)))
  arr)

(defmacro sakura-recurse [i tag1 tag2 idx]
  ~(let [it (tuple/slice ,i ,idx)]
     # Have to add a tag to indicate this result should be spliced
     # into the final array, because `splice` will just return a tuple
     # if it is the return value of a function.
     [recurse-sentinel ,tag1 (flat (map sakura-reify it)) ,tag2]))

(defn sakura-reify [i]
  (cond
   (tuple? i)
   (let [[sym val arg1 arg2 arg3] i]
     (cond
      (symbol= :surface sym)
      (string/format "\\s[%s]" (string val))

      (symbol= :anim sym)
      (cond
       (= arg1 :wait)
       (string/format "\\i[%s,%s]"
                      (string val)
                      (string arg1))

       (in? [:clear :pause :resume] arg1)
       (string/format "\\i[anim,%s,%s]"
                      (string arg1)
                      (string val))

       (= arg1 :offset)
       (string/format "\\i[anim,%s,%s,%d,%d]"
                      (string arg1)
                      (string val)
                      arg2
                      arg3)

       (= arg1 :waitall)
       (string/format "\\__w[animation,%s]"
                      (string val))


       (string/format "\\i[%s]" (string val)))

      (symbol= :scope sym)
      (string/format "\\p[%d]" (match-id val))

      (and
       (or (symbol= :lock sym)
           (symbol= :unlock sym))
       (in? [:repaint :balloon-repaint] val))
      (string/format "\\![%s,%s]"
                     (string sym)
                     (dehyphen val))

      (symbol= :end sym)
      "\\e"

      (and (symbol= :set-alignment-on-desktop sym)
           (in? [:bottom :top] val))
      (string/format "\\![set,alignmentondesktop,%s]"
                     (string val))

      (and (symbol= :set-alignment-to-desktop sym)
           (in? [:top :bottom :left :right :free :default] val))
      (string/format "\\![set,alignmenttodesktop,%s]"
                     (string val))

      (and (symbol= :set-scaling sym)
           (number? val))
      (string/format "\\![set,scaling,%d]"
                     val)

      (and (symbol= :set-alpha sym)
           (number? val)
           (>= val 0)
           (<= val 100))
      (string/format "\\![set,alpha,%d]" val)

      (symbol= :hanare sym)
      "\\4"

      (symbol= :sesshoku sym)
      "\\5"

                              # TODO
                              (symbol= :move sym)
                              "\\![move]"

                              # TODO
                              (symbol= :moveasync sym)
                              "\\![moveasync]"

                              (symbol= :set-zorder sym)
                              (string/format "\\![set,zorder,%s]"
                                             (id-list i))

                              (symbol= :reset-zorder sym)
                              "\\![reset,zorder]"

                              (symbol= :set-sticky-window sym)
                              (string/format "\\![set,sticky-window,%s]"
                                             (id-list i))

                              (symbol= :reset-sticky-window sym)
                              "\\![reset,sticky-window]"

                              (symbol= :balloon sym)
                              (string/format "\\b[%d]"
                                             (if (= val :hide) -1 val))

                              (symbol= :balloon-paste sym)
                              (if (= arg1 :inline)
                                  (string/format "\\_b[%s,inline%s]"
                                               val (option-list i 3))
                                (string/format "\\_b[%s,%d,%d%s]"
                                                 val arg1 arg2
                                                 (option-list i 4)))

                              (symbol= :nl sym)
                              (cond
                                     (= :half val) "\\n[half]"
                                     (= :nobreak val) "\\_n"
                                     (number? val) (string/format "\\n[%d]" val)
                                     "\\n")

                              (symbol= :clear sym)
                              (cond
                               (and (> (length i) 2)
                                    (in? [:char :line] val))
                               (string/format "\\c[%s,%s]"
                                              (string val)
                                              (arg-list i))

                               "\\c")

                              (and
                               (symbol= :cursor sym)
                               (= (length i) 3))
                              (string/format "\\_l[%s,%s]"
                                             (pos val)
                                             (pos arg1))

                              (symbol= :append sym)
                              "\\C"

                              (and
                               (symbol= :set-autoscroll sym)
                               (in? [:disable :enable] val))
                              (string/format "\\![set,autoscroll,%s]"
                                             (string val))

                              (and
                               (symbol= :set-balloon-offset sym)
                               (= (length i) 3))
                              (string/format "\\![set,balloonoffset,%s,%s]"
                                             (pos val) (pos arg1))

                              (and
                               (symbol= :set-balloon-align sym)
                               (in? [:left :center :top :right :bottom :none] val))
                              (string/format "\\![set,balloonalign,%s]"
                                             (string val))

                              (symbol= :set-balloon-num sym)
                              (if (= :clear val)
                                  "\\![set,balloonnum,,,]"
                                (string/format "\\![set,balloonnum,%s,%d,%d]"
                                               val arg1 arg2))

                              (and
                               (symbol= :set-balloon-timeout sym)
                               (number? val))
                              (string/format "\\![set,balloontimeout,%d]" val)

                              (symbol= :marker sym)
                              "\\![*]"

                              (and
                               (or (symbol= :enter sym)
                                   (symbol= :leave sym))
                               (in? [:online-mode :no-user-break-mode] val))
                              (string/format "\\![%s,%s]"
                                             (string sym)
                                             (dehyphen val))

                              # TODO block?
                              (symbol= :literal sym)
                              (string "\\_?" val "\\_?")

                              # TODO block?
                              (and
                               (symbol= :voice sym)
                               (in? [:disable :alternate] val))
                              (if (= :disable val)
                                  (let [tag (string/format "\\_v[%s]"
                                                           (string val))]

                                    (sakura-recurse i tag "\\_v" 2))
                                (let [tag (string/format "\\_v[%s,%s]"
                                                         (string val)
                                                         arg1)]

                                  (sakura-recurse i tag "\\_v" 3)))

                              (symbol= :font sym)
                              (parse-font (tuple/slice i 1))

                              (and
                               (symbol= :wait sym)
                               (number? val))
                              (match arg1
                                     :ms (string/format "\\_w[%d]" val)
                                     :total (string/format "\\__w[%d]" val)
                                     (string/format "\\w%d" val))

                              (symbol= :wait-for-click sym)
                              (if (= val :noclear) "\\x[noclear]" "\\x")

                              (symbol= :time-critical sym)
                              "\\t"

                              (symbol= :quick-section sym)
                              (sakura-recurse i
                                              "\\![quicksection,true]"
                                              "\\![quicksection,false]"
                                              1)

                              # TODO
                              (symbol= :synchronize sym)
                              (sakura-recurse i "\\_s" "\\_s" 1)

                              # TODO
                              (symbol= :choice sym)
                              (match arg1
                                     :script
                                     (string/format "\\q[%s,script:%s]" val (sakura-recurse i "" "" 3))
                                     :block
                                     "TODO"

                                     x
                                     (string/format "\\q[%s%s]" val (arg-list i 2)))

                              (symbol= :no-timeout sym)
                              "\\*"

                              (symbol= :set-choice-timeout sym)
                              (string/format "\\![set,choicetimeout,%d]" val)

                              # TODO
                              (and
                               (symbol= :anchor sym)
                               (string? val))
                              (let [tag (string/format "\\_a[%s]" val)]
                                (sakura-recurse i tag "\\_a" 2))

                              (symbol= :exit sym)
                              "\\-"

                              (symbol= :eval sym)
                              ~(:recurse  ,(tuple/slice i 1))

                              (symbol= :update-self sym)
                              (if (= :check-only val)"\\![updatebymyself,checkonly]" "\\![updatebymyself]")

                              (symbol= :update sym)
                             "\\![update,platform]"

                             [:recurse (tuple i)]))

   (string? i) (string/replace-all "\n" "\\n" i)

   ~(string ,i)))

(defmacro sakura-raw [& args]
  ~(string/join ,(flat (map sakura-reify args))))

(defmacro sakura [& args]
  ~(sakura-raw (scope :sakura) (surface 0) ,;args (end)))
