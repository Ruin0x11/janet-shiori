(defn match-id [id]
  (match id
         :sakura 0
         :kero 1
         x x))

(defn arg-list [val &opt slice-idx end-idx]
  (default slice-idx 1)
  (default end-idx (length val))
  (let [sl (tuple/slice val slice-idx end-idx)]
    (if (nil? (first sl))
        ""
      (string "," (string/join (map (fn [i] (string i)) sl) ","))))
  )

(defn id-list [val]
  (string/join
   (map
    (fn [i] (string (match-id i)))
    val)
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
       (= :em sym) (string/format "%dem" val)
       (= :% sym) (string/format "%d%%" val)
       (= :@ sym) (string/format "@%d" val)
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
            (each x (tuple/slice x 1) (array/insert arr -1 x))
          (array/insert arr -1 x)))
  (flatten arr))

(defmacro sakura-recurse [i tag1 tag2 idx]
  ~(let [it (tuple/slice ,i ,idx)]
     # Have to add a tag to indicate this result should be spliced
     # into the final array, because `splice` will just return a tuple
     # if it is the return value of a function.
     [recurse-sentinel ,tag1 (flat (map sakura-reify it)) ,tag2]))

(def- meths @{})

(defmacro defsakura [name & more]
  ~(defn ,name ,;more))

(defn dosakura [i]
  (if-let [meth (meths (keyword (get i 0)))]
    (meth (splice (tuple/slice i 1)))
    ""))

(defsakura surface [val]
  (string/format "\\s[%s]" (string val)))
(put meths :surface surface)

(defsakura anim [val &opt arg1 arg2 arg3]
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


   (string/format "\\i[%s]" (string val))))
(put meths :anim anim)

(defsakura scope [val]
  (string/format "\\p[%d]" (match-id val)))
(put meths :scope scope)

(defn in? [vals v]
  (some (fn [a] (= a v)) vals))

(defn verify-in [vals v sym]
  (unless (in? vals v)
    (let [s (string/join (map string vals) ", ")]
      (error (string/format "%s takes one of %s" (string sym) s)))))

(defn lock-unlock [sym val]
  (verify-in [:repaint :balloon-repaint] val sym)
  (string/format "\\![%s,%s]"
    (string sym)
    (dehyphen val)))

(defsakura lock [val]
  (lock-unlock :lock val))
(put meths :lock lock)

(defsakura unlock [val]
  (lock-unlock :unlock val))
(put meths :unlock unlock)

(defsakura end [] "\\e")
(put meths :end end)

(defmacro defsakuraset [name syms]
  (let [new-name (string/join ["set-" name])
        dehy (dehyphen name)]
    ~(defn ,(symbol new-name) [val]
       (verify-in ,syms val ,new-name)
       (string/format "\\![set,%s,%s]" ,dehy (string val)))))

(defsakuraset alignment-on-desktop [:bottom :top])
(put meths :set-alignment-on-desktop set-alignment-on-desktop)
(defsakuraset alignment-to-desktop [:top :bottom :left :right :free :default])
(put meths :set-alignment-to-desktop set-alignment-to-desktop)

(defsakura set-scaling [val]
  (if (number? val)
    (string/format "\\![set,scaling,%d]" val)
    (error "set-scaling takes a number")))
(put meths :set-scaling set-scaling)

(defn between? [val min max]
  (and (>= min val) (<= val max)))

(defsakura set-alpha [val]
  (if (and (number? val) (between? val 0 100))
    (string/format "\\![set,alpha,%d]" val)
    (error (string "set-alpha takes a number between 0 and 100"))))
(put meths :set-alpha set-alpha)

(defsakura move-close [] "\\4")
(defsakura move-away [] "\\5")

# TODO
(defsakura move [] "\\![move]")

# TODO
(defsakura moveasync [] "\\![moveasync]")

(defsakura set-zorder [& rest]
  (string/format "\\![set,zorder,%s]" (id-list rest)))

(defsakura reset-zorder [& rest]
  "\\![reset,zorder]")

(defsakura set-sticky-window [& rest]
  (string/format "\\![set,sticky-window,%s]" (id-list rest)))

(defsakura reset-sticky-window [& rest]
  "\\![reset,sticky-window]")

(defsakura balloon [val]
  (string/format "\\b[%d]"
    (if (= val :hide) -1 val)))

(defsakura balloon-paste [& rest]
  (let [[val arg1 arg2] rest]
    (if (= arg1 :inline)
      (string/format "\\_b[%s,inline%s]"
        val (option-list rest 3))
      (string/format "\\_b[%s,%d,%d%s]"
        val arg1 arg2
        (option-list rest 4)))))

(defsakura nl [&opt val]
  (cond
    (= :half val) "\\n[half]"
    (= :nobreak val) "\\_n"
    (number? val) (string/format "\\n[%d]" val)
    "\\n"))
(put meths :nl nl)

(defsakura clear [& rest]
  (let [val (get rest 0)]
    (cond
      (and (> (length rest) 2)
        (in? [:char :line] val))
      (string/format "\\c[%s%s]"
        (string val)
        (arg-list rest))
      "\\c")))

(defsakura cursor [val arg1]
  (string/format "\\_l[%s,%s]"
    (pos val)
    (pos arg1)))

(defsakura append []
  "\\C")

(defsakuraset autoscroll [:disable :enable])
(put meths :set-autoscroll set-autoscroll)

(defsakura set-balloon-offset [val arg1]
  (string/format "\\![set,balloonoffset,%s,%s]"
    (pos val) (pos arg1)))

(defsakuraset balloon-align [:left :center :top :right :bottom :none])
(put meths :set-balloon-align set-balloon-align)

(defun set-balloon-num [val & rest]
  (if (= :clear val)
    "\\![set,balloonnum,,,]"
    (string/format "\\![set,balloonnum,%s,%d,%d]"
      val (get 1 rest) (get 2 rest))))

(defsakura set-balloon-timeout [val]
  (if (number? val)
    (string/format "\\![set,balloontimeout,%d]" val)
    (error "set-balloon-timeout takes a number")))
(put meths :set-balloon-timeout set-balloon-timeout)

(defsakura marker [] "\\![*]")

(defn enter-leave [sym val]
  (verify-in [:online-mode :no-user-break-mode] val sym)
  (string/format "\\![%s,%s]"
    (string sym)
    (dehyphen val)))

(defsakura enter [val]
  (enter-leave :enter val))
(put meths :enter enter)

(defsakura leave [val]
  (enter-leave :leave val))
(put meths :leave leave)

(defsakura literal [val]
  (string "\\_?" val "\\_?"))

(defsakura literal-macro [& rest]
  (sakura-recurse rest "\\_?" "\\_?" 0))

(defsakura voice [val & rest]
  (verify-in [:disable :alternate] val "voice")
  (if (= :disable val)
         (let [tag (string/format "\\_v[%s]"
                     (string val))]
           (string tag (splice rest) "\\_v"))
         (let [tag (string/format "\\_v[%s,%s]"
                     (string val)
                     (get rest 0))]
           (string tag (splice (tuple/slice rest 1)) "\\_v"))))
(put meths :voice voice)

(defn symbol= [a b] (= (symbol a) b))

(defsakura voice-macro [val & rest]
  (verify-in [:disable :alternate] val "voice")
  (let [arg1 (get rest 0)]
    (if (= :disable val)
      (let [tag (string/format "\\_v[%s]"
                  (string val))]

        (sakura-recurse rest tag "\\_v" 1))
      (let [tag (string/format "\\_v[%s,%s]"
                  (string val)
                  arg1)]
        (sakura-recurse rest tag "\\_v" 1)))))
(put meths :voice-macro voice-macro)

(defsakura font [& rest]
  (parse-font rest))

(defsakura wait [val &opt arg1]
  (if (number? val)
    (match arg1
      :ms (string/format "\\_w[%d]" val)
      :total (string/format "\\__w[%d]" val)
      (string/format "\\w%d" val))
    (error "wait takes a number")))
(put meths :wait wait)

(defsakura wait-for-click [&opt val]
  (if (= val :no-clear) "\\x[noclear]" "\\x"))

(defsakura time-critical [] "\\t")
(put meths :time-critical time-critical)

(defsakura quick-section [& rest]
  (string "\\![quicksection,true]" (splice rest) "\\![quicksection,false]"))

(defsakura quick-section-macro [& rest]
  (sakura-recurse rest
    "\\![quicksection,true]"
    "\\![quicksection,false]"
    0))

# TODO
(defsakura synchronize [& rest]
  (string "\\_s" (splice rest) "\\_s"))

(defsakura synchronize-macro [& rest]
  (sakura-recurse rest "\\_s" "\\_s" 0))

(defsakura choice [val & rest]
  (let [[arg1 arg2 arg3] rest]
    (match arg1
      :script
      (string/format
        "\\q[%s,script:%s]"
        val
        (string
          (splice (tuple/slice rest 1))))
      :block
      "TODO"

      nil
      (string "\\q[" val "]")

      x
      (if (nil? arg2)
        (string "\\q[" val "," arg1 "]")
        (string "\\q[" val "," arg1 "," arg2 "]")))))

(defsakura choice-macro [val & rest]
  (let [[arg1 arg2 arg3] rest]
    (match arg1
      :script
      (string/format "\\q[%s,script:%s]" val (sakura-recurse rest "" "" 2))
      :block
      "TODO"

      nil
      (string "\\q[" val "]")

      x
      (if (nil? arg2)
        (string "\\q[" val "," arg1 "]")
        (string "\\q[" val "," arg1 "," arg2 "]")))))

(defsakura no-timeout [] "\\*")

(defsakura set-choice-timeout [val]
  (if (number? val)
    (string/format "\\![set,choicetimeout,%d]" val)
    (error "set-choice-timeout takes a number")))
(put meths :set-choice-timeout set-choice-timeout)

# TODO
(defsakura anchor [val & rest]
  (let [tag (string/format "\\_a[%s]" val)]
    (string tag (splice rest) "\\_a")))
(put meths :anchor anchor)

(defsakura anchor-macro [val & rest]
  (let [tag (string/format "\\_a[%s]" val)]
    (sakura-recurse rest tag "\\_a" 0)))
(put meths :anchor-macro anchor-macro)

(defsakura exit [] "\\-")

(defsakura eval-macro [& rest]
  ~(:recurse ,rest))

(defsakura update-by-myself [val]
  (if (= :check-only val)
    "\\![updatebymyself,checkonly]"
    "\\![updatebymyself]"))

(defsakura update [] "\\![update,platform]")

(defn sakura-reify [i]
  (cond
    (tuple? i)
    (let [[sym] i
          meth (get meths (keyword sym))
          meth-macro (get meths (keyword sym "-macro"))]
      (cond
        meth-macro
        (meth-macro (splice (tuple/slice i 1)))

        meth
        (meth (splice (tuple/slice i 1)))

        (error (string/format "unknown sakura '%s'" (string sym)))))

    (string? i) (string/replace-all "\n" "\\n" i)

    ~(string ,i)))

(defmacro compile-raw [& args]
  (string/join (flat (map sakura-reify args))))

(defmacro compile [& args]
  ~(sakura-raw (scope :sakura) (surface 0) ,;args (end)))

(defn concat-raw [& args]
  (string/join args))

(defn concat [& args]
  (concat-raw (scope :sakura) (surface 0) (splice args) (end)))


(sakura
  (anchor "dood" (voice :disable "zxc" "Qwe")))

(concat
  (anchor "dood" (voice :disable "zxc" "Qwe")))
