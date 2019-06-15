(import "shiori")
(import "lib" :prefix "")
(import "lib/sakura" :as "ss")

(shiori/register-handler "OnFirstBoot"
  (ss/compile
    "はじめまして。"
    (scope :kero)
    (surface 50) "yo"))

(shiori/register-handler "OnBoot"
  (ss/compile
    "よっ。"
    (scope :kero)
    (surface 50) "ちっす。"))

(shiori/register-handler "OnClose"
  (ss/compile (surface 200) "じゃあね。" (wait 9) (exit)))

(shiori/register-handler "OnGhostChanged"
  (ss/compile "交代、交代…"))

(shiori/register-handler "OnGhostChanging"
  (ss/compile "交代、交代…"))

(def phrases
@[(ss/compile "Janet言語って、" (wait 4) "胸キュン？")
  (ss/compile (surface 4) "ChickenSchemeもよろしく。" )
  (ss/compile "C++だとconst char*なリテラルをchar*に入れるなっていう警告が出るけど、" (wait 9) (surface 1) "まあコンパイルとおるからいいよね？")
  (ss/compile "古いVC++ではstdbool.hがなくてコンパイルできないから、" (wait 9) (surface 1) "マクロでbool定めてるっていう……。")
  (ss/compile (surface 3) "あとの実装はキミしだい。")])

(defn sample [col]
  (get col (math/floor (* (length col) (math/random)))))

(shiori/set-handler "OnSecondChange"
  (if (= (length status) 0)
    (let [aitalk (or (get state :aitalk) 0)]
      (if (> aitalk 90)
          [(sample phrases) (merge state {:aitalk 0})]
        [{:status 204} (merge state {:aitalk (inc aitalk)})]))))

(defmacro dotimes
    [times & body]
  (with-syms [$i]
             ~(for ,$i 0 ,times ,;body)))

(defn ref [opts i]
  (get opts (string/format "Reference%d" i)))

(defn likely-sakura? [escaped-str]
  (some (fn [pref]
            (string/has-prefix? pref escaped-str))
        @["\\\\0" "\\\\1" "\\\\p" "\\\\h" "\\\\b"]))

(defn display-result [disp]
  (ss/concat
    (ss/scope :kero)
    (ss/surface 52)
    (ss/quick-section "> " disp)
    (ss/nl)
    (ss/nl)))

(defn prettify [s]
  (escape-str (string/format "%.20p" s)))

(defn on-janet-eval-success [opts status state]
  (let [default-resp (fn [x]
                       (ss/concat
                         (ss/scope :kero)
                         (ss/surface 50)
                         (ss/quick-section "> " x (ss/nl) (ss/nl))
                         (ss/scope :sakura)
                         "こんなもんが出てきた。"))
        res (ref opts 0)
        disp (prettify res)]
    (match (ref opts 1)
      "shiori/set-handler"
      (ss/compile (surface 1) "技の設定、完了。")

      "shiori/register-handler"
      (ss/compile (surface 4) "よしっ、新しい技を覚えた！")

      "shiori/on-choice"
      (ss/compile "選択肢を編集しました。")

      "defn"
      (ss/concat
       (display-result disp)
       (ss/compile (surface 4)
        (scope :sakura) "新しいファンクションの誕生だっ！"
        (scope :kero) "いいぜ、もっとくれ！"))

      "defmacro"
      (ss/concat
        (display-result disp)
        (ss/compile
         (surface 1)
         (scope :sakura) "新しいマクロですね。"
         (scope :kero) "気をつけろよ。"))

      "get"
      (if (= res nil)
        (ss/compile "何も帰って来なかった。")
        (ss/concat
          (display-result disp)
          (ss/compile
            (scope :sakura) "ほいっ。")))

      nil
      (ss/compile "nandeshou," (wait 5) " kore...")

      x
      (cond
        (= (type res) :boolean)
        (if (ref opts 0) (ss/compile "はい、答えはtrueです。")
            (ss/compile "いいえ、答えはfalseです。"))

        (string? res)
        (if (likely-sakura? res)
          (unescape-str res)
          (default-resp (string "\"" res "\"")))

        (default-resp disp)))))

(defn on-janet-eval-failure [opts status state]
  (let [result (ref opts 0)]
    (ss/compile (surface 2)
      (scope :kero) (quick-section "> " result) (nl) (nl)
      (scope :sakura)"あれ？失敗しちゃった。"
      (scope :kero) (surface 51) "ぬっ？！")))

(shiori/set-handler "OnJanetEvalSuccess" on-janet-eval-success)
(shiori/set-handler "OnJanetEvalFailure" on-janet-eval-failure)

# (shiori/register-handler "OnJanetDocFinish"
#                          (if (ref 1)
#                              (sakura (string "ほいっ。\n" (ref 1)))
#                              (sakura (string (ref 2) "、見つからなかった…\n"))))

(def menu (ss/compile
            (time-critical)
            (nl :half)
            (nl)
            (quick-section
              (nl) (marker) (choice "おしゃべり" "ai")
              (nl) (marker) (choice "記憶" "kioku")
              (nl) (marker) (choice "ステータス" "status")
              (nl) (marker) (choice "定義" "state")
              (nl)
              (nl) (marker) (choice "別に" "end"))))

(defn on-mouse-double-click [opts status state]
  (when (= (ref opts 3) "0")
    (match (ref opts 4)

      "Head"
      (ss/concat
        (ss/surface 1) "はい、頭なんです。"
        (ss/wait 9) (ss/surface 2)
        (sample
          @["ドラムセット" "ゲームパッド" "太鼓"])
        "じゃなくて。")

      "Face"
      (ss/concat
        (ss/surface 2)
        (sample @["いたっ。" "…なんで顔を叩いているんですか？"]))

      "Bust"
      (ss/compile (surface 3) "…エッチ。")

      (ss/compile "なに？" menu))))

# (shiori/set-handler "OnMouseDoubleClick")

(defn on-hour-time-signal [opts status state]
  (ss/concat (ref opts 0) "時ですよ。"))

(shiori/set-handler "OnHourTimeSignal" on-hour-time-signal)

(defn choice-ai [opts status state]
  "トークを選択しました。")

(defn pull-refs
  "Converts a dict of {'Reference<n>' 'value' ...} to {n 'value' ...}."
  [opts]
  (def tbl @{})
  (var going true)
  (var i 0)
  (while going
    (if-let [it (get opts (string/format "Reference%d" i))]
      (do
        (put tbl i it)
        (set i (inc i)))
      (set going false)))
  tbl)

(defn show-memory [opts]
  (let [refs (pull-refs opts)
        print-kv (fn [[k v]] (ss/concat k " - " v (ss/nl)))]
    (string/join (map print-kv (partition 2 (values refs))))))

(defn choice-show-memory [opts status state]
  (ss/concat
    (ss/quick-section
      (show-memory opts)
      (ss/nl))
    "こういうのを受け取れた気がします。"))

# TODO: need eager
# (macex '(sakura (marker) (choice (string/join (kvs @{"a" "b" "c" "D"})))))
(let [opts (string/join (kvs @{"Reference0" "b" "Reference1" "D"}) ",")]
  (ss/concat (ss/marker) (ss/choice "Show memory" "showmemory" opts)))

(def memory @{})

(defn remember [event opts status]
  (unless (get shiori/handlers event)
    (put memory event opts)))

(shiori/on-request remember)

(defn memory-choice [[k v]]
  (let [opts (string/join (map escape-str (kvs v)) ",")]
    (ss/concat
      (ss/marker)
      (ss/choice k "showmemory" opts)
      "]")))

(defn memory-choices [memory]
  (string/join (map memory-choice (kvpairs memory)) "\\n"))

(defn choice-kioku [opts status state]
 (let [choices (memory-choices memory)]
    (ss/compile "えーっと、こういうイベントを覚えています。" (nl) (nl)
      (balloon 2)
      (quick-section choices)
      (nl)
      (nl) (marker) (choice "忘れろ" "wasurero")
      (nl) (marker) (choice "オッケー" "end"))))

(defn choice-wasurero [opts status state]
  (each k (keys memory) (put memory k nil))
  (ss/compile
    (surface 4) "はい、全部忘れました。"
    (scope :kero)
    (surface 51) "どうやって！？"))

(defn choice-status [opts status state]
  (let [len (string (length status))
        section (string/join (keys status) "\\n")]
    (ss/compile
      (scope :kero) (surface 52) (quick-section section (nl) (nl))
      (scope :sakura) "こっちは今の状況です。"
      (scope :kero) len "個のエントリー。")))

(shiori/on-choice "ai" choice-ai)
(shiori/on-choice "showmemory" choice-show-memory)
(shiori/on-choice "kioku" choice-kioku)
(shiori/on-choice "wasurero" choice-wasurero)
(shiori/on-choice "status" choice-status)
