(array/insert module/paths 0
              ["./modules/:all:.janet" :source]
              ["./modules/:all:/init.janet" :source]
              ["./modules/:all:.:native:" :native]
              ["./modules/:all:/:name.:native:" :native])

(import "shiori")
(import "lib" :prefix "")
(import "sakura" :prefix "")

(set shiori/version "0.0.1")
(set shiori/name "janet-shiori")
(set shiori/craftman "Ruin0x11")
(set shiori/craftmanw "ルイン")

(shiori/register-handler "OnFirstBoot"
  (sakura "はじめまして。"))

(shiori/register-handler "OnBoot"
  (sakura "よっ。" (scope :kero) (surface 50) "ちっす。"))

(shiori/register-handler "OnClose"
  (sakura (surface 200) "じゃあね。" (wait 9) (exit)))

(shiori/register-handler "OnGhostChanged"
  (sakura "交代、交代…"))

(shiori/register-handler "OnGhostChanging"
  (sakura "交代、交代…"))

(def phrases
@[(sakura "Janet言語って、" (wait 4) "胸キュン？")
  (sakura (surface 4) "ChickenSchemeもよろしく。" )
  (sakura "C++だとconst char*なリテラルをchar*に入れるなっていう警告が出るけど、" (wait 9) (surface 1) "まあコンパイルとおるからいいよね？")
  (sakura "古いVC++ではstdbool.hがなくてコンパイルできないから、" (wait 9) (surface 1) "マクロでbool定めてるっていう……。")
  (sakura (surface 3) "あとの実装はキミしだい。")])

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

(defmacro ref [i]
  ~(get opts (string/format "Reference%d" ,i)))

(defn is-likely-sakura? [escaped-str]
  (some (fn [pref]
            (string/has-prefix? pref escaped-str))
        @["\\\\0" "\\\\1" "\\\\p" "\\\\h" "\\\\b"]))

(defmacro display-result [disp]
  ~(sakura-raw (scope :kero) (surface 52) (quick-section "> " ,disp) (nl) (nl)))

(defn prettify [s]
  (escape-str (string/format "%.20p" s)))

(shiori/set-handler "OnJanetEvalSuccess"
                    (let [default-resp (fn [x]
                                           (sakura (scope :kero) (surface 50) (quick-section "> " x (nl) (nl))
                                                   (scope :sakura) "こんなもんが出てきた。"))
                           disp (prettify (ref 0))]
                      (match (ref 1)
                             "shiori/set-handler"
                             (sakura (surface 1) "技の設定、完了。")

                                "shiori/register-handler"
                                (sakura (surface 4) "よしっ、新しい技を覚えた！")

                                "shiori/on-choice"
                                (sakura "選択肢を編集しました。")

                                "defn"
                                (sakura (surface 4)
                                        (display-result disp)
                                        (scope :sakura) "新しいファンクションの誕生だっ！"
                                        (scope :kero) "いいぜ、もっとくれ！")

                                "defmacro"
                                (sakura (surface 1)
                                        (display-result disp)
                                        (scope :sakura) "新しいマクロですね。"
                                        (scope :kero) "気をつけろよ。")

                                "get"
                                (if (= (ref 0) nil)
                                    (sakura "何も帰って来なかった。")
                                  (sakura
                                   (display-result disp)
                                   (scope :sakura) "ほいっ。"))

                               x
                               (cond
                                (= (type (ref 0)) :boolean)
                                (if (ref 0) (sakura "はい、答えはtrueです。")
                                  (sakura "いいえ、答えはfalseです。"))

                                (string? (ref 0))
                                (if (is-likely-sakura? (ref 0))
                                    (unescape-str (ref 0))
                                  (default-resp (string "\"" (ref 0) "\"")))

                                (default-resp disp)))))

(shiori/set-handler "OnJanetEvalFailure"
                    (sakura (surface 2)
                            (scope :kero) (quick-section "> " (ref 0)) (nl) (nl)
                            (scope :sakura)"あれ？失敗しちゃった。"
                            (scope :kero) (surface 51) "ぬっ？！"))

# (shiori/register-handler "OnJanetDocFinish"
#                          (if (ref 1)
#                              (sakura (string "ほいっ。\n" (ref 1)))
#                              (sakura (string (ref 2) "、見つからなかった…\n"))))

(set env (fiber/getenv (fiber/current)))

(def memory @{})

(defn remember [event opts status]
  (unless (get shiori/handlers event)
    (put memory event opts)))

(shiori/on-request remember)

(def menu (sakura
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

(shiori/set-handler "OnMouseDoubleClick"
                    (when (= (ref 3) "0")
                      (match (ref 4)

                             "Head"
                             (sakura (surface 1) "はい、頭なんです。" (wait 9) (surface 2) (sample @["ドラムセット" "ゲームパッド" "太鼓"]) "じゃなくて。")

                             "Face"
                             (sakura (surface 2) (sample @["いたっ。" "…なんで顔を叩いているんですか？"]))

                             "Bust"
                             (sakura (surface 3) "…エッチ。")

                             (sakura "なに？" menu))))

(shiori/set-handler "OnHourTimeSignal"
                    (sakura (ref 0) "時ですよ。"))

(shiori/on-choice "ai"
                  "トークを選択しました。")

(defn pull-refs [opts]
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
  (let [refs (pull-refs opts)]
    (string/join (map (fn [[k v]] (sakura-raw k " - " v (nl))) (partition 2 (values refs))))))

(shiori/on-choice "showmemory" (sakura (quick-section (show-memory opts) (nl))
                                       "こういうのを受け取れた気がします。"))

# TODO: need eager
# (macex '(sakura (marker) (choice (string/join (kvs @{"a" "b" "c" "D"})))))
(let [opts (string/join (kvs @{"Reference0" "b" "Reference1" "D"}) ",")]
  (sakura (marker) (choice "Show memory" "showmemory" opts)))

(shiori/on-choice "kioku"
                  (let [mem-choice (fn [[k v]] (string "\\q[" k ",showmemory," (string/join (map escape-str (kvs v)) ",") "]"))]
                    (sakura "えーっと、こういうイベントを覚えています。" (nl) (nl)
                            (balloon 2)
                            (quick-section
                             (string/join (map mem-choice (kvpairs memory)) "\\n"))
                            (nl)
                            (nl) (marker) (choice "忘れろ" "wasurero")
                            (nl) (marker) (choice "オッケー" "end"))))

(shiori/on-choice "wasurero"
                  (each k (keys memory) (put memory k nil))
                  (sakura (surface 4) "はい、全部忘れました。"
                          (scope :kero) (surface 51) "どうやって！？"))

(shiori/on-choice "status"
                  (sakura (scope :kero) (surface 52) (quick-section (string/join (keys status) "\\n") (nl) (nl))
                          (scope :sakura) "こっちは今の状況です。"
                          (scope :kero)  (string (length status) "個のエントリー。")))

(shiori/on-choice "state"
                  (let [text (string/join (map (fn [[k v]]
                                                   (string/format "%s - %s" (string k) (string v)))
                                               (kvpairs state)) "\\n")]
                    (sakura (scope :kero) (surface 52) (quick-section text (nl) (nl))
                            (scope :sakura) "単数の定義です。"
                            (scope :kero)  (string (length state) "個のエントリー。"))))
