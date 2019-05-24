(array/insert module/paths 0
              ["./modules/:all:.janet" :source]
              ["./modules/:all:/init.janet" :source]
              ["./modules/:all:.:native:" :native]
              ["./modules/:all:/:name.:native:" :native])

(import "shiori")
(import "lib" :prefix "")

(set shiori/version "0.0.1")
(set shiori/name "janet-shiori")
(set shiori/craftman "Ruin0x11")
(set shiori/craftmanw "ルイン")

(def default-surface 100)

(defn sakura [text &opt i]
  (string/format "\\h\\s[%d]%s\\0\\e" (or i default-surface) (escape-str text)))

(shiori/register-handler "OnFirstBoot"
  (sakura "はじめまして。"))

(shiori/register-handler "OnBoot"
  (sakura "\\0よっ。\\1\\s[50]ちっす。"))

(shiori/register-handler "OnClose"
  "\\h\\s[200]じゃあね。\\w9\\-")

(shiori/register-handler "OnGhostChanged"
  (sakura "交代、交代…"))

(shiori/register-handler "OnGhostChanging"
  (sakura "交代、交代…"))

(def phrases
@[(sakura "Janet言語って、\\w4胸キュン？")
  (sakura "ChickenSchemeもよろしく。" 5)
  (sakura "C++だとconst char*なリテラルをchar*に入れるなっていう警告が出るけど、\\w9\\s[1]まあコンパイルとおるからいいよね？")
  (sakura "古いVC++ではstdbool.hがなくてコンパイルできないから、\\w9\\s[1]マクロでbool定めてるっていう……。")
  (sakura "あとの実装はキミしだい。" 3)])

(defn sample [col]
  (get col (math/floor (* (length col) (math/random)))))

(shiori/set-handler "OnSecondChange"
                    (set it status)
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

(defn is-likely-sakura? [str]
  (some (fn [pref]
            (string/has-prefix? pref str))
        @["\\h" "\\t"]))

(shiori/set-handler "OnJanetEvalSuccess"
                    (let [default-resp (fn [x]
                                           (sakura (string "> " x "\n\n\\0こんなもんが出てきた。")))]
                      (match (ref 1)
                             "shiori/set-handler"
                             (sakura "技の設定、完了。" 1)

                             "shiori/register-handler"
                             (sakura "よしっ、新しい技を覚えた！" 4)

                             "shiori/on-choice"
                             (sakura "選択肢を編集しました。" 0)

                             "defn"
                             (sakura (string "新しいファンクションの誕生だっ！\n\n> " (ref 0)))

                             "get"
                             (if (= (ref 0) nil)
                                 (sakura "何も帰って来なかった。")
                               (sakura (string "ほいっ。" (ref 0))))

                             x
                             (cond
                              (= (type (ref 0)) :boolean)
                              (if (ref 0) (sakura "はい、答えはtrueです。")
                                (sakura "いいえ、答えはfalseです。"))

                              (string? (ref 0))
                              (if (is-likely-sakura? (ref 0))
                                  (ref 0)
                                (default-resp (string "\"" (ref 0) "\"")))

                              (default-resp (ref 0))))))

(shiori/set-handler "OnJanetEvalFailure"
                    (sakura (string "あれ？失敗しちゃった。\n\n> " (ref 0))))

# (shiori/register-handler "OnJanetDocFinish"
#                          (if (ref 1)
#                              (sakura (string "ほいっ。\n" (ref 1)))
#                              (sakura (string (ref 2) "、見つからなかった…\n"))))

(set env (fiber/getenv (fiber/current)))

(def memory @{})

(defn remember [event opts status]
  (unless (get shiori/handlers event)
    (put memory event true)))

(shiori/on-request remember)

(def menu "\\t\\b[2]\\n[half]\\n\\_q\\n![*]\\q[おしゃべり,ai]\\n![*]\\q[記憶,kioku]\\n![*]\\q[ステータス,status]\\n![*]\\q[定義,state]\\n\\n![*]\\q[別に,end]")

(shiori/set-handler "OnMouseDoubleClick"
                    (when (= (ref 3) "0")
                      (match (ref 4)

                             "Head"
                             (sakura (string/format "はい、頭なんです。\\w9\\s[2]%sじゃなくて。"
                                                    (sample @["ドラムセット" "ゲームパッド" "太鼓"])) 1)

                             "Face"
                             (sample @[(sakura "いたっ。" 2)
                                       (sakura "…なんで顔を叩いているんですか？" 2)])

                             "Bust"
                             (sakura "…エッチ。" 3)

                             (string "\\0\\s[0]なに？" menu))))

(shiori/on-choice "ai"
                  "トークを選択しました。")

(shiori/on-choice "kioku"
                  (string "\\h\\s[0]えーっと、こういうイベントを覚えています。\\n\\n\\_q"
                          (string/join (sort (keys memory)) "\\n")
                          "\\_q\\n\\n\\q[忘れろ,wasurero]\\n\\q[オッケー,end]\\e"))

(shiori/on-choice "wasurero"
                  (each k (keys memory) (put memory k nil))
                  (sakura "はい、全部忘れました。\\1\\s[51]どうやって！？"))

(shiori/on-choice "status"
                  (sakura (string "こっちは今の状況です。\\n\\n"
                                  (string/join (keys status) "\\n"))))

(shiori/on-choice "state"
                  (sakura (string "単数の定義です。\\n\\n"
                                  (string/join (map (fn [[k v]] (string/format "%s - %s" (string k) (string v))) (kvpairs state)) "\\n"))))
