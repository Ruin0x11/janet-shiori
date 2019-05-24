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
  (string/format "\\h\\s[%d]%s\\e" (or i default-surface) (escape-str text)))

(shiori/register-handler "OnFirstBoot"
  (sakura "はじめまして。"))

(shiori/register-handler "OnBoot"
  (sakura "よっ。"))

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

(shiori/register-handler "OnSecondChange"
  (let [aitalk (or (get state :aitalk) 0)]
    (if (> aitalk 90)
        [(sample phrases) (merge state {:aitalk 0})]
        [{:status 204} (merge state {:aitalk (inc aitalk)})])))

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

(shiori/register-handler "OnJanetEvalSuccess"
                         (let [default-resp (fn [x]
                                                (sakura (string "> " x "\n\nこんなもんが出てきた。")))]
                           (match (ref 1)
                                  "shiori/register-handler"
                                  (sakura "よしっ、新しい技を覚えた！" 4)

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

(shiori/register-handler "OnJanetEvalFailure"
                         (sakura (string "あれ？失敗しちゃった。\n\n    " (ref 0))))

# (shiori/register-handler "OnJanetDocFinish"
#                          (if (ref 1)
#                              (sakura (string "ほいっ。\n" (ref 1)))
#                              (sakura (string (ref 2) "、見つからなかった…\n"))))

(set env (fiber/getenv (fiber/current)))

(def memory @{})

(defn remember [event opts]
  (unless (get shiori/handlers event)
    (put memory event true)))

(shiori/on-request remember)

(def menu "\\t\\b[2]\\n[half]\\n\\_q\\n![*]\\q[おしゃべり,ai]")

(shiori/register-handler "OnMouseDoubleClick"
                         (match (ref 3)

                                "Head"
                                (sakura (string/format "はい、頭なんです。\\w5\\s[2]%sじゃなくて。"
                                                       (sample @["ドラムセット" "ゲームパッド" "太鼓"])) 1)

                                "Face"
                                (sample @[(sakura "いたっ。" 2)
                                          (sakura "…なんで顔を叩いているんですか？" 2)])

                                "Bust"
                                (sakura "…エッチ。" 3)

                                ("\\0\\sなに？" menu)))
