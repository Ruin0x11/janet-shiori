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

(def default-surface 100)

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
  (sakura (surface 5) "ChickenSchemeもよろしく。" )
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
        @["\\\\0" "\\\\1" "\\\\p" "\\\\h"]))

(shiori/set-handler "OnJanetEvalSuccess"
                    (let [default-resp (fn [x]
                                           (sakura (scope :kero) (surface 50) (quick-section "> " x (nl) (nl))
                                                   (scope :sakura) "こんなもんが出てきた。"))
                           disp (string/format "%.20p" (ref 0))]
                      (match (ref 1)
                             "shiori/set-handler"
                             (sakura (surface 1) "技の設定、完了。")

                                "shiori/register-handler"
                                (sakura (surface 4) "よしっ、新しい技を覚えた！")

                                "shiori/on-choice"
                                (sakura "選択肢を編集しました。")

                                "defn"
                                (sakura (surface 4)
                                        (scope :kero) (surface 51) (quick-section "> " disp) (nl) (nl)
                                        (scope :sakura) "新しいファンクションの誕生だっ！"
                                        (scope :kero) "いいぜ、もっとくれ！")

                                "get"
                                (if (= (ref 0) nil)
                                    (sakura "何も帰って来なかった。")
                                  (sakura "ほいっ。" disp))

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
#
(def memory @{})
#
(defn remember [event opts status]
  (unless (get shiori/handlers event)
    (put memory event opts)))

(shiori/on-request remember)

(def menu (sakura
           (time-critical)
           (balloon 2)
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

(shiori/on-choice "ai"
                  "トークを選択しました。")

(shiori/on-choice "kioku"
                  (sakura "えーっと、こういうイベントを覚えています。" (nl) (nl)
                          (quick-section
                           (string/join (sort (keys memory)) "\\n"))
                          (nl)
                          (nl) (choice "忘れろ" "wasurero")
                          (nl) (choice "オッケー" "end")))

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
