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

(defn sakura [text &opt i]
  (escape-str (string/format "\\h\\s[%d]%s\\e" (or i 0) text)))

(shiori/register-handler "OnFirstBoot"
  (sakura "はじめまして。"))

(shiori/register-handler "OnBoot"
  (sakura "よっ。"))

(shiori/register-handler "OnClose"
  "\\h\\s[0]じゃあね。\\w9\\-")

(shiori/register-handler "OnGhostChanged"
  (sakura "交代、交代…"))

(shiori/register-handler "OnGhostChanging"
  (sakura "交代、交代…"))

(def phrases
@[(sakura "C言語って、\\w4胸キュン？")
  (sakura "MiyoJSもよろしく。" 5)
  (sakura "C++だとconst char*なリテラルをchar*に入れるなっていう警告が出るけど、\\w9\\s[8]まあコンパイルとおるからいいよね？")
  (sakura "古いVC++ではstdbool.hがなくてコンパイルできないから、\\w9\\s[8]マクロでbool定めてるっていう……。")
  (sakura "あとの実装はキミしだい。" 6)])

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

(shiori/register-handler "OnJanetEvalSuccess"
                         (match (ref 1)
                                "defn" (sakura (string "新しいファンクションの誕生だっ！\n\n    " (ref 0)))
                                x (sakura (string "> " (ref 0) "\n\nこんなもんが出てきた。"))))

(shiori/register-handler "OnJanetEvalFailure"
                         (sakura (string "あれ？失敗しちゃった。\n\n    " (ref 0))))

# (shiori/register-handler "OnJanetDocFinish"
#                          (if (ref 1)
#                              (sakura (string "ほいっ。\n" (ref 1)))
#                              (sakura (string (ref 2) "、見つからなかった…\n"))))

(set env (fiber/getenv (fiber/current)))
