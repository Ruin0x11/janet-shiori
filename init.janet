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
  (string/format "\\h\\s[%d]%s\\e" (or i 0) text))

(shiori/register-handler "OnFirstBoot"
  (sakura "First boot."))

(shiori/register-handler "OnClose"
  (sakura "Closing."))

(shiori/register-handler "OnGhostChanged"
  (sakura "Ghost changed."))

(shiori/register-handler "OnGhostChanging"
  (sakura "Ghost changing."))

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

(shiori/register-handler "OnJanetEval"
                         (let [code (get opts "Reference1")
                                    [stat res] (safe-eval-string code)]
                           (if (= stat :failure)
                               (sakura (string "あれ？失敗しちゃった。\n" res))
                             (sakura (string "こんなもんが出てきた。\n" res)))))

(print "loaded")
