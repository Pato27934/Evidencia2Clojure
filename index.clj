(def listaCalorias
  {"granulated sugar" 773
   "all-purpose flour" 455
   "cocoa powder" 196
   "powdered sugar" 467
   "chocolate chips" 805
   "egg" 72
   "canola oil" 1927
   "extra-virgin olive oil" 1910
   "almond flour" 640
   "baking powder" 53
   "kosher salt" 0
   "vanilla extract" 288
   "lemon zest" 6
   "lemon juice" 54
   "fettuccine pasta" 220
   "butter" 1627
   "heavy cream" 821
   "romano cheese" 431
   "parmesan cheese" 431
   "salt" 0
   "garlic salt" 5
   "vegetable oil" 1927
   "garlic" 203
   "fresh rosemary" 11
   "olive oil" 1910
   "white wine vinegar" 5
   "red pepper flakes" 49
   "smoked paprika" 19
   "parsley" 22})

(def listaConversiones
  {"granulated sugar" 200
   "all-purpose flour" 120
   "cocoa powder" 85
   "powdered sugar" 120
   "chocolate chips" 175
   "egg" 50
   "canola oil" 218
   "extra-virgin olive oil" 216
   "almond flour" 96
   "baking powder" 192
   "kosher salt" 288
   "vanilla extract" 208
   "lemon zest" 96
   "lemon juice" 240
   "fettuccine pasta" 100
   "butter" 227
   "heavy cream" 240
   "romano cheese" 100
   "parmesan cheese" 100
   "salt" 273
   "garlic salt" 288
   "vegetable oil" 218
   "garlic" 136
   "fresh rosemary" 28
   "olive oil" 216
   "white wine vinegar" 239
   "red pepper flakes" 45
   "smoked paprika" 92
   "parsley" 60})

(def listaRecetas
  ["recetas/Best Homemade Brownies-1.txt"
   "recetas/Lemon Cake-1.txt"
   "recetas/Fettuccine Alfredo.txt"
   "recetas/Pan-Seared Steak with Garlic Butter.txt"
   "recetas/Chimichurri Sauce.txt"])


(defn split [texto separador]
  (vec (.split texto separador)))

(defn splitLines [texto]
  (split texto "\n"))

(defn lowerCase [texto]
  (.toLowerCase texto))

(defn contienePalabra [linea palabra]
  (let [contenido (lowerCase linea)
        p (lowerCase palabra)]
    (not (nil? (re-find (re-pattern (str "(?i)" p)) contenido)))))

(defn contieneCaracter [linea]
  (not= linea (apply str (repeat (count linea) \space))))


(defn trim [s]
  (let [start (count (re-find #"^\s*" s))
        end (- (count s) (count (re-find #"\s*$" s)))]
    (subs s start end)))

(defn leerOptions [ruta]
  (let [lineas (splitLines (slurp ruta))]
    (into {}
          (map (fn [linea]
                 (let [pos (or (.indexOf linea ":") -1)
                       clave (keyword (trim (subs linea 0 pos)))
                       valor (trim (subs linea (inc pos)))]
                   [clave (lowerCase valor)]))
               lineas))))



(defn filtrarRecetas [archivos keyword]
  (filter (fn [ruta]
            (let [contenido (slurp ruta)
                  lineas (splitLines contenido)
                  titulo (first lineas)
                  start (->> lineas
                             (map-indexed vector)
                             (filter (fn [[_ l]] (re-find #"(?i)ingredients" l)))
                             first
                             first)]
              (if (= (lowerCase keyword) "all")
                true
                (or
                 ;; Buscar en el título
                 (contienePalabra titulo keyword)
                 ;; Buscar en los ingredientes
                 (when start
                   (let [seccion (subvec lineas (inc start))
                         ingredientes (take-while #(not (re-find #"(?i)instructions" %)) seccion)]
                     (some #(contienePalabra % keyword) ingredientes)))))))
          archivos))

;Obtener titulo receta
(defn obtenerTitulo [ruta]
  (first (splitLines (slurp ruta))))


(defn split-slashed [s]
  (let [idx (.indexOf s "/")]
    [(subs s 0 idx) (subs s (inc idx))]))


(defn parse-fraccion [s]
  (try
    (if (not= -1 (.indexOf s "/"))
      (let [[num den] (split-slashed s)]
        (/ (Double/parseDouble num) (Double/parseDouble den)))
      (Double/parseDouble s))
    (catch Exception _ nil)))

(defn conversionAGramos [cantidad unidad]
  (let [u (when unidad (lowerCase unidad))]
    (cond
      (or (= u "ounce") (= u "ounces")) (* cantidad 28.35)
      (or (= u "cup") (= u "cups")) (* cantidad 240)
      (or (= u "pint") (= u "pints")) (* cantidad 473)
      (or (= u "dash") (= u "dashes")) (* cantidad 0.6)
      (or (= u "tablespoon") (= u "tablespoons")) (* cantidad 15)
      (or (= u "teaspoon") (= u "teaspoons")) (* cantidad 5)
      :else cantidad)))

(defn conversionGramosTabla [cantidad unidad ingrediente]
  (let [u (when unidad (lowerCase unidad))
        ingr (when ingrediente (lowerCase ingrediente))
        factor (get listaConversiones ingr)]
    (cond
      (or (= u "gram") (= u "grams") (= u "g")) cantidad

      (and (or (= u "cup") (= u "cups")) factor) (* cantidad factor)
      
      (or (= u "ounce") (= u "ounces")) (* cantidad 28.35)
      
      (and (or (= u "pint") (= u "pints")) factor) (* cantidad factor 2)

      (and (or (= u "teaspoon") (= u "tsp") ) factor) (* cantidad factor 0.0208)

      (and (or (= u "tablespoon") (= u "tablespoons") (= u "tbsp")) factor) (* cantidad factor 0.0625)

      (or (= u "dash") (= u "dashes")) (* cantidad 0.6)

      :else cantidad)))


(defn mainReceta [ruta tipoConversion]
  (let [lineas (splitLines (slurp ruta))
        start (->> lineas
                   (map-indexed vector)
                   (filter (fn [[_ l]] (re-find #"(?i)ingredients" l)))
                   first
                   first)
        seccion (subvec lineas (inc start))
        ingredientes (->> seccion
                          (take-while #(not (re-find #"(?i)instructions" %)))
                          (filter #(re-find #"\S" %)))
        unidad-regex #"(?i)\b(cups?|pints?|ounces?|dashes?|tablespoons?|teaspoons?|grams?|kgs?|ml|liters?)\b"]
    (doseq [ing ingredientes]
      (let [cant-str (re-find #"[\d/\.]+" ing)
            cant (when cant-str (parse-fraccion cant-str))
            unidad-match (re-find unidad-regex ing)
            unidad (when unidad-match (lowerCase (second unidad-match)))
            cantidad-final (if (= (lowerCase tipoConversion) "metric")
                             (conversionAGramos cant unidad)
                             cant)
            unidad-final (if (nil? unidad) unidad (if (= (lowerCase tipoConversion) "metric") "grams" unidad))]
        (println {:cantidad cantidad-final :unidad unidad-final})))))




(defn -main []
  (let [opciones (leerOptions "options.txt")
        keywordReceta (:filtra opciones)
        tipoConversion (:sistema opciones)
        recetasFiltradas (filtrarRecetas listaRecetas keywordReceta)]
    (println (seq tipoConversion))
    (println "Recetas encontradas con filtro:" keywordReceta) 
    (if (empty? recetasFiltradas)
      (println "> Ninguna receta coincide con el filtro.")
      (doseq [ruta recetasFiltradas]
        ;(println ruta)
        (println (obtenerTitulo ruta))
        (mainReceta ruta tipoConversion)))))

(-main)