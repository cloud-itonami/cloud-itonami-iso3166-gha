(ns culture.facts
  "Country-level regional-culture catalog for Ghana (GHA) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"GHA"
   [{:culture/id "gha.dish.jollof-rice"
     :culture/name "Jollof rice"
     :culture/country "GHA"
     :culture/kind :dish
     :culture/summary "West African rice dish originating from Senegal, claimed by several West African nations and the subject of a friendly rivalry between Ghana and Nigeria over who makes it best."
     :culture/url "https://en.wikipedia.org/wiki/Jollof_rice"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "gha.dish.fufu"
     :culture/name "Fufu"
     :culture/country "GHA"
     :culture/kind :dish
     :culture/summary "Pounded dough-like staple made from boiled cassava, plantains or cocoyams, believed to originate among the Akan people of what is now Ghana and now widespread across West Africa."
     :culture/url "https://en.wikipedia.org/wiki/Fufu"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "gha.dish.waakye"
     :culture/name "Waakye"
     :culture/country "GHA"
     :culture/kind :dish
     :culture/summary "Ghanaian dish of cooked rice and beans, commonly eaten for breakfast or lunch, with the beans cooked with sorghum leaves for distinctive flavour and colour."
     :culture/url "https://en.wikipedia.org/wiki/Waakye"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "gha.dish.kelewele"
     :culture/name "Kelewele"
     :culture/country "GHA"
     :culture/kind :dish
     :culture/summary "Popular Ghanaian dish of fried plantains seasoned with spices such as ginger and cayenne pepper, typically sold by street vendors."
     :culture/url "https://en.wikipedia.org/wiki/Fried_plantain"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "gha.product.cocoa"
     :culture/name "Ghanaian cocoa"
     :culture/country "GHA"
     :culture/kind :product
     :culture/summary "Ghana is the world's second-largest exporter of cocoa beans, after Côte d'Ivoire; cocoa production is a central, regulated part of the national economy."
     :culture/url "https://en.wikipedia.org/wiki/Cocoa_production_in_Ghana"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "gha.craft.kente"
     :culture/name "Kente cloth"
     :culture/country "GHA"
     :culture/kind :craft
     :culture/summary "Ghanaian hand-woven textile of silk and cotton, officially recognised as a geographical indication of Ghana, traditionally woven by Asante, Akan and Ewe weavers."
     :culture/url "https://en.wikipedia.org/wiki/Kente_cloth"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "gha.festival.homowo"
     :culture/name "Homowo"
     :culture/country "GHA"
     :culture/kind :festival
     :culture/summary "Harvest festival celebrated by the Ga people of Ghana's Greater Accra Region, commemorating a historical famine, with celebrations occurring primarily in August."
     :culture/url "https://en.wikipedia.org/wiki/Homowo"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "gha.heritage.cape-coast-castle"
     :culture/name "Cape Coast Castle"
     :culture/country "GHA"
     :culture/kind :heritage
     :culture/summary "One of about forty slave castles built on the Gold Coast of West Africa (now Ghana), a UNESCO World Heritage Site designated in 1979 and today a museum of the transatlantic slave trade."
     :culture/url "https://en.wikipedia.org/wiki/Cape_Coast_Castle"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-gha culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "GHA"))
                 " GHA entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
