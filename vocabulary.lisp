(in-package #:lang)

;;; Master vocabulary for aphorism languages
;;; ~1030+ entries with rich semantic tags
;;; First tag = domain, then animacy/materiality/function/class tags
;;; Each word: (gloss category length-priority :tag1 :tag2 ...)
;;; Length priority 1-10: lower = more important, allocated first (shorter forms)

(defparameter *vocabulary*
  '(
    ;; ═══════════════════════════════════════════════════
    ;; NATURE / WEATHER / TERRAIN
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("sun" noun 1 :nature :inanimate :natural :celestial)
    ("moon" noun 1 :nature :inanimate :natural :celestial)
    ("star" noun 1 :nature :inanimate :natural :celestial)
    ("fire" noun 1 :nature :inanimate :natural)
    ("water" noun 1 :nature :inanimate :natural :liquid)
    ("stone" noun 1 :nature :inanimate :natural :mineral :stone)
    ("earth" noun 1 :nature :inanimate :natural :terrain)
    ("sky" noun 1 :nature :inanimate :natural :celestial)
    ("tree" noun 2 :nature :inanimate :natural :plant)
    ("root" noun 2 :nature :inanimate :natural :plant)
    ("seed" noun 2 :nature :inanimate :natural :plant)
    ("leaf" noun 2 :nature :inanimate :natural :plant)
    ("mountain" noun 2 :nature :inanimate :natural :terrain)
    ("river" noun 2 :nature :inanimate :natural :terrain :liquid)
    ("forest" noun 2 :nature :inanimate :natural :terrain :plant)
    ("sea" noun 1 :nature :inanimate :natural :terrain :liquid)
    ("night" noun 1 :nature :inanimate :natural :celestial)
    ("day" noun 1 :nature :inanimate :natural :celestial)
    ("wind" noun 2 :nature :inanimate :natural :weather)
    ("rain" noun 1 :nature :inanimate :natural :weather :liquid)
    ("ash" noun 3 :nature :inanimate :natural :mineral)
    ("dust" noun 3 :nature :inanimate :natural :mineral)
    ("smoke" noun 3 :nature :inanimate :natural)
    ("shadow" noun 2 :nature :inanimate :natural)
    ("light" noun 1 :nature :inanimate :natural :celestial)
    ("flower" noun 2 :nature :inanimate :natural :plant)
    ("snow" noun 2 :nature :inanimate :natural :weather)
    ("dawn" noun 2 :nature :inanimate :natural :celestial)
    ("cloud" noun 2 :nature :inanimate :natural :weather :celestial)
    ("thunder" noun 3 :nature :inanimate :natural :weather)
    ("lightning" noun 4 :nature :inanimate :natural :weather)
    ("frost" noun 3 :nature :inanimate :natural :weather)
    ("ice" noun 2 :nature :inanimate :natural :weather)
    ("fog" noun 3 :nature :inanimate :natural :weather)
    ("storm" noun 3 :nature :inanimate :natural :weather)
    ("dew" noun 4 :nature :inanimate :natural :weather :liquid)
    ("hail" noun 4 :nature :inanimate :natural :weather)
    ("mud" noun 4 :nature :inanimate :natural :terrain)
    ("sand" noun 3 :nature :inanimate :natural :terrain :mineral)
    ("cliff" noun 3 :nature :inanimate :natural :terrain :stone)
    ("cave" noun 3 :nature :inanimate :natural :terrain :stone)
    ("hill" noun 3 :nature :inanimate :natural :terrain)
    ("valley" noun 4 :nature :inanimate :natural :terrain)
    ("marsh" noun 4 :nature :inanimate :natural :terrain)
    ("lake" noun 3 :nature :inanimate :natural :terrain :liquid)
    ("spring" noun 3 :nature :inanimate :natural :terrain :liquid)
    ("tide" noun 4 :nature :inanimate :natural :terrain :liquid)
    ("island" noun 4 :nature :inanimate :natural :terrain)
    ("shore" noun 4 :nature :inanimate :natural :terrain)
    ("boulder" noun 5 :nature :inanimate :natural :terrain :stone)
    ("peak" noun 4 :nature :inanimate :natural :terrain)
    ("grove" noun 4 :nature :inanimate :natural :terrain :plant)
    ("moss" noun 5 :nature :inanimate :natural :plant)
    ("thorn" noun 4 :nature :inanimate :natural :plant)
    ("bark" noun 4 :nature :inanimate :natural :plant)
    ("vine" noun 4 :nature :inanimate :natural :plant)
    ("branch" noun 3 :nature :inanimate :natural :plant)
    ("pond" noun 4 :nature :inanimate :natural :terrain :liquid)
    ("meadow" noun 5 :nature :inanimate :natural :terrain :plant)
    ("dusk" noun 4 :nature :inanimate :natural :celestial)
    ("ember" noun 5 :nature :inanimate :natural)
    ("flame" noun 2 :nature :inanimate :natural)
    ("spark" noun 3 :nature :inanimate :natural)
    ("stream" noun 3 :nature :inanimate :natural :terrain :liquid)
    ("gorge" noun 5 :nature :inanimate :natural :terrain)
    ("desert" noun 5 :nature :inanimate :natural :terrain)
    ("jungle" noun 5 :nature :inanimate :natural :terrain :plant)
    ("steppe" noun 6 :nature :inanimate :natural :terrain)
    ("horizon" noun 6 :nature :inanimate :natural :celestial)
    ("rainbow" noun 6 :nature :inanimate :natural :weather :celestial)
    ("mist" noun 4 :nature :inanimate :natural :weather)
    ("breeze" noun 4 :nature :inanimate :natural :weather)
    ("pebble" noun 6 :nature :inanimate :natural :mineral :stone)
    ("crater" noun 7 :nature :inanimate :natural :terrain)
    ("rapids" noun 7 :nature :inanimate :natural :terrain :liquid)
    ("reef" noun 6 :nature :inanimate :natural :terrain)
    ("delta" noun 7 :nature :inanimate :natural :terrain :liquid)
    ("glade" noun 6 :nature :inanimate :natural :terrain :plant)
    ("thicket" noun 7 :nature :inanimate :natural :terrain :plant)
    ("canyon" noun 6 :nature :inanimate :natural :terrain)

    ;; -- Verbs --
    ("shine" verb 1 :nature :perception :stative)
    ("grow" verb 1 :nature :physical :creation)
    ("burn" verb 1 :nature :destruction :physical)
    ("fall" verb 1 :nature :motion :physical)
    ("rise" verb 1 :nature :motion :physical)
    ("walk" verb 1 :nature :motion :physical)
    ("run" verb 1 :nature :motion :physical)
    ("come" verb 1 :nature :motion :physical)
    ("go" verb 1 :nature :motion :physical)
    ("fly" verb 2 :nature :motion :physical)
    ("swim" verb 2 :nature :motion :physical)
    ("follow" verb 2 :nature :motion :social)
    ("turn" verb 2 :nature :motion :physical)
    ("flow" verb 3 :nature :motion :physical)
    ("bloom" verb 3 :nature :creation :physical)
    ("freeze" verb 3 :nature :physical :stative)
    ("melt" verb 3 :nature :destruction :physical)
    ("flood" verb 4 :nature :destruction :physical)
    ("wither" verb 5 :nature :destruction :physical)
    ("sprout" verb 4 :nature :creation :physical)

    ;; -- Adjectives --
    ("hot" adjective 1 :nature :sensory :temperature)
    ("cold" adjective 1 :nature :sensory :temperature)
    ("bright" adjective 1 :nature :sensory :color)
    ("dark" adjective 1 :nature :sensory :color)
    ("deep" adjective 1 :nature :dimensional :sensory)
    ("high" adjective 1 :nature :dimensional :sensory)
    ("green" adjective 2 :nature :sensory :color)
    ("warm" adjective 1 :nature :sensory :temperature)
    ("wet" adjective 3 :nature :sensory :temperature)
    ("steep" adjective 4 :nature :dimensional :sensory)
    ("barren" adjective 5 :nature :evaluative :sensory)
    ("lush" adjective 4 :nature :evaluative :sensory)
    ("wild" adjective 2 :nature :evaluative :sensory)
    ("calm" adjective 2 :nature :evaluative :sensory)
    ("frozen" adjective 5 :nature :sensory :temperature)
    ("clear" adjective 2 :nature :sensory :color)
    ("rocky" adjective 5 :nature :sensory :dimensional)
    ("vast" adjective 3 :nature :dimensional :evaluative)
    ("misty" adjective 6 :nature :sensory :color)
    ("stormy" adjective 5 :nature :sensory :evaluative)

    ;; ═══════════════════════════════════════════════════
    ;; BODY / HEALTH
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("blood" noun 1 :body :inanimate :natural :liquid :bodypart)
    ("bone" noun 1 :body :inanimate :natural :bodypart)
    ("heart" noun 1 :body :inanimate :natural :bodypart)
    ("eye" noun 1 :body :inanimate :natural :bodypart)
    ("hand" noun 1 :body :inanimate :natural :bodypart)
    ("tooth" noun 1 :body :inanimate :natural :bodypart)
    ("tongue" noun 2 :body :inanimate :natural :bodypart)
    ("head" noun 1 :body :inanimate :natural :bodypart)
    ("foot" noun 2 :body :inanimate :natural :bodypart)
    ("skin" noun 2 :body :inanimate :natural :bodypart)
    ("skull" noun 3 :body :inanimate :natural :bodypart)
    ("lung" noun 3 :body :inanimate :natural :bodypart)
    ("rib" noun 3 :body :inanimate :natural :bodypart)
    ("spine" noun 3 :body :inanimate :natural :bodypart)
    ("jaw" noun 2 :body :inanimate :natural :bodypart)
    ("arm" noun 2 :body :inanimate :natural :bodypart)
    ("leg" noun 2 :body :inanimate :natural :bodypart)
    ("knee" noun 3 :body :inanimate :natural :bodypart)
    ("shoulder" noun 4 :body :inanimate :natural :bodypart)
    ("chest" noun 2 :body :inanimate :natural :bodypart)
    ("belly" noun 4 :body :inanimate :natural :bodypart)
    ("throat" noun 2 :body :inanimate :natural :bodypart)
    ("brain" noun 3 :body :inanimate :natural :bodypart)
    ("vein" noun 4 :body :inanimate :natural :bodypart)
    ("flesh" noun 2 :body :inanimate :natural :bodypart)
    ("womb" noun 4 :body :inanimate :natural :bodypart)
    ("palm" noun 3 :body :inanimate :natural :bodypart)
    ("fist" noun 2 :body :inanimate :natural :bodypart)
    ("nail" noun 4 :body :inanimate :natural :bodypart)
    ("lip" noun 3 :body :inanimate :natural :bodypart)
    ("brow" noun 3 :body :inanimate :natural :bodypart)
    ("cheek" noun 4 :body :inanimate :natural :bodypart)
    ("ear" noun 2 :body :inanimate :natural :bodypart)
    ("neck" noun 2 :body :inanimate :natural :bodypart)
    ("hip" noun 5 :body :inanimate :natural :bodypart)
    ("wrist" noun 5 :body :inanimate :natural :bodypart)
    ("elbow" noun 6 :body :inanimate :natural :bodypart)
    ("scalp" noun 6 :body :inanimate :natural :bodypart)
    ("marrow" noun 7 :body :inanimate :natural :bodypart :liquid)
    ("sinew" noun 7 :body :inanimate :natural :bodypart)

    ;; -- Verbs --
    ("die" verb 1 :body :physical :stative)
    ("live" verb 1 :body :physical :stative)
    ("eat" verb 1 :body :consumption :physical)
    ("drink" verb 1 :body :consumption :physical)
    ("sleep" verb 1 :body :physical :stative)
    ("bite" verb 2 :body :physical :violence)
    ("sit" verb 1 :body :physical :stative)
    ("breathe" verb 3 :body :physical :stative)
    ("bleed" verb 3 :body :physical :stative)
    ("heal" verb 3 :body :physical :creation)
    ("sweat" verb 4 :body :physical :stative)
    ("cough" verb 5 :body :physical :stative)
    ("tremble" verb 5 :body :physical :emotional)
    ("starve" verb 5 :body :consumption :physical)
    ("scratch" verb 5 :body :physical :violence)

    ;; -- Adjectives --
    ("blind" adjective 1 :body :sensory :evaluative)
    ("sick" adjective 2 :body :evaluative :sensory)
    ("pale" adjective 3 :body :sensory :color)
    ("scarred" adjective 4 :body :evaluative :sensory)
    ("thin" adjective 3 :body :dimensional :sensory)
    ("fat" adjective 3 :body :dimensional :sensory)
    ("sore" adjective 5 :body :sensory :evaluative)
    ("numb" adjective 5 :body :sensory :evaluative)
    ("lame" adjective 5 :body :evaluative :sensory)
    ("hungry" adjective 4 :body :sensory :evaluative)

    ;; ═══════════════════════════════════════════════════
    ;; KINSHIP / FAMILY
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("man" noun 1 :kinship :human :animate)
    ("woman" noun 1 :kinship :human :animate)
    ("child" noun 1 :kinship :human :animate)
    ("father" noun 1 :kinship :human :animate)
    ("mother" noun 1 :kinship :human :animate)
    ("daughter" noun 1 :kinship :human :animate)
    ("wife" noun 1 :kinship :human :animate)
    ("brother" noun 2 :kinship :human :animate)
    ("son" noun 1 :kinship :human :animate)
    ("husband" noun 3 :kinship :human :animate)
    ("sister" noun 2 :kinship :human :animate)
    ("elder" noun 3 :kinship :human :animate)
    ("infant" noun 4 :kinship :human :animate)
    ("twin" noun 4 :kinship :human :animate)
    ("orphan" noun 5 :kinship :human :animate)
    ("widow" noun 5 :kinship :human :animate)
    ("ancestor" noun 6 :kinship :human :animate :abstract)
    ("heir" noun 3 :kinship :human :animate)
    ("firstborn" noun 5 :kinship :human :animate)
    ("uncle" noun 4 :kinship :human :animate)
    ("aunt" noun 4 :kinship :human :animate)
    ("cousin" noun 5 :kinship :human :animate)
    ("nephew" noun 5 :kinship :human :animate)
    ("niece" noun 5 :kinship :human :animate)
    ("bride" noun 4 :kinship :human :animate)
    ("groom" noun 4 :kinship :human :animate)
    ("clan" noun 3 :kinship :human :abstract)
    ("tribe" noun 3 :kinship :human :abstract)
    ("kin" noun 3 :kinship :human :abstract)
    ("maiden" noun 5 :kinship :human :animate)

    ;; -- Verbs --
    ("love" verb 1 :kinship :emotional :social)
    ("hate" verb 2 :kinship :emotional :social)
    ("marry" verb 4 :kinship :social :emotional)
    ("mourn" verb 3 :kinship :emotional :social)
    ("nurse" verb 4 :kinship :physical :social)
    ("adopt" verb 5 :kinship :social :emotional)
    ("wed" verb 4 :kinship :social :emotional)
    ("raise" verb 3 :kinship :social :physical)
    ("bear" verb 2 :kinship :physical :creation)
    ("cherish" verb 5 :kinship :emotional :social)

    ;; -- Adjectives --
    ("young" adjective 1 :kinship :age :evaluative)
    ("old" adjective 1 :kinship :age :evaluative)
    ("eldest" adjective 4 :kinship :age :evaluative)
    ("beloved" adjective 6 :kinship :evaluative :emotional)
    ("kindred" adjective 5 :kinship :evaluative :social)

    ;; ═══════════════════════════════════════════════════
    ;; ANIMAL
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("wolf" noun 1 :animal :animate :natural)
    ("bird" noun 1 :animal :animate :natural)
    ("fish" noun 1 :animal :animate :natural)
    ("snake" noun 2 :animal :animate :natural)
    ("dog" noun 1 :animal :animate :natural)
    ("horse" noun 1 :animal :animate :natural)
    ("bear" noun 2 :animal :animate :natural)
    ("eagle" noun 3 :animal :animate :natural)
    ("hawk" noun 2 :animal :animate :natural)
    ("raven" noun 3 :animal :animate :natural)
    ("owl" noun 3 :animal :animate :natural)
    ("crow" noun 3 :animal :animate :natural)
    ("bull" noun 2 :animal :animate :natural)
    ("ram" noun 3 :animal :animate :natural)
    ("stag" noun 3 :animal :animate :natural)
    ("doe" noun 3 :animal :animate :natural)
    ("boar" noun 3 :animal :animate :natural)
    ("lion" noun 3 :animal :animate :natural)
    ("fox" noun 2 :animal :animate :natural)
    ("hare" noun 3 :animal :animate :natural)
    ("rat" noun 3 :animal :animate :natural)
    ("spider" noun 5 :animal :animate :natural)
    ("beetle" noun 6 :animal :animate :natural)
    ("moth" noun 5 :animal :animate :natural)
    ("worm" noun 4 :animal :animate :natural)
    ("frog" noun 4 :animal :animate :natural)
    ("lizard" noun 5 :animal :animate :natural)
    ("crab" noun 5 :animal :animate :natural)
    ("whale" noun 4 :animal :animate :natural)
    ("eel" noun 5 :animal :animate :natural)
    ("ox" noun 3 :animal :animate :natural)
    ("goat" noun 3 :animal :animate :natural)
    ("sheep" noun 3 :animal :animate :natural)
    ("pig" noun 3 :animal :animate :natural)
    ("cat" noun 2 :animal :animate :natural)
    ("mule" noun 4 :animal :animate :natural)
    ("falcon" noun 5 :animal :animate :natural)
    ("swallow" noun 6 :animal :animate :natural)
    ("viper" noun 5 :animal :animate :natural)
    ("tortoise" noun 7 :animal :animate :natural)

    ;; -- Verbs --
    ("hunt" verb 1 :animal :violence :physical)
    ("prowl" verb 4 :animal :motion :physical)
    ("stalk" verb 4 :animal :motion :violence)
    ("howl" verb 3 :animal :communication :physical)
    ("nest" verb 4 :animal :creation :physical)
    ("swarm" verb 5 :animal :motion :physical)
    ("graze" verb 5 :animal :consumption :physical)
    ("peck" verb 5 :animal :physical :violence)
    ("burrow" verb 6 :animal :motion :physical)
    ("shed" verb 5 :animal :physical :stative)

    ;; -- Adjectives --
    ("fierce" adjective 2 :animal :evaluative :sensory)
    ("tame" adjective 3 :animal :evaluative :sensory)
    ("feral" adjective 5 :animal :evaluative :sensory)
    ("swift" adjective 2 :animal :speed :evaluative)
    ("venomous" adjective 7 :animal :evaluative :sensory)
    ("cunning" adjective 3 :animal :evaluative :moral)
    ("meek" adjective 3 :animal :evaluative :moral)
    ("rabid" adjective 7 :animal :evaluative :sensory)
    ("docile" adjective 6 :animal :evaluative :moral)
    ("nimble" adjective 5 :animal :speed :evaluative)

    ;; ═══════════════════════════════════════════════════
    ;; COMBAT / WARFARE
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("king" noun 1 :combat :human :animate)
    ("sword" noun 1 :combat :inanimate :manufactured :weapon :metal)
    ("shield" noun 2 :combat :inanimate :manufactured :weapon :metal)
    ("war" noun 1 :combat :inanimate :abstract)
    ("death" noun 1 :combat :inanimate :abstract)
    ("guard" noun 2 :combat :human :animate)
    ("fleet" noun 2 :combat :inanimate :manufactured)
    ("spear" noun 2 :combat :inanimate :manufactured :weapon :wood)
    ("axe" noun 2 :combat :inanimate :manufactured :weapon :metal)
    ("bow" noun 2 :combat :inanimate :manufactured :weapon :wood)
    ("arrow" noun 3 :combat :inanimate :manufactured :weapon :wood)
    ("helm" noun 3 :combat :inanimate :manufactured :weapon :metal)
    ("armor" noun 4 :combat :inanimate :manufactured :weapon :metal)
    ("blade" noun 2 :combat :inanimate :manufactured :weapon :metal)
    ("knife" noun 2 :craft :inanimate :manufactured :tool :metal)
    ("mace" noun 3 :combat :inanimate :manufactured :weapon :metal)
    ("dagger" noun 4 :combat :inanimate :manufactured :weapon :metal)
    ("lance" noun 3 :combat :inanimate :manufactured :weapon :metal)
    ("banner" noun 4 :combat :inanimate :manufactured)
    ("fortress" noun 5 :combat :inanimate :manufactured :building :stone)
    ("siege" noun 3 :combat :inanimate :abstract)
    ("battle" noun 3 :combat :inanimate :abstract)
    ("army" noun 3 :combat :human :animate)
    ("soldier" noun 3 :combat :human :animate)
    ("warrior" noun 4 :combat :human :animate)
    ("marshal" noun 5 :combat :human :animate)
    ("scout" noun 3 :combat :human :animate)
    ("archer" noun 4 :combat :human :animate)
    ("knight" noun 2 :combat :human :animate)
    ("warlord" noun 5 :combat :human :animate)
    ("champion" noun 6 :combat :human :animate)
    ("conquest" noun 5 :combat :inanimate :abstract)
    ("truce" noun 3 :combat :inanimate :abstract)
    ("wound" noun 2 :combat :inanimate :natural :bodypart)
    ("scar" noun 3 :combat :inanimate :natural :bodypart)
    ("blood-price" noun 6 :combat :inanimate :abstract)
    ("ambush" noun 5 :combat :inanimate :abstract)
    ("raid" noun 3 :combat :inanimate :abstract)
    ("rampart" noun 7 :combat :inanimate :manufactured :building :stone)
    ("catapult" noun 7 :combat :inanimate :manufactured :weapon :wood)
    ("quiver" noun 6 :combat :inanimate :manufactured :container :leather)
    ("sheath" noun 5 :combat :inanimate :manufactured :container :leather)
    ("gauntlet" noun 7 :combat :inanimate :manufactured :weapon :metal)
    ("bolt" noun 3 :combat :inanimate :manufactured :weapon :metal)
    ("pike" noun 4 :combat :inanimate :manufactured :weapon :metal)
    ("trebuchet" noun 8 :combat :inanimate :manufactured :weapon :wood)

    ;; -- Verbs --
    ("kill" verb 1 :combat :violence :physical)
    ("fight" verb 1 :combat :violence :physical)
    ("hold" verb 1 :combat :physical :stative)
    ("break" verb 1 :combat :destruction :physical)
    ("strike" verb 2 :combat :violence :physical)
    ("charge" verb 3 :combat :motion :violence)
    ("retreat" verb 4 :combat :motion :physical)
    ("siege" verb 3 :combat :violence :physical)
    ("conquer" verb 4 :combat :violence :social)
    ("slay" verb 2 :combat :violence :physical)
    ("wound" verb 2 :combat :violence :physical)
    ("pierce" verb 3 :combat :violence :physical)
    ("crush" verb 3 :combat :violence :physical)
    ("defend" verb 3 :combat :physical :social)
    ("ambush" verb 5 :combat :violence :physical)
    ("raid" verb 3 :combat :violence :physical)
    ("besiege" verb 6 :combat :violence :physical)
    ("march" verb 3 :combat :motion :physical)
    ("rout" verb 5 :combat :violence :social)
    ("surrender" verb 6 :combat :social :physical)

    ;; -- Adjectives --
    ("strong" adjective 1 :combat :evaluative :dimensional)
    ("weak" adjective 1 :combat :evaluative :dimensional)
    ("brave" adjective 1 :combat :evaluative :moral)
    ("sharp" adjective 1 :combat :sensory :evaluative)
    ("dead" adjective 1 :combat :evaluative :stative)
    ("bold" adjective 1 :combat :evaluative :moral)
    ("ruthless" adjective 5 :combat :evaluative :moral)
    ("fallen" adjective 4 :combat :evaluative :stative)
    ("valiant" adjective 5 :combat :evaluative :moral)
    ("bloody" adjective 4 :combat :sensory :evaluative)

    ;; ═══════════════════════════════════════════════════
    ;; COMMERCE / ECONOMY
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("gold" noun 1 :commerce :inanimate :natural :metal)
    ("iron" noun 2 :commerce :inanimate :natural :metal)
    ("trade" noun 2 :commerce :inanimate :abstract)
    ("goods" noun 2 :commerce :inanimate :manufactured)
    ("cloth" noun 2 :commerce :inanimate :manufactured :fabric)
    ("merchant" noun 2 :commerce :human :animate)
    ("dock" noun 2 :commerce :inanimate :manufactured :building)
    ("harbor" noun 3 :commerce :inanimate :manufactured :building)
    ("ship" noun 2 :commerce :inanimate :manufactured :vehicle :wood)
    ("clerk" noun 2 :commerce :human :animate)
    ("silver" noun 3 :commerce :inanimate :natural :metal)
    ("copper" noun 4 :commerce :inanimate :natural :metal)
    ("coin" noun 3 :commerce :inanimate :manufactured :metal)
    ("debt" noun 3 :commerce :inanimate :abstract)
    ("profit" noun 4 :commerce :inanimate :abstract)
    ("tax" noun 3 :commerce :inanimate :abstract)
    ("wage" noun 4 :commerce :inanimate :abstract)
    ("market" noun 3 :commerce :inanimate :manufactured :building)
    ("warehouse" noun 6 :commerce :inanimate :manufactured :building)
    ("cargo" noun 5 :commerce :inanimate :manufactured)
    ("ledger" noun 6 :commerce :inanimate :manufactured)
    ("price" noun 3 :commerce :inanimate :abstract)
    ("bargain" noun 4 :commerce :inanimate :abstract)
    ("vault" noun 4 :commerce :inanimate :manufactured :building :stone)
    ("scale" noun 4 :commerce :inanimate :manufactured :tool :metal)
    ("purse" noun 4 :commerce :inanimate :manufactured :container :leather)
    ("gem" noun 3 :commerce :inanimate :natural :mineral)
    ("pearl" noun 4 :commerce :inanimate :natural :mineral)
    ("wax" noun 5 :commerce :inanimate :natural)
    ("tin" noun 5 :commerce :inanimate :natural :metal)
    ("brass" noun 5 :commerce :inanimate :manufactured :metal)
    ("jade" noun 5 :commerce :inanimate :natural :mineral)
    ("ivory" noun 7 :commerce :inanimate :natural)
    ("amber" noun 6 :commerce :inanimate :natural :mineral)
    ("silk" noun 4 :commerce :inanimate :manufactured :fabric)

    ;; -- Verbs --
    ("sell" verb 1 :commerce :transfer :social)
    ("own" verb 1 :commerce :stative :social)
    ("send" verb 1 :commerce :transfer :physical)
    ("bring" verb 1 :commerce :transfer :physical)
    ("return" verb 2 :commerce :transfer :physical)
    ("give" verb 1 :commerce :transfer :social)
    ("take" verb 1 :commerce :transfer :physical)
    ("buy" verb 2 :commerce :transfer :social)
    ("barter" verb 5 :commerce :transfer :social)
    ("hoard" verb 4 :commerce :stative :physical)
    ("steal" verb 2 :commerce :transfer :violence)
    ("weigh" verb 4 :commerce :perception :physical)
    ("owe" verb 4 :commerce :stative :social)
    ("pay" verb 2 :commerce :transfer :social)
    ("lend" verb 3 :commerce :transfer :social)

    ;; -- Adjectives --
    ("rich" adjective 2 :commerce :evaluative :quantity)
    ("poor" adjective 2 :commerce :evaluative :quantity)
    ("cheap" adjective 3 :commerce :evaluative :quantity)
    ("costly" adjective 5 :commerce :evaluative :quantity)
    ("scarce" adjective 4 :commerce :evaluative :quantity)
    ("plentiful" adjective 8 :commerce :evaluative :quantity)
    ("precious" adjective 5 :commerce :evaluative :quantity)
    ("counterfeit" adjective 9 :commerce :evaluative :moral)

    ;; ═══════════════════════════════════════════════════
    ;; SOCIETY / GOVERNMENT
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("lord" noun 1 :society :human :animate)
    ("empire" noun 3 :society :inanimate :abstract)
    ("city" noun 2 :society :inanimate :manufactured :building)
    ("council" noun 2 :society :human :abstract)
    ("law" noun 1 :society :inanimate :abstract)
    ("slave" noun 2 :society :human :animate)
    ("chain" noun 2 :society :inanimate :manufactured :metal)
    ("letter" noun 2 :society :inanimate :manufactured)
    ("right" noun 2 :society :inanimate :abstract)
    ("palace" noun 4 :society :inanimate :manufactured :building :stone)
    ("square" noun 3 :society :inanimate :manufactured :terrain)
    ("pride" noun 2 :society :inanimate :abstract)
    ("promise" noun 3 :society :inanimate :abstract)
    ("traitor" noun 3 :society :human :animate)
    ("charge" noun 2 :society :inanimate :abstract)
    ("row" noun 4 :society :inanimate :abstract)
    ("house" noun 1 :society :inanimate :manufactured :building)
    ("people" noun 1 :society :human :animate)
    ("friend" noun 1 :society :human :animate)
    ("enemy" noun 2 :society :human :animate)
    ("crown" noun 2 :society :inanimate :manufactured :ornament :metal)
    ("throne" noun 2 :society :inanimate :manufactured :furniture)
    ("court" noun 3 :society :human :abstract)
    ("noble" noun 4 :society :human :animate)
    ("peasant" noun 4 :society :human :animate)
    ("judge" noun 2 :society :human :animate)
    ("herald" noun 5 :society :human :animate)
    ("rebel" noun 4 :society :human :animate)
    ("citizen" noun 6 :society :human :animate)
    ("exile" noun 4 :society :human :animate)
    ("prison" noun 4 :society :inanimate :manufactured :building :stone)
    ("treaty" noun 5 :society :inanimate :abstract)
    ("decree" noun 5 :society :inanimate :abstract)
    ("tribute" noun 5 :society :inanimate :abstract)
    ("guild" noun 4 :society :human :abstract)
    ("charter" noun 6 :society :inanimate :abstract)
    ("seal" noun 3 :society :inanimate :manufactured :ornament)
    ("oath" noun 2 :society :inanimate :abstract)
    ("rank" noun 3 :society :inanimate :abstract)
    ("edict" noun 6 :society :inanimate :abstract)

    ;; -- Verbs --
    ("lead" verb 2 :society :social :physical)
    ("rule" verb 1 :society :social :stative)
    ("serve" verb 1 :society :social :physical)
    ("judge" verb 2 :society :social :cognition)
    ("obey" verb 2 :society :social :stative)
    ("refuse" verb 2 :society :social :communication)
    ("ask" verb 1 :society :communication :social)
    ("protect" verb 2 :society :social :physical)
    ("chain" verb 2 :society :physical :violence)
    ("banish" verb 5 :society :social :violence)
    ("pardon" verb 5 :society :social :emotional)
    ("crown" verb 3 :society :social :creation)
    ("betray" verb 4 :society :social :violence)
    ("rebel" verb 4 :society :social :violence)
    ("decree" verb 5 :society :communication :social)

    ;; -- Adjectives --
    ("free" adjective 1 :society :evaluative :moral)
    ("proud" adjective 1 :society :evaluative :emotional)
    ("safe" adjective 2 :society :evaluative :sensory)
    ("just" adjective 2 :society :evaluative :moral)
    ("noble" adjective 4 :society :evaluative :moral)
    ("loyal" adjective 3 :society :evaluative :moral)
    ("lawful" adjective 5 :society :evaluative :moral)
    ("sovereign" adjective 8 :society :evaluative :moral)
    ("humble" adjective 3 :society :evaluative :moral)
    ("exiled" adjective 6 :society :evaluative :stative)

    ;; ═══════════════════════════════════════════════════
    ;; RELIGION / MYTHOLOGY
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("god" noun 1 :religion :animate :abstract)
    ("spirit" noun 3 :religion :animate :abstract)
    ("fate" noun 2 :religion :inanimate :abstract)
    ("temple" noun 4 :religion :inanimate :manufactured :building :stone)
    ("altar" noun 5 :religion :inanimate :manufactured :stone)
    ("priest" noun 2 :religion :human :animate)
    ("prophet" noun 5 :religion :human :animate)
    ("saint" noun 4 :religion :human :animate)
    ("demon" noun 4 :religion :animate :abstract)
    ("angel" noun 4 :religion :animate :abstract)
    ("soul" noun 2 :religion :inanimate :abstract)
    ("prayer" noun 2 :religion :inanimate :abstract)
    ("omen" noun 5 :religion :inanimate :abstract)
    ("curse" noun 2 :religion :inanimate :abstract)
    ("blessing" noun 4 :religion :inanimate :abstract)
    ("shrine" noun 4 :religion :inanimate :manufactured :building :stone)
    ("relic" noun 6 :religion :inanimate :manufactured)
    ("ritual" noun 6 :religion :inanimate :abstract)
    ("vision" noun 5 :religion :inanimate :abstract)
    ("heaven" noun 4 :religion :inanimate :abstract :celestial)
    ("hell" noun 3 :religion :inanimate :abstract)
    ("sin" noun 3 :religion :inanimate :abstract)
    ("virtue" noun 4 :religion :inanimate :abstract)
    ("martyr" noun 6 :religion :human :animate)
    ("pilgrim" noun 6 :religion :human :animate)
    ("idol" noun 6 :religion :inanimate :manufactured :stone)
    ("creed" noun 4 :religion :inanimate :abstract)
    ("heresy" noun 8 :religion :inanimate :abstract)
    ("miracle" noun 7 :religion :inanimate :abstract)
    ("pyre" noun 5 :religion :inanimate :manufactured :wood)
    ("tomb" noun 3 :religion :inanimate :manufactured :building :stone)
    ("hymn" noun 4 :religion :inanimate :abstract)
    ("oracle" noun 7 :religion :human :animate)
    ("sacrifice" noun 7 :religion :inanimate :abstract)
    ("tithe" noun 6 :religion :inanimate :abstract)

    ;; -- Verbs --
    ("believe" verb 2 :religion :cognition :emotional)
    ("hope" verb 1 :religion :emotional :cognition)
    ("pray" verb 2 :religion :communication :emotional)
    ("bless" verb 2 :religion :creation :social)
    ("curse" verb 2 :religion :destruction :social)
    ("worship" verb 5 :religion :social :emotional)
    ("sacrifice" verb 7 :religion :destruction :social)
    ("prophesy" verb 8 :religion :communication :cognition)
    ("atone" verb 6 :religion :emotional :social)
    ("sanctify" verb 8 :religion :creation :social)

    ;; -- Adjectives --
    ("true" adjective 1 :religion :evaluative :moral)
    ("holy" adjective 3 :religion :evaluative :moral)
    ("sacred" adjective 4 :religion :evaluative :moral)
    ("divine" adjective 4 :religion :evaluative :moral)
    ("cursed" adjective 3 :religion :evaluative :moral)
    ("blessed" adjective 3 :religion :evaluative :moral)
    ("profane" adjective 6 :religion :evaluative :moral)
    ("eternal" adjective 6 :religion :evaluative :age)

    ;; ═══════════════════════════════════════════════════
    ;; CRAFT / TRADES
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("hammer" noun 4 :craft :inanimate :manufactured :tool :metal)
    ("anvil" noun 5 :craft :inanimate :manufactured :tool :metal)
    ("forge" noun 3 :craft :inanimate :manufactured :building :metal)
    ("kiln" noun 5 :craft :inanimate :manufactured :building :stone)
    ("loom" noun 4 :craft :inanimate :manufactured :tool :wood)
    ("wheel" noun 3 :craft :inanimate :manufactured :tool :wood)
    ("needle" noun 5 :craft :inanimate :manufactured :tool :metal)
    ("chisel" noun 6 :craft :inanimate :manufactured :tool :metal)
    ("saw" noun 4 :craft :inanimate :manufactured :tool :metal)
    ("smith" noun 3 :craft :human :animate)
    ("mason" noun 5 :craft :human :animate)
    ("weaver" noun 5 :craft :human :animate)
    ("potter" noun 6 :craft :human :animate)
    ("tanner" noun 6 :craft :human :animate)
    ("carpenter" noun 7 :craft :human :animate)
    ("clay" noun 3 :craft :inanimate :natural :mineral)
    ("ore" noun 4 :craft :inanimate :natural :mineral :metal)
    ("rope" noun 3 :craft :inanimate :manufactured :plant)
    ("awl" noun 6 :craft :inanimate :manufactured :tool :metal)
    ("plank" noun 5 :craft :inanimate :manufactured :wood)
    ("brick" noun 4 :craft :inanimate :manufactured :stone)
    ("mortar" noun 6 :craft :inanimate :manufactured :mineral)
    ("glue" noun 6 :craft :inanimate :manufactured :liquid)
    ("dye" noun 4 :craft :inanimate :manufactured :liquid)
    ("ingot" noun 6 :craft :inanimate :manufactured :metal)

    ;; -- Verbs --
    ("make" verb 1 :craft :creation :physical)
    ("cut" verb 1 :craft :physical :destruction)
    ("forge" verb 3 :craft :creation :physical)
    ("carve" verb 3 :craft :creation :physical)
    ("weave" verb 4 :craft :creation :physical)
    ("mold" verb 4 :craft :creation :physical)
    ("grind" verb 4 :craft :destruction :physical)
    ("polish" verb 5 :craft :creation :physical)
    ("temper" verb 5 :craft :creation :physical)
    ("stitch" verb 5 :craft :creation :physical)
    ("hammer" verb 5 :craft :physical :creation)
    ("smelt" verb 5 :craft :creation :physical)

    ;; -- Adjectives --
    ("heavy" adjective 3 :craft :dimensional :sensory)
    ("rough" adjective 3 :craft :sensory :evaluative)
    ("smooth" adjective 3 :craft :sensory :evaluative)
    ("sturdy" adjective 5 :craft :evaluative :dimensional)
    ("brittle" adjective 6 :craft :evaluative :sensory)

    ;; ═══════════════════════════════════════════════════
    ;; AGRICULTURE / FARMING
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("field" noun 2 :agriculture :inanimate :natural :terrain)
    ("harvest" noun 3 :agriculture :inanimate :natural :plant)
    ("crop" noun 3 :agriculture :inanimate :natural :plant)
    ("grain" noun 3 :agriculture :inanimate :natural :plant :food-item)
    ("wheat" noun 4 :agriculture :inanimate :natural :plant :food-item)
    ("barley" noun 5 :agriculture :inanimate :natural :plant :food-item)
    ("vineyard" noun 5 :agriculture :inanimate :natural :terrain :plant)
    ("orchard" noun 5 :agriculture :inanimate :natural :terrain :plant)
    ("pasture" noun 5 :agriculture :inanimate :natural :terrain)
    ("plow" noun 4 :agriculture :inanimate :manufactured :tool :metal)
    ("sickle" noun 5 :agriculture :inanimate :manufactured :tool :metal)
    ("yoke" noun 5 :agriculture :inanimate :manufactured :tool :wood)
    ("barn" noun 4 :agriculture :inanimate :manufactured :building :wood)
    ("fence" noun 4 :agriculture :inanimate :manufactured :wood)
    ("well" noun 3 :agriculture :inanimate :manufactured :building)
    ("furrow" noun 6 :agriculture :inanimate :natural :terrain)
    ("herd" noun 3 :agriculture :animate :natural)
    ("flock" noun 3 :agriculture :animate :natural)
    ("farmer" noun 4 :agriculture :human :animate)
    ("shepherd" noun 5 :agriculture :human :animate)
    ("mill" noun 4 :agriculture :inanimate :manufactured :building)
    ("hay" noun 5 :agriculture :inanimate :natural :plant)
    ("chaff" noun 6 :agriculture :inanimate :natural :plant)
    ("soil" noun 3 :agriculture :inanimate :natural :terrain)
    ("compost" noun 7 :agriculture :inanimate :natural)

    ;; -- Verbs --
    ("sow" verb 2 :agriculture :creation :physical)
    ("reap" verb 2 :agriculture :physical :destruction)
    ("plow" verb 3 :agriculture :physical :creation)
    ("thresh" verb 5 :agriculture :physical :destruction)
    ("irrigate" verb 8 :agriculture :creation :physical)
    ("cultivate" verb 8 :agriculture :creation :physical)
    ("harvest" verb 4 :agriculture :physical :creation)
    ("plant" verb 3 :agriculture :creation :physical)
    ("tend" verb 3 :agriculture :physical :stative)
    ("prune" verb 5 :agriculture :destruction :physical)

    ;; -- Adjectives --
    ("fertile" adjective 6 :agriculture :evaluative :sensory)
    ("ripe" adjective 4 :agriculture :evaluative :sensory)
    ("fallow" adjective 6 :agriculture :evaluative :stative)
    ("abundant" adjective 7 :agriculture :evaluative :quantity)
    ("withered" adjective 5 :agriculture :evaluative :sensory)

    ;; ═══════════════════════════════════════════════════
    ;; MARITIME / SAILING
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("sail" noun 2 :maritime :inanimate :manufactured :fabric)
    ("anchor" noun 4 :maritime :inanimate :manufactured :tool :metal)
    ("mast" noun 4 :maritime :inanimate :manufactured :wood)
    ("hull" noun 5 :maritime :inanimate :manufactured :wood)
    ("rudder" noun 6 :maritime :inanimate :manufactured :tool :wood)
    ("oar" noun 4 :maritime :inanimate :manufactured :tool :wood)
    ("deck" noun 4 :maritime :inanimate :manufactured :wood)
    ("keel" noun 5 :maritime :inanimate :manufactured :wood)
    ("tiller" noun 7 :maritime :inanimate :manufactured :tool :wood)
    ("port" noun 3 :maritime :inanimate :manufactured :building)
    ("wharf" noun 5 :maritime :inanimate :manufactured :building :wood)
    ("lighthouse" noun 7 :maritime :inanimate :manufactured :building :stone)
    ("sailor" noun 4 :maritime :human :animate)
    ("captain" noun 4 :maritime :human :animate)
    ("navigator" noun 9 :maritime :human :animate)
    ("pirate" noun 4 :maritime :human :animate)
    ("current" noun 5 :maritime :inanimate :natural :liquid)
    ("wave" noun 3 :maritime :inanimate :natural :liquid)
    ("shoal" noun 6 :maritime :inanimate :natural :terrain)
    ("strait" noun 5 :maritime :inanimate :natural :terrain :liquid)
    ("prow" noun 6 :maritime :inanimate :manufactured :wood)
    ("stern" noun 5 :maritime :inanimate :manufactured :wood)
    ("rigging" noun 7 :maritime :inanimate :manufactured :plant)
    ("compass" noun 6 :maritime :inanimate :manufactured :tool :metal)
    ("voyage" noun 5 :maritime :inanimate :abstract)

    ;; -- Verbs --
    ("sail" verb 2 :maritime :motion :physical)
    ("row" verb 3 :maritime :motion :physical)
    ("navigate" verb 8 :maritime :cognition :physical)
    ("dock" verb 3 :maritime :motion :physical)
    ("capsize" verb 7 :maritime :destruction :physical)
    ("anchor" verb 5 :maritime :physical :stative)
    ("launch" verb 4 :maritime :motion :physical)
    ("tack" verb 5 :maritime :motion :physical)
    ("founder" verb 7 :maritime :destruction :physical)
    ("beach" verb 4 :maritime :motion :physical)

    ;; -- Adjectives --
    ("seaworthy" adjective 8 :maritime :evaluative :dimensional)
    ("adrift" adjective 6 :maritime :evaluative :stative)
    ("becalmed" adjective 7 :maritime :evaluative :stative)
    ("landlocked" adjective 8 :maritime :evaluative :dimensional)
    ("windswept" adjective 7 :maritime :sensory :evaluative)

    ;; ═══════════════════════════════════════════════════
    ;; ARCHITECTURE
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("door" noun 1 :architecture :inanimate :manufactured :building :wood)
    ("wall" noun 2 :architecture :inanimate :manufactured :building :stone)
    ("home" noun 1 :architecture :inanimate :manufactured :building)
    ("tower" noun 3 :architecture :inanimate :manufactured :building :stone)
    ("bridge" noun 2 :architecture :inanimate :manufactured :building :stone)
    ("gate" noun 2 :architecture :inanimate :manufactured :building :metal)
    ("roof" noun 3 :architecture :inanimate :manufactured :building :wood)
    ("floor" noun 3 :architecture :inanimate :manufactured :building :stone)
    ("pillar" noun 5 :architecture :inanimate :manufactured :building :stone)
    ("arch" noun 4 :architecture :inanimate :manufactured :building :stone)
    ("stair" noun 3 :architecture :inanimate :manufactured :building :stone)
    ("hearth" noun 3 :architecture :inanimate :manufactured :building :stone)
    ("window" noun 4 :architecture :inanimate :manufactured :building)
    ("beam" noun 4 :architecture :inanimate :manufactured :building :wood)
    ("cellar" noun 6 :architecture :inanimate :manufactured :building :stone)
    ("hall" noun 3 :architecture :inanimate :manufactured :building)
    ("chamber" noun 5 :architecture :inanimate :manufactured :building)
    ("threshold" noun 6 :architecture :inanimate :manufactured :building)
    ("chimney" noun 6 :architecture :inanimate :manufactured :building :stone)
    ("ruin" noun 4 :architecture :inanimate :manufactured :building :stone)

    ;; -- Verbs --
    ("open" verb 1 :architecture :physical :motion)
    ("stand" verb 1 :architecture :physical :stative)
    ("build" verb 1 :architecture :creation :physical)
    ("erect" verb 5 :architecture :creation :physical)
    ("raze" verb 5 :architecture :destruction :physical)
    ("crumble" verb 6 :architecture :destruction :physical)
    ("bar" verb 4 :architecture :physical :stative)
    ("bolt" verb 4 :architecture :physical :stative)

    ;; -- Adjectives --
    ("empty" adjective 2 :architecture :evaluative :dimensional)
    ("full" adjective 1 :architecture :evaluative :dimensional)
    ("ruined" adjective 5 :architecture :evaluative :stative)
    ("hollow" adjective 5 :architecture :dimensional :sensory)
    ("narrow" adjective 4 :architecture :dimensional :sensory)

    ;; ═══════════════════════════════════════════════════
    ;; FOOD / COOKING
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("food" noun 1 :food :inanimate :natural :food-item)
    ("bread" noun 2 :food :inanimate :manufactured :food-item :plant)
    ("meat" noun 2 :food :inanimate :natural :food-item)
    ("salt" noun 2 :food :inanimate :natural :food-item :mineral)
    ("honey" noun 4 :food :inanimate :natural :food-item :liquid)
    ("milk" noun 2 :food :inanimate :natural :food-item :liquid)
    ("wine" noun 2 :food :inanimate :manufactured :food-item :liquid)
    ("ale" noun 3 :food :inanimate :manufactured :food-item :liquid)
    ("broth" noun 4 :food :inanimate :manufactured :food-item :liquid)
    ("cheese" noun 3 :food :inanimate :manufactured :food-item)
    ("oil" noun 3 :food :inanimate :natural :food-item :liquid)
    ("spice" noun 4 :food :inanimate :natural :food-item :plant)
    ("fruit" noun 3 :food :inanimate :natural :food-item :plant)
    ("nut" noun 4 :food :inanimate :natural :food-item :plant)
    ("herb" noun 4 :food :inanimate :natural :food-item :plant)
    ("feast" noun 3 :food :inanimate :abstract)
    ("famine" noun 5 :food :inanimate :abstract)
    ("kettle" noun 6 :food :inanimate :manufactured :container :metal)
    ("bowl" noun 4 :food :inanimate :manufactured :container)
    ("oven" noun 5 :food :inanimate :manufactured :building)

    ;; -- Verbs --
    ("cook" verb 3 :food :creation :physical)
    ("roast" verb 4 :food :creation :physical)
    ("boil" verb 4 :food :creation :physical)
    ("bake" verb 4 :food :creation :physical)
    ("brew" verb 4 :food :creation :physical)
    ("feast" verb 4 :food :consumption :social)
    ("taste" verb 3 :food :perception :consumption)
    ("salt" verb 4 :food :creation :physical)

    ;; -- Adjectives --
    ("sweet" adjective 1 :food :sensory :evaluative)
    ("bitter" adjective 2 :food :sensory :evaluative)
    ("sour" adjective 3 :food :sensory :evaluative)
    ("rotten" adjective 5 :food :sensory :evaluative)
    ("fresh" adjective 2 :food :sensory :evaluative)

    ;; ═══════════════════════════════════════════════════
    ;; EMOTIONS / MENTAL
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("fear" noun 1 :emotion :inanimate :abstract)
    ("rage" noun 2 :emotion :inanimate :abstract)
    ("joy" noun 2 :emotion :inanimate :abstract)
    ("sorrow" noun 3 :emotion :inanimate :abstract)
    ("grief" noun 2 :emotion :inanimate :abstract)
    ("shame" noun 2 :emotion :inanimate :abstract)
    ("envy" noun 4 :emotion :inanimate :abstract)
    ("pity" noun 4 :emotion :inanimate :abstract)
    ("despair" noun 5 :emotion :inanimate :abstract)
    ("longing" noun 5 :emotion :inanimate :abstract)
    ("dread" noun 2 :emotion :inanimate :abstract)
    ("wrath" noun 3 :emotion :inanimate :abstract)
    ("bliss" noun 3 :emotion :inanimate :abstract)
    ("anguish" noun 6 :emotion :inanimate :abstract)
    ("doubt" noun 2 :emotion :inanimate :abstract)
    ("courage" noun 3 :emotion :inanimate :abstract)
    ("honor" noun 3 :emotion :inanimate :abstract)
    ("regret" noun 5 :emotion :inanimate :abstract)
    ("jealousy" noun 7 :emotion :inanimate :abstract)
    ("terror" noun 4 :emotion :inanimate :abstract)
    ("awe" noun 3 :emotion :inanimate :abstract)
    ("contempt" noun 5 :emotion :inanimate :abstract)
    ("remorse" noun 6 :emotion :inanimate :abstract)
    ("fury" noun 4 :emotion :inanimate :abstract)
    ("yearning" noun 6 :emotion :inanimate :abstract)

    ;; -- Verbs --
    ("fear" verb 1 :emotion :emotional :perception)
    ("weep" verb 1 :emotion :emotional :physical)
    ("laugh" verb 1 :emotion :emotional :physical)
    ("cry" verb 2 :emotion :emotional :communication)
    ("despair" verb 5 :emotion :emotional :stative)
    ("grieve" verb 3 :emotion :emotional :stative)
    ("rejoice" verb 5 :emotion :emotional :social)
    ("envy" verb 5 :emotion :emotional :perception)
    ("pity" verb 4 :emotion :emotional :social)
    ("dread" verb 3 :emotion :emotional :perception)

    ;; -- Adjectives --
    ("happy" adjective 1 :emotion :evaluative :emotional)
    ("good" adjective 1 :emotion :evaluative :moral)
    ("bad" adjective 1 :emotion :evaluative :moral)
    ("sad" adjective 2 :emotion :evaluative :emotional)
    ("angry" adjective 3 :emotion :evaluative :emotional)
    ("jealous" adjective 5 :emotion :evaluative :emotional)
    ("ashamed" adjective 5 :emotion :evaluative :emotional)
    ("lonely" adjective 4 :emotion :evaluative :emotional)
    ("fearful" adjective 4 :emotion :evaluative :emotional)
    ("cruel" adjective 2 :emotion :evaluative :moral)

    ;; ═══════════════════════════════════════════════════
    ;; MUSIC / ARTS
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("song" noun 2 :music :inanimate :abstract)
    ("drum" noun 3 :music :inanimate :manufactured :instrument :wood)
    ("flute" noun 3 :music :inanimate :manufactured :instrument :wood)
    ("harp" noun 3 :music :inanimate :manufactured :instrument :wood)
    ("horn" noun 3 :music :inanimate :manufactured :instrument :metal)
    ("lyre" noun 4 :music :inanimate :manufactured :instrument :wood)
    ("bell" noun 3 :music :inanimate :manufactured :instrument :metal)
    ("chant" noun 3 :music :inanimate :abstract)
    ("melody" noun 6 :music :inanimate :abstract)
    ("rhythm" noun 5 :music :inanimate :abstract)
    ("verse" noun 3 :music :inanimate :abstract)
    ("dance" noun 2 :music :inanimate :abstract)
    ("bard" noun 3 :music :human :animate)
    ("minstrel" noun 6 :music :human :animate)
    ("pipe" noun 4 :music :inanimate :manufactured :instrument :wood)

    ;; -- Verbs --
    ("sing" verb 1 :music :communication :emotional)
    ("play" verb 1 :music :creation :physical)
    ("chant" verb 3 :music :communication :social)
    ("drum" verb 4 :music :physical :creation)
    ("dance" verb 2 :music :motion :physical)

    ;; -- Adjectives --
    ("loud" adjective 2 :music :sensory :evaluative)
    ("quiet" adjective 3 :music :sensory :evaluative)
    ("shrill" adjective 4 :music :sensory :evaluative)
    ("melodic" adjective 7 :music :sensory :evaluative)
    ("mournful" adjective 5 :music :sensory :emotional)

    ;; ═══════════════════════════════════════════════════
    ;; CLOTHING / TEXTILES
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("robe" noun 3 :clothing :inanimate :manufactured :garment :fabric)
    ("cloak" noun 3 :clothing :inanimate :manufactured :garment :fabric)
    ("boot" noun 3 :clothing :inanimate :manufactured :garment :leather)
    ("belt" noun 3 :clothing :inanimate :manufactured :garment :leather)
    ("hood" noun 3 :clothing :inanimate :manufactured :garment :fabric)
    ("glove" noun 4 :clothing :inanimate :manufactured :garment :leather)
    ("tunic" noun 5 :clothing :inanimate :manufactured :garment :fabric)
    ("sandal" noun 6 :clothing :inanimate :manufactured :garment :leather)
    ("veil" noun 4 :clothing :inanimate :manufactured :garment :fabric)
    ("sash" noun 5 :clothing :inanimate :manufactured :garment :fabric)
    ("thread" noun 3 :clothing :inanimate :manufactured :fabric)
    ("wool" noun 3 :clothing :inanimate :natural :fabric)
    ("linen" noun 5 :clothing :inanimate :manufactured :fabric :plant)
    ("fur" noun 3 :clothing :inanimate :natural :fabric)
    ("hem" noun 5 :clothing :inanimate :manufactured :fabric)

    ;; -- Verbs --
    ("dress" verb 2 :clothing :physical :social)
    ("spin" verb 3 :clothing :creation :physical)
    ("sew" verb 4 :clothing :creation :physical)
    ("dye" verb 4 :clothing :creation :physical)
    ("mend" verb 4 :clothing :creation :physical)

    ;; -- Adjectives --
    ("worn" adjective 3 :clothing :evaluative :sensory)
    ("tattered" adjective 6 :clothing :evaluative :sensory)
    ("silken" adjective 6 :clothing :sensory :evaluative)

    ;; ═══════════════════════════════════════════════════
    ;; MAGIC / SUPERNATURAL
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("spell" noun 2 :magic :inanimate :abstract)
    ("charm" noun 3 :magic :inanimate :abstract)
    ("witch" noun 3 :magic :human :animate)
    ("wizard" noun 4 :magic :human :animate)
    ("sorcerer" noun 6 :magic :human :animate)
    ("potion" noun 5 :magic :inanimate :manufactured :liquid)
    ("wand" noun 3 :magic :inanimate :manufactured :tool :wood)
    ("staff" noun 3 :magic :inanimate :manufactured :tool :wood)
    ("crystal" noun 5 :magic :inanimate :natural :mineral)
    ("enchantment" noun 7 :magic :inanimate :abstract)
    ("phantom" noun 5 :magic :animate :abstract)
    ("wraith" noun 4 :magic :animate :abstract)
    ("familiar" noun 7 :magic :animate :natural)
    ("hex" noun 4 :magic :inanimate :abstract)
    ("sigil" noun 6 :magic :inanimate :abstract)
    ("talisman" noun 8 :magic :inanimate :manufactured :ornament)
    ("amulet" noun 8 :magic :inanimate :manufactured :ornament :metal)
    ("rune" noun 3 :magic :inanimate :abstract)
    ("ward" noun 3 :magic :inanimate :abstract)
    ("coven" noun 6 :magic :human :abstract)

    ;; -- Verbs --
    ("hide" verb 1 :magic :physical :stative)
    ("enchant" verb 5 :magic :creation :physical)
    ("bewitch" verb 6 :magic :creation :physical)
    ("summon" verb 4 :magic :communication :creation)
    ("dispel" verb 6 :magic :destruction :physical)
    ("conjure" verb 5 :magic :creation :physical)
    ("divine" verb 5 :magic :perception :cognition)
    ("haunt" verb 3 :magic :physical :emotional)

    ;; -- Adjectives --
    ("strange" adjective 1 :magic :evaluative :sensory)
    ("arcane" adjective 5 :magic :evaluative :sensory)
    ("enchanted" adjective 7 :magic :evaluative :sensory)
    ("spectral" adjective 6 :magic :evaluative :sensory)
    ("eldritch" adjective 7 :magic :evaluative :sensory)

    ;; ═══════════════════════════════════════════════════
    ;; ABSTRACT / PHILOSOPHICAL
    ;; ═══════════════════════════════════════════════════

    ;; -- Nouns --
    ("life" noun 1 :abstract :inanimate :abstract)
    ("name" noun 1 :abstract :inanimate :abstract)
    ("road" noun 1 :abstract :inanimate :manufactured :terrain)
    ("truth" noun 1 :abstract :inanimate :abstract)
    ("lie" noun 2 :abstract :inanimate :abstract)
    ("dark" noun 1 :abstract :inanimate :abstract)
    ("path" noun 1 :abstract :inanimate :natural :terrain)
    ("word" noun 1 :abstract :inanimate :abstract)
    ("silence" noun 2 :abstract :inanimate :abstract)
    ("story" noun 2 :abstract :inanimate :abstract)
    ("mercy" noun 2 :abstract :inanimate :abstract)
    ("wisdom" noun 3 :abstract :inanimate :abstract)
    ("folly" noun 4 :abstract :inanimate :abstract)
    ("power" noun 2 :abstract :inanimate :abstract)
    ("time" noun 1 :abstract :inanimate :abstract)
    ("dream" noun 2 :abstract :inanimate :abstract)
    ("memory" noun 5 :abstract :inanimate :abstract)
    ("secret" noun 3 :abstract :inanimate :abstract)
    ("riddle" noun 4 :abstract :inanimate :abstract)
    ("burden" noun 4 :abstract :inanimate :abstract)

    ;; -- Verbs --
    ("know" verb 1 :abstract :cognition :stative)
    ("think" verb 1 :abstract :cognition :stative)
    ("speak" verb 1 :abstract :communication :social)
    ("hear" verb 1 :abstract :perception :stative)
    ("see" verb 1 :abstract :perception :stative)
    ("begin" verb 2 :abstract :stative :physical)
    ("end" verb 1 :abstract :stative :physical)
    ("be" verb 1 :abstract :stative :stative)
    ("stop" verb 1 :abstract :stative :physical)
    ("wait" verb 1 :abstract :stative :physical)
    ("want" verb 1 :abstract :emotional :cognition)
    ("call" verb 1 :abstract :communication :social)
    ("shout" verb 2 :abstract :communication :physical)
    ("write" verb 1 :abstract :communication :creation)
    ("find" verb 1 :abstract :perception :physical)
    ("lose" verb 1 :abstract :transfer :physical)

    ;; -- Adjectives --
    ("big" adjective 1 :abstract :dimensional :evaluative)
    ("small" adjective 1 :abstract :dimensional :evaluative)
    ("new" adjective 1 :abstract :age :evaluative)
    ("long" adjective 1 :abstract :dimensional :evaluative)
    ("short" adjective 2 :abstract :dimensional :evaluative)
    ("fast" adjective 1 :abstract :speed :evaluative)
    ("slow" adjective 2 :abstract :speed :evaluative)
    ("wise" adjective 1 :abstract :evaluative :moral)
    ("red" adjective 1 :abstract :sensory :color)
    ("black" adjective 1 :abstract :sensory :color)
    ("white" adjective 1 :abstract :sensory :color)
    ("silent" adjective 2 :abstract :sensory :evaluative)

    ;; ═══════════════════════════════════════════════════
    ;; ADVERBS
    ;; ═══════════════════════════════════════════════════

    ("not" adverb 1) ("always" adverb 1) ("never" adverb 1) ("alone" adverb 1)
    ("again" adverb 2) ("once" adverb 1) ("still" adverb 1)
    ("soon" adverb 2) ("here" adverb 1) ("there" adverb 1)
    ("now" adverb 1) ("then" adverb 1) ("hence" adverb 2)
    ("thus" adverb 2) ("perhaps" adverb 3)))

;;; Compound word specs: (modifier head compound-gloss)
(defparameter *compound-specs*
  '(("sword" "song" "sword-song")
    ("blood" "king" "blood-king")
    ("cloth" "merchant" "cloth-merchant")
    ("dock" "city" "dock-city")
    ("chain" "man" "chain-man")
    ("trade" "house" "trade-house")))

;;; Compound derivations: words that CAN be expressed as compounds of simpler words
;;; Format: (target-gloss (modifier head) (modifier head) ...)
;;; Each language randomly decides whether to derive each word or keep its own root.
;;; When derived, the word's phonological form becomes a compound of the two base words,
;;; but its gloss (semantic identity) stays the same.
(defparameter *compound-derivations*
  '(;; ── Weapons ──────────────────────────────────────────────
    ;; A language might say "war-blade" or "long-knife" for sword
    ("sword"      ("war" "blade") ("long" "blade") ("war" "knife"))
    ("dagger"     ("short" "blade") ("small" "knife"))
    ("lance"      ("long" "spear"))
    ("mace"       ("war" "hammer") ("iron" "hammer"))
    ("pike"       ("long" "spear") ("iron" "spear"))
    ("bolt"       ("iron" "arrow") ("short" "arrow"))
    ("helm"       ("war" "hood") ("iron" "hood"))
    ("armor"      ("war" "iron") ("iron" "skin"))
    ("shield"     ("war" "wall"))
    ("catapult"   ("stone" "bow") ("war" "wheel"))
    ("gauntlet"   ("iron" "glove") ("war" "glove"))
    ("sheath"     ("blade" "skin") ("sword" "skin"))
    ("quiver"     ("arrow" "bag"))               ; note: needs "bag" — skip if missing
    ("axe"        ("iron" "blade") ("war" "blade"))
    ("banner"     ("war" "cloth"))
    ("sickle"     ("grain" "blade") ("short" "blade"))

    ;; ── Combat roles ────────────────────────────────────────
    ("warrior"    ("war" "man"))
    ("warlord"    ("war" "lord"))
    ("archer"     ("bow" "man"))
    ("scout"      ("war" "eye"))
    ("knight"     ("sword" "lord") ("war" "noble"))
    ("soldier"    ("war" "man") ("shield" "man"))
    ("champion"   ("war" "lord") ("big" "warrior"))
    ("marshal"    ("war" "judge") ("army" "lord"))
    ("guard"      ("gate" "man") ("shield" "man"))

    ;; ── Architecture / Structures ───────────────────────────
    ("fortress"   ("war" "tower") ("stone" "tower"))
    ("lighthouse" ("fire" "tower"))
    ("palace"     ("king" "house") ("gold" "house"))
    ("prison"     ("chain" "house") ("slave" "house"))
    ("chimney"    ("fire" "pillar"))
    ("cellar"     ("earth" "chamber"))
    ("threshold"  ("door" "stone"))
    ("hall"       ("big" "chamber") ("long" "house"))
    ("window"     ("wall" "eye") ("light" "door"))
    ("stair"      ("stone" "path") ("high" "path"))
    ("hearth"     ("fire" "stone") ("home" "fire"))
    ("ruin"       ("old" "stone") ("dead" "house"))
    ("arch"       ("stone" "door") ("stone" "bridge"))
    ("roof"       ("house" "sky") ("house" "shield"))

    ;; ── Maritime ────────────────────────────────────────────
    ("wharf"      ("ship" "bridge") ("dock" "bridge"))
    ("rudder"     ("ship" "hand") ("sail" "arm"))
    ("compass"    ("star" "needle") ("sea" "needle"))
    ("captain"    ("ship" "lord") ("sail" "lord"))
    ("pirate"     ("sea" "thief"))                ; note: needs "thief" — skip if missing
    ("navigator"  ("star" "man") ("sea" "man"))
    ("voyage"     ("sea" "road") ("long" "sail"))
    ("sailor"     ("sea" "man") ("sail" "man"))
    ("anchor"     ("ship" "stone") ("sea" "iron"))
    ("mast"       ("sail" "pillar") ("ship" "tree"))
    ("hull"       ("ship" "skin") ("ship" "bone"))
    ("oar"        ("sea" "arm") ("ship" "arm"))
    ("keel"       ("ship" "bone") ("ship" "spine"))
    ("harbor"     ("ship" "home") ("sea" "gate"))
    ("port"       ("ship" "door") ("sea" "door"))
    ("prow"       ("ship" "head"))
    ("stern"      ("ship" "back"))               ; note: needs "back" — skip if missing
    ("deck"       ("ship" "floor"))
    ("strait"     ("narrow" "sea"))
    ("shoal"      ("shallow" "sea"))             ; note: needs "shallow" — skip if missing
    ("wave"       ("sea" "hill") ("water" "hill"))
    ("current"    ("sea" "river") ("deep" "flow"))

    ;; ── Religion ────────────────────────────────────────────
    ("temple"     ("god" "house"))
    ("altar"      ("god" "stone"))
    ("shrine"     ("god" "door") ("god" "hearth"))
    ("tomb"       ("death" "house") ("death" "stone"))
    ("pyre"       ("death" "fire"))
    ("hymn"       ("god" "song"))
    ("oracle"     ("god" "mouth"))               ; note: needs "mouth" — skip if missing
    ("martyr"     ("death" "saint"))
    ("pilgrim"    ("god" "road") ("holy" "road"))
    ("priest"     ("god" "man") ("holy" "man"))
    ("prophet"    ("god" "eye") ("god" "voice"))  ; note: needs "voice" — skip if missing
    ("idol"       ("god" "stone") ("god" "image")) ; note: needs "image" — skip if missing
    ("heaven"     ("god" "sky") ("holy" "sky"))
    ("hell"       ("death" "fire") ("dark" "earth"))
    ("sin"        ("dark" "deed"))               ; note: needs "deed" — skip if missing
    ("blessing"   ("god" "word") ("holy" "word"))
    ("prayer"     ("god" "word") ("god" "call"))
    ("sacrifice"  ("god" "death") ("holy" "blood"))
    ("relic"      ("holy" "bone") ("god" "bone"))
    ("demon"      ("dark" "spirit") ("hell" "spirit"))
    ("angel"      ("god" "spirit") ("holy" "spirit"))
    ("miracle"    ("god" "power") ("holy" "power"))

    ;; ── Commerce ────────────────────────────────────────────
    ("warehouse"  ("goods" "house") ("trade" "house"))
    ("market"     ("trade" "square"))
    ("purse"      ("coin" "bag"))                ; note: needs "bag" — skip if missing
    ("vault"      ("gold" "house") ("gold" "stone"))
    ("ledger"     ("trade" "letter"))
    ("merchant"   ("trade" "man") ("goods" "man"))
    ("coin"       ("gold" "stone") ("small" "gold"))
    ("debt"       ("owe" "gold") ("dark" "trade"))
    ("profit"     ("trade" "gold") ("good" "trade"))
    ("tax"        ("king" "gold") ("lord" "gold"))
    ("harbor"     ("ship" "home") ("trade" "door"))
    ("cargo"      ("ship" "goods"))
    ("silver"     ("white" "gold") ("moon" "gold"))
    ("copper"     ("red" "gold") ("cheap" "gold"))
    ("gem"        ("bright" "stone") ("precious" "stone"))
    ("pearl"      ("sea" "gem") ("sea" "stone"))
    ("jade"       ("green" "stone") ("green" "gem"))
    ("amber"      ("gold" "stone") ("sun" "stone"))
    ("ivory"      ("white" "bone") ("tooth" "bone"))
    ("silk"       ("fine" "cloth"))              ; note: needs "fine" — skip if missing

    ;; ── Agriculture ─────────────────────────────────────────
    ("vineyard"   ("wine" "field"))
    ("orchard"    ("fruit" "field"))
    ("barn"       ("grain" "house"))
    ("mill"       ("grain" "wheel"))
    ("shepherd"   ("sheep" "man") ("flock" "man"))
    ("farmer"     ("field" "man") ("plow" "man"))
    ("pasture"    ("herd" "field") ("grass" "field")) ; note: needs "grass" — skip if missing
    ("harvest"    ("grain" "cut") ("field" "reap"))
    ("well"       ("earth" "water") ("deep" "water"))
    ("furrow"     ("plow" "path") ("earth" "path"))
    ("hay"        ("dry" "grass"))               ; note: needs "dry"/"grass" — skip if missing
    ("yoke"       ("ox" "chain") ("ox" "rope"))
    ("fence"      ("field" "wall"))
    ("soil"       ("dark" "earth") ("rich" "earth"))

    ;; ── Craft / Trades ──────────────────────────────────────
    ("forge"      ("fire" "house") ("iron" "house"))
    ("kiln"       ("fire" "stone") ("fire" "oven"))
    ("anvil"      ("iron" "stone"))
    ("smith"      ("iron" "man") ("hammer" "man"))
    ("carpenter"  ("wood" "man"))                ; note: needs "wood" — skip if missing
    ("mason"      ("stone" "man"))
    ("weaver"     ("cloth" "man") ("loom" "man"))
    ("potter"     ("clay" "man"))
    ("tanner"     ("fur" "man") ("skin" "man"))
    ("chisel"     ("stone" "knife") ("small" "hammer"))
    ("saw"        ("iron" "tooth") ("tooth" "blade"))
    ("rope"       ("long" "thread") ("strong" "thread"))
    ("brick"      ("fire" "clay") ("hard" "clay"))
    ("ingot"      ("iron" "stone") ("gold" "stone"))
    ("ore"        ("earth" "iron") ("stone" "iron"))
    ("plank"      ("flat" "tree"))               ; note: needs "flat" — skip if missing
    ("needle"     ("small" "blade") ("iron" "thorn"))
    ("awl"        ("small" "chisel") ("leather" "needle")) ; note: needs "leather" — skip if missing
    ("dye"        ("color" "water"))             ; note: needs "color" — skip if missing
    ("glue"       ("tree" "water") ("sticky" "water")) ; note: needs "sticky" — skip if missing

    ;; ── Magic ───────────────────────────────────────────────
    ("wizard"     ("spell" "man"))
    ("witch"      ("spell" "woman"))
    ("sorcerer"   ("spell" "lord"))
    ("wand"       ("small" "staff") ("spell" "staff"))
    ("potion"     ("spell" "water"))
    ("phantom"    ("death" "spirit"))
    ("wraith"     ("death" "spirit") ("dark" "spirit"))
    ("crystal"    ("bright" "stone") ("spell" "stone"))
    ("hex"        ("dark" "spell") ("curse" "word"))
    ("sigil"      ("spell" "letter") ("spell" "name"))
    ("rune"       ("old" "letter") ("spell" "letter"))
    ("ward"       ("spell" "wall") ("spell" "shield"))
    ("enchantment" ("spell" "song") ("spell" "charm"))
    ("talisman"   ("spell" "stone") ("ward" "stone"))
    ("amulet"     ("ward" "gem") ("spell" "gem"))
    ("familiar"   ("spell" "beast"))             ; note: needs "beast" — skip if missing
    ("coven"      ("witch" "clan") ("spell" "clan"))

    ;; ── Clothing ────────────────────────────────────────────
    ("sandal"     ("foot" "belt"))
    ("glove"      ("hand" "skin"))
    ("hood"       ("head" "cloth"))
    ("veil"       ("eye" "cloth"))
    ("cloak"      ("dark" "cloth") ("long" "cloth"))
    ("robe"       ("long" "cloth") ("priest" "cloth"))
    ("boot"       ("foot" "skin") ("hard" "sandal"))
    ("belt"       ("waist" "rope"))              ; note: needs "waist" — skip if missing
    ("tunic"      ("body" "cloth"))              ; note: needs "body" — skip if missing
    ("sash"       ("silk" "belt") ("cloth" "belt"))
    ("crown"      ("king" "gold") ("gold" "ring"))
    ("throne"     ("king" "seat"))               ; note: needs "seat" — skip if missing

    ;; ── Body ────────────────────────────────────────────────
    ("skull"      ("head" "bone"))
    ("fist"       ("hand" "stone") ("closed" "hand")) ; note: needs "closed" — skip if missing
    ("spine"      ("back" "bone"))               ; note: needs "back" — skip if missing
    ("rib"        ("chest" "bone"))
    ("jaw"        ("mouth" "bone"))              ; note: needs "mouth" — skip if missing
    ("knee"       ("leg" "joint"))               ; note: needs "joint" — skip if missing
    ("elbow"      ("arm" "knee"))
    ("wrist"      ("hand" "arm"))
    ("shoulder"   ("arm" "head") ("high" "arm"))
    ("throat"     ("neck" "mouth"))              ; note: needs "mouth" — skip if missing
    ("brain"      ("head" "heart") ("thought" "heart")) ; note: needs "thought" — skip if missing
    ("vein"       ("blood" "road") ("blood" "river"))
    ("flesh"      ("body" "skin"))               ; note: needs "body" — skip if missing
    ("womb"       ("child" "house") ("birth" "house")) ; note: needs "birth" — skip if missing
    ("palm"       ("hand" "heart") ("open" "hand"))
    ("lip"        ("mouth" "skin"))              ; note: needs "mouth" — skip if missing
    ("brow"       ("eye" "hill") ("head" "hill"))
    ("cheek"      ("eye" "skin") ("face" "side")) ; note: needs "face"/"side" — skip if missing
    ("marrow"     ("bone" "blood") ("bone" "heart"))
    ("sinew"      ("bone" "rope") ("flesh" "rope"))
    ("lung"       ("breath" "house"))            ; note: needs "breath" — skip if missing
    ("scalp"      ("head" "skin"))
    ("nail"       ("finger" "bone"))             ; note: needs "finger" — skip if missing
    ("tongue"     ("mouth" "hand"))              ; note: needs "mouth" — skip if missing

    ;; ── Kinship ─────────────────────────────────────────────
    ("husband"    ("man" "oath") ("house" "man"))
    ("wife"       ("woman" "oath") ("house" "woman"))
    ("widow"      ("death" "wife") ("alone" "woman"))
    ("orphan"     ("alone" "child") ("death" "child"))
    ("elder"      ("old" "man") ("wise" "man"))
    ("infant"     ("small" "child") ("new" "child"))
    ("ancestor"   ("old" "father") ("first" "father")) ; note: needs "first" — skip if missing
    ("heir"       ("son" "lord") ("child" "king"))
    ("firstborn"  ("first" "son") ("first" "child")) ; note: needs "first" — skip if missing
    ("bride"      ("new" "wife") ("young" "woman"))
    ("groom"      ("new" "husband") ("young" "man"))
    ("twin"       ("two" "child"))               ; note: needs "two" — skip if missing
    ("clan"       ("blood" "house") ("kin" "house"))
    ("tribe"      ("big" "clan") ("blood" "people"))
    ("maiden"     ("young" "woman") ("unwed" "woman")) ; note: needs "unwed" — skip if missing

    ;; ── Animals ─────────────────────────────────────────────
    ("eagle"      ("big" "hawk") ("king" "bird"))
    ("raven"      ("black" "bird") ("dark" "bird"))
    ("owl"        ("night" "bird") ("wise" "bird"))
    ("crow"       ("black" "bird") ("death" "bird"))
    ("falcon"     ("fast" "hawk") ("swift" "bird"))
    ("swallow"    ("small" "bird") ("swift" "bird"))
    ("viper"      ("small" "snake") ("death" "snake"))
    ("whale"      ("big" "fish") ("sea" "beast"))    ; note: needs "beast" — skip if missing
    ("eel"        ("snake" "fish") ("water" "snake"))
    ("spider"     ("web" "beast"))               ; note: needs "web"/"beast" — skip if missing
    ("lion"       ("king" "beast"))              ; note: needs "beast" — skip if missing
    ("wolf"       ("wild" "dog") ("forest" "dog"))
    ("fox"        ("cunning" "dog") ("red" "dog"))
    ("bull"       ("big" "ox") ("strong" "ox"))
    ("ram"        ("horn" "sheep"))
    ("stag"       ("horn" "deer"))               ; note: needs "deer" — skip if missing
    ("boar"       ("wild" "pig") ("fierce" "pig"))
    ("mule"       ("horse" "ox") ("half" "horse")) ; note: needs "half" — skip if missing
    ("hare"       ("fast" "rat") ("field" "rat"))
    ("frog"       ("water" "lizard") ("marsh" "beast")) ; note: needs "beast" — skip if missing
    ("tortoise"   ("stone" "lizard") ("slow" "lizard"))
    ("moth"       ("night" "bird") ("fire" "fly"))   ; "fly" is a verb in vocab — creative!
    ("crab"       ("sea" "spider") ("shell" "fish")) ; note: needs "shell" — skip if missing
    ("beetle"     ("small" "beetle"))            ; skip — no simpler decomposition
    ("worm"       ("earth" "snake") ("small" "snake"))

    ;; ── Nature ──────────────────────────────────────────────
    ("mountain"   ("big" "hill") ("high" "earth"))
    ("forest"     ("big" "tree") ("deep" "tree"))
    ("river"      ("long" "water") ("big" "stream"))
    ("lake"       ("still" "water") ("deep" "water"))
    ("valley"     ("low" "hill"))                ; note: needs "low" — skip if missing
    ("marsh"      ("wet" "field") ("water" "field"))
    ("island"     ("sea" "hill") ("water" "earth"))
    ("cliff"      ("steep" "stone") ("high" "stone"))
    ("cave"       ("dark" "stone") ("earth" "door"))
    ("spring"     ("earth" "water") ("birth" "water")) ; note: needs "birth" — skip if missing
    ("storm"      ("big" "wind") ("war" "sky"))
    ("thunder"    ("sky" "drum") ("storm" "voice")) ; note: needs "voice" — skip if missing
    ("lightning"  ("sky" "fire") ("storm" "light"))
    ("frost"      ("cold" "water") ("ice" "dust"))
    ("fog"        ("water" "smoke") ("cold" "smoke"))
    ("hail"       ("ice" "rain") ("sky" "stone"))
    ("dew"        ("dawn" "water") ("night" "rain"))
    ("rainbow"    ("rain" "light") ("storm" "light"))
    ("mist"       ("thin" "fog") ("light" "fog"))
    ("breeze"     ("small" "wind") ("calm" "wind"))
    ("ember"      ("small" "fire") ("dying" "fire")) ; note: needs "dying" — skip if missing
    ("flame"      ("bright" "fire") ("hot" "light"))
    ("spark"      ("small" "fire") ("fire" "seed"))
    ("stream"     ("small" "river") ("fast" "water"))
    ("pond"       ("small" "lake") ("still" "water"))
    ("gorge"      ("deep" "valley"))
    ("desert"     ("dry" "earth"))               ; note: needs "dry" — skip if missing
    ("jungle"     ("hot" "forest") ("wild" "forest"))
    ("canyon"     ("deep" "gorge") ("big" "gorge"))
    ("boulder"    ("big" "stone"))
    ("pebble"     ("small" "stone"))
    ("crater"     ("fire" "hole"))               ; note: needs "hole" — skip if missing
    ("reef"       ("sea" "stone") ("sea" "wall"))
    ("delta"      ("river" "mouth"))             ; note: needs "mouth" — skip if missing
    ("glade"      ("forest" "meadow") ("tree" "field"))
    ("thicket"    ("thick" "forest"))            ; note: needs "thick" — skip if missing
    ("meadow"     ("green" "field") ("flower" "field"))
    ("dawn"       ("sun" "birth") ("day" "birth")) ; note: needs "birth" — skip if missing
    ("dusk"       ("sun" "death") ("day" "death"))
    ("shadow"     ("dark" "light") ("sun" "dark"))
    ("ash"        ("fire" "dust") ("dead" "fire"))
    ("smoke"      ("fire" "cloud") ("fire" "fog"))
    ("sand"       ("small" "stone") ("sea" "dust"))
    ("mud"        ("wet" "earth") ("water" "earth"))
    ("horizon"    ("sky" "earth") ("far" "sky"))  ; note: needs "far" — skip if missing
    ("tide"       ("moon" "water") ("sea" "breath")) ; note: needs "breath" — skip if missing
    ("shore"      ("sea" "earth") ("water" "edge")) ; note: needs "edge" — skip if missing

    ;; ── Emotions ────────────────────────────────────────────
    ("terror"     ("big" "fear"))
    ("fury"       ("fire" "rage"))
    ("wrath"      ("war" "rage"))
    ("anguish"    ("long" "sorrow"))
    ("despair"    ("death" "hope"))
    ("bliss"      ("big" "joy"))
    ("dread"      ("dark" "fear") ("deep" "fear"))
    ("grief"      ("deep" "sorrow") ("death" "sorrow"))
    ("shame"      ("dark" "honor") ("bad" "pride"))
    ("envy"       ("dark" "want") ("bitter" "want"))
    ("pity"       ("sad" "love") ("soft" "sorrow"))
    ("longing"    ("long" "want") ("far" "love"))   ; note: needs "far" — skip if missing
    ("courage"    ("heart" "fire") ("brave" "heart"))
    ("honor"      ("bright" "name") ("good" "name"))
    ("regret"     ("old" "sorrow") ("dead" "hope"))
    ("jealousy"   ("dark" "envy") ("fire" "envy"))
    ("awe"        ("holy" "fear") ("god" "fear"))
    ("contempt"   ("small" "hate") ("cold" "hate"))
    ("remorse"    ("deep" "shame") ("heart" "sorrow"))
    ("yearning"   ("deep" "longing") ("fire" "longing"))
    ("doubt"      ("dark" "thought"))            ; note: needs "thought" — skip if missing

    ;; ── Food / Drink ────────────────────────────────────────
    ("bread"      ("grain" "food") ("fire" "grain"))
    ("ale"        ("grain" "water") ("grain" "wine"))
    ("broth"      ("meat" "water") ("hot" "water"))
    ("cheese"     ("old" "milk") ("hard" "milk"))
    ("honey"      ("sweet" "gold") ("bee" "gold"))  ; note: needs "bee" — skip if missing
    ("feast"      ("big" "food") ("king" "food"))
    ("famine"     ("death" "food") ("long" "hunger")) ; note: needs "hunger" — skip if missing
    ("kettle"     ("fire" "bowl") ("iron" "bowl"))
    ("oven"       ("fire" "stone") ("fire" "house"))
    ("oil"        ("fat" "water") ("fruit" "water"))
    ("spice"      ("hot" "herb") ("fire" "herb"))    ; note: needs appropriate adj
    ("vinegar"    ("sour" "wine") ("old" "wine"))    ; note: needs "vinegar" in vocab — skip

    ;; ── Music / Arts ────────────────────────────────────────
    ("drum"       ("war" "song") ("skin" "bowl"))
    ("flute"      ("wind" "song") ("hollow" "bone"))
    ("harp"       ("string" "song"))             ; note: needs "string" — skip if missing
    ("horn"       ("war" "call") ("bull" "bone"))
    ("bell"       ("iron" "song") ("iron" "call"))
    ("lyre"       ("small" "harp"))
    ("melody"     ("sweet" "song") ("long" "song"))
    ("chant"      ("holy" "song") ("god" "song"))
    ("bard"       ("song" "man") ("word" "man"))
    ("minstrel"   ("song" "man") ("road" "bard"))
    ("pipe"       ("small" "flute") ("hollow" "reed")) ; note: needs "reed" — skip if missing
    ("rhythm"     ("drum" "song") ("heart" "song"))
    ("verse"      ("word" "song") ("short" "song"))
    ("dance"      ("body" "song"))               ; note: needs "body" — skip if missing

    ;; ── Abstract / Philosophical ────────────────────────────
    ("wisdom"     ("old" "truth") ("deep" "truth"))
    ("folly"      ("dark" "wisdom") ("blind" "path"))
    ("power"      ("strong" "will"))             ; note: needs "will" — skip if missing
    ("secret"     ("dark" "word") ("hidden" "truth")) ; note: needs "hidden" — skip if missing
    ("riddle"     ("dark" "word") ("strange" "word"))
    ("burden"     ("heavy" "stone") ("long" "chain"))
    ("memory"     ("old" "dream") ("heart" "word"))
    ("silence"    ("dead" "word") ("empty" "song"))
    ("mercy"      ("soft" "law") ("heart" "law"))
    ("dream"      ("night" "eye") ("sleep" "vision"))

    ;; ── Society / Government ────────────────────────────────
    ("empire"     ("big" "king") ("king" "earth"))
    ("council"    ("lord" "hall") ("wise" "hall"))
    ("slave"      ("chain" "man"))
    ("traitor"    ("lie" "man") ("dark" "friend"))
    ("rebel"      ("war" "slave") ("free" "slave"))
    ("herald"     ("king" "voice"))              ; note: needs "voice" — skip if missing
    ("exile"      ("alone" "road") ("far" "man")) ; note: needs "far" — skip if missing
    ("treaty"     ("peace" "word"))              ; note: needs "peace" — skip if missing
    ("decree"     ("king" "word") ("law" "word"))
    ("tribute"    ("war" "gold") ("king" "gold"))
    ("oath"       ("blood" "word") ("god" "word"))
    ("seal"       ("king" "mark"))               ; note: needs "mark" — skip if missing
    ("guild"      ("trade" "clan") ("craft" "clan")) ; note: needs "craft" as noun — may conflict
    ("charter"    ("law" "letter") ("king" "letter"))
    ("blood-price" ("death" "gold") ("blood" "gold"))
    ("conquest"   ("war" "victory"))             ; note: needs "victory" — skip if missing
    ("truce"      ("war" "oath") ("peace" "oath"))   ; note: needs "peace" — skip if missing
    ("siege"      ("war" "wall") ("long" "war"))
    ("battle"     ("big" "fight") ("war" "fight"))
    ("ambush"     ("dark" "war") ("hidden" "war"))   ; note: needs "hidden" — skip if missing
    ("raid"       ("fast" "war") ("night" "war"))
    ("wound"      ("blade" "blood") ("war" "blood"))
    ("scar"       ("old" "wound") ("dead" "wound"))))
