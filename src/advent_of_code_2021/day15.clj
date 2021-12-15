(ns advent-of-code-2021.day15)
(require '[clojure.data.priority-map :refer [priority-map]])

(defn process-input [input]
  (->> input
       clojure.string/split-lines
       (mapv #(clojure.string/split % #""))
       (mapv (fn [x] (mapv #(. Integer parseInt  %) x)))))


(defn all-done [paths destination]
  (every? #(= destination %)
          (map last paths)))

(defn oob [[a b] area]
  (or (or (< a 0)
          (< b 0))
      (or (>= a (count (first area)))
          (>= b (count area)))))

(defn more-expensive [path costs area [x y :as e]]
  (let [prev (costs (last path))
        this (get-in area [y x])
        existing (get costs e)]
    (if (and existing (<= existing (+ prev this)))
      true
      false)))

(defn next-steps [path a b c d area costs]
  (let [new-paths []
        new-paths (if (or (oob a area)
                          (some #{a} path)
                          (more-expensive path costs area a)) new-paths (conj new-paths (conj path a)))
        new-paths (if (or (oob b area)
                          (some #{b} path)
                          (more-expensive path costs area b)) new-paths (conj new-paths (conj path b)))
        new-paths (if (or (oob c area)
                          (some #{c} path)
                          (more-expensive path costs area c)) new-paths (conj new-paths (conj path c)))
        new-paths (if (or (oob d area)
                          (some #{d} path)
                          (more-expensive path costs area d)) new-paths (conj new-paths (conj path d)))]
    new-paths))

(defn more-paths [paths destination area costs]
  (reduce (fn [[paths costs] path]
            (let [[x y :as end] (last path)
                  a [(inc x) y]
                  b [(dec x) y]
                  c [x (inc y)]
                  d [x (dec y)]]
              (let [new-paths (next-steps path a b c d area costs)]
                [(into paths new-paths)
                 (reduce (fn [costs new-path]
                           (assoc costs (last new-path) (+ (get costs (last path))
                                                           (get-in area [(second (last new-path))
                                                                         (first (last new-path))]))))
                         costs
                         new-paths)])))
          [[] costs]
          paths))

(defn find-path [input]
  (loop [area (process-input input)
         paths [[[0 0]]]
         destination [(dec (count (first area))) (dec (count area))]
         costs {[0 0] 0}
         i 0]
    (if (or (= i 1000) (all-done paths destination))
      (get costs destination)
      (let [[paths costs] (more-paths paths destination area costs)]
        (recur area
               paths
               destination
               costs
               (inc i))))))






(defn plus-mod [x y]
  (inc (mod (dec (+ x y)) 9)))

(defn tile-area [area n]
  (let [long-area (reduce (fn [new-area m]
                            (into new-area (mapv (fn [row]
                                                   (mapv #(plus-mod % m) row))
                                                 area)))
                          []
                          (range 0 n))]
    (mapv (fn [row]
           (reduce (fn [new-row m]
                     (into new-row (mapv #(plus-mod % m) row)))
                   []
                   (range 0 n)))
         long-area)))

(defn get-unvisited-neighbors [[x y] unvisited-nodes]
  (clojure.set/intersection unvisited-nodes #{[(inc x) y]
                                              [(dec x) y]
                                              [x (inc y)]
                                              [x (dec y)]}))


(defn update-costs2 [current-node area costs unvisited-costs unvisited-nodes]
  (let [unvisited-neighbors (get-unvisited-neighbors current-node unvisited-nodes)
        current-node-cost (get costs current-node)]
    (reduce (fn [[costs unvisited-costs] [x y :as neighbour]]
              (let [existing-cost (get costs neighbour)
                    new-cost (+ current-node-cost (get-in area [y x]))]
                (if (< new-cost existing-cost)
                  [(assoc costs neighbour new-cost)
                   (assoc unvisited-costs neighbour new-cost)]
                  [costs unvisited-costs])))
            [costs unvisited-costs]
            unvisited-neighbors)))

(defn next-node [unvisited-costs]
  (first (first unvisited-costs)))



(defn find-path-v2 [input]
  (let [area (tile-area (process-input input) 5)
        destination [(dec (count (first area))) (dec (count area))]]
    (loop [unvisited-nodes (set (for [x (range 0 (count (first area)))
                                      y (range 0 (count area))]
                                  [x y]))
           costs (assoc (zipmap unvisited-nodes (repeat Integer/MAX_VALUE))
                        [0 0]
                        0)
           unvisited-costs (into (priority-map) costs)
           current-node [0 0]
           i 0]
      (if (or (= i 300000) (< (get costs destination)
                              Integer/MAX_VALUE))
        (get costs destination)
        (let [[costs unvisited-costs] (update-costs2 current-node area costs unvisited-costs unvisited-nodes)
              unvisited-nodes (disj unvisited-nodes current-node)
              unvisited-costs (dissoc unvisited-costs current-node)
              current-node (next-node unvisited-costs)]
          (recur unvisited-nodes
                 costs
                 unvisited-costs
                 current-node
                 (inc i)))))))



(def very-small-input
  "12
11")


(def small-input
  "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(def big-input
  "9197799967142949711924912559857266425989892895795349989935678852856491866597249883454355721838994689
9454699428952977916248797687374199797579937166959985575934687799286278395799834599823725191768264239
1476124567678962892998242114658368169969196949893799969968932492875835669928535888516585889557294857
5791918239996678837798182119995286749674958736799996579937942199999478117296491681956799173479469697
7169635399716469741872348399984999149999916841724586589959931149912818998567241788593818767699883858
2926741911914636315187918554497249439756557979631699768168949899866796175449994999986688215771687939
5589949979145888769995885998892975199254611397986145389381187633664799879995799643887357299151772988
8889465966717911191788336792214278123584765871499472917878297279329199386164989887784939824172612679
4449128566587999796639991816798948898951855942998816126957898821255977428897977658516999988855449929
3999895395663995233116579986688778269699389615982998939659193247918999668975597863299499276267179878
9989933899158995519999896763539929676935591584991626616943495964888658981438483529376937899999918183
4829998579654495951971971638691618176277519996856988429168266495831212992486396955413697829886599968
1853513748489779798696199795337967571689486839899518982881879699995999878971985995885285319954968987
3342299981292915486979299839195993258148889649679616595927478298168379999445161915519894982439638946
5741832898766672398489974989541186971799425919242686599739939448731546446299879192192895526548848951
3898679419518979696926476691872981828746218226918199699898828285487879261678997791767663979571336987
4545997398729587476481757668995896459799851954917678912468571968713532133518634849896488769111976969
7176919181998999666976198114775775798299797193972885999975499799991922929283534929891716288936629899
9795297985299599814789928888579191456982553599491674489819963299121598519964298979361694995981918669
5647159999798466469859785261859791688182892567989542289667144152647594189396864175439725912999894891
6854155179889746985993265989674997919485741213772961693991919182852979468522779889946746945979139453
6849999911316858193858699583834199555997997298798955876892868311369313778113995754199257992891592559
1988966163847446296579892411648896918797968187847519149178391233772828798998561969939976192876594877
7445186982744995399469915773579589785637114845746289739271566921956439346833864949599959561392652394
3457499487895199558673758745718298693289788928968598234991939776753988979879884594869769919728168682
6996592996822655364687387657828999681917991869422948671996356522399889425477419816794863249587691838
6963522693998786991158579735619989997779699892985724296997797588989857395266828998498892969725581196
9859966989665161194892178862589775985492499749698891478876986292862589878985699921967994865598972548
1951799998898999888869856412883984488695132895819836992634797721986196995697288654968728619832457546
7746279941985434489778669988695998998439885687679569278711958998865855737897118129117847196853321494
9957647198999793952568873984933619771962796482459488211592975286761921259894198995991177799599947499
5957899335983477859964986862162667699875586266873467199919735154293718617894984899199896937396169696
4896218869754451981289699181969462479929965399769819937374193998929686992335854771796763991919183598
7963369964836263326663688188689148549887888951793951217896899686213646979917379984981861249989999549
5651487588851599919775191184948759985836569954819197496898639599422949846969583849582827172889781259
9235786148996892968113972727222986857799699985848999699389987299798673597919859879142818978191972699
6268697925898964971528988971776886593959277157692478749421918568187987285285135794835198267899129444
9613855792729968968874877878798998856696549799999913939938579165969969665999919899989935397982894528
4997991949899429999159159577397714989959998878621218913971667491519691231899789262795929815768874159
9965848836163359381441957989236819923219977614754398952174759999868197879998492826999779999614619568
9916479998414596768939496337988287974996399692819868258768657938573834799119189969847332997799632969
5698447299748197429895187981999374113864719439577495356699467714279996684998225889418687785989668887
9688999296549647577489651538861651536198886343717392492893999157912496817872939573767915879229444698
7945998891978892984213828992819989424969949768551929992192128218217979514375299388969726393999499369
3584919181979999899979852691745679631417252595865981248933848728931198122729313884939499699196979282
8858721793437795394466977529919919969898771537432681496711998968886579589984829981259798525932187987
9588959797973868917749898922898847727866979662399888886799981719567732464855989797669812177692858535
7988797967989166987697149888751549873389785967924498738998179527778487979748999358518578993477944998
8392255427879688981984196873699686714549989266862782196183895285962499989186484692151849999723391279
8437165279966699964797868697998719161124994112294916994518516866198924776139989889587889979986973818
9999579881886267995145198999799886398912847997191746855988699599911799599898975959729735998989189975
8878929798597768927495551149914988384939936997932243691336699915979516239489822517874899851799315136
7999937717928948478557923788987292447971689615966385935897391982999178899753365199984698878762193326
8739178873349956119674999899899922517642862785199198189839768689799987895289957563983787873796927158
3817196699929999634811599428583198399931978688187175989163313588911897784917986562919151677117114699
5998827143845268748266989727984179597967556224858671494997248677843425189898194987624713779882979969
1291199757967569999681279489899999698569168979799631778127984994974999291683887763915481598891197984
6596739399885895975872877952961612873986668999381529399939575996779494715118543849969191539989723991
6488878279986978392869749226828917999488959898183448591336188881881342378769238973895889159556799994
7789977876961783236719451481787118498693824114361919936786189899181887229316982176859199279192424145
8187849893991985235499899257668519419992986629984899637874749832449368983739929946889995497697325581
1999982976844663788727661849815985519275791155239799816978282652839694868889597292189945989632996979
5996994949412446932824171982727298987629311193781981698399298991594594318144966895296816539941754949
4499757999646423149686819991849992649795216851367986996698575817689371687185996929563284536999154386
2781727997917211978984917918358938837999218781629649869594796149935179779682659626959939761694881741
9699391853588289298623919874976619984895139963999399988216318999974869174298698357691152898189998496
4692148898295579969948321265945989989678928997293841391798864427891761873993391876712881438989147999
9385356844999749799945931891327611796839596933696939993396192727919575919518191178749822279279367589
4178987198748999878181711949999695619267692321571299944319696913971989366952788997529689965967283949
9956943168197988495985856862549149864798959861938533689969366917899972839492619882179391815935536891
9994859897582628653476292984929998647975796929748931992994879889878464879929465595995992499919947572
6499868699811766722988787191388665966891869899747691981279799666949896666275972998579926239169811939
2572961395969294529184998258989427519999852294469971796436995767291982749996658711449539791567874971
9499369277479918778697894247918649998897952638574461819998871478917181981936547491991768414982337918
9139161883819343891979967499192937131885599318821172899173915229917976858689192989939199991289159588
4587788966993732467999991118887435975882488728997989199779497777813778259387411882768829389897869948
6997871988679928999697816865977788699836897997854449113119982463997827969671859996988242232984999889
7669989748189979988148659979989987372886949695793489235995874977587946688195985837946785781783754816
9949977989988894882969918629379321181514166935688979461975779949979998632331698911979279798659979815
5739999517928987777759268697378941991838969832887199538988396198131293696546792877869527666136992871
6911198997889976167798919887987969916181719673692949986396122688692969199128693334898362117657497517
9883388858893695671398924917987816149792972888297996689846999676959672799729896341199962982814179963
1987996499791423676141938518943291126997238886492375594975195733297999897928779997671984971349299794
9797999699795947998399566993717699681855989845386398419161478733246296117437796411784118959196917998
8918619981898886893637776664327129356976966478348969779281122928599816797994927899617318362459879958
9971138998498917194898897279183767784951818668325759559698816239496896576197991168919876998549693617
8864749334257944937895657676795539372789673639872688475887819638876957529359787979549486769488261586
5675561487189788698114476929665946299225219355166913169787589274369169599695999791361786439583994855
9999956599855466924168799196849941988317721987382287618899839584541991357959855181948139679757198596
9298494784765969999565792229687116986883978789756616279977893754489916919955299588991939854273184891
1996653993894556999967399479699996198846899917783471221742335181981132427871897521999889788298436396
9758778754778989199415992499539647886721912925345489999475288846844991977933449238525898929369479361
3799665999998683118595895926116918889199463219817924394877958956698592828621676399199495183592857893
5747926316986915679999389948959192389893246767655852888242899482429279815191681819582769989759282619
8557944289929978419989549449497998796197294159289979966862799744989186832759979889797777669569898889
8887698917197579219988794948269467999589319398686198699795225872219199592696935791919185797715588919
1498696799724916519515862974893197748928697249299992999324589578163647988841492955996651937876156824
8213962617983488919185253728699798941119614245999164229898897586922898965749999199113889146668885849
9547881767999183972817991938941799195699921244999582934834983168189915191587942659337539518189911869
9958877559791958745797981284916899539983986257869698291725831953753946984998659154999916977712958828")