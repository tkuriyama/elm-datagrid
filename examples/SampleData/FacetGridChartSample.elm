module SampleData.FacetGridChartSample exposing ( dataByVenue, dataByGroup )

{-| Sample data for use in Grid Chart examples.
Mkt share: https://www.cboe.com/us/equities/market_share/
Mkt prices: https://iexcloud.io
ETF composition: https://ishares.com


-}

import DataGrid.ChartConfig as Cfg 

--------------------------------------------------------------------------------

dataByVenue : List (Cfg.GridSeries Cfg.GridPair)
dataByVenue =
  [   ( "American"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.5186456567050259
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.5382150216612
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.5547570210630001
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.30034759539023087
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.3359493347392
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.30753645428299997
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.9765393847645671
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.9469235383149996
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.20747357396
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.19740516896620033
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.2221709417766
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.210040036712
            )
          ]
        )
      ]
    )
  ,   ( "Arca"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 8.276338307331057
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 8.6091423056425
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 8.1221756069
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 5.959727027357183
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 6.260986663453501
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 6.47156429522
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 16.49737832535424
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 17.114719520995
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 14.7498753706
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 7.40950267885966
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 7.7203306317720015
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 7.36561069531
            )
          ]
        )
      ]
    )
  ,   ( "BX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.6732551467605892
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.6240603050828499
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.63847540117
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.666361576966284
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.60230356596795
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.659601410312
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.891082428640435
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.8518424766152
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.738924996132
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.601253898364743
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.5602053494827
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.583203844346
            )
          ]
        )
      ]
    )
  ,   ( "BYX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.5424906172292367
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.46548801671
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.3570819324499999
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.5360222852761933
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.4345490558485001
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.4022023238399999
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.501389619146887
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.4155488039330004
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.96985896403
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.1972063405524402
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.1299202269512998
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.0994187548699998
            )
          ]
        )
      ]
    )
  ,   ( "BZX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 4.488045694649944
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 4.416151016838
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 3.75449586804
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 4.258536673222169
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 4.150315708282499
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 3.82733454697
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 6.8646725543936915
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 6.992236083330002
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 4.98123667808
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 3.8259499516938402
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 3.6673255416204995
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 3.25527394016
            )
          ]
        )
      ]
    )
  ,   ( "Chicago"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.16845828531274046
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.14076460959087503
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.0707244998539
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.1565482202532397
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.151612423924595
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.110255573178
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.3571167307057951
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.24450378504848996
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.0446628019676
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.1100407237530722
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.0890229791893
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.0423329127192
            )
          ]
        )
      ]
    )
  ,   ( "EDGA"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.3725659800300587
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.5072375619065002
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.34062420066
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.4250238390563215
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.5162170577580003
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.4333090690099999
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.8313830959786948
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.1498024749659996
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.64710585514
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.1552300161406206
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.246416868572
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.1451879383299999
            )
          ]
        )
      ]
    )
  ,   ( "EDGX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 7.3540023019810885
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 7.121206777658
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 7.24415664556
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 6.989736185224521
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 6.880894644754999
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 7.00948033506
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 7.251910412284686
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 7.190472960274
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 7.09592432753
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 7.714895234300803
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 7.324616804303499
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 7.51891139996
            )
          ]
        )
      ]
    )
  ,   ( "IEX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.3675280037151047
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.4248086590575
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.2136437294499998
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 3.0523133489917553
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 3.0272354964995003
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.86668137788
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.2017925568866443
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.270406061008
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.19177225211
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.1694647398710107
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.2663796161115
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.95194010636
            )
          ]
        )
      ]
    )
  ,   ( "LTSE"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.00023603077370576615
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.00019549859456759002
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.000258263334581
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.00032953626367149117
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.00022643398180279494
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.72472372863e-05
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.00010185487428195071
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 3.9982215747049e-05
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.32130726383e-05
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.0002079684653002807
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.00022947453003683999
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.000572979330213
            )
          ]
        )
      ]
    )
  ,   ( "MEMX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.0917055103075244
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.3872751826605
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.79868419543
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.8463244626828186
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.1285886901
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.53512362855
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.244098084603775
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.5845276255805
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.86212709726
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.2634812927079975
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.5751884489625003
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 3.02665032581
            )
          ]
        )
      ]
    )
  ,   ( "NYSE"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 10.108495152702115
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 10.3384660626085
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 8.88994641564
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 22.169544368701295
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 21.68428805997
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 19.3705993573
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.6347911396227177
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.6590331233330007
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.9761047573
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.5411662455440012
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.5954723031169997
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.3641329580900001
            )
          ]
        )
      ]
    )
  ,   ( "NYSE_TRF"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 7.964237016301645
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 7.699157101567
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 9.63108229994
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 5.6224604214357194
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 5.804169511129
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 6.7756617856600005
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 7.786575474716496
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 7.387553489457001
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 9.88890728486
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 10.192467430760175
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 9.7639309509125
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 12.251452844500001
            )
          ]
        )
      ]
    )
  ,   ( "Nasdaq"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 15.773625319575482
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 15.66021861691
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 14.4076901625
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 11.01994184947818
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 11.3073796042695
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 10.9311855564
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 10.168353913969058
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 10.423937277694499
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 8.78698687019
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 22.321316939652906
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 22.19132914823
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 19.678586398500002
            )
          ]
        )
      ]
    )
  ,   ( "Nasdaq_TRF_Carteret"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 34.79129880648413
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 34.74514505966499
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 36.837657885300004
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 32.49641639070354
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 32.408128571295
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 34.0357983797
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 33.614977582142735
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 33.04343236101
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 38.8319844731
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 37.39123996880209
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 37.85118946209
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 38.7983945071
            )
          ]
        )
      ]
    )
  ,   ( "Nasdaq_TRF_Chicago"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.24654461179594364
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.2290060681065
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.238247347846
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.20495950135321875
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.19325731684069997
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.189620450612
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.16891244110018916
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.15391748322635
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.18426061508400002
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.3141455041998484
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.29508915451635
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.30333749558
            )
          ]
        )
      ]
    )
  ,   ( "National"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.271621388322091
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.0992001616187
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.04154034292
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.433426804352272
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.2705604292189998
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.30312153523
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.143259751239736
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.6921943746894996
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.44792787728
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.8041186120433093
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.6981862852128999
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.650739156361
            )
          ]
        )
      ]
    )
  ,   ( "PSX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.7743684862756619
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.73279991131735
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.631380677443
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.7049525666406626
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.65263108752535
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.590180869586
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.3845533186247891
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.3204191113785
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.0050334009700002
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.6164125973415033
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.5866754851909
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.539497258057
            )
          ]
        )
      ]
    )
  ,   ( "Pearl"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.21653768374857849
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.26146206280940004
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.227377504544
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.15702734665201726
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.19070634444124998
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.180725803955
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.4811113309508369
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.5584894669282
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.38981959127
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.1744946879758917
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.2163203274425
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.214716447899
            )
          ]
        )
      ]
    )
  ]

dataByGroup : List (Cfg.GridSeries Cfg.GridPair)
dataByGroup =
  [   ( "CBOE"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 14.75710459389033
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 14.510083373112497
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 13.69635864671
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 14.209318982779207
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 13.981976466644
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 13.672326274880001
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 18.449355681803958
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 18.748060322502997
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 15.694125824779997
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 13.893281542687705
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 13.368279441447301
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 13.01879203332
            )
          ]
        )
      ]
    )
  ,   ( "IEX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.3675280037151047
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.4248086590575
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.2136437294499998
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 3.0523133489917553
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 3.0272354964995003
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.86668137788
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.2017925568866443
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 1.270406061008
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.19177225211
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.1694647398710107
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.2663796161115
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.95194010636
            )
          ]
        )
      ]
    )
  ,   ( "MEMX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.0917055103075244
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.3872751826605
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.79868419543
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 1.8463244626828186
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.1285886901
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.53512362855
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.244098084603775
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.5845276255805
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 2.86212709726
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 2.2634812927079975
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 2.5751884489625003
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 3.02665032581
            )
          ]
        )
      ]
    )
  ,   ( "MIAX"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.21653768374857849
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.26146206280940004
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.227377504544
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.15702734665201726
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.19070634444124998
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.180725803955
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.4811113309508369
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.5584894669282
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.38981959127
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.1744946879758917
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.2163203274425
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.214716447899
            )
          ]
        )
      ]
    )
  ,   ( "NYSE"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 20.343558790373034
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 20.725788161121777
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 18.6791438863769
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 30.019594016054214
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 29.70339691130629
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 27.563077215211003
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 23.609085331687066
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 23.657374342380987
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 20.426044381107598
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 10.062233429166243
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 10.3251831410678
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 9.6328557591922
            )
          ]
        )
      ]
    )
  ,   ( "Nasdaq"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 17.221248952611734
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 17.0170788333102
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 15.677546241113
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 12.391255993085128
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 12.562314257762802
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 12.180967836298
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 12.443989661234282
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 12.5961988656882
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 10.530945267292
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 23.538983435359157
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 23.3382099829036
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 20.801287500903
            )
          ]
        )
      ]
    )
  ,   ( "Other"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.00023603077370576615
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.00019549859456759002
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.000258263334581
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.00032953626367149117
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.00022643398180279494
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.72472372863e-05
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.00010185487428195071
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 3.9982215747049e-05
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 1.32130726383e-05
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 0.0002079684653002807
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 0.00022947453003683999
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 0.000572979330213
            )
          ]
        )
      ]
    )
  ,   ( "TRF"
    ,   [   ( "Market"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 43.00208043458172
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 42.6733082293385
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 46.706987533086
            )
          ]
        )
      ,   ( "Tape A"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 38.323836313492485
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 38.4055553992647
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 41.001080615972
            )
          ]
        )
      ,   ( "Tape B"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 41.57046549795941
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 40.58490333369335
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 48.905152373044
            )
          ]
        )
      ,   ( "Tape C"
        ,   [   ( "2021-03-09 - 2021-06-04"
            , 47.897852903762114
            )
          ,   ( "2021-05-06 - 2021-06-04"
            , 47.910209567518855
            )
          ,   ( "2021-06-04 - 2021-06-04"
            , 51.35318484718
            )
          ]
        )
      ]
    )
  ]

