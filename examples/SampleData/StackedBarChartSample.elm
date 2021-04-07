module SampleData.StackedBarChartSample exposing ( dataByTape, dataByGroup )

{-| Sample data for use in Stacked Bar Chart examples.
Source: https://www.cboe.com/us/equities/market_share/

-}

import DataGrid.ChartConfig as Cfg 

--------------------------------------------------------------------------------

dataByTape : List (Cfg.StdSeries String)
dataByTape =
  [ ("Tape A",
      [ ("2020-10-09", 3.94410)
      , ("2020-10-12", 3.43577)
      , ("2020-10-13", 3.61110)
      , ("2020-10-14", 3.85852)
      , ("2020-10-15", 3.72456)
      , ("2020-10-16", 4.68804)
      , ("2020-10-19", 4.09109)
      , ("2020-10-20", 3.91786)
      , ("2020-10-21", 4.10397)
      , ("2020-10-22", 4.17206)
      , ("2020-10-23", 3.65161)
      , ("2020-10-26", 4.00221)
      , ("2020-10-27", 3.96240)
      , ("2020-10-28", 5.13998)
      , ("2020-10-29", 4.91187)
      , ("2020-10-30", 4.84393)
      , ("2020-11-02", 4.31469)
      , ("2020-11-03", 4.24138)
      , ("2020-11-04", 4.79040)
      , ("2020-11-05", 4.85815)
      , ("2020-11-06", 4.84247)
      , ("2020-11-09", 8.57051)
      , ("2020-11-10", 6.03747)
      , ("2020-11-11", 4.63556)
      , ("2020-11-12", 4.90967)
      , ("2020-11-13", 4.71959)
      , ("2020-11-16", 5.29552)
      , ("2020-11-17", 4.81219)
      , ("2020-11-18", 5.28482)
      , ("2020-11-19", 4.36338)
      , ("2020-11-20", 4.23638)
      , ("2020-11-23", 5.05755)
      , ("2020-11-24", 6.28030)
      , ("2020-11-25", 4.91045)
      , ("2020-11-27", 2.77840)
      , ("2020-11-30", 6.30841)
      , ("2020-12-01", 5.41849)
      , ("2020-12-02", 5.04125)
      , ("2020-12-03", 5.06534)
      , ("2020-12-04", 5.09962)
      , ("2020-12-07", 4.80451)
      , ("2020-12-08", 4.58440)
      , ("2020-12-09", 5.23280)
      , ("2020-12-10", 4.65849)
      , ("2020-12-11", 4.37547)
      , ("2020-12-14", 4.62385)
      , ("2020-12-15", 4.38709)
      , ("2020-12-16", 4.06704)
      , ("2020-12-17", 4.19282)
      , ("2020-12-18", 7.09711)
      , ("2020-12-21", 4.74858)
      , ("2020-12-22", 4.05331)
      , ("2020-12-23", 3.77916)
      , ("2020-12-24", 1.88379)
      , ("2020-12-28", 3.53547)
      , ("2020-12-29", 3.39329)
      , ("2020-12-30", 3.15486)
      , ("2020-12-31", 3.17904)
      , ("2021-01-04", 5.01501)
      , ("2021-01-05", 4.59102)
      , ("2021-01-06", 6.06412)
      , ("2021-01-07", 5.09916)
      , ("2021-01-08", 4.77305)
      , ("2021-01-11", 4.46544)
      , ("2021-01-12", 4.99495)
      , ("2021-01-13", 4.60252)
      , ("2021-01-14", 5.19848)
      , ("2021-01-15", 5.36982)
      , ("2021-01-19", 5.01444)
      , ("2021-01-20", 4.56619)
      , ("2021-01-21", 4.50176)
      , ("2021-01-22", 5.11089)
      , ("2021-01-25", 7.00084)
      , ("2021-01-26", 6.13750)
      , ("2021-01-27", 9.97653)
      , ("2021-01-28", 6.99278)
      , ("2021-01-29", 6.64337)
      , ("2021-02-01", 5.43624)
      , ("2021-02-02", 5.51410)
      , ("2021-02-03", 4.86487)
      , ("2021-02-04", 4.87925)
      , ("2021-02-05", 4.87947)
      , ("2021-02-08", 4.64837)
      , ("2021-02-09", 4.56833)
      , ("2021-02-10", 4.83708)
      , ("2021-02-11", 4.59096)
      , ("2021-02-12", 4.13507)
      , ("2021-02-16", 5.05899)
      , ("2021-02-17", 4.73065)
      , ("2021-02-18", 4.79365)
      , ("2021-02-19", 4.84532)
      , ("2021-02-22", 5.91710)
      , ("2021-02-23", 6.29662)
      , ("2021-02-24", 6.01279)
      , ("2021-02-25", 6.54748)
      , ("2021-02-26", 6.52607)
      , ("2021-03-01", 5.11482)
      , ("2021-03-02", 5.53601)
      , ("2021-03-03", 6.17367)
      , ("2021-03-04", 7.19540)
      , ("2021-03-05", 6.85107)
      , ("2021-03-08", 5.87171)
      , ("2021-03-09", 5.51357)
      , ("2021-03-10", 5.84739)
      , ("2021-03-11", 5.31289)
      , ("2021-03-12", 4.47629)
      , ("2021-03-15", 4.90011)
      , ("2021-03-16", 4.61308)
      , ("2021-03-17", 4.56167)
      , ("2021-03-18", 5.11860)
      , ("2021-03-19", 7.75743)
      , ("2021-03-22", 4.31626)
      , ("2021-03-24", 4.78302)
      , ("2021-03-25", 4.94835)
      , ("2021-03-26", 5.48316)
      , ("2021-03-29", 4.62819)
      , ("2021-03-30", 4.12151)
      , ("2021-03-31", 4.57806)
      , ("2021-04-01", 4.16214)
      , ("2021-04-05", 4.00504)
      , ("2021-04-06", 4.08128)
      ]
    )
  , ("Tape B",
      [ ("2020-10-09", 1.57363)
      , ("2020-10-12", 1.17161)
      , ("2020-10-13", 1.40734)
      , ("2020-10-14", 1.31866)
      , ("2020-10-15", 1.25601)
      , ("2020-10-16", 1.11861)
      , ("2020-10-19", 1.25696)
      , ("2020-10-20", 1.34869)
      , ("2020-10-21", 1.37406)
      , ("2020-10-22", 1.25965)
      , ("2020-10-23", 1.07488)
      , ("2020-10-26", 1.63500)
      , ("2020-10-27", 1.21775)
      , ("2020-10-28", 2.07891)
      , ("2020-10-29", 1.74854)
      , ("2020-10-30", 1.86381)
      , ("2020-11-02", 1.61584)
      , ("2020-11-03", 1.65757)
      , ("2020-11-04", 2.14126)
      , ("2020-11-05", 1.91057)
      , ("2020-11-06", 1.51886)
      , ("2020-11-09", 2.91608)
      , ("2020-11-10", 1.93596)
      , ("2020-11-11", 1.35231)
      , ("2020-11-12", 1.62217)
      , ("2020-11-13", 1.56714)
      , ("2020-11-16", 1.65081)
      , ("2020-11-17", 1.67479)
      , ("2020-11-18", 1.55809)
      , ("2020-11-19", 1.39353)
      , ("2020-11-20", 1.26892)
      , ("2020-11-23", 1.80944)
      , ("2020-11-24", 1.87537)
      , ("2020-11-25", 1.56848)
      , ("2020-11-27", 0.85117)
      , ("2020-11-30", 1.82503)
      , ("2020-12-01", 1.97373)
      , ("2020-12-02", 1.64138)
      , ("2020-12-03", 1.49666)
      , ("2020-12-04", 1.43316)
      , ("2020-12-07", 1.43705)
      , ("2020-12-08", 1.31325)
      , ("2020-12-09", 1.77912)
      , ("2020-12-10", 1.40956)
      , ("2020-12-11", 1.39460)
      , ("2020-12-14", 1.43369)
      , ("2020-12-15", 1.46409)
      , ("2020-12-16", 1.39499)
      , ("2020-12-17", 1.40174)
      , ("2020-12-18", 1.74288)
      , ("2020-12-21", 1.90588)
      , ("2020-12-22", 1.47371)
      , ("2020-12-23", 1.66000)
      , ("2020-12-24", 1.06713)
      , ("2020-12-28", 1.51203)
      , ("2020-12-29", 1.49769)
      , ("2020-12-30", 1.27073)
      , ("2020-12-31", 1.46745)
      , ("2021-01-04", 2.81677)
      , ("2021-01-05", 3.14946)
      , ("2021-01-06", 3.17863)
      , ("2021-01-07", 2.11273)
      , ("2021-01-08", 2.57369)
      , ("2021-01-11", 3.01198)
      , ("2021-01-12", 3.23673)
      , ("2021-01-13", 2.39894)
      , ("2021-01-14", 2.64430)
      , ("2021-01-15", 2.49829)
      , ("2021-01-19", 2.86812)
      , ("2021-01-20", 2.52968)
      , ("2021-01-21", 1.84801)
      , ("2021-01-22", 1.99689)
      , ("2021-01-25", 2.43594)
      , ("2021-01-26", 1.77421)
      , ("2021-01-27", 2.88371)
      , ("2021-01-28", 3.19311)
      , ("2021-01-29", 2.93831)
      , ("2021-02-01", 3.67335)
      , ("2021-02-02", 2.12352)
      , ("2021-02-03", 1.80937)
      , ("2021-02-04", 2.18652)
      , ("2021-02-05", 2.22557)
      , ("2021-02-08", 2.56515)
      , ("2021-02-09", 2.70560)
      , ("2021-02-10", 3.28641)
      , ("2021-02-11", 2.59304)
      , ("2021-02-12", 2.01275)
      , ("2021-02-16", 2.51932)
      , ("2021-02-17", 2.61436)
      , ("2021-02-18", 2.11258)
      , ("2021-02-19", 2.08967)
      , ("2021-02-22", 2.30822)
      , ("2021-02-23", 2.90849)
      , ("2021-02-24", 2.22812)
      , ("2021-02-25", 3.17086)
      , ("2021-02-26", 3.36034)
      , ("2021-03-01", 2.22686)
      , ("2021-03-02", 2.18719)
      , ("2021-03-03", 2.53862)
      , ("2021-03-04", 3.51099)
      , ("2021-03-05", 3.14538)
      , ("2021-03-08", 2.44889)
      , ("2021-03-09", 2.38461)
      , ("2021-03-10", 2.17303)
      , ("2021-03-11", 1.94440)
      , ("2021-03-12", 1.89705)
      , ("2021-03-15", 1.89779)
      , ("2021-03-16", 2.34495)
      , ("2021-03-17", 2.15115)
      , ("2021-03-18", 2.25548)
      , ("2021-03-19", 2.20670)
      , ("2021-03-22", 1.63720)
      , ("2021-03-24", 2.02774)
      , ("2021-03-25", 2.22988)
      , ("2021-03-26", 1.93123)
      , ("2021-03-29", 1.77269)
      , ("2021-03-30", 1.68532)
      , ("2021-03-31", 1.89454)
      , ("2021-04-01", 2.02782)
      , ("2021-04-05", 1.77098)
      , ("2021-04-06", 1.65494)
      ]
    )
  , ("Tape C",
      [ ("2020-10-09", 3.50423)
      , ("2020-10-12", 3.88504)
      , ("2020-10-13", 3.63414)
      , ("2020-10-14", 3.41042)
      , ("2020-10-15", 3.32528)
      , ("2020-10-16", 3.16699)
      , ("2020-10-19", 3.47742)
      , ("2020-10-20", 3.51005)
      , ("2020-10-21", 3.46732)
      , ("2020-10-22", 3.43405)
      , ("2020-10-23", 3.14558)
      , ("2020-10-26", 3.20511)
      , ("2020-10-27", 3.10478)
      , ("2020-10-28", 3.93069)
      , ("2020-10-29", 3.24419)
      , ("2020-10-30", 3.67161)
      , ("2020-11-02", 3.20901)
      , ("2020-11-03", 3.21659)
      , ("2020-11-04", 3.62076)
      , ("2020-11-05", 3.84816)
      , ("2020-11-06", 4.18323)
      , ("2020-11-09", 5.86391)
      , ("2020-11-10", 4.73656)
      , ("2020-11-11", 3.85469)
      , ("2020-11-12", 3.88667)
      , ("2020-11-13", 3.65423)
      , ("2020-11-16", 4.17257)
      , ("2020-11-17", 4.14503)
      , ("2020-11-18", 4.73143)
      , ("2020-11-19", 5.38520)
      , ("2020-11-20", 5.35097)
      , ("2020-11-23", 5.47325)
      , ("2020-11-24", 6.33959)
      , ("2020-11-25", 4.44865)
      , ("2020-11-27", 3.40183)
      , ("2020-11-30", 7.76040)
      , ("2020-12-01", 6.51242)
      , ("2020-12-02", 5.19496)
      , ("2020-12-03", 5.20999)
      , ("2020-12-04", 5.08691)
      , ("2020-12-07", 4.77182)
      , ("2020-12-08", 4.80949)
      , ("2020-12-09", 5.16846)
      , ("2020-12-10", 4.51858)
      , ("2020-12-11", 4.29269)
      , ("2020-12-14", 4.49194)
      , ("2020-12-15", 4.42365)
      , ("2020-12-16", 4.59271)
      , ("2020-12-17", 5.02984)
      , ("2020-12-18", 7.12581)
      , ("2020-12-21", 5.19286)
      , ("2020-12-22", 5.81186)
      , ("2020-12-23", 7.10025)
      , ("2020-12-24", 3.30597)
      , ("2020-12-28", 5.10915)
      , ("2020-12-29", 4.72692)
      , ("2020-12-30", 5.34302)
      , ("2020-12-31", 4.81584)
      , ("2021-01-04", 6.63618)
      , ("2021-01-05", 6.97186)
      , ("2021-01-06", 7.68988)
      , ("2021-01-07", 6.84148)
      , ("2021-01-08", 7.28940)
      , ("2021-01-11", 6.96048)
      , ("2021-01-12", 7.26407)
      , ("2021-01-13", 7.10481)
      , ("2021-01-14", 6.73410)
      , ("2021-01-15", 6.45836)
      , ("2021-01-19", 6.29787)
      , ("2021-01-20", 6.82099)
      , ("2021-01-21", 7.25864)
      , ("2021-01-22", 5.97276)
      , ("2021-01-25", 7.19372)
      , ("2021-01-26", 6.84960)
      , ("2021-01-27", 11.62120)
      , ("2021-01-28", 9.95927)
      , ("2021-01-29", 7.87226)
      , ("2021-02-01", 7.09253)
      , ("2021-02-02", 7.32528)
      , ("2021-02-03", 7.54695)
      , ("2021-02-04", 7.29537)
      , ("2021-02-05", 6.79292)
      , ("2021-02-08", 8.56454)
      , ("2021-02-09", 8.76678)
      , ("2021-02-10", 10.78594)
      , ("2021-02-11", 10.91678)
      , ("2021-02-12", 7.41695)
      , ("2021-02-16", 7.72211)
      , ("2021-02-17", 7.28050)
      , ("2021-02-18", 6.49943)
      , ("2021-02-19", 6.73782)
      , ("2021-02-22", 6.48374)
      , ("2021-02-23", 7.51652)
      , ("2021-02-24", 5.88623)
      , ("2021-02-25", 6.39038)
      , ("2021-02-26", 5.90612)
      , ("2021-03-01", 5.07953)
      , ("2021-03-02", 4.94815)
      , ("2021-03-03", 5.52999)
      , ("2021-03-04", 7.86439)
      , ("2021-03-05", 7.72520)
      , ("2021-03-08", 6.01023)
      , ("2021-03-09", 6.34557)
      , ("2021-03-10", 6.02389)
      , ("2021-03-11", 6.00886)
      , ("2021-03-12", 5.49722)
      , ("2021-03-15", 5.97237)
      , ("2021-03-16", 5.49390)
      , ("2021-03-17", 5.53612)
      , ("2021-03-18", 5.73973)
      , ("2021-03-19", 7.63060)
      , ("2021-03-22", 5.32176)
      , ("2021-03-24", 6.24866)
      , ("2021-03-25", 5.63430)
      , ("2021-03-26", 5.22437)
      , ("2021-03-29", 4.96510)
      , ("2021-03-30", 4.72196)
      , ("2021-03-31", 5.00473)
      , ("2021-04-01", 4.57649)
      , ("2021-04-05", 4.45900)
      , ("2021-04-06", 4.07372)
      ]
    )
  ]

dataByGroup : List (Cfg.StdSeries String)
dataByGroup =
  [ ("CBOE",
      [ ("2020-10-09", 1.40311)
      , ("2020-10-12", 1.29618)
      , ("2020-10-13", 1.36108)
      , ("2020-10-14", 1.36153)
      , ("2020-10-15", 1.32289)
      , ("2020-10-16", 1.40568)
      , ("2020-10-19", 1.40341)
      , ("2020-10-20", 1.43004)
      , ("2020-10-21", 1.40144)
      , ("2020-10-22", 1.39951)
      , ("2020-10-23", 1.25722)
      , ("2020-10-26", 1.44952)
      , ("2020-10-27", 1.32672)
      , ("2020-10-28", 1.82502)
      , ("2020-10-29", 1.59753)
      , ("2020-10-30", 1.65830)
      , ("2020-11-02", 1.40973)
      , ("2020-11-03", 1.44746)
      , ("2020-11-04", 1.70054)
      , ("2020-11-05", 1.67317)
      , ("2020-11-06", 1.71333)
      , ("2020-11-09", 3.00287)
      , ("2020-11-10", 2.05255)
      , ("2020-11-11", 1.52043)
      , ("2020-11-12", 1.58480)
      , ("2020-11-13", 1.48241)
      , ("2020-11-16", 1.61052)
      , ("2020-11-17", 1.53775)
      , ("2020-11-18", 1.71780)
      , ("2020-11-19", 1.68631)
      , ("2020-11-20", 1.63275)
      , ("2020-11-23", 1.86596)
      , ("2020-11-24", 2.11064)
      , ("2020-11-25", 1.60556)
      , ("2020-11-27", 1.03668)
      , ("2020-11-30", 2.10388)
      , ("2020-12-01", 2.07967)
      , ("2020-12-02", 1.72448)
      , ("2020-12-03", 1.68459)
      , ("2020-12-04", 1.72983)
      , ("2020-12-07", 1.66876)
      , ("2020-12-08", 1.56339)
      , ("2020-12-09", 1.89819)
      , ("2020-12-10", 1.57281)
      , ("2020-12-11", 1.48212)
      , ("2020-12-14", 1.52925)
      , ("2020-12-15", 1.47337)
      , ("2020-12-16", 1.49506)
      , ("2020-12-17", 1.56138)
      , ("2020-12-18", 1.71736)
      , ("2020-12-21", 1.76182)
      , ("2020-12-22", 1.58309)
      , ("2020-12-23", 1.71165)
      , ("2020-12-24", 0.90294)
      , ("2020-12-28", 1.49544)
      , ("2020-12-29", 1.42700)
      , ("2020-12-30", 1.38726)
      , ("2020-12-31", 1.39149)
      , ("2021-01-04", 2.16702)
      , ("2021-01-05", 2.14275)
      , ("2021-01-06", 2.60134)
      , ("2021-01-07", 2.03169)
      , ("2021-01-08", 2.16855)
      , ("2021-01-11", 2.01392)
      , ("2021-01-12", 2.25033)
      , ("2021-01-13", 2.06704)
      , ("2021-01-14", 2.12062)
      , ("2021-01-15", 2.03595)
      , ("2021-01-19", 1.96324)
      , ("2021-01-20", 1.97412)
      , ("2021-01-21", 1.91548)
      , ("2021-01-22", 1.87580)
      , ("2021-01-25", 2.45278)
      , ("2021-01-26", 2.13714)
      , ("2021-01-27", 3.80539)
      , ("2021-01-28", 3.00943)
      , ("2021-01-29", 2.61790)
      , ("2021-02-01", 2.33513)
      , ("2021-02-02", 2.18091)
      , ("2021-02-03", 2.04497)
      , ("2021-02-04", 2.02833)
      , ("2021-02-05", 1.97325)
      , ("2021-02-08", 2.27962)
      , ("2021-02-09", 2.34837)
      , ("2021-02-10", 2.90258)
      , ("2021-02-11", 2.72302)
      , ("2021-02-12", 2.05125)
      , ("2021-02-16", 2.31319)
      , ("2021-02-17", 2.27434)
      , ("2021-02-18", 2.09040)
      , ("2021-02-19", 2.04284)
      , ("2021-02-22", 2.26711)
      , ("2021-02-23", 2.66393)
      , ("2021-02-24", 2.06461)
      , ("2021-02-25", 2.72869)
      , ("2021-02-26", 2.48386)
      , ("2021-03-01", 1.93061)
      , ("2021-03-02", 1.91484)
      , ("2021-03-03", 2.29866)
      , ("2021-03-04", 3.16636)
      , ("2021-03-05", 2.95123)
      , ("2021-03-08", 2.30464)
      , ("2021-03-09", 2.19288)
      , ("2021-03-10", 2.19963)
      , ("2021-03-11", 1.99870)
      , ("2021-03-12", 1.80965)
      , ("2021-03-15", 1.96742)
      , ("2021-03-16", 1.93917)
      , ("2021-03-17", 1.93301)
      , ("2021-03-18", 2.09285)
      , ("2021-03-19", 1.97778)
      , ("2021-03-22", 1.64392)
      , ("2021-03-24", 1.98756)
      , ("2021-03-25", 2.00946)
      , ("2021-03-26", 1.91031)
      , ("2021-03-29", 1.71614)
      , ("2021-03-30", 1.59625)
      , ("2021-03-31", 1.66934)
      , ("2021-04-01", 1.67552)
      , ("2021-04-05", 1.59539)
      , ("2021-04-06", 1.47618)
      ]
    )
  , ("NYSE",
      [ ("2020-10-09", 1.88608)
      , ("2020-10-12", 1.67185)
      , ("2020-10-13", 1.75197)
      , ("2020-10-14", 1.74527)
      , ("2020-10-15", 1.71325)
      , ("2020-10-16", 1.81788)
      , ("2020-10-19", 1.80020)
      , ("2020-10-20", 1.77293)
      , ("2020-10-21", 1.84391)
      , ("2020-10-22", 1.84557)
      , ("2020-10-23", 1.60354)
      , ("2020-10-26", 1.83852)
      , ("2020-10-27", 1.72757)
      , ("2020-10-28", 2.52849)
      , ("2020-10-29", 2.23156)
      , ("2020-10-30", 2.44091)
      , ("2020-11-02", 2.00628)
      , ("2020-11-03", 1.98484)
      , ("2020-11-04", 2.38312)
      , ("2020-11-05", 2.27387)
      , ("2020-11-06", 2.14680)
      , ("2020-11-09", 4.03507)
      , ("2020-11-10", 2.77237)
      , ("2020-11-11", 2.07293)
      , ("2020-11-12", 2.25035)
      , ("2020-11-13", 1.99173)
      , ("2020-11-16", 2.38207)
      , ("2020-11-17", 2.13767)
      , ("2020-11-18", 2.28947)
      , ("2020-11-19", 2.23379)
      , ("2020-11-20", 2.07906)
      , ("2020-11-23", 2.41575)
      , ("2020-11-24", 2.83632)
      , ("2020-11-25", 2.21369)
      , ("2020-11-27", 1.35960)
      , ("2020-11-30", 3.55415)
      , ("2020-12-01", 2.62768)
      , ("2020-12-02", 2.32245)
      , ("2020-12-03", 2.17334)
      , ("2020-12-04", 2.24319)
      , ("2020-12-07", 2.12646)
      , ("2020-12-08", 2.04099)
      , ("2020-12-09", 2.37280)
      , ("2020-12-10", 2.13027)
      , ("2020-12-11", 2.01267)
      , ("2020-12-14", 2.19249)
      , ("2020-12-15", 2.06997)
      , ("2020-12-16", 2.01486)
      , ("2020-12-17", 2.08033)
      , ("2020-12-18", 4.44132)
      , ("2020-12-21", 2.45935)
      , ("2020-12-22", 2.16663)
      , ("2020-12-23", 2.25128)
      , ("2020-12-24", 1.12295)
      , ("2020-12-28", 1.86466)
      , ("2020-12-29", 1.83478)
      , ("2020-12-30", 1.75778)
      , ("2020-12-31", 1.89057)
      , ("2021-01-04", 2.93791)
      , ("2021-01-05", 2.78628)
      , ("2021-01-06", 3.36211)
      , ("2021-01-07", 2.63318)
      , ("2021-01-08", 2.70830)
      , ("2021-01-11", 2.67694)
      , ("2021-01-12", 2.82859)
      , ("2021-01-13", 2.53101)
      , ("2021-01-14", 2.66430)
      , ("2021-01-15", 2.86249)
      , ("2021-01-19", 2.67963)
      , ("2021-01-20", 2.55576)
      , ("2021-01-21", 2.32490)
      , ("2021-01-22", 2.49113)
      , ("2021-01-25", 3.06271)
      , ("2021-01-26", 2.74174)
      , ("2021-01-27", 5.16159)
      , ("2021-01-28", 4.20898)
      , ("2021-01-29", 3.65058)
      , ("2021-02-01", 3.05847)
      , ("2021-02-02", 2.81121)
      , ("2021-02-03", 2.52873)
      , ("2021-02-04", 2.55908)
      , ("2021-02-05", 2.46363)
      , ("2021-02-08", 2.76684)
      , ("2021-02-09", 2.61173)
      , ("2021-02-10", 3.12306)
      , ("2021-02-11", 2.96737)
      , ("2021-02-12", 2.29181)
      , ("2021-02-16", 2.68860)
      , ("2021-02-17", 2.59794)
      , ("2021-02-18", 2.40030)
      , ("2021-02-19", 2.50206)
      , ("2021-02-22", 2.77729)
      , ("2021-02-23", 3.15563)
      , ("2021-02-24", 2.81415)
      , ("2021-02-25", 3.17537)
      , ("2021-02-26", 3.51397)
      , ("2021-03-01", 2.50018)
      , ("2021-03-02", 2.65401)
      , ("2021-03-03", 2.93927)
      , ("2021-03-04", 3.82887)
      , ("2021-03-05", 3.51231)
      , ("2021-03-08", 2.91909)
      , ("2021-03-09", 2.82298)
      , ("2021-03-10", 2.83353)
      , ("2021-03-11", 2.57741)
      , ("2021-03-12", 2.31185)
      , ("2021-03-15", 2.42054)
      , ("2021-03-16", 2.42099)
      , ("2021-03-17", 2.52404)
      , ("2021-03-18", 2.70926)
      , ("2021-03-19", 5.08035)
      , ("2021-03-22", 2.25051)
      , ("2021-03-24", 2.54387)
      , ("2021-03-25", 2.64279)
      , ("2021-03-26", 2.62653)
      , ("2021-03-29", 2.37282)
      , ("2021-03-30", 2.10122)
      , ("2021-03-31", 2.55090)
      , ("2021-04-01", 2.11076)
      , ("2021-04-05", 2.01518)
      , ("2021-04-06", 1.94789)
      ]
    )
  , ("Nasdaq",
      [ ("2020-10-09", 1.52735)
      , ("2020-10-12", 1.51317)
      , ("2020-10-13", 1.47714)
      , ("2020-10-14", 1.46116)
      , ("2020-10-15", 1.45441)
      , ("2020-10-16", 1.52243)
      , ("2020-10-19", 1.54144)
      , ("2020-10-20", 1.53131)
      , ("2020-10-21", 1.58400)
      , ("2020-10-22", 1.57349)
      , ("2020-10-23", 1.37481)
      , ("2020-10-26", 1.56751)
      , ("2020-10-27", 1.45568)
      , ("2020-10-28", 1.97900)
      , ("2020-10-29", 1.78369)
      , ("2020-10-30", 1.97970)
      , ("2020-11-02", 1.65265)
      , ("2020-11-03", 1.64858)
      , ("2020-11-04", 2.01055)
      , ("2020-11-05", 1.85024)
      , ("2020-11-06", 1.86488)
      , ("2020-11-09", 3.26377)
      , ("2020-11-10", 2.30307)
      , ("2020-11-11", 1.71262)
      , ("2020-11-12", 1.75726)
      , ("2020-11-13", 1.65700)
      , ("2020-11-16", 1.87876)
      , ("2020-11-17", 1.75510)
      , ("2020-11-18", 1.89754)
      , ("2020-11-19", 1.91821)
      , ("2020-11-20", 1.89439)
      , ("2020-11-23", 1.96923)
      , ("2020-11-24", 2.36825)
      , ("2020-11-25", 1.75574)
      , ("2020-11-27", 1.14739)
      , ("2020-11-30", 2.80630)
      , ("2020-12-01", 2.26697)
      , ("2020-12-02", 1.89935)
      , ("2020-12-03", 1.80614)
      , ("2020-12-04", 1.92007)
      , ("2020-12-07", 1.77478)
      , ("2020-12-08", 1.75820)
      , ("2020-12-09", 2.07546)
      , ("2020-12-10", 1.78259)
      , ("2020-12-11", 1.68081)
      , ("2020-12-14", 1.80067)
      , ("2020-12-15", 1.71192)
      , ("2020-12-16", 1.75370)
      , ("2020-12-17", 1.77002)
      , ("2020-12-18", 3.69829)
      , ("2020-12-21", 2.03557)
      , ("2020-12-22", 1.84567)
      , ("2020-12-23", 1.95476)
      , ("2020-12-24", 0.95679)
      , ("2020-12-28", 1.63960)
      , ("2020-12-29", 1.56577)
      , ("2020-12-30", 1.54142)
      , ("2020-12-31", 1.58677)
      , ("2021-01-04", 2.33979)
      , ("2021-01-05", 2.19216)
      , ("2021-01-06", 2.73573)
      , ("2021-01-07", 2.20970)
      , ("2021-01-08", 2.39286)
      , ("2021-01-11", 2.17990)
      , ("2021-01-12", 2.27034)
      , ("2021-01-13", 2.17176)
      , ("2021-01-14", 2.21343)
      , ("2021-01-15", 2.29548)
      , ("2021-01-19", 2.14725)
      , ("2021-01-20", 2.20491)
      , ("2021-01-21", 2.18889)
      , ("2021-01-22", 2.10523)
      , ("2021-01-25", 2.76377)
      , ("2021-01-26", 2.36438)
      , ("2021-01-27", 4.35820)
      , ("2021-01-28", 3.72713)
      , ("2021-01-29", 3.22148)
      , ("2021-02-01", 2.64216)
      , ("2021-02-02", 2.52504)
      , ("2021-02-03", 2.29878)
      , ("2021-02-04", 2.31831)
      , ("2021-02-05", 2.23283)
      , ("2021-02-08", 2.59246)
      , ("2021-02-09", 2.49657)
      , ("2021-02-10", 2.97934)
      , ("2021-02-11", 2.97108)
      , ("2021-02-12", 2.18771)
      , ("2021-02-16", 2.52311)
      , ("2021-02-17", 2.38959)
      , ("2021-02-18", 2.26197)
      , ("2021-02-19", 2.30310)
      , ("2021-02-22", 2.57891)
      , ("2021-02-23", 3.16512)
      , ("2021-02-24", 2.51374)
      , ("2021-02-25", 2.97757)
      , ("2021-02-26", 3.01379)
      , ("2021-03-01", 2.19945)
      , ("2021-03-02", 2.25260)
      , ("2021-03-03", 2.52817)
      , ("2021-03-04", 3.47556)
      , ("2021-03-05", 3.29347)
      , ("2021-03-08", 2.61272)
      , ("2021-03-09", 2.45311)
      , ("2021-03-10", 2.46066)
      , ("2021-03-11", 2.24511)
      , ("2021-03-12", 2.02678)
      , ("2021-03-15", 2.09202)
      , ("2021-03-16", 2.08866)
      , ("2021-03-17", 2.22947)
      , ("2021-03-18", 2.36109)
      , ("2021-03-19", 4.06079)
      , ("2021-03-22", 1.98654)
      , ("2021-03-24", 2.28535)
      , ("2021-03-25", 2.35756)
      , ("2021-03-26", 2.27581)
      , ("2021-03-29", 2.03314)
      , ("2021-03-30", 1.86235)
      , ("2021-03-31", 2.13974)
      , ("2021-04-01", 1.88052)
      , ("2021-04-05", 1.74543)
      , ("2021-04-06", 1.69218)
      ]
    )
  , ("Other",
      [ ("2020-10-09", 0.22930)
      , ("2020-10-12", 0.22013)
      , ("2020-10-13", 0.24481)
      , ("2020-10-14", 0.22906)
      , ("2020-10-15", 0.23587)
      , ("2020-10-16", 0.22860)
      , ("2020-10-19", 0.23439)
      , ("2020-10-20", 0.23713)
      , ("2020-10-21", 0.27009)
      , ("2020-10-22", 0.26269)
      , ("2020-10-23", 0.25135)
      , ("2020-10-26", 0.27008)
      , ("2020-10-27", 0.28043)
      , ("2020-10-28", 0.34846)
      , ("2020-10-29", 0.34347)
      , ("2020-10-30", 0.35671)
      , ("2020-11-02", 0.32253)
      , ("2020-11-03", 0.32103)
      , ("2020-11-04", 0.36075)
      , ("2020-11-05", 0.35870)
      , ("2020-11-06", 0.33687)
      , ("2020-11-09", 0.51938)
      , ("2020-11-10", 0.44661)
      , ("2020-11-11", 0.34110)
      , ("2020-11-12", 0.33010)
      , ("2020-11-13", 0.31586)
      , ("2020-11-16", 0.36261)
      , ("2020-11-17", 0.35283)
      , ("2020-11-18", 0.36936)
      , ("2020-11-19", 0.31948)
      , ("2020-11-20", 0.30801)
      , ("2020-11-23", 0.37931)
      , ("2020-11-24", 0.42733)
      , ("2020-11-25", 0.30532)
      , ("2020-11-27", 0.17386)
      , ("2020-11-30", 0.35983)
      , ("2020-12-01", 0.48087)
      , ("2020-12-02", 0.49148)
      , ("2020-12-03", 0.76375)
      , ("2020-12-04", 0.35217)
      , ("2020-12-07", 0.32034)
      , ("2020-12-08", 0.31137)
      , ("2020-12-09", 0.37377)
      , ("2020-12-10", 0.31993)
      , ("2020-12-11", 0.30976)
      , ("2020-12-14", 0.33256)
      , ("2020-12-15", 0.31447)
      , ("2020-12-16", 0.31008)
      , ("2020-12-17", 0.31876)
      , ("2020-12-18", 0.35326)
      , ("2020-12-21", 0.31709)
      , ("2020-12-22", 0.29816)
      , ("2020-12-23", 0.30401)
      , ("2020-12-24", 0.15455)
      , ("2020-12-28", 0.26664)
      , ("2020-12-29", 0.26836)
      , ("2020-12-30", 0.26386)
      , ("2020-12-31", 0.25556)
      , ("2021-01-04", 0.39393)
      , ("2021-01-05", 0.37360)
      , ("2021-01-06", 0.48202)
      , ("2021-01-07", 0.39653)
      , ("2021-01-08", 0.36366)
      , ("2021-01-11", 0.32909)
      , ("2021-01-12", 0.37495)
      , ("2021-01-13", 0.36143)
      , ("2021-01-14", 0.39599)
      , ("2021-01-15", 0.39368)
      , ("2021-01-19", 0.38634)
      , ("2021-01-20", 0.38281)
      , ("2021-01-21", 0.37285)
      , ("2021-01-22", 0.37067)
      , ("2021-01-25", 0.46772)
      , ("2021-01-26", 0.43144)
      , ("2021-01-27", 0.64227)
      , ("2021-01-28", 0.57002)
      , ("2021-01-29", 0.53817)
      , ("2021-02-01", 0.48924)
      , ("2021-02-02", 0.50627)
      , ("2021-02-03", 0.46082)
      , ("2021-02-04", 0.46806)
      , ("2021-02-05", 0.42336)
      , ("2021-02-08", 0.46812)
      , ("2021-02-09", 0.48851)
      , ("2021-02-10", 0.55175)
      , ("2021-02-11", 0.51967)
      , ("2021-02-12", 0.45882)
      , ("2021-02-16", 0.52027)
      , ("2021-02-17", 0.50680)
      , ("2021-02-18", 0.47110)
      , ("2021-02-19", 0.48021)
      , ("2021-02-22", 0.53715)
      , ("2021-02-23", 0.52996)
      , ("2021-02-24", 0.51451)
      , ("2021-02-25", 0.59521)
      , ("2021-02-26", 0.56519)
      , ("2021-03-01", 0.45845)
      , ("2021-03-02", 0.45890)
      , ("2021-03-03", 0.52440)
      , ("2021-03-04", 0.70938)
      , ("2021-03-05", 0.68309)
      , ("2021-03-08", 0.58495)
      , ("2021-03-09", 0.57100)
      , ("2021-03-10", 0.54960)
      , ("2021-03-11", 0.50679)
      , ("2021-03-12", 0.46767)
      , ("2021-03-15", 0.51782)
      , ("2021-03-16", 0.50846)
      , ("2021-03-17", 0.50032)
      , ("2021-03-18", 0.56721)
      , ("2021-03-19", 0.56042)
      , ("2021-03-22", 0.45557)
      , ("2021-03-24", 0.58391)
      , ("2021-03-25", 0.56441)
      , ("2021-03-26", 0.55399)
      , ("2021-03-29", 0.50647)
      , ("2021-03-30", 0.46322)
      , ("2021-03-31", 0.47562)
      , ("2021-04-01", 0.46837)
      , ("2021-04-05", 0.44249)
      , ("2021-04-06", 0.44945)
      ]
    )
  , ("TRF",
      [ ("2020-10-09", 3.97613)
      , ("2020-10-12", 3.79109)
      , ("2020-10-13", 3.81758)
      , ("2020-10-14", 3.79058)
      , ("2020-10-15", 3.57943)
      , ("2020-10-16", 3.99905)
      , ("2020-10-19", 3.84602)
      , ("2020-10-20", 3.80518)
      , ("2020-10-21", 3.84589)
      , ("2020-10-22", 3.78450)
      , ("2020-10-23", 3.38516)
      , ("2020-10-26", 3.71669)
      , ("2020-10-27", 3.49453)
      , ("2020-10-28", 4.46861)
      , ("2020-10-29", 3.94833)
      , ("2020-10-30", 3.94372)
      , ("2020-11-02", 3.74835)
      , ("2020-11-03", 3.71363)
      , ("2020-11-04", 4.09746)
      , ("2020-11-05", 4.46091)
      , ("2020-11-06", 4.48268)
      , ("2020-11-09", 6.52940)
      , ("2020-11-10", 5.13539)
      , ("2020-11-11", 4.19548)
      , ("2020-11-12", 4.49601)
      , ("2020-11-13", 4.49397)
      , ("2020-11-16", 4.88494)
      , ("2020-11-17", 4.84866)
      , ("2020-11-18", 5.30017)
      , ("2020-11-19", 4.98432)
      , ("2020-11-20", 4.94206)
      , ("2020-11-23", 5.71000)
      , ("2020-11-24", 6.75273)
      , ("2020-11-25", 5.04727)
      , ("2020-11-27", 3.31387)
      , ("2020-11-30", 7.06967)
      , ("2020-12-01", 6.44944)
      , ("2020-12-02", 5.43982)
      , ("2020-12-03", 5.34417)
      , ("2020-12-04", 5.37442)
      , ("2020-12-07", 5.12304)
      , ("2020-12-08", 5.03318)
      , ("2020-12-09", 5.46016)
      , ("2020-12-10", 4.78102)
      , ("2020-12-11", 4.57739)
      , ("2020-12-14", 4.69452)
      , ("2020-12-15", 4.70510)
      , ("2020-12-16", 4.48103)
      , ("2020-12-17", 4.89390)
      , ("2020-12-18", 5.75558)
      , ("2020-12-21", 5.27348)
      , ("2020-12-22", 5.44534)
      , ("2020-12-23", 6.31771)
      , ("2020-12-24", 3.11965)
      , ("2020-12-28", 4.89030)
      , ("2020-12-29", 4.52200)
      , ("2020-12-30", 4.81829)
      , ("2020-12-31", 4.33794)
      , ("2021-01-04", 6.62930)
      , ("2021-01-05", 7.21755)
      , ("2021-01-06", 7.75143)
      , ("2021-01-07", 6.78227)
      , ("2021-01-08", 7.00276)
      , ("2021-01-11", 7.23804)
      , ("2021-01-12", 7.77154)
      , ("2021-01-13", 6.97503)
      , ("2021-01-14", 7.18255)
      , ("2021-01-15", 6.73887)
      , ("2021-01-19", 7.00397)
      , ("2021-01-20", 6.79926)
      , ("2021-01-21", 6.80630)
      , ("2021-01-22", 6.23771)
      , ("2021-01-25", 7.88352)
      , ("2021-01-26", 7.08661)
      , ("2021-01-27", 10.51398)
      , ("2021-01-28", 8.62961)
      , ("2021-01-29", 7.42581)
      , ("2021-02-01", 7.67711)
      , ("2021-02-02", 6.93946)
      , ("2021-02-03", 6.88789)
      , ("2021-02-04", 6.98736)
      , ("2021-02-05", 6.80489)
      , ("2021-02-08", 7.67101)
      , ("2021-02-09", 8.09552)
      , ("2021-02-10", 9.35269)
      , ("2021-02-11", 8.91966)
      , ("2021-02-12", 6.57518)
      , ("2021-02-16", 7.25525)
      , ("2021-02-17", 6.85684)
      , ("2021-02-18", 6.18189)
      , ("2021-02-19", 6.34460)
      , ("2021-02-22", 6.54861)
      , ("2021-02-23", 7.20698)
      , ("2021-02-24", 6.22013)
      , ("2021-02-25", 6.63189)
      , ("2021-02-26", 6.21573)
      , ("2021-03-01", 5.33252)
      , ("2021-03-02", 5.39100)
      , ("2021-03-03", 5.95178)
      , ("2021-03-04", 7.39060)
      , ("2021-03-05", 7.28156)
      , ("2021-03-08", 5.90944)
      , ("2021-03-09", 6.20378)
      , ("2021-03-10", 6.00089)
      , ("2021-03-11", 5.93814)
      , ("2021-03-12", 5.25461)
      , ("2021-03-15", 5.77246)
      , ("2021-03-16", 5.49466)
      , ("2021-03-17", 5.06209)
      , ("2021-03-18", 5.38340)
      , ("2021-03-19", 5.91538)
      , ("2021-03-22", 4.93869)
      , ("2021-03-24", 5.65874)
      , ("2021-03-25", 5.23831)
      , ("2021-03-26", 5.27213)
      , ("2021-03-29", 4.73740)
      , ("2021-03-30", 4.50576)
      , ("2021-03-31", 4.64174)
      , ("2021-04-01", 4.63126)
      , ("2021-04-05", 4.43653)
      , ("2021-04-06", 4.24423)
      ]
    )
  ]

