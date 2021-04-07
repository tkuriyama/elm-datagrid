module SampleData.BarChartSample exposing ( dataShares, dataNotional )

{-| Sample data for use in Bar Chart examples.
Source: https://www.cboe.com/us/equities/market_share/

-}

import DataGrid.ChartConfig as Cfg 

--------------------------------------------------------------------------------

dataShares : List (Cfg.StdSeries String)
dataShares =
  [ ("SUM(mkt_volume)",
      [ ("2020-10-09", 9.02196)
      , ("2020-10-12", 8.49241)
      , ("2020-10-13", 8.65258)
      , ("2020-10-14", 8.58759)
      , ("2020-10-15", 8.30585)
      , ("2020-10-16", 8.97363)
      , ("2020-10-19", 8.82546)
      , ("2020-10-20", 8.77660)
      , ("2020-10-21", 8.94534)
      , ("2020-10-22", 8.86576)
      , ("2020-10-23", 7.87207)
      , ("2020-10-26", 8.84232)
      , ("2020-10-27", 8.28493)
      , ("2020-10-28", 11.14958)
      , ("2020-10-29", 9.90459)
      , ("2020-10-30", 10.37935)
      , ("2020-11-02", 9.13954)
      , ("2020-11-03", 9.11554)
      , ("2020-11-04", 10.55242)
      , ("2020-11-05", 10.61688)
      , ("2020-11-06", 10.54456)
      , ("2020-11-09", 17.35050)
      , ("2020-11-10", 12.71000)
      , ("2020-11-11", 9.84256)
      , ("2020-11-12", 10.41851)
      , ("2020-11-13", 9.94097)
      , ("2020-11-16", 11.11889)
      , ("2020-11-17", 10.63201)
      , ("2020-11-18", 11.57434)
      , ("2020-11-19", 11.14210)
      , ("2020-11-20", 10.85627)
      , ("2020-11-23", 12.34025)
      , ("2020-11-24", 14.49526)
      , ("2020-11-25", 10.92757)
      , ("2020-11-27", 7.03140)
      , ("2020-11-30", 15.89384)
      , ("2020-12-01", 13.90464)
      , ("2020-12-02", 11.87759)
      , ("2020-12-03", 11.77199)
      , ("2020-12-04", 11.61969)
      , ("2020-12-07", 11.01338)
      , ("2020-12-08", 10.70713)
      , ("2020-12-09", 12.18038)
      , ("2020-12-10", 10.58663)
      , ("2020-12-11", 10.06275)
      , ("2020-12-14", 10.54948)
      , ("2020-12-15", 10.27483)
      , ("2020-12-16", 10.05474)
      , ("2020-12-17", 10.62439)
      , ("2020-12-18", 15.96579)
      , ("2020-12-21", 11.84732)
      , ("2020-12-22", 11.33888)
      , ("2020-12-23", 12.53941)
      , ("2020-12-24", 6.25688)
      , ("2020-12-28", 10.15664)
      , ("2020-12-29", 9.61791)
      , ("2020-12-30", 9.76860)
      , ("2020-12-31", 9.46233)
      , ("2021-01-04", 14.46796)
      , ("2021-01-05", 14.71235)
      , ("2021-01-06", 16.93264)
      , ("2021-01-07", 14.05338)
      , ("2021-01-08", 14.63613)
      , ("2021-01-11", 14.43789)
      , ("2021-01-12", 15.49575)
      , ("2021-01-13", 14.10627)
      , ("2021-01-14", 14.57689)
      , ("2021-01-15", 14.32647)
      , ("2021-01-19", 14.18043)
      , ("2021-01-20", 13.91686)
      , ("2021-01-21", 13.60842)
      , ("2021-01-22", 13.08054)
      , ("2021-01-25", 16.63050)
      , ("2021-01-26", 14.76132)
      , ("2021-01-27", 24.48143)
      , ("2021-01-28", 20.14516)
      , ("2021-01-29", 17.45394)
      , ("2021-02-01", 16.20212)
      , ("2021-02-02", 14.96290)
      , ("2021-02-03", 14.22119)
      , ("2021-02-04", 14.36114)
      , ("2021-02-05", 13.89796)
      , ("2021-02-08", 15.77806)
      , ("2021-02-09", 16.04070)
      , ("2021-02-10", 18.90942)
      , ("2021-02-11", 18.10079)
      , ("2021-02-12", 13.56476)
      , ("2021-02-16", 15.30042)
      , ("2021-02-17", 14.62551)
      , ("2021-02-18", 13.40566)
      , ("2021-02-19", 13.67281)
      , ("2021-02-22", 14.70906)
      , ("2021-02-23", 16.72162)
      , ("2021-02-24", 14.12715)
      , ("2021-02-25", 16.10872)
      , ("2021-02-26", 15.79253)
      , ("2021-03-01", 12.42122)
      , ("2021-03-02", 12.67135)
      , ("2021-03-03", 14.24228)
      , ("2021-03-04", 18.57077)
      , ("2021-03-05", 17.72166)
      , ("2021-03-08", 14.33083)
      , ("2021-03-09", 14.24375)
      , ("2021-03-10", 14.04431)
      , ("2021-03-11", 13.26615)
      , ("2021-03-12", 11.87056)
      , ("2021-03-15", 12.77027)
      , ("2021-03-16", 12.45193)
      , ("2021-03-17", 12.24893)
      , ("2021-03-18", 13.11380)
      , ("2021-03-19", 17.59473)
      , ("2021-03-22", 11.27523)
      , ("2021-03-24", 13.05942)
      , ("2021-03-25", 12.81252)
      , ("2021-03-26", 12.63876)
      , ("2021-03-29", 11.36597)
      , ("2021-03-30", 10.52879)
      , ("2021-03-31", 11.47733)
      , ("2021-04-01", 10.76644)
      , ("2021-04-05", 10.23502)
      , ("2021-04-06", 9.80993)
      ]
    )
  ]

dataNotional : List (Cfg.StdSeries String)
dataNotional =
  [ ("SUM(mkt_volume)",
      [ ("2020-10-09", 395.91171)
      , ("2020-10-12", 454.43766)
      , ("2020-10-13", 444.29488)
      , ("2020-10-14", 421.85049)
      , ("2020-10-15", 406.42610)
      , ("2020-10-16", 420.85673)
      , ("2020-10-19", 404.89597)
      , ("2020-10-20", 392.55806)
      , ("2020-10-21", 417.71049)
      , ("2020-10-22", 410.09244)
      , ("2020-10-23", 349.82204)
      , ("2020-10-26", 435.24301)
      , ("2020-10-27", 396.71140)
      , ("2020-10-28", 525.96107)
      , ("2020-10-29", 492.29726)
      , ("2020-10-30", 551.18723)
      , ("2020-11-02", 455.43939)
      , ("2020-11-03", 458.92783)
      , ("2020-11-04", 602.45331)
      , ("2020-11-05", 534.05298)
      , ("2020-11-06", 459.25560)
      , ("2020-11-09", 856.17544)
      , ("2020-11-10", 642.42743)
      , ("2020-11-11", 460.58355)
      , ("2020-11-12", 470.89603)
      , ("2020-11-13", 434.70463)
      , ("2020-11-16", 507.13099)
      , ("2020-11-17", 469.97488)
      , ("2020-11-18", 501.86863)
      , ("2020-11-19", 459.13647)
      , ("2020-11-20", 456.06722)
      , ("2020-11-23", 506.03305)
      , ("2020-11-24", 571.20353)
      , ("2020-11-25", 453.75764)
      , ("2020-11-27", 286.11795)
      , ("2020-11-30", 677.90193)
      , ("2020-12-01", 571.22919)
      , ("2020-12-02", 493.32718)
      , ("2020-12-03", 500.63437)
      , ("2020-12-04", 479.43610)
      , ("2020-12-07", 476.27170)
      , ("2020-12-08", 471.39861)
      , ("2020-12-09", 592.00652)
      , ("2020-12-10", 498.70939)
      , ("2020-12-11", 475.25100)
      , ("2020-12-14", 522.23880)
      , ("2020-12-15", 501.61231)
      , ("2020-12-16", 491.50385)
      , ("2020-12-17", 511.18464)
      , ("2020-12-18", 952.77929)
      , ("2020-12-21", 573.63220)
      , ("2020-12-22", 491.49640)
      , ("2020-12-23", 419.81205)
      , ("2020-12-24", 241.71952)
      , ("2020-12-28", 431.55788)
      , ("2020-12-29", 422.34668)
      , ("2020-12-30", 387.35282)
      , ("2020-12-31", 411.17526)
      , ("2021-01-04", 638.63198)
      , ("2021-01-05", 505.10008)
      , ("2021-01-06", 723.39339)
      , ("2021-01-07", 605.73962)
      , ("2021-01-08", 630.68062)
      , ("2021-01-11", 550.13359)
      , ("2021-01-12", 591.06650)
      , ("2021-01-13", 543.10452)
      , ("2021-01-14", 602.32258)
      , ("2021-01-15", 626.81127)
      , ("2021-01-19", 564.30268)
      , ("2021-01-20", 579.33207)
      , ("2021-01-21", 532.79521)
      , ("2021-01-22", 516.14823)
      , ("2021-01-25", 687.58948)
      , ("2021-01-26", 574.66810)
      , ("2021-01-27", 891.32659)
      , ("2021-01-28", 691.97146)
      , ("2021-01-29", 740.17970)
      , ("2021-02-01", 585.64126)
      , ("2021-02-02", 596.57843)
      , ("2021-02-03", 558.43718)
      , ("2021-02-04", 541.05812)
      , ("2021-02-05", 508.46294)
      , ("2021-02-08", 529.62069)
      , ("2021-02-09", 512.34829)
      , ("2021-02-10", 593.46409)
      , ("2021-02-11", 535.18647)
      , ("2021-02-12", 467.34432)
      , ("2021-02-16", 579.88319)
      , ("2021-02-17", 549.72948)
      , ("2021-02-18", 529.17597)
      , ("2021-02-19", 583.36953)
      , ("2021-02-22", 633.77590)
      , ("2021-02-23", 777.90923)
      , ("2021-02-24", 643.97109)
      , ("2021-02-25", 810.76230)
      , ("2021-02-26", 813.96054)
      , ("2021-03-01", 614.24836)
      , ("2021-03-02", 588.28200)
      , ("2021-03-03", 686.74166)
      , ("2021-03-04", 923.74323)
      , ("2021-03-05", 872.71218)
      , ("2021-03-08", 732.17458)
      , ("2021-03-09", 716.74408)
      , ("2021-03-10", 688.14713)
      , ("2021-03-11", 622.01072)
      , ("2021-03-12", 552.45226)
      , ("2021-03-15", 554.45923)
      , ("2021-03-16", 572.15388)
      , ("2021-03-17", 615.72178)
      , ("2021-03-18", 659.93480)
      , ("2021-03-19", 942.22866)
      , ("2021-03-22", 557.53260)
      , ("2021-03-24", 603.66624)
      , ("2021-03-25", 634.01252)
      , ("2021-03-26", 646.92818)
      , ("2021-03-29", 578.56744)
      , ("2021-03-30", 515.59260)
      , ("2021-03-31", 617.38467)
      , ("2021-04-01", 566.77256)
      , ("2021-04-05", 540.30176)
      , ("2021-04-06", 494.82691)
      ]
    )
  ]

