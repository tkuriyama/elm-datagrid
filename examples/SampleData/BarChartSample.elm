module SampleData.BarChartSample exposing (data)

{- | Sample data for Bar Chart example
   Source: <https://www.cboe.com/us/equities/market_share/>
-}

import DataGrid.Config as Cfg



--------------------------------------------------------------------------------


data : Cfg.StdSeriesPairs String
data =
    [ ( "Shares"
      , [ ( "2020-10-07", 9181835573.0 )
        , ( "2020-10-08", 9161733481.0 )
        , ( "2020-10-09", 9021962916.0 )
        , ( "2020-10-12", 8492412613.0 )
        , ( "2020-10-13", 8652584552.0 )
        , ( "2020-10-14", 8587593519.0 )
        , ( "2020-10-15", 8305846439.0 )
        , ( "2020-10-16", 8973632166.0 )
        , ( "2020-10-19", 8825461367.0 )
        , ( "2020-10-20", 8776596638.0 )
        , ( "2020-10-21", 8945339682.0 )
        , ( "2020-10-22", 8865758477.0 )
        , ( "2020-10-23", 7872068783.0 )
        , ( "2020-10-26", 8842322107.0 )
        , ( "2020-10-27", 8284928007.0 )
        , ( "2020-10-28", 11149580547.0 )
        , ( "2020-10-29", 9904592986.0 )
        , ( "2020-10-30", 10379347903.0 )
        , ( "2020-11-02", 9139535443.0 )
        , ( "2020-11-03", 9115542109.0 )
        , ( "2020-11-04", 10552422056.0 )
        , ( "2020-11-05", 10616884643.0 )
        , ( "2020-11-06", 10544556049.0 )
        , ( "2020-11-09", 17350500938.0 )
        , ( "2020-11-10", 12710000022.0 )
        , ( "2020-11-11", 9842559146.0 )
        , ( "2020-11-12", 10418508277.0 )
        , ( "2020-11-13", 9940966344.0 )
        , ( "2020-11-16", 11118894079.0 )
        , ( "2020-11-17", 10632011882.0 )
        , ( "2020-11-18", 11574339512.0 )
        , ( "2020-11-19", 11142101190.0 )
        , ( "2020-11-20", 10856268711.0 )
        , ( "2020-11-23", 12340246315.0 )
        , ( "2020-11-24", 14495256559.0 )
        , ( "2020-11-25", 10927574357.0 )
        , ( "2020-11-27", 7031398815.0 )
        , ( "2020-11-30", 15893837631.0 )
        , ( "2020-12-01", 13904637883.0 )
        , ( "2020-12-02", 11877592122.0 )
        , ( "2020-12-03", 11771991965.0 )
        , ( "2020-12-04", 11619688957.0 )
        , ( "2020-12-07", 11013381797.0 )
        , ( "2020-12-08", 10707134920.0 )
        , ( "2020-12-09", 12180380965.0 )
        , ( "2020-12-10", 10586625724.0 )
        , ( "2020-12-11", 10062752866.0 )
        , ( "2020-12-14", 10549479406.0 )
        , ( "2020-12-15", 10274830342.0 )
        , ( "2020-12-16", 10054740606.0 )
        , ( "2020-12-17", 10624390757.0 )
        , ( "2020-12-18", 15965794222.0 )
        , ( "2020-12-21", 11847316537.0 )
        , ( "2020-12-22", 11338881032.0 )
        , ( "2020-12-23", 12539414262.0 )
        , ( "2020-12-24", 6256883042.0 )
        , ( "2020-12-28", 10156639531.0 )
        , ( "2020-12-29", 9617909650.0 )
        , ( "2020-12-30", 9768599193.0 )
        , ( "2020-12-31", 9462334377.0 )
        , ( "2021-01-04", 14467958762.0 )
        , ( "2021-01-05", 14712345155.0 )
        , ( "2021-01-06", 16932635781.0 )
        , ( "2021-01-07", 14053376403.0 )
        , ( "2021-01-08", 14636127971.0 )
        , ( "2021-01-11", 14437892954.0 )
        , ( "2021-01-12", 15495751940.0 )
        , ( "2021-01-13", 14106274417.0 )
        , ( "2021-01-14", 14576885395.0 )
        , ( "2021-01-15", 14326472468.0 )
        , ( "2021-01-19", 14180431625.0 )
        , ( "2021-01-20", 13916861462.0 )
        , ( "2021-01-21", 13608416326.0 )
        , ( "2021-01-22", 13080536951.0 )
        , ( "2021-01-25", 16630500430.0 )
        , ( "2021-01-26", 14761315926.0 )
        , ( "2021-01-27", 24481434014.0 )
        , ( "2021-01-28", 20145163272.0 )
        , ( "2021-01-29", 17453936369.0 )
        , ( "2021-02-01", 16202115677.0 )
        , ( "2021-02-02", 14962895945.0 )
        , ( "2021-02-03", 14221189579.0 )
        , ( "2021-02-04", 14361141278.0 )
        , ( "2021-02-05", 13897957199.0 )
        , ( "2021-02-08", 15778055353.0 )
        , ( "2021-02-09", 16040700196.0 )
        , ( "2021-02-10", 18909421881.0 )
        , ( "2021-02-11", 18100789341.0 )
        , ( "2021-02-12", 13564764689.0 )
        , ( "2021-02-16", 15300419289.0 )
        , ( "2021-02-17", 14625512685.0 )
        , ( "2021-02-18", 13405661128.0 )
        , ( "2021-02-19", 13672811393.0 )
        , ( "2021-02-22", 14709061728.0 )
        , ( "2021-02-23", 16721620340.0 )
        , ( "2021-02-24", 14127150993.0 )
        , ( "2021-02-25", 16108721758.0 )
        , ( "2021-02-26", 15792532791.0 )
        , ( "2021-03-01", 12421216236.0 )
        , ( "2021-03-02", 12671347157.0 )
        , ( "2021-03-03", 14242279044.0 )
        , ( "2021-03-04", 18570771931.0 )
        , ( "2021-03-05", 17721656931.0 )
        , ( "2021-03-08", 14330826956.0 )
        ]
      )
    ]