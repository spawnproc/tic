Crypto Liquidity Integrator
===========================

Origins
-------

https://t.me/alkotrading

Features
--------

* L3 Order Book persistence
* Realtime WebSocket Connections
* Stream Recording and Replaying
* BitMEX, OKCoin, GDAX and other venues
* Reconnecting on stream failures
* Fast ETS storage as matching engine
* 150 LOC

Launch
------

```
$ brew install erlang
$ git clone git://github.com/spawnproc/ticker && cd ticker
$ ./mad dep com pla && ./mad rep
```

Text Log
--------

```
$ tail priv/gdax/2017-2-26/BTC-USD
2017-02-27T19:32:30.978000Z:[107 1192.4 +0.93]
2017-02-27T19:32:30.989000Z:[107 1192.4 +0.9]
2017-02-27T19:32:30.996000Z:[8 -0]
2017-02-27T19:32:31.006000Z:[8 1188.57 +12.51138698]
2017-02-27T19:32:31.043000Z:[8 -0]
2017-02-27T19:32:31.058000Z:[8 1188.57 +12.51138698]
2017-02-27T19:32:31.087000Z:[8 -0]
2017-02-27T19:32:31.103000Z:[8 1188.57 +12.51138698]
2017-02-27T19:32:31.141000Z:[8 -0]
2017-02-27T19:32:31.256000Z:[8 1188.57 +0.99098943]
```

Commands
--------

## Obtain Supported Instruments

```
> book:instruments().
[btc_usd,btc_eur,btc_gpb,eth_btc,eth_usd]
```

## Book Print

```
> book:print(eth_usd).
  Id Price Size
---- ----- ----------
   7 15.15 -235.08011
  28 15.03 -90.77931
  18  15.0 -1.0
  25 14.98 -50.09
  26 14.97 -445.21
  27 14.96 -201.47
  19 14.94 -54.09
   4 14.92 -300.894
  22 14.86 -1.00641
  14 14.82 434.71422
  16 14.79 325.01467
   1 14.78 305.32
  17 14.77 711.95
   5 14.76 378.72
   6 14.75 318.78
   8 14.74 260.41
  11 14.62 451.0586
Depth: 17
Total: 1806.34766
ok
```

## Enable Console Log

```
> application:set_env(trade,log,show).
ok
>
gdax:"BTC-EUR":"sell":"2017-02-27T19:37:17.687000Z:[28 -0]\n":97f64637-84ef-438b-95c7-ae3849310143
gdax:"BTC-GBP":"buy":"2017-02-27T19:37:17.693000Z:[112 955.62 +0.67172193]\n":07e7f77f-c2a6-43ac-abbd-c13a6e65f27b
gdax:"BTC-EUR":"sell":"2017-02-27T19:37:17.699000Z:[30 -0]\n":add5d9e8-160f-45e6-9e4c-92cbe86a70bd
gdax:"BTC-EUR":"buy":"2017-02-27T19:37:17.700000Z:[42 -0]\n":c9f55ec4-7ed3-4505-a24c-263e2926edef
gdax:"BTC-EUR":"buy":"2017-02-27T19:37:17.716000Z:[448 -0]\n":31ee8a39-1aa3-43b2-b777-64e2ca71cbc7
```

## Orders Table

```
> kvs:all(order).
[{order,"4fb397c0-2e45-4d98-8f76-06399e9f9cf5",30,eth_btc},
 {order,"f8a81058-06bd-435c-b1cb-9ac8464af9ce",260,btc_eur},
 {order,"71ffcb78-f036-467c-b1ea-12c0e3d98a99",326,btc_eur},
 {order,"ad325868-23cd-4177-bfba-ed06a11ab707",216,btc_gpb},
 {order,"f991aa88-1278-4464-8992-b958eb1e977b",139,btc_gpb},
 {order,"830e9767-6182-42a6-b320-424f1e9211ca",312,btc_eur},
 {order,[...],...},
 {order,...},
 {...}|...]
```

Credits
-------

* Maxim Sokhatsky
* Alexander Temerev

OM A HUM
