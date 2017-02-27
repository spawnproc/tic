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
2017-02-26T07:28:13.325000Z:[27 +111000000 114897000000]
2017-02-26T07:28:13.332000Z:[114 +102000000 114874000000]
2017-02-26T07:28:13.339000Z:[27 +109000000 114897000000]
2017-02-26T07:28:13.348000Z:[29 +115000000 114911000000]
2017-02-26T07:28:13.356000Z:[30 +109000000 114901000000]
2017-02-26T07:28:13.363000Z:[31 +109000000 114903000000]
2017-02-26T07:28:13.372000Z:[114 +098000000 114874000000]
2017-02-26T07:28:13.420000Z:[14 -010610000 115845000000]
2017-02-26T07:28:13.546000Z:[14 -0]
2017-02-26T07:28:13.561000Z:[37 -010610000 115844000000]
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
  Id Price      Size
---- ---------- ------------
  13 1470000000 -9902060000
   5 1466000000 -10266942
   3 1462000000 -1416915060
   2 1461000000 -2715654940
  12 1454000000 19294000000
  16 1453000000 4595000000
  14 1452000000 5802000000
  24 1450000000 102060523900
  22 1449000000 10462341000
  11 1448000000 36004000000
  23 1446000000 10614000000
   1 1445000000 10624000000
Depth: 12
Total: 185410967958
ok
```

## Enable Console Log

```
> sym:hide().
ok
> sym:show().
ok
>
gdax:"ETH-BTC":"sell":2017-02-26T16:06:55.749000Z:[110 -0]
gdax:"BTC-GBP":"buy":2017-02-26T16:06:55.748000Z:[878 +005000000 94020000000]
gdax:"ETH-BTC":"sell":2017-02-26T16:06:55.768000Z:[93 -17664952000 001190000]
gdax:"ETH-BTC":"sell":2017-02-26T16:06:55.778000Z:[93 -34614317000 001190000]
gdax:"ETH-BTC":"sell":2017-02-26T16:06:55.808000Z:[516 -0]
gdax:"BTC-EUR":"sell":2017-02-26T16:06:55.805000Z:[518 -0]
```

Credits
-------

* Maxim Sokhatsky
* Alexander Temerev

OM A HUM
