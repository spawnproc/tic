
-record(bitmex, { table=[], action=[], data=[], keys=[], types=[], attributes=[], foreignKeys=[] }).
-record(bshot,  { id=[], side=[], size=[], price=[], symbol=[] }).
-record(bsym,   { id=[], symbol=[], side=[], size=[], price=[],timestamp=[], tickDirection=[],
                  trdMatchID=[], grossValue=[], homeNotional=[], foreignNotional=[]}).
