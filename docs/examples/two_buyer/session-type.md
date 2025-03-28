# TwoBuyer Protocol

## Global Protocol
```
Buyer1 -> Seller : title(string) .
Seller -> Buyer1 : quote(number) .
Buyer1 -> Buyer2 : share(number) . {
    Buyer2 -> Seller : address(string) . Seller -> Buyer2 : date(date) . end,
    Buyer2 -> Seller : quit(unit) . end
}
```

## Local Protocols

### Seller
``` 
&Buyer1:title(string).+Buyer1:quote(number).&Buyer2:{
    address(string).+Buyer2:date(date).end,
    quit(unit).end
}
```

### Buyer1
```
+Seller:title(string).&Seller:quote(number).+Buyer2:share(number).end
```

### Buyer2
```
&Buyer1:share(number).+Seller:{
    address(string).&Seller:date(date).end,
    quit(unit).end
}
```