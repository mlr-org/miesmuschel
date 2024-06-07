#!/bin/bash
sed -i 's_\\link\[\(bbotk\|mlr3tuning\):\(Optim\|Tun\)[^]]*\]{\([^}]*\)}_\3_g' man/*.Rd
