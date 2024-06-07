#!/bin/bash
sed -i 's_\\link\[\(bbotk\|mlr3tuning\):[^]]*\]{\([^}]*\)}_\2_g' man/*.Rd
