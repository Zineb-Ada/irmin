#!/bin/bash
ocamlmerlin single locate -look-for ml -position 120:12 -filename ./examples/custom_merge.ml < ./examples/custom_merge.ml \
     | jq '{"results": [{"name": "ocamlmerlin_locate", "metrics": [{"name": "ocaml:4_14_0", "value": .timing.query, "units": "ms"}]}]}'

ocamlmerlin single locate -look-for ml -position 26:20 -filename ./src/irmin-containers/irmin_containers.ml < ./src/irmin-containers/irmin_containers.ml \
     | jq '{"results": [{"name": "ocamlmerlin_locate", "metrics": [{"name": "ocaml:4_14_0", "value": .timing.query, "units": "ms"}]}]}'

ocamlmerlin single locate -look-for ml -position 3:15 -filename ./src/libirmin/types_intf.ml < ./src/libirmin/types_intf.ml \
     | jq '{"results": [{"name": "ocamlmerlin_locate", "metrics": [{"name": "ocaml:4_14_0", "value": .timing.query, "units": "ms"}]}]}'

ocamlmerlin single locate -look-for ml -position 23:16 -filename ./examples/irmin_git_store.ml < ./examples/irmin_git_store.ml \
     | jq '{"results": [{"name": "ocamlmerlin_locate", "metrics": [{"name": "ocaml:4_14_0", "value": .timing.query, "units": "ms"}]}]}'

ocamlmerlin single locate -look-for ml -position 23:22 -filename ./examples/irmin_git_store.ml < ./examples/irmin_git_store.ml \
     | jq '{"results": [{"name": "ocamlmerlin_locate", "metrics": [{"name": "ocaml:4_14_0", "value": .timing.query, "units": "ms"}]}]}'

# 1246, i : 120,  k : 12 filename: ./examples/custom_merge.ml, word: Irmin 
# 1256, i : 26,  k : 20 filename: ./src/irmin-containers/irmin_containers.ml, word: unix 
# 1271, i : 3,  k : 15 filename: ./src/libirmin/types_intf.ml, word: Irmin 
# 1302, i : 23,  k : 16 filename: ./examples/irmin_git_store.ml, word: Irmin 
# 1303, i : 23,  k : 22 filename: ./examples/irmin_git_store.ml, word: unix 