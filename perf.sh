#!/bin/bash

set -eo pipefail

if [[ "$(sysctl kernel.perf_event_paranoid)" != "-1" ]]; then
    sudo sysctl kernel.perf_event_paranoid=-1
fi

if [[ "$(cat /proc/sys/kernel/kptr_restrict)" != "0" ]]; then
    sudo sh -c " echo 0 > /proc/sys/kernel/kptr_restrict"
fi

perf record --freq=99 -g --call-graph fp \
    -- ./_build/luau_example torch7/generated/luau_test.lua

perf script > out.perf
/home/misi5/home/nvme_ssd/FlameGraph/stackcollapse-perf.pl --addr out.perf > out.folded
/home/misi5/home/nvme_ssd/FlameGraph/flamegraph.pl out.folded > flame_graph.svg
firefox flame_graph.svg &

