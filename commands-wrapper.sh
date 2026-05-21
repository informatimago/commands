#!/bin/bash
set -euo pipefail

script_dir="$(cd "$(dirname "$0")" && pwd -P)"
program_name="$(basename "$0")"
host_name="$(hostname -s 2>/dev/null || hostname 2>/dev/null || echo unknown)"
os_name="$(uname -s 2>/dev/null || echo unknown)"
arch_name="$(uname -m 2>/dev/null || echo unknown)"

candidates=()

if [ -n "${COMMANDS_BIN:-}" ] ; then
    case "$COMMANDS_BIN" in
        /*) candidates+=("$COMMANDS_BIN") ;;
        *)  candidates+=("$script_dir/$COMMANDS_BIN") ;;
    esac
fi

candidates+=(
    "$script_dir/commands-${os_name}-${arch_name}-${host_name}"
    "$script_dir/commands-${os_name}-${arch_name}"
    "$script_dir/commands-${arch_name}"
)

for candidate in "${candidates[@]}" ; do
    if [ -x "$candidate" ] && [ "$(cd "$(dirname "$candidate")" && pwd -P)/$(basename "$candidate")" != "$script_dir/commands" ] ; then
        exec -a "$program_name" "$candidate" "$@"
    fi
done

printf 'commands: no executable variant found for host=%s os=%s arch=%s\n' "$host_name" "$os_name" "$arch_name" >&2
printf 'commands: looked for:\n' >&2
for candidate in "${candidates[@]}" ; do
    printf '  %s\n' "$candidate" >&2
done
exit 69
