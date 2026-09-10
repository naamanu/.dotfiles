#!/usr/bin/env bash
# Claude Code status line — Starship-styled, Claude-relevant fields.
#
# Mirrors ~/.config/starship.toml's visual language (the bold-green ┌─ opener,
# "in <dir>" in bold cyan, "on  <branch>" in bold purple, [!?] flags in bold
# red) but replaces username/hostname — which never change during a session —
# with the model and context-window usage, which do.
#
#   ┌─ Opus 5  in ~/w/l/tools/ocaml-devtools on  main [!2?1]  42%
#
# Must never fail or stall: degrades to a bare directory if jq is missing,
# skips git entirely outside a repo, and uses --no-optional-locks so it can
# never block on a concurrent git process.

set -uo pipefail

input=$(cat)

# ---------- palette (matches starship.toml) ----------
G='\033[1;32m'   # bold green   — the ┌─ frame
B='\033[1;34m'   # bold blue    — model (was [username].style_user)
C='\033[1;36m'   # bold cyan    — [directory].style
P='\033[1;35m'   # bold purple  — [git_branch].style
R='\033[1;31m'   # bold red     — [git_status].style
Y='\033[1;33m'   # bold yellow  — context warning
D='\033[0;90m'   # bright-black — [git_state].style
X='\033[0m'      # reset

# ---------- payload ----------
if command -v jq >/dev/null 2>&1; then
  cwd=$(printf '%s' "$input"   | jq -r '.workspace.current_dir // .cwd // empty' 2>/dev/null)
  model=$(printf '%s' "$input" | jq -r '.model.display_name // empty' 2>/dev/null)
  used=$(printf '%s' "$input"  | jq -r '.context_window.used_percentage // empty' 2>/dev/null)
else
  cwd=""; model=""; used=""
fi
[ -n "$cwd" ] || cwd=$(pwd)

git() { command git --no-optional-locks -C "$cwd" "$@" 2>/dev/null; }

in_repo=false
git rev-parse --is-inside-work-tree >/dev/null && in_repo=true

# ---------- directory: truncate_to_repo = true, truncation_length = 3 ----------
if $in_repo; then
  root=$(git rev-parse --show-toplevel)
  if [ -n "$root" ]; then
    rel=${cwd#"$root"}
    path="$(basename "$root")${rel}"
  else
    path=${cwd/#$HOME/\~}
  fi
else
  path=${cwd/#$HOME/\~}
fi

# keep only the last 3 components
IFS='/' read -r -a _seg <<< "$path"
_n=${#_seg[@]}
if [ "$_n" -gt 3 ]; then
  path=$(printf '%s/%s/%s' "${_seg[_n-3]}" "${_seg[_n-2]}" "${_seg[_n-1]}")
fi

dir_part="${C}in ${path}${X}"
[ -w "$cwd" ] || dir_part="${dir_part}${R} 󰌾${X}"     # [directory].read_only

# ---------- git branch + status ----------
# One `git status --porcelain=v2 --branch` yields the branch, upstream
# ahead/behind counts and every file state, instead of five git invocations.
git_part=""
if $in_repo; then
  porcelain=$(git status --porcelain=v2 --branch)
  branch=$(printf '%s\n' "$porcelain" | sed -n 's/^# branch.head //p')
  [ "$branch" = "(detached)" ] && branch=$(git rev-parse --short HEAD)

  if [ -n "$branch" ]; then
    git_part=" ${P}on  ${branch}${X}"

    # Entries: "1 XY ..." ordinary, "2 XY ..." rename/copy, "u ..." unmerged,
    # "? path" untracked. XY is the staged/unstaged pair.
    staged=$(printf '%s\n'    "$porcelain" | grep -c '^[12] [MADRC]')
    modified=$(printf '%s\n'  "$porcelain" | grep -c '^[12] .[MD]')
    untracked=$(printf '%s\n' "$porcelain" | grep -c '^? ')
    conflict=$(printf '%s\n'  "$porcelain" | grep -c '^u ')

    flags=""
    [ "$conflict"  -gt 0 ] && flags="${flags}🏳"
    [ "$staged"    -gt 0 ] && flags="${flags}+${staged}"
    [ "$modified"  -gt 0 ] && flags="${flags}!${modified}"
    [ "$untracked" -gt 0 ] && flags="${flags}?${untracked}"

    ab=$(printf '%s\n' "$porcelain" | sed -n 's/^# branch.ab +\([0-9]*\) -\([0-9]*\)$/\1 \2/p')
    if [ -n "$ab" ]; then
      ahead=${ab%% *}
      behind=${ab##* }
      [ "${ahead:-0}"  -gt 0 ] 2>/dev/null && flags="${flags}⇡${ahead}"
      [ "${behind:-0}" -gt 0 ] 2>/dev/null && flags="${flags}⇣${behind}"
    fi

    [ -n "$flags" ] && git_part="${git_part} ${R}[${flags}]${X}"

    # [git_state] — mid-rebase/merge/bisect
    gd=$(git rev-parse --git-dir)
    state=""
    [ -d "$gd/rebase-merge" ] || [ -d "$gd/rebase-apply" ] && state="REBASING"
    [ -f "$gd/MERGE_HEAD" ]   && state="MERGING"
    [ -f "$gd/BISECT_LOG" ]   && state="BISECTING"
    [ -n "$state" ] && git_part="${git_part} ${D}(${state})${X}"
  fi
fi

# ---------- context usage ----------
ctx_part=""
if [ -n "$used" ]; then
  pct=${used%.*}
  if   [ "${pct:-0}" -gt 90 ] 2>/dev/null; then ctx_part="  ${R}${pct}%${X}"
  elif [ "${pct:-0}" -gt 70 ] 2>/dev/null; then ctx_part="  ${Y}${pct}%${X}"
  else                                          ctx_part="  ${D}${pct}%${X}"
  fi
fi

model_part=""
[ -n "$model" ] && model_part=" ${B}${model}${X}"

# %b (not %s) so the \033 escapes inside the assembled parts are interpreted.
printf '%b' "${G}┌─${X}${model_part} ${dir_part}${git_part}${ctx_part}"
