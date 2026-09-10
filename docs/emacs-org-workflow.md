# Emacs Notes Workflow

This document covers the notes side of the Emacs setup in this repository:

- inbox capture and tasks (Org)
- durable, linked notes (Denote)
- writing comfort (org-modern, olivetti, jinx)
- presentations (org-present)
- terminal-agent assistance via the shared DEV_AGENT

There are deliberately two layers, and the split matters. **Org** is for
things with a *state* — a task that is TODO and later DONE, an agenda that
changes daily. **Denote** is for things with a *life* — an idea you will link
to, revisit and build on. Putting durable notes in an agenda file buries them;
putting tasks in Denote means nothing ever surfaces them.

---

## 1. Org: capture and agenda

Org owns three files under `~/org/`:

| File | Holds |
| :--- | :--- |
| `inbox.org` | Tasks and unsorted ideas |
| `projects.org` | Active and someday project work |
| `journal.org` | Older dated entries; new ones are Denote journal notes (`C-c n j`) |

### Capture

`C-c c` works from anywhere, mid-edit included.

| Key | Template | Goes to |
| :--- | :--- | :--- |
| `t` | Todo | `inbox.org` → Tasks |
| `i` | Idea | `inbox.org` → Ideas |
| `p` | Project task | `projects.org` → Active |

Finish with `C-c C-c`, abandon with `C-c C-k`. Each capture records a
`CREATED` timestamp and a link back to the exact file and line you were
looking at — which is most of the value when you capture from inside code.

### States and tags

```
TODO → NEXT → WAIT → DONE / CANCELLED
```

`WAIT` and `CANCELLED` prompt for a note, so you record *why*. Completion is
timestamped, and notes go into a drawer to keep the outline readable.

Tags: `@research`, `@coding`, `@writing`, `@admin`.

### Agenda

`C-c a` opens the agenda; `C-c a d` is the dashboard — today, then `NEXT`,
then the inbox, then `@coding` and `@research`. `C-c a r` and `C-c a w`
filter to research and writing.

Refile with `C-c C-w` (two levels deep, across all agenda files).

---

## 2. Denote: durable notes

Denote stores notes as plain files in `~/notes/`, with everything the system
needs encoded in the filename:

```
20260726T091235--eglot-setup-notes__emacs_tools.org
└── identifier ─┘  └─── title ───┘  └── keywords ─┘
```

There is no database. Nothing to rebuild, corrupt, or sync. `ripgrep` and
`find` work on your notes because they are just files, and they will still
open in any editor in ten years.

| Key | Action |
| :--- | :--- |
| `C-c n n` | New note — prompts for title, then keywords |
| `C-c n l` | Insert a link to another note |
| `C-c n b` | Show backlinks to this note |
| `C-c n r` | Rename or retag (rewrites the filename correctly) |
| `C-c n f` | Find a note by filename |
| `C-c n s` | Ripgrep across all notes |

A new note opens with front matter filled in:

```org
#+title:      Eglot setup notes
#+date:       [2026-07-26 Sun 09:12]
#+filetags:   :emacs:tools:
#+identifier: 20260726T091235
```

Note that `C-c n n` creates the *buffer*; the file lands on disk when you save
with `C-x C-s`. That is deliberate — an abandoned note leaves nothing behind.

### Linking

`C-c n l` inserts a link by identifier, not by filename, so renaming a note
never breaks a link pointing at it. Follow links with `RET`, come back with
`C-c &`. `C-c n b` lists everything pointing here.

The habit that makes this pay off: when a note grows a second idea, split it
and link the two. Small, densely linked notes beat large ones.

---

## 3. Writing

Org buffers get three things automatically:

- **org-modern** — clean rendering of headings, blocks, checkboxes, timestamps
- **olivetti** — text centred at ~90 columns
- **jinx** — just-in-time spellcheck; `M-$` corrects the word at point

Toggle centring with `C-c t o` and line wrapping with `C-c t w`.

---

## 4. Presentations

Slides are an ordinary org file: top-level headings become slides.

```org
* What we shipped
  - Streaming parser
  - 40% lower p99

* What broke
  #+begin_src sh
  cargo bench --bench parse
  #+end_src
```

`M-x org-present` to start.

| Key | Action |
| :--- | :--- |
| `<right>` / `<left>` | Next / previous slide |
| `C-c C-q` | Quit |

Entering scales the font up, hides chrome and line numbers, shows inline
images, and makes the buffer read-only so a stray keystroke cannot edit your
deck mid-talk. Leaving restores everything.

Since a deck is just org, code blocks stay syntax-highlighted and you can jump
into a live buffer to demo, then come back.

---

## 5. Terminal agent

Use `C-c g g` to open the shared coding agent at the project root and `C-c g c` to copy a current file/line reference before opening it. The agent defaults to Codex, then Claude, and can be overridden with `DEV_AGENT`.

---

## A working rhythm

**Capture without thinking.** `C-c c t` mid-task. Sorting later is cheaper
than losing the thought.

**Process the inbox on a fixed cadence.** `C-c n i`, then for each item:
delete it, refile it to a project (`C-c C-w`), or promote it to a Denote note
if it is an idea rather than a task.

**Write notes when you learn something, not when you finish.** A note written
while confused is more useful later than a tidy summary written once it seems
obvious.

**Link aggressively.** An unlinked note is one you will never find again.

---

## Short reference

```
Capture / agenda
  C-c c        capture           C-c a       agenda
  C-c a d      dashboard         C-c C-w     refile

Denote
  C-c n n      new note          C-c n l     insert link
  C-c n b      backlinks         C-c n r     rename / retag
  C-c n f      find note         C-c n s     search notes

Org files
  C-c n i      inbox             C-c n p     projects
  C-c n j      today's journal note (Denote)

Writing / presenting
  M-$          spellcheck        C-c t o     centred text
  C-c t w      visual line       M-x org-present

LLM
  C-c g        send region       C-c G       chat buffer
```
