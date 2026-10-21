# Neovim Note-Taking Workflow

> **Note:** this guide predates the near-stock Neovim setup (2026-10-21); plugins it mentions may be gone. The current setup is in `nvim.md`.

This guide explains how to use this Neovim setup for lightweight note-taking without turning the editor into a separate personal knowledge system.

The emphasis here is:

- fast file creation
- fast retrieval
- readable Markdown
- easy movement between notes and code

If you want heavyweight task capture, agenda views, and citation workflows, the Emacs Org setup in this repo is still the stronger system. This Neovim setup is better for notes that live close to your projects and code.

---

## 0. `~/notes` is a Denote directory shared with Emacs

Notes created with `<leader>nn` are byte-compatible with Emacs `denote`
(`denote-file-type 'markdown-yaml`): `ID--title-slug__tag1_tag2.md`, the
same aligned YAML front matter, `[title](denote:ID)` links, journal entries in
`journal/`. Either editor can create, link, rename and backlink any note.
The keys are listed in `nvim.md` → *Notes*. Set `NOTES_DIR` / `BIBLIOGRAPHY`
to move the directory or the shared Zotero export.

---

## 1. The right use case

Use Neovim notes for:

- project notes
- design logs
- debugging journals
- research scratchpads
- meeting notes tied to code work
- architecture sketches in Markdown

Do not overcomplicate it. The strength of this setup is that notes live near the work.

---

## 2. Prefer Markdown and normal directories

This config already supports Markdown well.

Use simple project-local structures like:

```text
project/
  notes/
    index.md
    decisions.md
    todo.md
    2026-04-02-debug-log.md
```

or a personal notes tree like:

```text
~/notes/
  inbox/
  projects/
  research/
  archive/
```

You do not need a special note database to be productive here.

---

## 3. Create and organize notes quickly

The main file workflow tools are:

| Key | Action |
| :--- | :--- |
| `-` | open parent directory in Oil |
| `<leader>e` | open Oil |
| `<leader>ff` | find files |
| `<leader>fr` | recent files |

Practical creation flow:

1. Open the target directory with `-` or `<leader>e`.
2. Create or rename files directly through Oil.
3. Open the note and start writing.
4. Use `<leader>fr` later to reopen it quickly.

This is intentionally simple. Directory discipline matters more than plugin complexity.

---

## 4. Search notes constantly

The real power tool is search, not browsing.

Important keys:

| Key | Action |
| :--- | :--- |
| `<leader>fg` | search across all notes in the repo or current working tree |
| `<leader>f/` | search inside the current note |
| `<leader>fc` | grep the word under cursor |
| `<leader>ff` | find note by filename |
| `<leader>f.` | resume the last search |

Best practice:

- search note content with `fg`
- search structure within one note with `f/`
- use filenames for stable entry points like `index.md`, `decisions.md`, or `todo.md`

---

## 5. Make notes readable while editing

Markdown rendering is already configured, so headings, bullets, checkboxes, and code blocks read cleanly in the buffer.

That means Markdown can carry more of the workflow than plain text usually can.

Good patterns:

- use headings aggressively
- use checkboxes for short operational lists
- keep code snippets inline in the note if they explain the thought
- move durable code into source files once it stops being explanatory

---

## 6. Link notes to active work

Use Telescope and buffer navigation when a note is part of the working set.

| Key | Action |
| :--- | :--- |
| `<leader>fb` | find open buffers |
| `<leader>fr` | find recent files |
| `<leader>fg` | search across notes and code |

This works especially well when you pin:

- one design note
- one task list
- one main implementation file
- one test file

That turns the note into part of the actual development loop instead of an external document.

---

## 7. Use notes during debugging and reviews

A strong pattern is to keep a running note open while debugging:

- symptom
- reproduction steps
- hypotheses
- commands run
- conclusion

Then use:

- `<leader>gp` to inspect code diffs
- `<leader>gh` for file history
- `<leader>fg` to search previous notes for similar problems

The point is to leave behind a durable trail, not just solve the issue once.

---

## 8. Suggested note templates

### Debug log

```md
# Debug log: issue name

## Symptom

## Reproduction

## Hypotheses

## Findings

## Fix

## Follow-up
```

### Project note

```md
# Project name

## Goal

## Current state

## Open questions

## Next actions
```

### Reading note

```md
# Paper or article title

## Thesis

## Key ideas

## What matters for my work

## Open questions
```

---

## 9. A practical lightweight notes routine

One simple routine that fits this setup:

1. Keep a `notes/` directory in active repos.
2. Create `index.md`, `todo.md`, and date-stamped logs as needed.
3. Use Telescope grep instead of manually browsing old notes.
4. Keep the active note in a buffer and reopen it with `<leader>fb` or `<leader>fr`.
5. Archive or compress notes once the project stabilizes.

That gives you a useful notes system without building a second job around note management.
