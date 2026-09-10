# Emacs TypeScript Workflow

This document is a hands-on walkthrough of building a small TypeScript project entirely inside the Emacs setup in this dotfiles repository.

It is the TypeScript companion to [Emacs Development Workflow](emacs-dev-workflow.md), which covers the same ground using Python. Read either one first; the navigation, search, and Git sections apply to both.

You will build `wordcount`, a small CLI that prints the most frequent words in a text file. It is small enough to finish in one sitting and real enough to exercise the whole loop: project creation, LSP, formatting, compilation, tests, notes, and a commit.

If you are on a new machine, run `C-c e h` first. It reports which external tools are present and is the fastest way to spot a missing language server.

---

## 1. What the TypeScript path depends on

| Component | Provided by | Notes |
| :--- | :--- | :--- |
| Syntax, indentation, imenu | `typescript-ts-mode` / `tsx-ts-mode` | Needs the `typescript` and `tsx` tree-sitter grammars |
| Code intelligence | `eglot` + `vtsls` | Install with `npm install -g @vtsls/language-server` |
| Formatting on save | `apheleia` + `prettier` | Runs in the background, does not move point |
| Type checking | `tsc` from the project's devDependencies | Driven by `C-c p m` |
| Terminal | `vterm` | Native module; needs `cmake` and GNU `libtool` (`glibtool`) to build |

Emacs ships tree-sitter support but no grammars. If highlighting is missing in a `.ts` buffer, install them with `C-c e g` (`my/install-missing-grammars`).

This compiles every grammar in `treesit-language-source-alist` into `~/.emacs.d/tree-sitter/`. Verify with `M-:` and `(treesit-language-available-p 'typescript)`.

`C-c e h` reports the whole external toolchain at once, which is the faster way to spot a missing server or formatter.

---

## 2. Create the project

Open a terminal without leaving Emacs with `M-x vterm`:

```sh
mkdir -p ~/workspace/projects/wordcount
cd ~/workspace/projects/wordcount
git init
npm init -y
npm install -D typescript @types/node
mkdir src
```

Node 24 executes TypeScript directly through type stripping, so there is no build step and no `ts-node`. The `typescript` package is only needed for type *checking*.

---

## 3. Register and open the project

`project.el` detects a root from `.git`, so this directory already qualifies. To add it to the switch list explicitly, open any file in it and press `C-c p a`.

From then on:

| Key | Action |
| :--- | :--- |
| `C-c p p` | Switch project |
| `C-c p P` | Switch project and jump straight to find-file |
| `C-c p f` | Find a file in the current project |
| `C-c p b` | Switch buffer within the project |
| `C-c p o` | Find a file from the project root (new files too; `C-c p D` is Dired there) |

`C-c p f` matches with Orderless, so `src ts` finds `src/index.ts`. Fragments may be typed in any order and need not be contiguous.

---

## 4. Configure the project

Open `package.json` with `C-c p f` and set the scripts:

```json
{
  "name": "wordcount",
  "type": "module",
  "scripts": {
    "start": "node src/index.ts",
    "test": "node --test \"src/**/*.test.ts\"",
    "typecheck": "tsc --noEmit"
  }
}
```

`"type": "module"` is required. Without it Node treats `.ts` as CommonJS and rejects the `import` statements.

These script names matter to this config specifically:

- `my/js-mode-defaults` sets the buffer-local `compile-command` to the `typecheck` script, falling back to `build`. That is what `C-c p m` offers.
- `my/node-test-command` looks for the `test` script, and detects npm, pnpm, yarn, or bun from the lockfile. That is what `C-c p t` runs.

Next, create `tsconfig.json` in the project root:

```json
{
  "compilerOptions": {
    "target": "es2023",
    "module": "nodenext",
    "moduleResolution": "nodenext",
    "allowImportingTsExtensions": true,
    "rewriteRelativeImportExtensions": true,
    "noEmit": true,
    "strict": true,
    "types": ["node"]
  },
  "include": ["src"]
}
```

`allowImportingTsExtensions` permits `import ... from "./wordcount.ts"`, which is the form Node wants at runtime.

Save with `C-x C-s`. Apheleia reformats the file with prettier in the background without moving the cursor.

---

## 5. Write the core module

Open `src/wordcount.ts`. The mode line should read `TypeScript`, and eglot should attach within a second or two.

```typescript
export function wordFrequency(text: string): Map<string, number> {
  const counts = new Map<string, number>();
  for (const word of text.toLowerCase().match(/[a-z']+/g) ?? []) {
    counts.set(word, (counts.get(word) ?? 0) + 1);
  }
  return counts;
}

export function topWords(text: string, n: number): [string, number][] {
  return [...wordFrequency(text)]
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .slice(0, n);
}
```

While typing this:

- Corfu appears after two characters. `TAB` cycles, `RET` inserts. Type `counts.` to get real `Map` methods from the language server, with documentation alongside after a short delay.
- Rainbow delimiters colour nesting depth, which helps in the `.sort()` chain.
- Line numbers are relative, so `M-8 C-p` moves up exactly eight lines.
- `tab-always-indent` is `complete` in programming buffers, so `TAB` indents when indentation is wrong and completes when it is already correct.

---

## 6. Use the LSP commands

Place the cursor on `wordFrequency` inside `topWords`:

| Key | Action |
| :--- | :--- |
| `C-c l d` | Jump to definition |
| `C-c l D` | Find all references |
| `C-c l r` | Rename across the project |
| `C-c l a` | Code actions, including import fixes |
| `C-c l i` | Find implementation |
| `C-c l e` | List diagnostics for the buffer |
| `C-c l n` / `C-c l p` | Next / previous diagnostic (`M-g f` lists them) |

`C-c l r` performs a real project-wide rename through the language server, not a text substitution.

To see flymake work, temporarily change `n: number` to `n: string`. The call site underlines and the diagnostic appears at the end of the line. Undo with `C-/`.

---

## 7. Write the entry point

Open `src/index.ts`:

```typescript
import { readFileSync } from "node:fs";
import { topWords } from "./wordcount.ts";

const path = process.argv[2];
if (!path) {
  console.error("usage: wordcount <file>");
  process.exit(1);
}

for (const [word, count] of topWords(readFileSync(path, "utf8"), 5)) {
  console.log(`${count.toString().padStart(4)}  ${word}`);
}
```

Note the `.ts` extension in the import. That is `allowImportingTsExtensions` in use.

---

## 8. Typecheck

```
C-c p m
```

`my/project-compile` saves modified buffers, changes to the project root, and pre-fills the minibuffer with `npm run typecheck`. Press `RET`.

In the `*compilation*` buffer:

- `RET` on an error jumps to that source line.
- `M-g n` and `M-g p` step through errors from anywhere.
- `g` re-runs the last command.

`compilation-scroll-output` is set to `first-error`, so the buffer scrolls but stops at the first problem instead of running past it.

To see it fail, add a deliberate type error and re-run:

```typescript
const bad: number = topWords("x", 1);
```

```
src/index.ts(14,7): error TS2322: Type '[string, number][]' is not assignable to type 'number'.
```

Remove the line before continuing.

---

## 9. Write and run tests

Open `src/wordcount.test.ts`:

```typescript
import { test } from "node:test";
import assert from "node:assert/strict";
import { topWords, wordFrequency } from "./wordcount.ts";

test("counts repeated words", () => {
  assert.equal(wordFrequency("the cat the hat").get("the"), 2);
});

test("ranks by frequency then alphabetically", () => {
  assert.deepEqual(topWords("b a a", 2), [["a", 2], ["b", 1]]);
});
```

Run with:

```
C-c p t
```

`my/project-test` is mode-aware. In a TypeScript buffer it resolves to the npm `test` script; in Rust it would offer `cargo test`, in OCaml `dune test`, in Haskell `cabal test`. The key is the same everywhere.

Then run the program itself with `C-c p v` to open vterm at the project root:

```sh
printf 'the quick brown fox the lazy dog the fox\n' > sample.txt
npm start sample.txt
```

```
   3  the
   2  fox
   1  brown
   1  dog
   1  lazy
```

---

## 10. Navigate the code

| Key | Action |
| :--- | :--- |
| `C-c s s` | Ripgrep the current project |
| `C-c s r` | Ripgrep, prompting for the directory |
| `C-c s l` | Search lines in the current buffer |
| `C-c s i` | Jump to a symbol in this file |
| `C-c s o` | Jump by outline heading |
| `C-x b` | Switch buffer, including recent files and bookmarks |
| `M-g g` | Go to line |

Two things worth practising:

**Live preview.** Moving down the result list in `C-c s s` shows each hit in place, so the search doubles as a way to read the codebase.

**Editable grep.** Run `C-c s s`, press `C-.` for embark, then `E` to export the results to a grep buffer. Enable wgrep with `C-c C-p`, edit the matches as ordinary text, and write every change back with `C-c C-c`. This is the fastest multi-file replace in this config.

---

## 11. Take notes

Two levels, depending on how permanent the thought is.

**Quick capture.** `C-c c` works from anywhere, including mid-edit. Press `t` for a TODO, `i` for an idea, `p` for a project task. Finish with `C-c C-c`, abort with `C-c C-k`. The captured entry records a `CREATED` timestamp and a link back to the exact file and line you were viewing.

Review with `C-c a d`, the agenda Dashboard: today's schedule, inbox, `NEXT` actions, and anything tagged `@research` or `@coding`.

**Durable notes.** `C-c n n` creates a Denote note in `~/notes/` — a plain file whose name encodes the date, title and keywords, with no database behind it.

| Key | Action |
| :--- | :--- |
| `C-c n n` | New note (title, then keywords) |
| `C-c n l` | Insert a link to another note |
| `C-c n b` | Backlinks to this note |
| `C-c n r` | Rename or retag |
| `C-c n f` | Find a note |
| `C-c n s` | Ripgrep all notes |

Write up the design decision you just made — why `topWords` breaks ties alphabetically, say — tag it `typescript`, and link it from the project note. Save with `C-x C-s`; until you do, `C-c n n` has only made a buffer.

Org and Denote buffers get org-modern for clean rendering, Olivetti for centred text, and Jinx for spellcheck (`M-$` to correct). See [the notes workflow guide](emacs-org-workflow.md) for the full picture.

---

## 12. Commit with Magit

`C-x g` opens the status buffer. Add a `.gitignore` first:

```
node_modules/
*.log
```

Press `g` in Magit to refresh, then:

| Key | Action |
| :--- | :--- |
| `TAB` | Expand or collapse the diff under point |
| `s` | Stage the file, hunk, or selected region |
| `u` | Unstage |
| `k` | Discard, with confirmation |
| `RET` | Visit the file at that line |

Partial staging is the important trick: select lines with `C-SPC`, then `s` stages only those. That is how a messy working tree becomes a series of clean commits.

Commit with `c c`. A message buffer opens with the diff beside it:

```
Add wordcount CLI with frequency ranking

Counts word frequency in a text file and prints the top N by
count, breaking ties alphabetically. Runs TypeScript directly
on Node 24, so there is no build step.
```

`C-c C-c` finishes, `C-c C-k` cancels.

The rest of the vocabulary:

| Key | Action |
| :--- | :--- |
| `c a` | Amend the last commit |
| `b b` | Checkout a branch |
| `b c` | Create and checkout a branch |
| `l l` | Log |
| `d d` | Diff |
| `P p` | Push |
| `F p` | Pull |
| `z z` | Stash |
| `?` | Full menu for the current context |

Throughout, diff-hl marks added, changed, and deleted lines in the left fringe, updating live and refreshing after each Magit operation.

---

## 13. The loop, compressed

```
C-c p p     switch to the project
C-c p f     open a file
   ...      edit; corfu completes, eglot checks, prettier formats on save
C-c l d     jump to a definition
C-c s s     search the project
C-c p m     typecheck
C-c p t     run tests
C-c c       capture a thought
C-x g       stage and commit
```

---

## Short Reference

**Projects** — `C-c p`

```
p switch    P switch+find   f find file   b buffer    a remember
d find dir  D dired         o root dired  O switch+dired
m compile   t test          s search      v vterm     V switch+vterm
```

**Search** — `C-c s`

```
r ripgrep   s project search   l line   o outline   i imenu
```

**LSP** — `C-c l`

```
d definition   D references   i implementation   t type definition
r rename       a code action  f format
e diagnostics  n next error   p previous error
```

**Notes** — `C-c n`

```
n new note     l insert link   b backlinks   r rename/retag
f find note    s search notes
i inbox        p projects      j journal
```

**Debug** — `C-c d`

```
d start   b breakpoint   n next   i step in   o step out   c continue   q quit
```

**Global**

```
C-x g   magit            C-c a   agenda          C-c c   capture
C-x b   switch buffer    C-.     embark act      M-$     spellcheck
M-o     other window     C-c w   delete window   M-y     yank history
C-c t t toggle theme     C-c e h health check    C-c e g install grammars
C-c g g terminal agent    C-c g c copy context      <escape> quit
```

**REPLs** — `C-c f`

```
o ocaml   p python   n node   s sql   e elisp
```

When unsure of a prefix, pause and let which-key list the options. `C-h B` shows every binding active in the current buffer.
