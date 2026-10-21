# Neovim Tutorials

> **Note:** this guide predates the near-stock Neovim setup (2026-10-21); plugins it mentions may be gone. The current setup is in `nvim.md`.

This tutorial set explains how to use the Neovim setup in this repository as an actual working environment.

These are practical guides for this exact config, not generic Neovim documentation.

Start here:

- **[Getting Started (Hands-On)](nvim-getting-started.md)** — a do-along walkthrough of files, motion, code, git, tasks, and notes with this exact config. **Begin here.**
- [Neovim Development Workflow](nvim-dev-workflow.md)
- [Neovim Research Workflow](nvim-research-workflow.md)
- [Neovim Note-Taking Workflow](nvim-note-taking-workflow.md)
- [Neovim Starting Projects](nvim-starting-projects.md)

Recommended reading order:

1. Do the [Getting Started (Hands-On)](nvim-getting-started.md) lesson to get productive fast.
2. Read [Neovim setup overview](nvim.md) for the map and plugin inventory.
3. Read [Neovim Development Workflow](nvim-dev-workflow.md) to learn the core motion, search, code, task, and review loop.
4. Read [Neovim Research Workflow](nvim-research-workflow.md) if you use Python, Markdown, and external notebook tools.
5. Read [Neovim Note-Taking Workflow](nvim-note-taking-workflow.md) if you want a lightweight notes system inside the same editor.
6. Read [Neovim Starting Projects](nvim-starting-projects.md) when you want repeatable project bootstrap routines.

The important idea is that this setup is built around a few centers:

- Telescope for finding and searching
- Oil for file movement and file creation
- native LSP for code intelligence
- Conform and linting for cleanup
- Overseer for scripts, builds, tests, and one-off execution
- Python support through Ruff, Ty, pytest, and `uv`
