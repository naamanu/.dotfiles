# Neovim Tutorials

Practical guides for the Neovim setup in this repository — this exact
config, not generic Neovim documentation.

Start here:

- **[Getting Started (Hands-On)](nvim-getting-started.md)** — a do-along walkthrough of files, search, code, formatting, tasks and git with this config. **Begin here.**
- [Neovim Development Workflow](nvim-dev-workflow.md)
- [Neovim Research Workflow](nvim-research-workflow.md)
- [Neovim Note-Taking Workflow](nvim-note-taking-workflow.md)
- [Neovim Starting Projects](nvim-starting-projects.md)

Recommended reading order:

1. Do the [Getting Started (Hands-On)](nvim-getting-started.md) lesson to get productive fast.
2. Read the [Neovim setup overview](nvim.md) for every plugin and key.
3. Read the [Development Workflow](nvim-dev-workflow.md) for the read / edit / run / review loop.
4. Read the [Research Workflow](nvim-research-workflow.md) if you use Python, Markdown and SQL for research.
5. Read the [Note-Taking Workflow](nvim-note-taking-workflow.md) for Markdown notes next to your code.
6. Read [Starting Projects](nvim-starting-projects.md) for project shapes the tooling attaches to.

The setup is near-stock Neovim 0.12 built around a few centers:

- the snacks picker for finding and searching
- oil for moving and creating files
- the built-in LSP client, with servers installed by mason
- blink.cmp for completion and conform for format on save
- mini.ai and mini.surround for text objects and surrounding pairs
- `core/tasks.lua` for builds, tests, watch mode and REPLs
- gitsigns, codediff and LazyGit for review
- Python through basedpyright, ruff, pytest, debugpy and `uv`
