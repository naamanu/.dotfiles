return {
  "mfussenegger/nvim-dap",
  dependencies = {
    { "rcarriga/nvim-dap-ui", dependencies = { "nvim-neotest/nvim-nio" }, opts = {} },
  },
  keys = {
    { "<leader>dd", function() require("dap").continue() end, desc = "Debug start/continue" },
    { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Debug breakpoint" },
    { "<leader>dn", function() require("dap").step_over() end, desc = "Debug step over" },
    { "<leader>di", function() require("dap").step_into() end, desc = "Debug step into" },
    { "<leader>do", function() require("dap").step_out() end, desc = "Debug step out" },
    { "<leader>dq", function() require("dap").terminate() end, desc = "Debug terminate" },
    { "<leader>du", function() require("dapui").toggle() end, desc = "Debug UI" },
  },
  config = function()
    local dap = require("dap")
    dap.listeners.before.attach.dotfiles = function() require("dapui").open() end
    dap.listeners.before.launch.dotfiles = function() require("dapui").open() end
    dap.listeners.before.event_terminated.dotfiles = function() require("dapui").close() end
    dap.listeners.before.event_exited.dotfiles = function() require("dapui").close() end

    -- Python: resolve the interpreter per launch, the same way core/lsp.lua
    -- does for basedpyright, so the debuggee runs inside the project venv
    -- and can import its dependencies. debugpy has to live in that venv too
    -- (`uv add --dev debugpy`); a global `python` was never the right one.
    local function project_python()
      return require("naamanu.core.tasks").python_executable()
    end
    dap.adapters.python = function(callback)
      local python = project_python()
      local probe = vim.system({ python, "-c", "import debugpy" }):wait()
      if probe.code ~= 0 then
        vim.notify(
          ("debugpy is not installed for %s\nInstall it into the project: uv add --dev debugpy"):format(python),
          vim.log.levels.ERROR,
          { title = "nvim-dap" }
        )
        return
      end
      callback({ type = "executable", command = python, args = { "-m", "debugpy.adapter" } })
    end
    dap.configurations.python = {
      {
        type = "python",
        request = "launch",
        name = "Current file",
        program = "${file}",
        cwd = "${workspaceFolder}",
        pythonPath = project_python,
      },
    }

    if vim.fn.executable("lldb-dap") == 1 then
      dap.adapters.lldb = { type = "executable", command = "lldb-dap", name = "lldb" }
      local native = {{ type = "lldb", request = "launch", name = "Launch executable", program = function() return vim.fn.input("Executable: ", vim.fn.getcwd() .. "/", "file") end, cwd = "${workspaceFolder}", stopOnEntry = false }}
      dap.configurations.c = native
      dap.configurations.cpp = native
      dap.configurations.rust = native
    end

    -- Go: let nvim-dap own the dlv process. "${port}" is allocated fresh per
    -- session and the server is waited on and torn down with the session,
    -- unlike a detached jobstart on a fixed port, which leaked dlv across
    -- sessions and raced a 100ms timer against its startup.
    if vim.fn.executable("dlv") == 1 then
      dap.adapters.go = {
        type = "server",
        port = "${port}",
        executable = { command = "dlv", args = { "dap", "-l", "127.0.0.1:${port}" } },
      }
      dap.configurations.go = {{ type = "go", name = "Debug package", request = "launch", program = "${fileDirname}" }}
    end
  end,
}
