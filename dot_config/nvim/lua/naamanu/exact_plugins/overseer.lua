return {
  "stevearc/overseer.nvim",
  cmd = {
    "OverseerRun",
    "OverseerToggle",
    "OverseerOpen",
  },
  keys = {
    { "<leader>or", "<cmd>OverseerRun<cr>", desc = "Overseer: Run task" },
    {
      "<leader>os",
      function()
        require("naamanu.core.tasks").run_package_script()
      end,
      desc = "Overseer: Run package script",
    },
    {
      "<leader>od",
      function()
        require("naamanu.core.tasks").run_named_package_script("dev")
      end,
      desc = "Overseer: Run dev script",
    },
    {
      "<leader>ol",
      function()
        require("naamanu.core.tasks").run_named_package_script("lint")
      end,
      desc = "Overseer: Run lint script",
    },
    {
      "<leader>oy",
      function()
        require("naamanu.core.tasks").run_named_package_script("typecheck")
      end,
      desc = "Overseer: Run typecheck script",
    },
    {
      "<leader>of",
      function()
        require("naamanu.core.tasks").run_named_package_script("format")
      end,
      desc = "Overseer: Run format script",
    },
    {
      "<leader>ob",
      function()
        require("naamanu.core.tasks").run_project_build()
      end,
      desc = "Overseer: Build project",
    },
    {
      "<leader>op",
      function()
        require("naamanu.core.tasks").run_python_file()
      end,
      desc = "Overseer: Run Python file",
    },
    {
      "<leader>oT",
      function()
        require("naamanu.core.tasks").run_python_tests()
      end,
      desc = "Overseer: Run Python tests",
    },
    {
      "<leader>on",
      function()
        require("naamanu.core.tasks").run_project_tests()
      end,
      desc = "Overseer: Run project tests",
    },
    {
      "<leader>oC",
      function()
        require("naamanu.core.tasks").compile_c_family_file()
      end,
      desc = "Overseer: Compile C/C++ file",
    },
    { "<leader>ot", "<cmd>OverseerToggle<cr>", desc = "Overseer: Toggle panel" },
    { "<leader>oa", "<cmd>OverseerTaskAction<cr>", desc = "Overseer: Task action" },
  },
  opts = {
    strategy = "terminal",
    task_list = {
      direction = "bottom",
      min_height = 15,
      default_detail = 1,
    },
  },
}
