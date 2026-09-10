return {
  "mistweaverco/kulala.nvim",
  ft = { "http", "rest" },
  keys = {
    {
      "<leader>Rs",
      function()
        require("kulala").run()
      end,
      desc = "HTTP send request",
    },
    {
      "<leader>Ra",
      function()
        require("kulala").run_all()
      end,
      desc = "HTTP send all",
    },
    {
      "<leader>Rt",
      function()
        require("kulala").toggle_view()
      end,
      desc = "HTTP toggle view",
    },
    {
      "<leader>Rl",
      function()
        require("kulala").replay()
      end,
      desc = "HTTP replay last",
    },
    {
      "<leader>Rc",
      function()
        require("kulala").copy()
      end,
      desc = "HTTP copy as curl",
    },
    {
      "<leader>Ri",
      function()
        require("kulala").inspect()
      end,
      desc = "HTTP inspect request",
    },
    {
      "<leader>R]",
      function()
        require("kulala").jump_next()
      end,
      desc = "HTTP next request",
    },
    {
      "<leader>R[",
      function()
        require("kulala").jump_prev()
      end,
      desc = "HTTP prev request",
    },
    {
      "<leader>Re",
      function()
        require("kulala").set_selected_env()
      end,
      desc = "HTTP select env",
    },
  },
  opts = {
    default_view = "body",
    default_env = "dev",
    debug = false,
  },
}
