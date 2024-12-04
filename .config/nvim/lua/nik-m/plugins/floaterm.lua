return {
    'voldikss/vim-floaterm',  -- Floaterm plugin
    config = function()
      vim.g.floaterm_width = 0.8  -- Set floating terminal width (as a fraction of screen width)
      vim.g.floaterm_height = 0.8  -- Set floating terminal height (as a fraction of screen height)
      vim.g.floaterm_position = 'center'  -- Positioning of the floating window (center, top, bottom, left, right)
    end
  }
