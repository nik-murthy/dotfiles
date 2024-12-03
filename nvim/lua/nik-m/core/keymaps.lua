vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Keymaps for better default experience
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- Space + s saves the file
vim.keymap.set("n", "<Leader>s", ":write<CR>", { silent = true })

-- Move normally between wrapped lines
vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Move to first symbol on the line
vim.keymap.set("n", "H", "^")

-- Move to last symbol of the line
vim.keymap.set("n", "L", "$")

-- Shift + q - Quit
vim.keymap.set("n", "Q", "<C-W>q")

-- vv - Makes vertical split
vim.keymap.set("n", "vv", "<C-W>v")
-- ss - Makes horizontal split
vim.keymap.set("n", "ss", "<C-W>s")

-- Quick jumping between splits
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-l>", "<C-w>l")

-- Indenting in visual mode (tab/shift+tab)
vim.keymap.set("v", "<Tab>", ">gv")
vim.keymap.set("v", "<S-Tab>", "<gv")

-- Move to the end of yanked text after yank and paste
vim.cmd("vnoremap <silent> y y`]")
vim.cmd("vnoremap <silent> p p`]")
vim.cmd("nnoremap <silent> p p`]")

-- Space + Space to clean search highlight
vim.keymap.set("n", "<Leader>h", ":noh<CR>", { silent = true })

-- Fixes pasting after visual selection.
vim.keymap.set("v", "p", '"_dP')

-- set jj to esc
vim.keymap.set("i", "jk", "<Esc>", { noremap = true, silent = true })

-- fzf keymaps
vim.keymap.set("n", "<leader>p", require("fzf-lua").files, { desc = "FZF Files" })
vim.keymap.set("n", "<leader><leader>", require("fzf-lua").resume, { desc = "FZF Resume" })
vim.keymap.set("n", "<leader>r", require("fzf-lua").registers, { desc = "Registers" })
vim.keymap.set("n", "<leader>m", require("fzf-lua").marks, { desc = "Marks" })
vim.keymap.set("n", "<leader>k", require("fzf-lua").keymaps, { desc = "Keymaps" })
vim.keymap.set("n", "<leader>f", require("fzf-lua").live_grep, { desc = "FZF Grep" })
vim.keymap.set("n", "<leader>b", require("fzf-lua").buffers, { desc = "FZF Buffers" })
vim.keymap.set("v", "<leader>8", require("fzf-lua").grep_visual, { desc = "FZF Selection" })
vim.keymap.set("n", "<leader>7", require("fzf-lua").grep_cword, { desc = "FZF Word" })
vim.keymap.set("n", "<leader>j", require("fzf-lua").helptags, { desc = "Help Tags" })
vim.keymap.set("n", "<leader>gc", require("fzf-lua").git_bcommits, { desc = "Browse File Commits" })
vim.keymap.set("n", "<leader>gs", require("fzf-lua").git_status, { desc = "Git Status" })
vim.keymap.set("n", "<leader>s", require("fzf-lua").spell_suggest, { desc = "Spelling Suggestions" })
vim.keymap.set("n", "<leader>cj", require("fzf-lua").lsp_definitions, { desc = "Jump to Definition" })
