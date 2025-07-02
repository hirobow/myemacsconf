# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal Emacs configuration repository with a single `.emacs` configuration file. The configuration is tailored for modern web development with extensive AI/ML tool integrations.

## Key Configuration Details

### Package Management
- Uses built-in `package.el` with MELPA and ELPA repositories
- Configuration is organized using the `leaf` package management system (a modern alternative to `use-package`)

### Development Environment

#### Supported Languages and File Types
- **JavaScript/TypeScript**: JS2-mode, RJSX-mode, TypeScript-mode, TSX via web-mode
- **Web Development**: Web-mode for HTML, CSS, SCSS, and template files (Volt, Tpl, CTP)
- **Python**: Python-mode
- **PHP**: PHP-mode with PSR12 standard (phpcbf configured)
- **Other**: JSON, YAML, Markdown, XML/SVG

#### Code Style and Formatting
- Tab width: 2 spaces (no tabs)
- UTF-8 encoding throughout
- Automatic trailing whitespace removal on save for programming and text modes
- Prettier.js integration for JavaScript formatting
- EditorConfig support enabled

### AI/ML Integrations

The configuration includes multiple AI-powered tools:
1. **Company-TabNine**: AI code completion (lines 308-313)
2. **GitHub Copilot**: Full integration with keybindings (M-TAB to accept, C-g to reject)
3. **Ellama**: Local LLM integration via Ollama supporting codellama, gemma2, and llama3.2
4. **Claude Code**: Integration with Claude AI (C-c x for command map)

### Key Keybindings
- **Code Navigation**: 
  - M-g j: Jump to definition
  - M-g o: Jump to definition in other window
- **AI Tools**:
  - M-TAB: Accept Copilot suggestion
  - C-g: Reject Copilot suggestion
  - C-c e: Ellama prefix commands
  - C-c x: Claude Code command map

### Important Paths
- Backup files: `~/.emacs.d/backup/`
- Auto-save files: `~/.emacs.d/backup/`

## Development Notes

- Theme: doom-gruvbox (from doom-themes package)
- Font: HackGen Console NF
- Line spacing: 0.15
- Trailing whitespace is automatically removed on save
- The configuration uses `rg` (ripgrep) for dumb-jump functionality
- Tree-sitter configuration is present but commented out (lines 332-359)