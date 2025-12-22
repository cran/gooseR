# gooseR News

## 0.1.1 (2024-12-17)
- **Major Enhancement**: Comprehensive memory management system
  - `goose_clear_category()`, `goose_clear_tags()`, `goose_clear_all()` for bulk cleanup
  - `goose_exists()`, `goose_rename()`, `goose_backup()` for smart memory operations
  - Session tracking with `goose_session_start()`, `with_goose_session()` wrapper
- **AI Response Formatting**: Beautiful markdown output by default
  - `format_ai_response()` with headers, bullets, code blocks, colors
  - `goose_format_options()` for global configuration
  - Enhanced `goose_ask()` with automatic formatting
- **18 New Functions**: Doubling package functionality
- **100% Backward Compatibility**: All existing code continues to work
- **Improved Error Handling**: Progress bars, confirmations, comprehensive validation

## 0.1.0 (2024-12-17) - CRAN Release
- Initial professional release ✅ **NOW LIVE ON CRAN**
- Organized repository structure (docs/, inst/examples, tests/manual, vignettes/)
- Added user-first README and demos
- Introduced roadmap and contributing guidelines
- Core memory system with goose_save(), goose_load(), goose_list()
- Brand theming system with theme_brand()
- CLI integration and AI assistant functions
