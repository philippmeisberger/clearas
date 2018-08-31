General
=======

- Improve usability
  - Visual grouping
  - Multiselect
- Receive notifications about registry changes
- Two execution modes
  - Admin: Read and edit all items (like now)
  - User: Read and edit only items owned by user
- Add support for cleaning shell bags
- Guide/Manual
- Divide search in multiple threads
- Use ref-counting

Context menu
============

- Display icons
- When disabling ShellNew item and updating related software ShellNew item gets usually registered again (ShellNew and _ShellNew exist in same key)
  --> Disable ShellNew again (but first remove _ShellNew)
- Search in key "CLSID"
- Add support for editing file path of ShellNew items
- Add support for editing file paths of Shell Cascading items
- Add support for cascading shell submenus in "ExtendedSubCommandsKey" subkey

Services
========

- Receive notifications about service changes by using NotifyServiceStatusChange()

Scheduled tasks
===============

- Add support for merging backups during import

Shell extensions
================

- Support all shell extensions like "property sheets": <https://msdn.microsoft.com/en-us/library/windows/desktop/cc144110(v=vs.85).aspx>
  - Create new class TShellExList
  - Move TContextMenuShellExItem items from TContextMenuList to TShellExList
- Add support for items whose key is a GUID
