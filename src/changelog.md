Clearas
=======

An open-source project by PM Code Works

Version 5.0 [*??.09.17*]
-----------

* Added automatic search for invalid items
  * Added "Delete invalid items" to "Edit" menu
  * No symbol is displayed for invalid Autostart items
  * Added warning for invalid items after disabling and enabling
* Startup user items are handled prior to Windows 7 in the same way as from Windows 8
  * Removed exporting as registry file but instead as .Startup and .CommonStartup
  * Removed "Delete backups after enabling" menu item
* Removed "Default column width" menu item
* Added "Quick-Search" to Autostart tab
* Added "Execute" to popup menu
* Added "Properties" to popup menu
* Added support for changing extended attribute of shell context menu items
* Improved "Open in Explorer"
* Added loading description of ShellEx and ShellNew items
* Added loading of MUI strings
* Added loading context menu items from AllFileSystemObjects, Directory\Background, Network and Printers per default
* Added warning if quit was clicked and export is pending
* Closed memory leak during export
* Closed memory leak when importing a task
* Bug fixes:
  * In 64 bit version: File path of 32 bit context menu entries could not be found
  * ShellNew items could have empty name
  * AV when using keyboard shortcuts during a search
  * Wrong listview was refreshed when switching the tab during a search
  * Column "Deactivation time" did not have correct caption after changing language
  * Caption was not refreshed after editing the path
  * Arguments are missing after adding a .bat file to autostart
  * Loading primary language if requested locale could not be found in language file
  * Renaming disabled startup user items was not possible
  * Disabled startup user items were enabled after changing file path
* Removed support for Windows 2000 and XP
* Updater v3.1
  * Removed dependency to "indy" components library
  * Binary up to 1MB smaller

Version 4.3.2 [*06.07.16*]
-------------

* Fixed bug in registry file export
  * Binary data types have correct format and can be successful imported again

Version 4.3.1 [*27.05.16*]
-------------

* Added time delay to "Quick-Search"
  * Search will not start until input is finished
* Bug fix: Disabling and deleting Autostart items did not work after upgrading to Windows 10
* Some layout enhancements
* New manifest for better compatibility with Windows 10
* Usage of the new "About ..." dialog
* Improved report bug system
  * Added translations for german, english and french
  * In case no mail client is installed for sending the bug report the website will be shown
* Program executable is now signed with SHA-256

Version 4.3 [*22.04.16*]
-----------

* Added "Tasks" feature:
  * Allows to edit scheduled tasks
  * "Export entries" saves all tasks as a ZIP file
  * "Import backup" imports previously exported tasks
  * "Expert mode" finds hidden tasks
* Added support for "Cascading" context menu items
* Added "F2" keyboard shortcut to rename the selected item
* Added support for deactivation time introduced in Windows 8
* Bug fixes:
  * "Rename" now shows the correct text
  * After renaming an item the new name is updated when "Show description" is selected
  * Item cannot be renamed to an already existing item
  * Counting of enabled Autostart items now works correctly
  * Exporting Autostart items does not fail if there is no entry in the "StartupApproved" key yet
  * "ShellNew" entries can no longer be enabled and disabled at the same time

Version 4.2 [*04.10.15*]
-----------

* Autostart feature now supports Windows 8 and later
  * Changes to startup user items
    * Cannot be exported as registry file anymore but instead as .Startup and .CommonStartup files
    * Removed creation of automatic backups in "%WinDir%\pss"
* Added "Show description" to the "View" menu
  * Either display the description of the .exe or item internal name
* Added "Rename" to popup menu
* Added "Change icon" for "Shell" context menu items to popup menu
* Improved "Open in Explorer"
* Improved event system
* Usage of generic container classes (generics)
* Loading icons with white background
* Disabling menu items (gray out) instead of hiding them
* New manifest

Version 4.1 [*27.08.15*]
-----------

* Changes on "Context menu" page:
  * Added support for "ShellNew" context menu items
  * Added message after new item has been added and if user-defined program for file type exists
  * Added "Add context menu" to "File" menu:
    * Any file types or file extensions can be used
    * Items can be added hidden (only visible by Shift+right-click)
* Displayed progress bar during the registry file export
* Using progress bar only as progress indicator
* Added progress bar to "services" page
* Added "Show icons" to "View" menu
* Added buttons to "Quick-Search"
* Lists can now be sorted in ascending and descending order
* Bug fixes:
  * Canceling while adding items now works correctly
  * After a fatal error has occurred, the lists can still be refreshed
  * After a new service is added the correct path (with arguments) is displayed
* Updated icon: http://en.divinity.wikia.com/wiki/File:Icon_Besen.svg.png
* Usage of Windows specific graphical features (since Vista)
* Language is loaded according to the user
* Added support for Unicode
* 32/64 bit binary files
* Updater v3.0 with SSL support

Version 4.0 [*28.03.15*]
-----------

* Introduced multi-threading
* Improved exception handling
* New feature "Services":
  * Allows editing of Windows services
* Added usage of TaskDialog
* Changes in the popup menu:
  * New function "Edit path" added
    * File parameters and parameters can be edited
  * Removed "Search for program" (replaced by "Edit path")
  * Added "Open in RegEdit"
    * Opens the registry path of the selected item in RegEdit
  * Added "Open in Explorer"
    * Opens the file path of the selected item in Explorer
  * Added "Copy location"
    * Copies the path of the selected item to the clipboard
  * Removed "Properties" (replaced by "Copy location")
* Changes to the "context menu" page:
  * Menu
    * Added "Add context menu"
    * Added "Export items"
  * Loading caption of Shell items
  * Added "Quick-Search" for filtering items
  * Improved "Expert mode"
    * Accelerated search
    * More results
* Changes to the "Autostart" page:
  * Replaced "RunOnce" menu item by a checkbox
  * Added "Show icons" menu item
    * Icons of programs are displayed per default
  * Added marking of 32 bit items on 64 bit operating system
  * Added support for 32 bit "RunOnce" items on 64 bit operating system
* Accelerated visual selection of Autostart and context menu items
* Improved "Import backup"
* Improved registry file export
* Bug fix: Autostart items are correctly removed from list after they are deleted
* New manifest
* Updater v2.2
* OSUtils v2.1
* Open-source

Version 3.0 [*03.10.13*]
-----------

* Added possibility to specify parameters in "Add program"
* Added support for "RunOnce" Autostart items
* Discontinued "Personal Edition"

Version 2.1.2 [*07.03.13*]
-------------

* Improved window management
* Fixed a rare error when Autostart items have the same name
* Minor bug fixes

Version 2.1.1 [*29.12.12*]
-------------

* Improved updater
* Batch files can now be added into the Autostart
* Fixed issues with duplicate context menu items
* Add single instance mode
* Minor GUI tweaks

Version 2.1 [*22.11.12*]
-----------

* Added context menu feature
* Added translation for french (thanks to Angela Hansen)
* Added "Edit path" to popup menu
* Bug fix: Programs do not occur twice
* Extented "Help" menu
  * Added "Search for update"
  * Added "Install certificate"
  * Introduced "Personal Edition" which allows to store settings
* Added Support for Windows 2000 and 8
* New manifest
* Minor bug fixes for 64 bit systems

Version 2.0 [*08.02.12*]
-----------

* Source code cleanup
  * Reworked and largely rewritten
  * Clearas needs even less resources
  * Improved error detection
* Added translation for english (thanks to Angela Hansen)
* Added "File" menu
  * Export the complete list as plain text or registry file
  * Added import of programs into the Autostart
  * Added import of startup user backups
* Added editing of program path if it is empty
  * "Export" of startup user items creates a backup in "%WinDir%\pss"
  * Added "Delete backups after enabling" menu item
* Added "Date of deactivation" menu item
* Added "Show service pack" menu item
* Serveral bug fixes

Version 1.2.21 [*09.11.11*]
--------------

* Added support for Windows XP
* Added "Edit" and "Help" menus
* Automatically creating backups of startup user items in "%WinDir%\pss"
* Showing the operating system including service pack
* Serveral bug fixes

Version 1.2.2 [*15.10.11*]
-------------

* Added support for Vista and later
* Form is resizable
* Serveral bug fixes

Version 1.2.1 [*07.10.11*]
-------------

* Added menu
* Added "Properties" to popup menu
  * Allows to display the registry key
* Added showing operating system version

Version 1.2 [*05.10.11*]
-----------

* Added registry file export
* Added popup menu
* Added keyboard shortcuts
  * "Del" = Delete selected entry
  * "Ctrl+C" = Export selected entry
  * "F5" = Refresh
  * "F6" = Standard column size
  * "F7" = Optimize column size
* Added showing "Save as"-dialog before deleting an item
* Added view sorting
* Improved layout
  * Bigger shape
  * Increased button dimensions
* Serveral bug fixes

Version 1.1 [*03.10.11*]
-----------

* Improved search for startup user items
* Added administrator manifest
* Serveral bug fixes
* New icon

Version 1.0 [*13.04.11*]
-----------

Initial release

--------------------------------------------------------------------------------
Clearas was released under the D-FSL license and is open-source. The source code can be downloaded from GitHub or from the website.

If Clearas is going to be pressed onto a commercial CD-ROM (with magazine), it would be nice to send me an issue of this CD-ROM (with magazine). You can contact me under team@pm-codeworks.de and I will give you my address.
