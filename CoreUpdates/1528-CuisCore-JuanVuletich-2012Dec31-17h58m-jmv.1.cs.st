'From Cuis 4.1 of 12 December 2012 [latest update: #1527] on 31 December 2012 at 6:14:40 pm'!

!String methodsFor: 'user interface' stamp: 'jmv 12/31/2012 18:07'!
edit: labelString

	TextModel new contents: self; openLabel: labelString! !


!Text methodsFor: 'user interface' stamp: 'jmv 12/31/2012 18:07'!
edit: labelString

	TextModel new contents: self; openLabel: labelString! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 12/31/2012 18:10'!
codeManagementInCuisContents
	^ self class firstCommentAt: #codeManagementInCuisContents

"
(from http://jvuletich.org/Cuis/CodeManagementInCuis4.html )

Cuis 4 includes new tools and new suggested procedures for managing Smalltalk code. Code that is not part of the Cuis Core image itself, like applications, frameworks and libraries, should be stored in Packages. New code that is meant as patches, fixes or additions; that could eventually become part of Cuis itself, is not part of any Package, and is therefore automatically stored in Change Sets.


Packages
------------

Let's start with Packages. The Package implementation in Cuis 4 is based on PackageInfo, the standard way to specify packages in Squeak and its derivatives, and used, for example, by Monticello. It uses Package names, to specify prefixes for Class and Method categories. Classes and Methods whose categories match a Package's prefixes belong in that Package. More details about how PackageInfo decides what code belongs in a package are available at http://wiki.squeak.org/squeak/3329 .

To install packages (.pck files) in Cuis, use the FileList, navigate to the appropriate directory (on disk, or in a GitHub repository, etc), select the package file and click on [Install Package].

Cuis includes a tool to manage installed Packages. It is at World / Open / Installed Packages. To create a new package (instead of installing an existing one from a file), click on [Create Package] This creates a new package, and associates with it all the existing code in the image that matches the package name.

The operations available on installed or newly created packages are:

[Save] Saves a package on the file system. Overwrites any existing version. It is good to save package from time to time, to reduce the risk of losing code.

[Delete] Removes the Package instance from the image. Does not remove any code. This means, effectively, to merge back the code into Cuis.

[Browse unsaved Changes] This opens a ChangeSorter on the ChangeSet that captures all the changes done to the Package since it was last saved. Therefore it shows the work done on the package that would be lost if the package is not saved.

[Browse Package Code] This opens a Class Browser that only shows the code that belongs in the package. This is useful for working on a package, or studying it.

The tool shows, for each Package, the name, whether it is dirty (has unsaved changes) and the file it was installed from / saved to.

Handling Packages like this, Cuis behaves as a sort of document editor (like, for example a regular text editor) whose documents are Package files (.pck). Cuis doesn't handle Package versions, ancestries, etc. If versioning of Packages is desired, the best is to use a versioning file repository, such as Git or Mercurial. The recommendation is to use a GitHub repository with a name beginning with 'Cuis-', so it will be easy for anybody to find it. Cuis Package files (.pck) are uncompressed, use Lf (ASCII 10) as newLine, and are encoded in ISO 8859-15. This means that are Git friendly, and Git/GitHub can diff and merge them, and browse them with syntax highlighting.

This is not unlike using Git or GitHub with a more conventional development environment such as Eclipse or a text editor. Like Cuis 4, these tools don't do version handling themselves, they just load and save files; and let Git do its magic.


Changes to the Cuis base image
--------------------------------------

The way ChangeSets are created and managed in Cuis 4 is very different from previous versions of Cuis (and Squeak & derivatives). This was done to make ChangeSets a good way to manage changes to the base Cuis Core image, while keeping code in Pakges out of the way, so they don't get mixed together.

What is not in a Package belongs (at least temporarily) in the Cuis Core image. Such code is automatically captured in a ChangeSet. The ChangeSet for Core changes is created automatically and named like '1243-CuisCore-JuanVuletich-2012Apr03-22h50m'. The number at the beginning is the next number for the Cuis update stream, and is provided only as a suggestion. The 'CuisCore' part is to reveal that the code belongs in the base image and not in some package. Then we have author name and date / time of creation. These ChangeSets are created automatically. There is no longer a way to manually create them, or make them 'current' or 'active'. It is best not to rename them. These ChangeSets will not capture any code that belongs in a Package.

Opening a Change Sorter will show the CuisCore change set. This is useful, for example, to check that no code that was intended for a Package ends here by mistake (because of the wrong class or method category). But it is also useful when doing changes to the base system. Now, we can do changes both to the base system and to a number of packages, all in the same session, without having to be careful about selecting the proper change set before saving a method: The code is automatically added to the proper Package or ChangeSet, simply following the class or method category. Gone are the days of messed up change sets and lost code!!

When the changes to the base system are complete, it is a good time to review the CuisCore change set and, maybe remove from it changes that we don't want to keep (for example, experiments, halts, etc). Then, just do right click / File out and remove. This saves the ChangeSet on disk. It also removes it from the ChangeSorter (but it doesn't remove any code). This is good, because the next changes done will end in a new CuisCore change set, and there's no risk of having undesired changes in the old one. As changes to the base image progress, and several CuisCore change sets are saved to disk, these numbered files are created in sequence. They will be ready to be loaded back in proper order in a fresh Cuis image, or to be sent to Cuis maintainers for integration in the update stream and in next releases of Cuis.


Loading ChangeSet files into Cuis
---------------------------------------

There are two ways to load ChangeSet files (.cs): [FileIn] and [Install].

[FileIn] loads the code without creating a new ChangeSet object. This means that changes that belong in the base image (and not in a package) will be added to the current ChangeSet for Cuis core changes, as if they were done by the user. This is appropriate when we are combining code from more than one source into a single ChangeSet. Any change that belongs in an installed package will be added to it, and the package will appear as dirty.

[Install] loads the code into a separate ChangeSet object (viewable in the ChangeSorter tool). This is appropriate for loading Cuis updates, or other code that we are not authoring, as it doesn't add new items (class or method definitions) to the current ChangeSet for our changes to Cuis. Usually any ChangeSets should be installed before doing changes to the image. The reason is that an installed ChangeSet could overwrite changes done by you, or packages you have installed. If this is the case, the affected packages would appear as dirty, and your change set would include any installed changes (that don't belong in a package). Be careful when saving packages or change sets if this was the case!!
"

"
Utilities codeManagementInCuisContents edit
"! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 12/31/2012 18:09'!
cuisAndGitHubContents
	^ self class firstCommentAt: #cuisAndGitHubContents

"
(from http://jvuletich.org/Cuis/CuisAndGitHub.html )

Cuis 4 includes new tools and new suggested procedures for managing Smalltalk code. Central to this is the management of Packages and Package Files (.pck). But Cuis doesn't do version control. Instead, we suggest using external VCS tools.  In particular, we're using GitHub, and the first project we're hosting there is StyledTextEditor.

The guiding principle is do not duplicate concepts and behavior. As we're using an external tool (Git) for version control, then use it as it meant to be used. Most people use Git for version control and a file based IDE such as Eclipse for development. Such IDEs don't do version control themselves. It is done by Git. Do the same: do not include package version control in Cuis. This is a departure from the Monticello /Git integration (smallsource and MonticelloFileTree) by Otto Behrens, Dale Henrichs, etc.

We use GitHub to host, version, diff and merge external packages (.pck files), i.e. code that is maintained independently and outside Cuis.

Package files need to be simple text files. Cuis encoding for latin alphabet (ISO 8859-15) is handled without problems by GitHub. Cuis uses the LF (ascii code 10) newline convention, as preferred in GitHub. This allows Git/GitHub to diff versions, and merge branches.

Each GitHub repository has one set of users and permissions. Each GitHub repository has one state (Git commits repositories, not individual files). Branch and merges are done on the whole repository and not on individual files. Therefore, we need a separate GitHub repository for each project, i.e., for each package or set of closely related packages that are always loaded and maintained together as a whole.
Development process for External Packages

This is the suggested procedure for developing external packages. Usually do this every day.

    Start with a standard (i.e. fresh) Cuis image. Never save the image.

    Set up Git repositories for external packages (if not already done)

    Install packages from Git repositories.

    Develop. Modify and/or create packages.

    Save own packages (to Git repositories).

    Git add / commit / push as appropriate.

    Fileout changes that are not part of any package. These are automatically captured in numbered changesets, separated from changes to packages.

    Exit the image. Usually without saving.

"

"
Utilities cuisAndGitHubContents edit
"! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 12/31/2012 18:08'!
openCodeManagementInCuis
	"
	Utilities openCodeManagementInCuis
	"

	self codeManagementInCuisContents edit: 'Code Management in Cuis'! !

!Utilities class methodsFor: 'support windows' stamp: 'jmv 12/31/2012 18:09'!
openCuisAndGitHub
	"
	Utilities openCuisAndGitHub
	"

	self cuisAndGitHubContents edit: 'Using Git and GitHub to host and manage Cuis code'! !


!String methodsFor: 'user interface' stamp: 'jmv 12/31/2012 18:07'!
edit

	self edit: 'Text Editor'! !


!Text methodsFor: 'user interface' stamp: 'jmv 12/31/2012 18:07'!
edit

	self edit: 'Text Editor'! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 12/31/2012 18:10'!
helpMenu
	"Build the help menu for the world."
	| menu |
	menu _ self menu: 'Help...'.
	self
		fillIn: menu
		from: {
			{'About this System...'. {Smalltalk. #aboutThisSystem}. 'current version information.'}.
			{'Code management in Cuis'. {Utilities. #openCodeManagementInCuis}. 'current version information.'}.
			{'Using GitHub to host Cuis packages'. {Utilities. #openCuisAndGitHub}. 'current version information.'}.
			nil.
			{'Editor keyboard shortcuts'. {SmalltalkEditor. #openHelp}. 'summary of keyboard shortcuts in editors for Smalltalk code.'}
		}.
	self addGestureHelpItemsTo: menu.
	self
		fillIn: menu
		from: {
			{'Useful Expressions'. {Utilities. #openStandardWorkspace}. 'a window full of useful expressions.'}.
			nil.
			{'VM Statistics'. {self. #vmStatistics}. 'obtain some intriguing data about the vm.'}.
			{'Space Left'. {self. #garbageCollect}. 'perform a full garbage-collection and report how many bytes of space remain in the image.'}
		}.
	^ menu! !


!Theme methodsFor: 'menus' stamp: 'jmv 12/31/2012 18:13'!
basicIcons

"Minimal menu scheme."

	^ { 
		#('open...') -> #openIcon.
		#('windows...' 'find window') -> #windowIcon.
		#('help...' 'explain' 'about this system...' 'Code management in Cuis' 'Using GitHub to host Cuis packages' ) -> #helpIcon.
		#('themes...') -> #appearanceIcon.
		#('do...' 'Cursor normal show.' 'do it (d)') -> #doItIcon.
		#('new morph...' 'objects (o)' 'save world as morph file') -> #morphsIcon.
		#('save' 'save project on file...' ) -> #saveIcon.
		#('save as...' 'change category...' 'rename change set (r)' 'rename') -> #saveAsIcon.
		#('save as new version') -> #saveAsNewVersionIcon.
		#('quit') -> #quitIcon.
		#('save and quit' ) -> #saveAndQuitIcon.
		#('inspect it (i)' 'inspect world'  'inspect model' 'inspect morph'
		 'inspect owner chain' 'inspect' 'inspect (i)' 'basic inspect' 'message names' 'find message names' 'inspect instances' 'inspect subinstances' 'inspect change set' 'inspect context (c)' 'inspect receiver (i)' 'start CPUWatcher')
			-> #inspectIcon.
		#('explore' 'explore it (I)' 'explore world' 'explore morph' 'explore (I)' 'explore context (C)' 'explore receiver (I)' 'explore pointers' 'explore all pointers (incl.weak)' 'shortest ref. path from globals (slow!!)') -> #exploreIcon.
		#('find...(f)' 'find class... (f)' 'find method...' 'find recent submissions' 'show hierarchy' 'show definition' 'show comment' 'filter' 'filter message list...' 'find context... (f)') -> #findIcon.
		#('add item...' 'new category...' 'create new change set...' 'new change set... (n)' 'add new file') -> #newIcon.
		#('remove method (x)' 'remove' 'remove class (x)' 'remove method from system (x)' 'remove class from system (x)' 'remove postscript') -> #deleteIcon.
		#('delete method from changeset (d)' 'delete class from change set (d)' 'destroy change set (X)' 'revert & remove from changes' 'delete unchanged windows' 'delete non windows' 'delete both of the above' 'reset variables' 'remove contained in class categories...' 'clear this change set' 'uninstall this change set' 'delete directory...' 'delete') -> #warningIcon.
		#('do again (j)' 'Redo - multiple (Z)') -> #redoIcon.
		#('undo (z)' 'revert to previous version' 'Undo - multiple (z)') -> #undoIcon.
		#('copy (c)' 'copy class...' 'copy class chgs to other side' 'copy method to other side' 'copy all to other side (c)' 'copy name to clipboard' 'copy selector to clipboard') -> #copyIcon.
		#('paste (v)' 'Paste without Format') -> #pasteIcon.
		#('cut (x)' 'move class chgs to other side' 'move method to other side' 'submerge into other side') -> #cutIcon.
		#('paste...' 'icons...') -> #worldIcon.
}! !

!Theme methodsFor: 'menus' stamp: 'jmv 12/31/2012 18:13'!
miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history') -> #changesIcon.
		#('print PS to file...' ) -> #printIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' ) -> #cancelIcon.
		#('debug...' 'debug it' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'expand all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner') -> #weatherFewCloudsIcon.
		#('system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'All preferences...' 'what to show...') -> #preferencesIcon.
		#('Editor keyboard shortcuts') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.		"currently unused, but a neat icon"
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set code author...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'profile messages (m)') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance' 'inspect Pointers (P)') -> #instanceIcon.
		#('Use Selection for Find (j)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
		#('method strings with it (E)') -> #genericTextIcon.
		#('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon.
		#('move up' 'make next-to-topmost') -> #goUpIcon.
		#('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon.
		#('inheritance (i)' 'move down') -> #goDownIcon.
		#('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon.
		#('spawn full protocol') -> #speadsheetTemplateIcon.
		#('alphabetize') -> #fontXGenericIcon.
		#('Installed Packages' 'browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon.
		#('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon.
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)') -> #listAddIcon.
		#('toggle diffing (D)' 'toggle selections') -> #switchIcon.
		#('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon.
		#('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon.
		#('update' 'turn on auto-update (a)' 'update list (u)') -> #updateIcon.
		#('find changed windows...') -> #newWindowIcon.
		#('make undraggable') -> #pushPinIcon.
		#('Utilities saveScreenshot.') -> #stillCameraIcon.
		#('add new directory') -> #newFolderIcon.
		#('select all' 'deselect all') -> #selectAllIcon.
		#('sort by date') -> #dateIcon.
		#('justified') -> #formatJustifyFillIcon.
		#('centered') -> #formatJustifyCenterIcon.
		#('set alignment...' 'leftFlush') -> #formatJustifyLeftIcon.
		#('rightFlush') -> #formatJustifyRightIcon.
		#('signal Semaphore (S)') -> #haloHelpIcon.
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...') -> #fontXGenericIcon.
	}! !


!Utilities class methodsFor: 'support windows' stamp: 'jmv 12/31/2012 18:07'!
openStandardWorkspace 
	"Open up a throwaway workspace with useful expressions in it.  1/22/96 sw"
	"Utilities openStandardWorkspace"

	self standardWorkspaceContents edit: 'Useful Expressions'! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Theme current class beCurrent!

