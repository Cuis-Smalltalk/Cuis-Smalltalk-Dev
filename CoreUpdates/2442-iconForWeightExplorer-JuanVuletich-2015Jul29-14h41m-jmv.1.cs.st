'From Cuis 4.2 of 25 July 2013 [latest update: #2442] on 29 July 2015 at 2:43:45.331602 pm'!

!Theme methodsFor: 'menus' stamp: 'jmv 7/29/2015 14:42'!
basicIcons

	"Minimal menu scheme.
	Theme current class beCurrent
	"

	^ { 
		#('open...') -> #openIcon.
		#('windows...' 'find window' 'Focus follows mouse' 'Click to focus') -> #windowIcon.
		#('help...' 'explain' 'about this system...' 'Terse Guide to Cuis' 'Code management in Cuis' 'Using GitHub to host Cuis packages' ) -> #helpIcon.
		#('themes...') -> #appearanceIcon.
		#('do...' 'Cursor normal show.' 'do it (d)') -> #doItIcon.
		#('new morph...' 'objects (o)' 'save world as morph file') -> #morphsIcon.
		#('save' 'save project on file...' ) -> #saveIcon.
		#('Save options...' 'save as...' 'change category...' 'rename change set (r)' 'rename') -> #saveAsIcon.
		#('save as new version') -> #saveAsNewVersionIcon.
		#('quit') -> #quitIcon.
		#('save and quit' ) -> #saveAndQuitIcon.
		#('inspect it (i)' 'inspect world'  'inspect model' 'inspect morph'
		 'inspect owner chain' 'inspect' 'inspect (i)' 'basic inspect' 'message names' 'find message names' 'inspect instances' 'inspect subinstances' 'inspect change set' 'inspect context (c)' 'inspect receiver (i)' 'start CPUWatcher')
			-> #inspectIcon.
		#('explore' 'explore it (I)' 'explore world' 'explore morph' 'explore (I)' 'explore context (C)' 'explore receiver (I)' 'references finder' 'weight explorer') -> #exploreIcon.
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Theme current class beCurrent!

