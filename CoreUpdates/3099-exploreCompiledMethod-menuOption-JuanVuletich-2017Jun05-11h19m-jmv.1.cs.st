'From Cuis 5.0 of 7 November 2016 [latest update: #3086] on 5 June 2017 at 11:53:55 am'!

!CodeProvider methodsFor: 'message list menu' stamp: 'jmv 6/5/2017 11:51:33'!
exploreCompiledMethod
	"Open an Explorer on the CompiledMethod itself"

	self selectedMessageName ifNotNil: [
		(self selectedClassOrMetaClass compiledMethodAt: self selectedMessageName)
			explore ]! !


!BrowserWindow methodsFor: 'menu building' stamp: 'jmv 6/5/2017 11:46:27'!
messageListMenu
	"Answer the message-list menu"
	"Changed by emm to include menu-item for breakpoints"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addList:#(
			('what to show...'					offerWhatToShowMenu)
                	('toggle break on entry'		toggleBreakOnEntry						''		model)
            	-
			('browse full (b)' 					browseMethodFull)
			('browse hierarchy (h)'			browseHierarchy)
			('browse method (O)'			openSingleMessageBrowser)
			('browse protocol (p)'			browseFullProtocol)
			-
			('fileOut'							fileOutMessage							''		model)
			('explore CompiledMethod'		exploreCompiledMethod				''		model)
			-
			('senders of... (n)'				browseSendersOfMessages)
			('implementors of... (m)'		browseMessages)
			('inheritance (i)'					methodHierarchy)
			('versions (v)'						browseVersions)
			-
			('inst var refs...'					browseInstVarRefs)
			('inst var defs...'					browseInstVarDefs)
			('class var refs...'				browseClassVarRefs)
			('class variables'					browseClassVariables)
			('class refs (N)'					browseClassRefs)
			-
			('remove method (x)'			removeMessage							''		model)
			('Run test (t)'						runMethodTest 							'' 	model)
			('Debug test (r)'					debugMethodTest 						'' 	model)
			-
			('more...'							openShiftedMessageListMenu)).
	^ aMenu
! !


!Theme methodsFor: 'menus' stamp: 'jmv 6/5/2017 11:46:34'!
basicIcons

	"Minimal menu scheme.
	Theme current class beCurrent
	"

	^ { 
		#('open...') -> #openIcon.
		#('windows...' 'find window' 'Focus follows mouse' 'Click to focus') -> #windowIcon.
		#('help...' 'explain' 'about this system...' 'Terse Guide to Cuis' 'Class Comment Browser' 'Code management in Cuis' 'Using GitHub to host Cuis packages' ) -> #helpIcon.
		#('themes...') -> #appearanceIcon.
		#('do it (d)') -> #doItIcon.
		#('new morph...' 'objects (o)' 'save world as morph file') -> #morphsIcon.
		#('save' ) -> #saveIcon.
		#('Save options...' 'save as...' 'change category...' 'rename change set (r)' 'rename') -> #saveAsIcon.
		#('save as new version') -> #saveAsNewVersionIcon.
		#('quit') -> #quitIcon.
		#('save and quit' ) -> #saveAndQuitIcon.
		#('inspect it (i)' 'inspect world'  'inspect model' 'inspect morph'
		 'inspect owner chain' 'inspect' 'inspect (i)' 'basic inspect' 'message names' 'find message names' 'inspect instances' 'inspect subinstances' 'inspect change set' 'inspect context (c)' 'inspect receiver (i)' 'start CPUWatcher' 'stop CPUWatcher')
			-> #inspectIcon.
		#('explore' 'explore it (I)' 'explore world' 'explore morph' 'explore (I)' 'explore context (C)' 'explore receiver (I)' 'references finder' 'weight explorer' 'explore CompiledMethod') -> #exploreIcon.
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

