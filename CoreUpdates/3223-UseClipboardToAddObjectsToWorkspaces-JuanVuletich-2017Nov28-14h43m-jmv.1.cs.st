'From Cuis 5.0 of 7 November 2016 [latest update: #3222] on 28 November 2017 at 3:11:44 pm'!

!Object methodsFor: 'inspecting' stamp: 'jmv 11/28/2017 15:08:54'!
copyToClipboard
	"Create and schedule an Inspector in which the user can examine the receiver's variables."

	Clipboard storeObject:  self! !


!TextModel methodsFor: 'testing' stamp: 'jmv 11/28/2017 14:52:16'!
canBindVariables
	^ false! !


!Workspace methodsFor: 'variable declarations' stamp: 'jmv 11/28/2017 14:56:27'!
nameForObject: object
	"Answer a name suitable for a Workspace variable"
	object name ifNotNil: [ :displayName |
		^displayName asIdentifier: false ].
	^ object class name asLowercase , object identityHash asString! !

!Workspace methodsFor: 'testing' stamp: 'jmv 11/28/2017 14:52:28'!
canBindVariables
	^ true! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/28/2017 14:57:38'!
paste
	| object objectName |
	model canBindVariables ifTrue: [
		object _ Clipboard retrieveObject.
		objectName _ model nameForObject: object.
		(model bindingOf: objectName) value: object.
		self replaceSelectionWith: objectName.
		^ self ].
	^ super paste! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 11/28/2017 14:47:12'!
copySelectionToClipboard
	"For example, for pasting a reference in a Workspace"

	Clipboard storeObject: model selection! !


!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 11/28/2017 14:47:30'!
copySelectionToClipboard
	"For example, for pasting a reference in a Workspace"

	Clipboard storeObject: model object! !


!Editor methodsFor: 'menu messages' stamp: 'jmv 12/19/2011 12:24'!
paste
	"Paste the text from the shared buffer over the current selection and 
	redisplay if necessary."

	self replaceSelectionWith: self clipboardStringOrText! !


!Morph methodsFor: 'debug and other' stamp: 'jmv 11/28/2017 15:07:24'!
buildDebugMenu: aHand 
	"Answer a debugging menu for the receiver.  The hand argument is seemingly historical and plays no role presently"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu addStayUpIcons.
	self isKnownFailing
		ifTrue: [
			aMenu add: 'start drawing again' action: #resumeAfterDrawError.
			aMenu addLine].
	(self hasProperty: #errorOnStep) 
		ifTrue: [
			aMenu add: 'start stepping again' action: #resumeAfterStepError.
			aMenu addLine].
	aMenu add: 'inspect morph' action: #inspect.
	aMenu add: 'inspect owner chain' action: #inspectOwnerChain.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'inspect model'
				target: self model
				action: #inspect].
	aMenu 
		add: 'explore morph'
		target: self
		selector: #explore.
	aMenu 
		add: 'copy to clipboard (c)'
		target: self
		selector: #copyToClipboard.
	aMenu addLine.
	aMenu 
		add: 'browse morph class'
		target: self
		selector: #browseClassHierarchy.
	self hasModel 
		ifTrue: [
			aMenu 
				add: 'browse model class'
				target: self model
				selector: #browseClassHierarchy].
	aMenu addLine.
	aMenu
		add: 'edit balloon help' action: #editBalloonHelpText.
	^aMenu! !


!InspectorWindow methodsFor: 'menu building' stamp: 'jmv 11/28/2017 14:44:33'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu object |
	aMenu _ MenuMorph new defaultTarget: self.

	aMenu addList: #(
		('inspect (i)'							inspectSelection)
		('explore (I)'							exploreSelection)
		('copy to clipboard (c)'				copySelectionToClipboard)
		('basic inspect'						inspectBasic)
		('references finder'					openReferencesFinder)
		('weight explorer'					openWeightExplorer)).

	object _ model object.
	(object is: #Dictionary) ifTrue: [ aMenu addList: #(
		-
		('senders of this key'				sendersOfSelectedKey)
		('add key'								addEntry)
		('rename key'							renameEntry)
		('remove'								removeSelection			''		model)) ]
	
	ifFalse: [ (object is: #Set) ifTrue: [ aMenu addList: #(
		-
		('remove'								removeSelection			''		model))]].

	aMenu addList: #(
		-
		('browse full (b)'						browseMethodFull)
		('browse hierarchy (h)'				browseHierarchy)
		('browse protocol (p)'				browseFullProtocol)).
	^ aMenu! !


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'jmv 11/28/2017 14:45:17'!
genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				selector: #yourself]
		ifNotNil: [
			aMenu addList: #(
				('inspect (i)'							inspectSelection)
				('explore (I)'							exploreSelection)
				('copy to clipboard (c)'				copySelectionToClipboard)
				('basic inspect'						inspectBasic)
				('references finder'					openReferencesFinder)
				('weight explorer'					openWeightExplorer)
				-
				('browse full (b)'						browseMethodFull)
				('browse hierarchy (h)'				browseHierarchy)
				('browse protocol (p)'				browseFullProtocol)).
			aMenu addLine;
				add: 'monitor changes'
				target: self
				selector: #monitor:
				argument: model getCurrentSelection.
			model class == ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					selector: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: self
				selector: #stopMonitoring ].
	^ aMenu! !


!Theme methodsFor: 'menus' stamp: 'jmv 11/28/2017 14:43:40'!
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
		#('copy to clipboard (c)' 'copy (c)' 'copy class...' 'copy class chgs to other side' 'copy method to other side' 'copy all to other side (c)' 'copy name to clipboard' 'copy selector to clipboard') -> #copyIcon.
		#('paste (v)' 'Paste without Format') -> #pasteIcon.
		#('cut (x)' 'move class chgs to other side' 'move method to other side' 'submerge into other side') -> #cutIcon.
		#('paste...' 'icons...') -> #worldIcon.
}! !

!methodRemoval: WorkspaceWindow #allowsMorphDrop!
WorkspaceWindow removeSelector: #allowsMorphDrop!
!methodRemoval: WorkspaceWindow #nameForObject:!
WorkspaceWindow removeSelector: #nameForObject:!
!methodRemoval: WorkspaceWindow #wantsDroppedMorph:event:!
WorkspaceWindow removeSelector: #wantsDroppedMorph:event:!
!methodRemoval: ObjectExplorerWindow #objectsForWorkspace!
ObjectExplorerWindow removeSelector: #objectsForWorkspace!
!methodRemoval: InspectorWindow #objectsForWorkspace!
InspectorWindow removeSelector: #objectsForWorkspace!
!methodRemoval: SystemWindow #objectsForWorkspace!
SystemWindow removeSelector: #objectsForWorkspace!
!methodRemoval: Morph #objectsForWorkspace!
Morph removeSelector: #objectsForWorkspace!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Theme current class beCurrent!

