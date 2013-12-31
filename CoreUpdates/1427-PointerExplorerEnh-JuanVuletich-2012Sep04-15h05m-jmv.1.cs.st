'From Cuis 4.0 of 21 April 2012 [latest update: #1426] on 4 September 2012 at 3:34:16 pm'!
!classDefinition: #PointerExplorer category: #'Tools-Explorer'!
ObjectExplorer subclass: #PointerExplorer
	instanceVariableNames: 'includeWeakRefs '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Explorer'!
!classDefinition: #SystemDictionaryTest category: #'System-Tests'!
TestCase subclass: #SystemDictionaryTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Tests'!

!ProtoObject methodsFor: 'tracing' stamp: 'jmv 9/3/2012 23:20'!
referenceDescriptionTo: anObject
	"Answer a string that describes how I reference anObject"

	self class == anObject ifTrue: [ ^ 'class' ].
	1 to: self class instSize do: [ :i |
		(self instVarAt: i) == anObject ifTrue: [ ^ self class allInstVarNames at: i ]].
	1 to: self basicSize do: [ :i |
		(self basicAt: i) == anObject ifTrue: [ ^ 'at: ', i printString ]].
	^ 'unknown'! !


!Object methodsFor: 'tracing' stamp: 'jmv 9/3/2012 22:30'!
exploreAllPointers
	"Include Weak references."

	ObjectExplorerWindow
		open: (PointerExplorer new includeWeakRefs: true; rootObject: self)
		label: 'References (incl. weak) to ', self printString! !


!CompiledMethod methodsFor: 'tracing' stamp: 'jmv 9/3/2012 23:20'!
referenceDescriptionTo: anObject
	"Answer a string that describes how I reference anObject"

	self class == anObject ifTrue: [ ^ 'class' ].
	1 to: self numLiterals do: [ :index |
		(self literalAt: index) == anObject ifTrue: [ ^'literalAt: ', index printString ]].
	^ 'unknown'! !


!IndentingListItemMorph methodsFor: 'accessing' stamp: 'jmv 9/4/2012 14:45'!
beExpanded

	isExpanded _ true! !

!IndentingListItemMorph methodsFor: 'private-container protocol' stamp: 'jmv 9/4/2012 00:04'!
openPath: anArray adaptor: aSymbol compare: comparison

	anArray isEmpty ifTrue: [ ^container setSelectedMorph: nil ].
	self withSiblingsDo: [ :each | 
		(anArray first isNil or: [
			(each complexContents perform: aSymbol)
				perform: comparison
					with: (anArray first perform: aSymbol) ]) ifTrue: [
			each isExpanded ifFalse: [
				each toggleExpandedState.
				owner adjustExtent.
				container setScrollDeltas ].
			each redrawNeeded.
			anArray size = 1 ifTrue: [
				^container setSelectedMorph: each ].
			each firstChild ifNil: [^container setSelectedMorph: nil ].
			^each firstChild openPath: anArray allButFirst adaptor: aSymbol compare: comparison ]].
	^container setSelectedMorph: nil! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 9/3/2012 22:31'!
exploreAllObjectPointers
	"Create and schedule a Pointers Explorer on the receiver's model's currently selected object."

	^ model selection exploreAllPointers! !


!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 9/4/2012 00:00'!
expandPathFromRoot
	"Expand the shortest path from root references (i.e. globals).
	Only for PointerExplorers!!"
	
	| endOfRefChain chain node |
	endOfRefChain _ listMorph selectedMorph complexContents shortestPathFromRoot.
	
	chain _ OrderedCollection new.
	node _ endOfRefChain.
	[ node notNil ] whileTrue: [
		chain addFirst: node.
		node _ node parent ].
	listMorph scroller submorphs first openPath: chain adaptor: #item compare: #==! !

!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 9/3/2012 22:31'!
exploreAllObjectPointers
	"Create and schedule a Pointers Explorer on the receiver's model's currently selected object."

	^ model object exploreAllPointers! !


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 9/3/2012 18:34'!
scroller
^scroller! !


!PointerExplorer methodsFor: 'accessing' stamp: 'jmv 9/2/2012 22:41'!
includeWeakRefs
	^includeWeakRefs! !

!PointerExplorer methodsFor: 'accessing' stamp: 'jmv 9/2/2012 22:18'!
includeWeakRefs: aBoolean
	includeWeakRefs _ aBoolean! !

!PointerExplorer methodsFor: 'initialize-release' stamp: 'jmv 9/2/2012 22:18'!
initialize
	super initialize.
	includeWeakRefs _ false! !


!PointerExplorerWrapper methodsFor: 'reference chain' stamp: 'jmv 9/4/2012 13:13'!
shortestPathFromRoot

	Smalltalk garbageCollect.
	^ self shortestPathFromRoot: { self }! !

!PointerExplorerWrapper methodsFor: 'reference chain' stamp: 'jmv 9/3/2012 23:44'!
shortestPathFromRoot: anArrayOfWrappers

	| nextLevel allPointers eachWrapper pointersToEachObject alreadyIncluded |
	alreadyIncluded _ IdentitySet new.
	nextLevel _ Array streamContents: [ :strm |
		allPointers _ Smalltalk pointersToEachIn: (anArrayOfWrappers collect: [ :eachWrapper1 | eachWrapper1 item ]).

		1 to: anArrayOfWrappers size do: [ :i |
			eachWrapper _ anArrayOfWrappers at: i.
			pointersToEachObject _ allPointers at: i.

			"Can we have any other root?"
			eachWrapper item == Smalltalk specialObjectsArray ifTrue: [
				^eachWrapper ].		"Found it!!"

			pointersToEachObject do: [ :pointingObject |
				"Reject PointerExplorer stuff (wrapper and main model).
				Reject weak refs, unles includeWeakRefs is true."
				(pointingObject class = self class or: [ pointingObject class = PointerExplorer or: [
				pointingObject class isWeak and: [model includeWeakRefs not]]]) ifFalse: [
					(alreadyIncluded includes: pointingObject) ifFalse: [
						alreadyIncluded add: pointingObject.
						strm nextPut: (self class
							with: pointingObject
							name: pointingObject identityHash asString
							model: model
							parent: eachWrapper )]]]]].

	"Release unneeded references"
	allPointers do: [ :oc |
		oc setContents: #() ].
	alreadyIncluded _ nil.
nextLevel size print.
	nextLevel isEmpty ifTrue: [ ^nil ].
	^self shortestPathFromRoot: nextLevel! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 9/4/2012 13:10'!
pointersToEachIn: anArray
	"Find all occurrences in the system of pointers to elements of the argument
	anObject.
	| p1 p2 |
	p1 _ (Smalltalk pointersTo: World).
	p2 _ (Smalltalk pointersToEachIn: {World}) first.
	p1 = p2.
	
	Maybe write a few tests...
	"
	| object lastObject pointers subject |

	Smalltalk garbageCollect.
	lastObject _ Object new.
	"big collection shouldn't grow, so it's contents array is always the same"
	pointers _ anArray collect: [ :each | OrderedCollection new: 1000 ].
	"#allObjectsDo: and #pointsTo: are expanded inline to keep spurious
	 method and block contexts out of the results"
	object _ self someObject.
	[ lastObject == object ] whileFalse: [
		object isInMemory ifTrue: [
			1 to: anArray size do: [ :i |
				subject _ anArray at: i.
				((object statePointsTo: subject)
					or: [ object class == subject ])
						ifTrue: [ (pointers at: i) add: object ]]].
		object _ object nextObject].

	pointers do: [ :oc |
		oc
			remove: anArray;
			remove: thisContext ifAbsent: nil;
			remove: thisContext sender ifAbsent: nil;
			remove: thisContext sender sender ifAbsent: nil;
			remove: oc collector ifAbsent: nil ].
	^pointers! !


!SystemDictionaryTest methodsFor: 'testing' stamp: 'jmv 9/4/2012 13:13'!
testPointersToEachIn
	"
	SystemDictionaryTest new testPointersToEachIn
	"
	| p1 p2 |
	p1 _ (Smalltalk pointersTo: World).
	p2 _ (Smalltalk pointersToEachIn: {World}) first.
	self assert: p1 = p2! !


!Object methodsFor: 'tracing' stamp: 'jmv 9/3/2012 22:30'!
explorePointers
	"Don't include Weak references."

	ObjectExplorerWindow
		open: (PointerExplorer new rootObject: self)
		label: 'References to ', self printString! !

!Object methodsFor: 'tracing' stamp: 'jmv 9/4/2012 13:06'!
inboundPointersExcluding: objectsToExclude
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts. Adapted from PointerFinder class >> #pointersTo:except:"

	| object lastObject pointers objectsToAlwaysExclude |
	Smalltalk garbageCollect.
	lastObject _ Object new.
	"big collection shouldn't grow, so it's contents array is always the same"
	pointers := OrderedCollection new: 1000.
	"#allObjectsDo: and #pointsTo: are expanded inline to keep spurious
	 method and block contexts out of the results"
	object := self someObject.
	[lastObject == object] whileFalse: [
		object isInMemory
			ifTrue: [((object statePointsTo: self)
				or: [object class == self])
					ifTrue: [pointers add: object]].
		object := object nextObject].

	objectsToAlwaysExclude := {
		pointers collector.
		thisContext.
		thisContext sender.
		thisContext sender sender.
		objectsToExclude.
	}.

	^ pointers removeAllSuchThat: [ :ea |
		(objectsToAlwaysExclude identityIncludes: ea)
			or: [ objectsToExclude identityIncludes: ea ]]! !


!FileList methodsFor: 'private' stamp: 'jmv 9/3/2012 22:47'!
postOpen

	directory ifNotNil: [
		self changed: #(openPath), directory pathParts ]! !


!IndentingListItemMorph methodsFor: 'private-container protocol' stamp: 'jmv 9/4/2012 13:36'!
toggleExpandedState
	| newChildren toDelete c |
	isExpanded _ isExpanded not.
	toDelete _ OrderedCollection new.
	firstChild ifNotNil: [
		firstChild withSiblingsDo: [ :aNode |
			aNode recursiveAddTo: toDelete ]].
	container noteRemovalOfAll: toDelete.
	(isExpanded and: [ complexContents hasContents ]) ifFalse: [
		firstChild _ nil.
"	 	nextSibling _ firstChild _ nil."
		^ self redrawNeeded ].
	(c _ complexContents contents) isEmpty ifTrue: [ ^ self redrawNeeded ].
	newChildren _ container
		addSubmorphsAfter: self
		fromCollection: c
		allowSorting: true.
	firstChild _ newChildren first! !


!InspectorWindow methodsFor: 'menu building' stamp: 'jmv 9/3/2012 23:24'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu object |
	aMenu _ MenuMorph new defaultTarget: self.

	aMenu addList: #(
		('inspect (i)'							inspectSelection)
		('explore (I)'						exploreSelection)
		('basic inspect'						inspectBasic)
		('explore pointers'					exploreObjectPointers)
		('explore all pointers (incl.weak)'	exploreAllObjectPointers)).

	object _ model object.
	(object isKindOf: Dictionary) ifTrue: [ aMenu addList: #(
		-
		('senders of this key'				sendersOfSelectedKey)
		('add key'							addEntry)
		('rename key'						renameEntry)
		('remove'							removeSelection			''		model)) ]

	ifFalse: [ (object isKindOf: Set) ifTrue: [ aMenu addList: #(
		-
		('remove'							removeSelection			''		model))]].

	aMenu addList: #(
		-
		('browse full (b)'					browseMethodFull)
		('browse hierarchy (h)'				browseHierarchy)
		('browse protocol (p)'				browseFullProtocol)).
	^ aMenu! !


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'jmv 9/4/2012 12:49'!
genericMenu
	"Borrow a menu from my inspector"
	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				selector: #yourself]
		ifNotNil: [
			aMenu addList: #(
				('inspect (i)'							inspectSelection)
				('explore (I)'						exploreSelection)
				('basic inspect'						inspectBasic)
				('explore pointers'					exploreObjectPointers)
				('explore all pointers (incl.weak)'	exploreAllObjectPointers)
				-
				('browse full (b)'					browseMethodFull)
				('browse hierarchy (h)'				browseHierarchy)
				('browse protocol (p)'				browseFullProtocol)).
			aMenu addLine;
				add: 'monitor changes'
				target: model			"Model!!"
				selector: #monitor:
				argument: model getCurrentSelection.
			model class = PointerExplorer ifTrue: [
				aMenu addLine;
					add: 'shortest ref. path from globals (slow!!)'
					target: self
					selector: #expandPathFromRoot ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: model			"Model!!"
				selector: #stopMonitoring].
	^ aMenu! !


!HierarchicalListMorph methodsFor: 'updating' stamp: 'jmv 9/4/2012 00:00'!
update: aSymbol
	super update: aSymbol.
	aSymbol == getSelectionSelector 
		ifTrue: [
			self selection: self getCurrentSelectionItem.
			^self].
	aSymbol == getListSelector 
		ifTrue: [
			self list: self getList.
			^self].
	((aSymbol isKindOf: Array) 
		and: [ aSymbol notEmpty and: [aSymbol first == #openPath]]) 
			ifTrue: [
				^(scroller submorphs at: 1 ifAbsent: [^self]) 
					openPath: aSymbol allButFirst adaptor: #asString compare: #=]! !

!HierarchicalListMorph methodsFor: 'private' stamp: 'jmv 9/4/2012 14:45'!
addMorphsTo: morphList from: aCollection allowSorting: sortBoolean withExpandedItems: expandedItems atLevel: newIndent

	| priorMorph newCollection firstAddition |
	priorMorph _ nil.
	newCollection _ (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		aCollection asOrderedCollection sort: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]
	] ifFalse: [
		aCollection
	].
	firstAddition _ nil.
	newCollection do: [:item | 
		priorMorph _ self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: newIndent.
		firstAddition ifNil: [firstAddition _ priorMorph].
		morphList add: priorMorph.
		((item hasEquivalentIn: expandedItems) or: [priorMorph isExpanded]) ifTrue: [
			priorMorph beExpanded.
			priorMorph 
				addChildrenForList: self 
				addingTo: morphList
				withExpandedItems: expandedItems.
		].
	].
	^firstAddition
	
! !


!PointerExplorerWrapper methodsFor: 'accessing' stamp: 'jmv 9/4/2012 12:53'!
contents
	| objects |
	Smalltalk garbageCollect.
	objects _ Smalltalk pointersTo: item except: (Array with: self).
	^ (objects reject: [ :each |
		"Reject PointerExplorer stuff (wrapper and main model).
		Reject weak refs, unles includeWeakRefs is true."
		each class = self class or: [ each class = PointerExplorer or: [
				each class isWeak and: [model includeWeakRefs not]]]])
			collect: [ :each |
				self class
					with: each
					name: '<-- #', (each referenceDescriptionTo: item), ' <--(', each identityHash asString, ')'
					model: model
					parent: self ]! !


!Theme methodsFor: 'menus' stamp: 'jmv 9/4/2012 12:50'!
basicIcons

"Minimal menu scheme."

	^ { 
		#('open...') -> #openIcon.
		#('windows...' 'find window') -> #windowIcon.
		#('help...' 'explain' 'about this system...') -> #helpIcon.
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

!classDefinition: #PointerExplorer category: #'Tools-Explorer'!
ObjectExplorer subclass: #PointerExplorer
	instanceVariableNames: 'includeWeakRefs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Explorer'!
!methodRemoval: IndentingListItemMorph #isExpanded:!
IndentingListItemMorph removeSelector: #isExpanded:!
!methodRemoval: IndentingListItemMorph #openPath:!
IndentingListItemMorph removeSelector: #openPath:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Theme current initialize!

