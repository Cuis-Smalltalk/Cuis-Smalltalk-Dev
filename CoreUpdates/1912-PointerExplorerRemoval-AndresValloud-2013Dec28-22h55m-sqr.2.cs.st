'From Cuis 4.2 of 25 July 2013 [latest update: #1910] on 28 December 2013 at 11:14:39.92911 pm'!

!InspectorWindow methodsFor: 'menu building' stamp: 'sqr 12/28/2013 23:04'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu object |
	aMenu _ MenuMorph new defaultTarget: self.

	aMenu addList: #(
		('inspect (i)'							inspectSelection)
		('explore (I)'						exploreSelection)
		('basic inspect'						inspectBasic)
		('references finder'					openReferencesFinder)).

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


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'sqr 12/28/2013 23:05'!
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
				('explore (I)'						exploreSelection)
				('basic inspect'						inspectBasic)
				('references finder'					openReferencesFinder)
				-
				('browse full (b)'					browseMethodFull)
				('browse hierarchy (h)'				browseHierarchy)
				('browse protocol (p)'				browseFullProtocol)).
			aMenu addLine;
				add: 'monitor changes'
				target: model			"Model!!"
				selector: #monitor:
				argument: model getCurrentSelection.
			model class = ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					selector: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: model			"Model!!"
				selector: #stopMonitoring].
	^ aMenu! !

!methodRemoval: ObjectExplorerWindow #exploreAllObjectPointers!
ObjectExplorerWindow removeSelector: #exploreAllObjectPointers!
!methodRemoval: ObjectExplorerWindow #exploreObjectPointers!
ObjectExplorerWindow removeSelector: #exploreObjectPointers!
!methodRemoval: InspectorWindow #exploreAllObjectPointers!
InspectorWindow removeSelector: #exploreAllObjectPointers!
!methodRemoval: InspectorWindow #exploreObjectPointers!
InspectorWindow removeSelector: #exploreObjectPointers!
!methodRemoval: CompiledMethod #referenceDescriptionTo:!
CompiledMethod removeSelector: #referenceDescriptionTo:!
!methodRemoval: Object #exploreAllPointers!
Object removeSelector: #exploreAllPointers!
!methodRemoval: Object #explorePointers!
Object removeSelector: #explorePointers!
!methodRemoval: ProtoObject #referenceDescriptionTo:!
ProtoObject removeSelector: #referenceDescriptionTo:!
!classRemoval: #PointerExplorer!
Smalltalk removeClassNamed: #PointerExplorer!
!classRemoval: #PointerExplorerWrapper!
Smalltalk removeClassNamed: #PointerExplorerWrapper!
