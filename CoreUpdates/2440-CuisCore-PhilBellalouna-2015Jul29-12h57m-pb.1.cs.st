'From Cuis 4.2 of 25 July 2013 [latest update: #2439] on 29 July 2015 at 1:02:46.091621 pm'!

!InspectorWindow methodsFor: 'menu commands' stamp: 'pb 7/29/2015 12:58'!
openWeightExplorer
	"Create and schedule a Weight Explorer on the receiver's model's currently selected object."

	^WeightTracer openExplorerOn: model selection! !


!ObjectExplorerWindow methodsFor: 'as yet unclassified' stamp: 'pb 7/29/2015 13:01'!
openWeightExplorer
	"Create and schedule a Weight Explorer on the receiver's model's currently selected object."

	^WeightTracer openExplorerOn: model object! !


!InspectorWindow methodsFor: 'menu building' stamp: 'pb 7/29/2015 12:57'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu object |
	aMenu _ MenuMorph new defaultTarget: self.

	aMenu addList: #(
		('inspect (i)'							inspectSelection)
		('explore (I)'						exploreSelection)
		('basic inspect'						inspectBasic)
		('references finder'					openReferencesFinder)
		('weight explorer'					openWeightExplorer)).

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


!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'pb 7/29/2015 12:59'!
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
				('weight explorer'					openWeightExplorer)
				-
				('browse full (b)'					browseMethodFull)
				('browse hierarchy (h)'				browseHierarchy)
				('browse protocol (p)'				browseFullProtocol)).
			aMenu addLine;
				add: 'monitor changes'
				target: self
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
				target: self
				selector: #stopMonitoring ].
	^ aMenu! !

