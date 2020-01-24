'From Cuis 5.0 [latest update: #4029] on 19 January 2020 at 11:57:15 pm'!
"Change Set:		4030-CuisCore-AuthorName-2020Jan19-23h10m
Date:			19 January 2020
Author:			Nahuel Garbezza

Small improvements to the inspector contextual menu"!


!InspectorWindow methodsFor: 'menu building' stamp: 'RNG 1/19/2020 23:53:02'!
addCollectionSpecificMenuOptionsTo: aMenu

	| object |
	object _ model object.
	(object is: #Dictionary) ifTrue: [
		aMenu addItemsFromDictionaries: self menuOptionsForDictionary ]
	ifFalse: [ (object is: #Set) ifTrue: [
		aMenu addItemsFromDictionaries: self menuOptionsForSet ]]! !

!InspectorWindow methodsFor: 'menu building' stamp: 'RNG 1/19/2020 23:27:18'!
basicMenuOptions

	^ `{
		{
			#label 			-> 		'inspect (i)'.
			#selector 		-> 		#inspectSelection.
			#icon 			-> 		#inspectIcon
		} asDictionary.
		{
			#label 			-> 		'explore (I)'.
			#selector 		-> 		#exploreSelection.
			#icon 			-> 		#exploreIcon
		} asDictionary.
		{
			#label 			-> 		'copy to clipboard (c)'.
			#selector 		-> 		#copySelectionToClipboard.
			#icon 			-> 		#copyIcon
		} asDictionary.
		{
			#label 			-> 		'basic inspect'.
			#selector 		-> 		#inspectBasic.
			#icon 			-> 		#inspectIcon
		} asDictionary.
		{
			#label 			-> 		'references finder'.
			#selector 		-> 		#openReferencesFinder.
			#icon 			-> 		#exploreIcon
		} asDictionary.
		{
			#label 			-> 		'weight explorer'.
			#selector 		-> 		#openWeightExplorer.
			#icon 			-> 		#exploreIcon
		} asDictionary.
	}`! !

!InspectorWindow methodsFor: 'menu building' stamp: 'RNG 1/19/2020 23:24:15'!
menuOptionsForBrowsing

	^ `{
		nil.
		{
			#label 			-> 		'browse full (b)'.
			#selector 		-> 		#browseMethodFull.
			#icon 			-> 		#editFindReplaceIcon
		} asDictionary.
		{
			#label 			-> 		'browse hierarchy (h)'.
			#selector 		-> 		#browseHierarchy.
			#icon 			-> 		#goTopIcon
		} asDictionary.
		{
			#label 			-> 		'browse protocol (p)'.
			#selector 		-> 		#browseFullProtocol.
			#icon 			-> 		#spreadsheetIcon
		} asDictionary.
	}`! !

!InspectorWindow methodsFor: 'menu building' stamp: 'RNG 1/19/2020 23:25:40'!
menuOptionsForDictionary

	^ `{
		nil.
		{
			#label 			-> 		'senders of this key'.
			#selector 		-> 		#sendersOfSelectedKey.
			#icon 			-> 		#mailForwardIcon
		} asDictionary.
		{
			#label 			-> 		'add key'.
			#selector 		-> 		#addEntry.
			#icon 			-> 		#listAddIcon
		} asDictionary.
		{
			#label 			-> 		'rename key'.
			#selector 		-> 		#renameEntry.
			#icon 			-> 		#saveAsIcon
		} asDictionary.
		{
			#label 			-> 		'remove'.
			#object 			-> 		#model.
			#selector 		-> 		#removeSelection.
			#icon 			-> 		#listRemoveIcon
		} asDictionary.
	}`! !

!InspectorWindow methodsFor: 'menu building' stamp: 'RNG 1/19/2020 23:25:27'!
menuOptionsForSet

	^ `{
		nil.
		{
			#label 			-> 		'remove'.
			#object 			-> 		#model.
			#selector 		-> 		#removeSelection.
			#icon 			-> 		#listRemoveIcon
		} asDictionary.
	}`! !


!InspectorWindow methodsFor: 'menu building' stamp: 'RNG 1/19/2020 23:52:42'!
fieldListMenu
	"Arm the supplied menu with items for the field-list of the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addItemsFromDictionaries: self basicMenuOptions.
	self addCollectionSpecificMenuOptionsTo: aMenu.
	aMenu addItemsFromDictionaries: self menuOptionsForBrowsing.
	^ aMenu! !

!InspectorWindow methodsFor: 'menu commands' stamp: 'RNG 1/19/2020 23:11:27'!
sendersOfSelectedKey
	| key |
	key _ model selectedKey.
	key isString ifFalse: [ ^self ].
	Smalltalk browseAllCallsOn: key! !


!InspectorWindow reorganize!
('GUI building' buildContentsText buildEvaluatorText buildList buildMorphicWindow initialExtent)
('menu building' addCollectionSpecificMenuOptionsTo: basicMenuOptions fieldListMenu menuOptionsForBrowsing menuOptionsForDictionary menuOptionsForSet)
('menu commands' addEntry browseClassRefs browseFullProtocol browseHierarchy browseMethodFull copySelectionToClipboard exploreSelection inspectBasic inspectSelection openReferencesFinder openWeightExplorer renameEntry sendersOfSelectedKey)
('keyboard shortcuts' inspectorKey:from:)
('accessing' classDefinitionChangedFrom:to: model:)
!

