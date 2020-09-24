'From Cuis 5.0 [latest update: #4373] on 18 September 2020 at 1:40:56 pm'!

!ProtocolBrowser methodsFor: 'class list' stamp: 'KenD 9/18/2020 13:21:11'!
selectedClass
	"Answer the class that is currently selected. 
	Answer base if no selection exists."
	| className |
	className := (self selectiveClassListIndex = 0) 
					ifTrue:  [self selectiveClassList last]
					ifFalse:  [self selectiveClassList at: self selectiveClassListIndex].
	^ Smalltalk at: ((className findTokens: ' ') first asSymbol)! !

!ProtocolBrowser methodsFor: 'class list' stamp: 'KenD 9/18/2020 12:40:47'!
selectedClassOrMetaClass
	"I ignore metaClass, so.."
	^ self selectedClass! !


!ProtocolBrowserWindow methodsFor: 'menu building' stamp: 'KenD 9/18/2020 12:37:51'!
protocolClassListMenu
	"Fill aMenu with items appropriate for the class list"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Class List'.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'browse full (b)'.
				#selector 			-> 		#browseMethodFull.
				#icon 			-> 		#editFindReplaceIcon
			} asDictionary.
			{
				#label 			-> 		'browse hierarchy (h)'.
				#selector 			-> 		#browseHierarchy.
				#icon 			-> 		#goTopIcon
			} asDictionary.

		}`.
	^ aMenu! !

!ProtocolBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'KenD 9/18/2020 13:39:04'!
protocolClassListKey: aChar from: view 
	"Respond to a Command key.  I am a model with a list of 
	classes and a list of methods.  The 
	view knows how to get the list and selection."

	aChar == $b ifTrue: [^ self browseMethodFull ].
	aChar == $h ifTrue: [^ self browseHierarchy].
	
	^ nil! !


!ProtocolBrowserWindow methodsFor: 'GUI building' stamp: 'KenD 9/18/2020 13:37:59'!
buildSelectiveClassList
	"Define the class hierarchy list pane"

	^PluggableListMorph
		model: model
		listGetter: #selectiveClassList
		indexGetter: #selectiveClassListIndex
		indexSetter: #selectiveClassListIndex:
		 mainView: self 
		menuGetter: #protocolClassListMenu 
		keystrokeAction: #protocolClassListKey:from: ! !

!ProtocolBrowser reorganize!
('accessing' growable labelString)
('private' hierarchyForClass: initListFrom:highlighting: on: onSubProtocolOf: protocolFor: selectiveClassList selectiveClassListIndex selectiveClassListIndex:)
('initialization' initialize)
('class list' selectedClass selectedClassOrMetaClass)
!


!ProtocolBrowserWindow reorganize!
('GUI building' buildMorphicWindow buildSelectiveClassList)
('menu building' protocolClassListMenu)
('updating' update:)
('keyboard shortcuts' protocolClassListKey:from:)
!

