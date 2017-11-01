'From Cuis 5.0 of 7 November 2016 [latest update: #3205] on 1 November 2017 at 4:07:42 pm'!
!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MessageSet subclass: #ProtocolBrowser
	instanceVariableNames: 'selectiveClassListIndex selectiveClassList baseClass selectedName exclude '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
!classDefinition: #ProtocolBrowserWindow category: #'Morphic-Tools'!
MessageSetWindow subclass: #ProtocolBrowserWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!ProtocolBrowserWindow commentStamp: '<historical>' prior: 0!
A view of the messages available to a class from itself upward through the class hierarchy. The viewed protocol can be pruned by selecting a superclass in the class hierchy pane.!


!ProtocolBrowser methodsFor: 'accessing' stamp: 'dhn 10/31/2017 17:06:51'!
labelString
	"Answer the string for the window title"
	
	^ 'Protocol for: ', baseClass name, ' up to: ', selectedName! !

!ProtocolBrowser methodsFor: 'private' stamp: 'dhn 10/31/2017 17:38:39'!
hierarchyForClass: aClass
	"Set the class hierarchy for the list pane"
	| tab |

	selectiveClassList _ OrderedCollection new.
	tab _ ''.
	aClass withAllSuperclasses reverse do: [:ea | 
		selectiveClassList add: tab , ea name.
		tab _ tab , '  '].
	self classListIndex: 0! !

!ProtocolBrowser methodsFor: 'private' stamp: 'dhn 10/31/2017 16:56:42'!
protocolFor: anIndex
	"Change the listed protocol"

	exclude _ OrderedCollection new.
	anIndex > 0
		ifTrue: [
			selectedName _ (selectiveClassList at: anIndex) withBlanksTrimmed.
			(1 to: anIndex - 1) do: [:ix |
				exclude addLast: (selectiveClassList at: ix) withBlanksTrimmed]]
		ifFalse: [
			selectedName _ nil.
			].
	self on: baseClass.
	self changed: #relabel! !

!ProtocolBrowser methodsFor: 'private' stamp: 'dhn 10/30/2017 15:40:07'!
selectiveClassList
	"Answer the value of selectiveClassList"

	^ selectiveClassList! !

!ProtocolBrowser methodsFor: 'private' stamp: 'dhn 10/30/2017 17:02:13'!
selectiveClassListIndex
	"Answer the value of selectiveClassListIndex"

	selectiveClassListIndex ifNil: [selectiveClassListIndex _ 0].
	^ selectiveClassListIndex! !

!ProtocolBrowser methodsFor: 'private' stamp: 'dhn 10/30/2017 17:02:28'!
selectiveClassListIndex: anObject
	"Set the value of selectiveClassListIndex"

	selectiveClassListIndex _ anObject.
	self protocolFor: selectiveClassListIndex
! !

!ProtocolBrowser methodsFor: 'initialization' stamp: 'dhn 10/31/2017 15:03:26'!
initialize

	exclude _ OrderedCollection new! !


!ProtocolBrowserWindow methodsFor: 'GUI building' stamp: 'dhn 10/30/2017 17:28:22'!
buildMorphicWindow
	"Answer a morphic window that can display the receiver with a class hierarchy"
	| topRow |

	topRow _ LayoutMorph newRow.
	topRow 
		addMorph: self buildSelectiveClassList proportionalWidth: 0.3;
		addAdjusterMorph;
		addMorph: self buildMorphicMessageList proportionalWidth: 0.7.
	self layoutMorph
		addMorph: topRow proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.8.
	model changed: #editSelection! !

!ProtocolBrowserWindow methodsFor: 'GUI building' stamp: 'dhn 10/30/2017 16:56:49'!
buildSelectiveClassList
	"Define the class hierarchy list pane"

	^PluggableListMorph
		model: model
		listGetter: #selectiveClassList
		indexGetter: #selectiveClassListIndex
		indexSetter: #selectiveClassListIndex:! !

!ProtocolBrowserWindow methodsFor: 'updating' stamp: 'dhn 10/30/2017 19:23:00'!
update: aSymbol
	"Respond to events of the Dependency Mechanism"
	
	super update: aSymbol.
	aSymbol == #relabel
		ifTrue: [self setLabel: model labelString]! !


!ProtocolBrowser methodsFor: 'private' stamp: 'dhn 10/31/2017 16:33:52'!
initListFrom: selectorCollection highlighting: aClass 
	"Make up the messageList with items from aClass in boldface."
	| defClass item |

	messageList _ OrderedCollection new.
	selectorCollection do: [ :selector |  
		defClass _ aClass whichClassIncludesSelector: selector.
		item _ selector, '     (' , defClass name , ')'.
		defClass == aClass ifTrue: [item _ item asText allBold].
		messageList add: (
			MethodReference new
				setClass: defClass 
				methodSymbol: selector 
				stringVersion: item)].
	self hierarchyForClass: (baseClass _ aClass)! !

!ProtocolBrowser methodsFor: 'private' stamp: 'dhn 10/31/2017 18:08:50'!
on: aClass
	"Initialize the protocol for the class, aClass."
	"Optionally, the upper part of the protocol is excluded."
	| selectors |
	
	selectors _ Set new.
	aClass withAllSuperclasses do: [ :each |
		(exclude includes: each name) ifFalse: [selectors addAll: each selectors]].
	self
		initListFrom: selectors asArray sort
		highlighting: aClass! !


!CodeWindow methodsFor: 'menu commands' stamp: 'dhn 10/30/2017 16:28:04'!
browseFullProtocol
	"Create and schedule a new protocol browser on the currently selected class or meta."

	| aPBrowser label |
	model selectedClassOrMetaClass ifNotNil: [ :classOrMetaclass |
		aPBrowser _ ProtocolBrowser new on: classOrMetaclass.
		label _ 'Entire protocol of: ', classOrMetaclass name.
		ProtocolBrowserWindow open: aPBrowser label: label ]! !

!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MessageSet subclass: #ProtocolBrowser
	instanceVariableNames: 'baseClass selectiveClassList selectiveClassListIndex selectedName exclude'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
