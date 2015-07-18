'From Cuis 4.2 of 25 July 2013 [latest update: #2407] on 9 July 2015 at 11:22:29.946362 am'!

!MessageSet commentStamp: '<historical>' prior: 0!
Note: In Squeak, this class is not subclass of Browser anymore. See http://lists.squeakfoundation.org/pipermail/packages/2011-June/004764.html . It might make sense to do the same in Cuis.

I represent a query path of the retrieval result of making a query about methods in the system. The result is a set of methods, denoted by a message selector and the class in which the method was found. As a TextProvider, the string I represent is the source code of the currently selected method. I am typically viewed in a Message Set Browser consisting of a MessageListView and a BrowserCodeView.

Ivar messageList is a collection of MethodReferences.
Ivar selectedMessage is not as symbol (as in superclasses) but the selected MethodReference.!

!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MessageSet subclass: #ProtocolBrowser
	instanceVariableNames: 'selectedClass selectedSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

!MessageSet methodsFor: 'message list' stamp: 'jmv 7/9/2015 10:38'!
selectedMessageName
	"Answer the name of the currently selected message."

	^selectedMessage ifNotNil: [ :methodReference | methodReference methodSymbol ]! !

!MessageSet methodsFor: 'class list' stamp: 'jmv 7/9/2015 10:39'!
selectedClassOrMetaClass
	"Answer the currently selected class (or metaclass)."

	^selectedMessage ifNotNil: [ :methodReference | methodReference actualClass ]! !


!ProtocolBrowser methodsFor: 'private' stamp: 'jmv 7/8/2015 22:31'!
initListFrom: selectorCollection highlighting: aClass 
	"Make up the messageList with items from aClass in boldface."
	| defClass item |

	messageList := OrderedCollection new.
	selectorCollection do: [ :selector |  
		defClass := aClass whichClassIncludesSelector: selector.
		item _ selector, '     (' , defClass name , ')'.
		defClass == aClass ifTrue: [item _ item asText allBold].
		messageList add: (
			MethodReference new
				setClass: defClass 
				methodSymbol: selector 
				stringVersion: item
		)
	]! !


!MessageSetWindow reorganize!
('GUI building' buildLowerPanes buildMorphicWindow windowColor)
('menu building' addExtraShiftedItemsTo:)
('menu commands' filterMessageList)
('top window' activateAndSendTopToBack:)
!

!methodRemoval: ProtocolBrowser #getList!
ProtocolBrowser removeSelector: #getList!
!methodRemoval: ProtocolBrowser #list!
ProtocolBrowser removeSelector: #list!
!methodRemoval: ProtocolBrowser #selectedClassOrMetaClass!
ProtocolBrowser removeSelector: #selectedClassOrMetaClass!
!methodRemoval: ProtocolBrowser #selector!
ProtocolBrowser removeSelector: #selector!
!methodRemoval: ProtocolBrowser #selector:!
ProtocolBrowser removeSelector: #selector:!
!methodRemoval: ProtocolBrowser #setSelector:!
ProtocolBrowser removeSelector: #setSelector:!
!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MessageSet subclass: #ProtocolBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
