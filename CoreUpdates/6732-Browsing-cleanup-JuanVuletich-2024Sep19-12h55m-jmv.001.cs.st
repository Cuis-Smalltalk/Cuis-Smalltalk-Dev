'From Cuis7.1 [latest update: #6731] on 19 September 2024 at 12:57:46 pm'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 9/19/2024 11:46:48'!
browseMessageList: methodReferences name: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Create and schedule a MessageSet browser on the message list."

	| messageListSize title |

	messageListSize := methodReferences size.
	messageListSize = 0 ifTrue: [
		PopUpMenu inform: ('There are no\' , labelString) withNewLines.
		^nil ].

	title := messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	^ MethodSetWindow
		openMessageList: methodReferences
		label: title
		autoHighlight: autoHighlightString
		allOccurrences: aBoolean! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 11:54:39'!
browseHierarchy
	"Create and schedule a new hierarchy browser
	on the currently selected class or meta."

	| answer |
	answer := nil.
	model selectedClassOrMetaClass ifNotNil: [ :myClass |
		answer := HierarchyBrowserWindow
			onClass: myClass selector: model selectedMessageName ].
	^answer! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 11:43:20'!
browseHierarchyAndReplaceMe
	"Create and schedule a new hierarchy browser on the currently selected class or meta.
	Resize and position it over me and dismiss me."

	self browseHierarchy ifNotNil: [ :newBrowserWindow |
		newBrowserWindow
			positionInWorld: self positionInWorld;
			extentInWorld: self extentInWorld.
		self dismissMorph ].! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 11:41:50'!
browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	| answer |
	answer := nil.
	model selectedClassOrMetaClass ifNotNil: [ :myClass |
		answer := BrowserWindow
			fullOnClass: myClass selector: model selectedMessageName ].
	^answer! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 11:43:54'!
browseMethodFullAndReplaceMe
	"Create and schedule a full Browser and then select the current class and message.
	Resize and position it over me and dismiss me."

	self browseMethodFull ifNotNil: [ :newBrowserWindow |
		newBrowserWindow
			positionInWorld: self positionInWorld;
			extentInWorld: self extentInWorld.
		self dismissMorph ].! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 11:46:13'!
methodInheritance
	"Create and schedule a method browser on the inheritance of implementors."

	| list aClassNonMeta isMeta theClassOrMeta aClass sel |
	aClass := model selectedClassOrMetaClass.
	sel := model selectedMessageName.
	aClass ifNil: [ ^ nil ].
	sel ifNil: [ ^ nil ].
	aClassNonMeta := aClass theNonMetaClass.
	isMeta := aClassNonMeta ~~ aClass.
	list := OrderedCollection new.
	aClass allSuperclasses reverseDo: [ :cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (MethodReference class: cl selector: sel) ]].
	aClassNonMeta
		allSubclassesWithLevelDo: [ :cl :level |
			theClassOrMeta := isMeta
				ifTrue: [ cl class ]
				ifFalse: [ cl ].
			(theClassOrMeta includesSelector: sel) ifTrue: [
				list addLast: (MethodReference class: theClassOrMeta selector: sel) ]]
		startingLevel: 0.
	^ Smalltalk
		browseMessageList: list
		name: 'Implementors of ' , sel, ' in ', aClass name, ' and super / sub classes'.! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 11:47:31'!
methodInheritanceAndReplaceMe
	"Create and schedule a method browser on the inheritance of implementors.
	Resize and position it over me and dismiss me."

	self browseHierarchy ifNotNil: [ :newBrowserWindow |
		newBrowserWindow
			positionInWorld: self positionInWorld;
			extentInWorld: self extentInWorld.
		self dismissMorph ].! !


!CodeFileBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 11:55:33'!
browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	|  answer myClass |
	answer := nil.
	(myClass := model selectedClassOrMetaClass) ifNotNil: [
		answer := BrowserWindow fullOnClass: myClass realClass selector: model selectedMessageName ].
	^answer! !

!CodeFileBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 11:45:57'!
methodInheritance
	(model selectedClassOrMetaClass isNil or:
		[model selectedClassOrMetaClass hasDefinition])
			ifFalse: [^super methodInheritance].
	^nil! !


!HierarchyBrowserWindow class methodsFor: 'GUI building' stamp: 'jmv 9/19/2024 11:57:57'!
forClassesNamed: nameList title: aTitle
	"Initialize the receiver for the class-name-list and title provided"

	| browser |
	browser := ClassListBrowser new initForClassesNamed: nameList title: aTitle.
	self openNoSysCat: browser label: aTitle

	"HierarchyBrowserWindow forClassesNamed: #(Browser Morph Float) title: 'Frogs'"! !

!methodRemoval: CodeProvider #hierarchyBrowser stamp: 'jmv 9/19/2024 12:56:49'!
CodeProvider removeSelector: #hierarchyBrowser!
