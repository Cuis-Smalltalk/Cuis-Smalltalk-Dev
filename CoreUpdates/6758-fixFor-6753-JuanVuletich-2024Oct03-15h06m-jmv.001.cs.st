'From Cuis7.1 [latest update: #6752] on 3 October 2024 at 3:11:55 pm'!

!ClassDescription methodsFor: 'accessing' stamp: 'jmv 10/3/2024 15:08:15'!
smalltalkComment
	"Answer the receiver's comment. (If missing, supply a template) "
	| aString |
	aString := self theNonMetaClass organization classComment.
	aString isEmpty ifFalse: [^ aString].
	^
'Main comment stating the purpose of this class and relevant relationship to other classes.

Possible useful expressions for doIt or printIt.

Structure:
 instVar1		type -- comment about the purpose of instVar1
 instVar2		type -- comment about the purpose of instVar2

Any further useful comments about the general approach of this implementation.'! !


!PseudoClass methodsFor: 'class' stamp: 'jmv 10/3/2024 15:08:20'!
smalltalkComment
	| rStr |
	rStr := self organization commentRemoteStr.
	^rStr
		ifNil: [ self name,' has not been commented in this file']
		ifNotNil: [ rStr string]! !


!ClassDescription methodsFor: 'accessing' stamp: 'jmv 10/3/2024 15:08:36'!
comment
	"Answer the receiver's comment. (If missing, supply a template) "
	^self smalltalkComment.! !


!Browser methodsFor: 'accessing' stamp: 'jmv 10/3/2024 15:09:58'!
acceptedStringOrText
	"Depending on the current selection, different information is retrieved.
	Answer a string description of that information. This information is the
	method of the currently selected class and message."

	| comment theClass latestCompiledMethod |
	latestCompiledMethod := currentCompiledMethod.
	currentCompiledMethod := nil.

	editSelection == #none ifTrue: [^ ''].
	editSelection == #editSystemCategories 
		ifTrue: [^ systemOrganizer printString].
	self isEditingNewClass ifTrue: [
		selectedSystemCategory ifNil: [ ^'' ].
		^ (theClass := self selectedClass)
			ifNil: [
				Class template: selectedSystemCategory]
			ifNotNil: [
				Class templateForSubclassOf: theClass category: selectedSystemCategory]].
	self isEditingExistingClass 
		ifTrue: [^ self classDefinitionText ].
	editSelection == #editComment 
		ifTrue: [
			(theClass := self selectedClass) ifNil: [^ ''].
			comment := theClass smalltalkComment.
			currentCompiledMethod := theClass organization commentRemoteStr.
			^ comment isEmpty
				ifTrue: ['This class has not yet been commented.']
				ifFalse: [comment]].
	editSelection == #editMessageCategories 
		ifTrue: [^ self classOrMetaClassOrganizer printString].
	editSelection == #newMessage
		ifTrue: [
			^ (theClass := self selectedClassOrMetaClass) 
				ifNil: ['']
				ifNotNil: [theClass sourceCodeTemplate]].
	editSelection == #editMessage
		ifTrue: [
			self showingByteCodes ifTrue: [^ self selectedBytecodes].
			currentCompiledMethod := latestCompiledMethod.
			^ self selectedMessage].

	self error: 'Browser internal error: unknown edit selection.'! !

!Browser methodsFor: 'class functions' stamp: 'jmv 10/3/2024 15:10:06'!
classCommentText
	"return the text to display for the comment of the currently selected class"
	| theClass |
	theClass := self selectedClassOrMetaClass.
	^ Text
		initialFont: (Preferences at: #standardCodeFont)
		stringOrText:
			((theClass notNil and: [ theClass hasComment ])
				ifTrue: [ theClass smalltalkComment ]
				ifFalse: [ '' ]).! !

!Browser methodsFor: 'class functions' stamp: 'jmv 10/3/2024 15:10:17'!
copyClass
	| originalClass originalName copysName newDefinition newMetaDefinition newClass |
	selectedClassName ifNil: [^ self].
	originalClass := self selectedClass.
	originalName := originalClass name.
	self request: 'New class name?'
		initialAnswer: originalName
		verifying: [:aString| aString notEmpty and: [aString ~= originalName]]
		do: [:aString|
			copysName := aString asSymbol.
			(Smalltalk includesKey: copysName)
				ifTrue: [self error: copysName , ' already exists'].
			newDefinition := originalClass definition
				copyReplaceAll: originalName printString
				with: copysName printString.
			newClass := Compiler evaluate: newDefinition logged: true.
			newMetaDefinition := originalClass class definition
				copyReplaceAll: originalClass class name
				with: newClass class name.
			Compiler evaluate: newMetaDefinition logged: true.
			newClass copyAllCategoriesFrom: originalClass.
			newClass class copyAllCategoriesFrom: originalClass class.
			originalClass hasComment ifTrue: [newClass comment: originalClass smalltalkComment].
			self classListIndex: 0.
			self changed: #classList]! !


!MethodSet methodsFor: 'message list' stamp: 'jmv 10/3/2024 15:10:39'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class := selectedMessage actualClass.
	selector := selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod := class organization commentRemoteStr.
			^ class smalltalkComment ].
		selector == #Definition ifTrue: [
			^ class definition ].].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod := nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod := class compiledMethodAt: selector ifAbsent: nil.

	^self sourceStringPrettifiedAndDiffed! !


!ChangeList methodsFor: 'viewing access' stamp: 'jmv 10/3/2024 15:11:04'!
contentsDiffedFromCurrent
	"Answer the contents diffed forward from current (in-memory) method version"

	|  aChange aClass  name aSelector |
	listIndex = 0
		ifTrue: [^ ''].
	aChange := changeList at: listIndex.
	((aChange changeType == #method 
			and: [(aClass := aChange changeClass) notNil]) 
			and: [aClass includesSelector: aChange methodSelector]) ifTrue: [
		aSelector := aChange methodSelector.
		(aClass notNil and: [aClass includesSelector: aSelector])
			ifFalse: [ ^aChange text copy ].
		^DifferenceFinder
				displayPatchFrom: (aClass sourceCodeAt: aSelector)
				to: aChange text
				tryWords: self shouldDiffWords
				prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [aClass]) ].
	(aChange changeType == #classDefinition and: [
			name := aChange changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		aClass := Smalltalk at: name.
		aChange isMetaClassChange ifTrue: [ aClass := aClass class ].
		^DifferenceFinder
				displayPatchFrom: aClass definition to: aChange text tryWords: true].
	(aChange changeType == #classComment
			and: [(aClass := aChange changeClass) notNil]) ifTrue: [
		^DifferenceFinder
				displayPatchFrom: aClass smalltalkComment to: aChange text tryWords: self shouldDiffWords].
	^(changeList at: listIndex) text! !


!PseudoClass methodsFor: 'class' stamp: 'jmv 10/3/2024 15:08:47'!
comment

	^self smalltalkComment.! !

!PseudoClass methodsFor: 'removing' stamp: 'jmv 10/3/2024 15:11:25'!
removeAllUnmodified
	| stClass |
	self exists ifFalse:[^self].
	self removeUnmodifiedMethods: self selectors.
	stClass := self realClass.
	(self hasDefinition and:[stClass definition = self definition]) ifTrue:[definition := nil].
	(self hasComment and:[stClass smalltalkComment asPlainString = self smalltalkComment asPlainString]) ifTrue: [
		self classComment: nil].
	metaClass ifNotNil: [ metaClass removeAllUnmodified]! !

!methodRemoval: ClassDescription class #commentOf: stamp: 'jmv 10/3/2024 15:06:17'!
ClassDescription class removeSelector: #commentOf:!
