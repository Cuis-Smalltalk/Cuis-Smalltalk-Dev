'From Cuis7.9 [latest update: #8315] on 4 November 2026 at 5:14:59 pm'!

!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 4/Nov/2026, 8:13:37 pm (UTC)'!
classCommentOrNil
	^ classComment! !


!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 4/Nov/2026, 8:11:19 pm (UTC)'!
didCodeChangeElsewhere
	"Determine whether the code for the currently selected method and class has been changed somewhere else."

	| aClass aSelector aCompiledMethod |
	currentCompiledMethod ifNil: [^ false].
	(aClass := self selectedClassOrMetaClass) ifNil: [^ false].
	(aSelector := self selectedMessageName) ifNil: [^ false].
	^ ((aCompiledMethod := aClass compiledMethodAt: aSelector ifAbsent: [^ false]) ~~ currentCompiledMethod)
		and: [aCompiledMethod last ~= 0 "either not yet installed"
				or: [currentCompiledMethod last = 0 "or these methods don't have source pointers"]]
	! !


!Browser methodsFor: 'accessing' stamp: 'jmv 4/Nov/2026, 8:14:42 pm (UTC)'!
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
			currentCompiledMethod := theClass organization classCommentOrNil.
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


!MethodSet methodsFor: 'message list' stamp: 'jmv 4/Nov/2026, 8:13:53 pm (UTC)'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class := selectedMessage actualClass.
	selector := selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod := class organization classCommentOrNil.
			^ class smalltalkComment ].
		selector == #Definition ifTrue: [
			^ class definition ].].

	(class notNil and: [ class includesSelector: selector]) ifFalse: [
		currentCompiledMethod := nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod := class compiledMethodAt: selector ifAbsent: nil.

	self showingProperties ifTrue: [
		^String streamContents: [ :strm |
			strm
				nextPutAll: '"The following Smalltalk expression evaluates to the Properties for this method'; newLine;
				nextPutAll: 'It must have the form of an Array of associations. For instance something like:'; newLine;
				nextPutAll: '{#propertyName1 -> 3.141592. #propertyName2 -> #stuff. #propertyName3 -> #(#toto #toton #totona)}'; newLine;
				nextPutAll: 'You can edit it and save as needed"'; newLine.
			currentCompiledMethod propertiesAsArray storePropertiesOn: strm ]].

	^self sourceStringPrettifiedAndDiffed
! !


!PseudoClass methodsFor: 'class' stamp: 'jmv 4/Nov/2026, 8:14:46 pm (UTC)'!
smalltalkComment
	| rStr |
	rStr := self organization classCommentOrNil.
	^rStr
		ifNil: [ self name,' has not been commented in this file']
		ifNotNil: [ rStr string]! !

!PseudoClass methodsFor: 'testing' stamp: 'jmv 4/Nov/2026, 8:14:30 pm (UTC)'!
hasComment
	^self organization classCommentOrNil notNil! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 4/Nov/2026, 8:14:34 pm (UTC)'!
fileOutDefinitionOn: aStream
	self hasDefinition ifFalse:[^self].
	aStream nextChunkPut: self definition; newLine.
	self hasComment ifTrue: [
		aStream newLine.
		self organization classCommentOrNil fileOutOn: aStream]! !

!methodRemoval: BasicClassOrganizer #commentRemoteStr stamp: 'jmv 4/Nov/2026, 8:14:49 pm (UTC)'!
BasicClassOrganizer removeSelector: #commentRemoteStr!
