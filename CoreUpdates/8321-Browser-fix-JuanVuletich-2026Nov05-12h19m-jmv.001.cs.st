'From Cuis7.9 [latest update: #8320] on 5 November 2026 at 12:19:17 pm'!

!Browser methodsFor: 'accessing' stamp: 'jmv 5/Nov/2026, 3:17:18 pm (UTC)'!
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
			currentCompiledMethod := "theClass organization classCommentOrNil" nil.
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


!MethodSet methodsFor: 'message list' stamp: 'jmv 5/Nov/2026, 3:17:36 pm (UTC)'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class := selectedMessage actualClass.
	selector := selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod := "class organization classCommentOrNil"nil.
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

