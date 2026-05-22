"
I represent a query path into the internal representation of an object. As a TextProvider, the string I represent is the value of the currently selected variable of the observed object.
"
Class {
	#name : 'Inspector',
	#superclass : 'TextProvider',
	#instVars : [
		'acceptedContentsCache',
		'object',
		'selectionIndex',
		'bindings'
	],
	#category : 'Tools-Inspector',
	#package : 'Tools',
	#tag : 'Inspector'
}

{ #category : 'instance creation' }
Inspector class >> inspect: anObject [
	"Answer an instance of me to provide an inspector for anObject."
	
	"We call basicNew to avoid a premature initialization; the instance method 
	inspect: anObject will do a self initialize."
	
	"This does not open an inspector in the User Interface, it just answers an Inspector Model."

	^self basicNew inspect: anObject
]

{ #category : 'instance creation' }
Inspector class >> openOn: anObject [
	^ self openOn: anObject withLabel: nil
]

{ #category : 'instance creation' }
Inspector class >> openOn: anObject withLabel: label [
	InspectorWindow open: (self inspect: anObject) label: label
]

{ #category : 'shout styling' }
Inspector >> aboutToStyleWith: anSHTextStyler [
	"This is a notification that anSHTextStyler is about to re-style its text."

	anSHTextStyler workspace: self.
]

{ #category : 'selecting' }
Inspector >> accept: aString [

	| result |
	result := self doItReceiver class compilerClass new
				evaluate: aString asPlainString readStream
				in: self doItContext
				to: self doItReceiver
				notifying: nil	"fix this"
				ifFail:  [^ false].
	acceptedContentsCache := result printString.
	self replaceSelectionValue: result.	"may put contents back"
	self acceptedContentsChanged.
	^ true
]

{ #category : 'contents' }
Inspector >> acceptedStringOrText [
	"We need our cache not to be modified by user changes"
	^(acceptedContentsCache ifNil: '') copy
]

{ #category : 'shout styling' }
Inspector >> allowsStylingFor: selector isTextDirty: hasUnsavedEdits [
	"Answer true to allow styling to proceed, or false to veto the styling.
	Not even asked if text stylyng makes no sense for this model.
	True by default."

	selector = #acceptedContents ifTrue: [
		^hasUnsavedEdits ].

	^true
]

{ #category : 'user interface support' }
Inspector >> autoCompleterClassFor: textGetter [
	^SmalltalkCompleter
]

{ #category : 'accessing' }
Inspector >> baseFieldList [
	"Answer an Array consisting of 'self'
	and the instance variable names of the inspected object."

	^ (Array with: 'self' with: 'all inst vars')
			, self objectClass allInstVarNames
]

{ #category : 'shout styling' }
Inspector >> bindingNamesDo: aBlock [

	self objectClass allInstVarNames do: aBlock.
	bindings keysDo: aBlock
]

{ #category : 'binding' }
Inspector >> bindingOf: aString [
	
	(bindings includesKey: aString) ifFalse: [
		aString first isUppercase
			ifTrue: [^nil]
			ifFalse: [bindings at: aString put: nil]].
		
	^bindings associationAt: aString
]

{ #category : 'private' }
Inspector >> changedSelectionIndex [

	acceptedContentsCache := self selectionPrintString.
	self acceptedContentsChanged.
	self changed: #selectionIndex
]

{ #category : 'private' }
Inspector >> classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition [

	self objectClass = newClass ifTrue: [self changed: #fieldList]
]

{ #category : 'auto complete' }
Inspector >> computeMessageEntriesIn: anAutocompleter ofInstVarNamed: aName   [

	anAutocompleter computeMessageEntriesForClass: (object instVarNamed: aName) class 
]

{ #category : 'user interface support' }
Inspector >> displayLabel [

	| label |
	object isObject ifFalse: [^ 'Inspect: ', self objectClass name].
	label := [object shortPrintString]
		on: UnhandledError
		do: [:ex | ex return: self objectClass printString, ' (printing failed)'].
	"Note this is ambiguous: what happens with (Array with: Array)?"
	(label includesSubString: self objectClass name) ifTrue: [^'Inspect: ', label].
	^ 'Inspect: ', self objectClass name, ': ', label
]

{ #category : 'user interface support' }
Inspector >> doItContext [
	"Answer the context in which a text selection can be evaluated."

	^nil
]

{ #category : 'user interface support' }
Inspector >> doItReceiver [
	"Answer the object that should be informed of the result of evaluating a
	text selection."
	^object
]

{ #category : 'user interface support' }
Inspector >> editorClassFor: textGetter [
	^SmalltalkEditor
]

{ #category : 'accessing' }
Inspector >> fieldList [
	"Answer the base field list plus an abbreviated list of indices."

	self objectClass isVariable ifFalse: [^ self baseFieldList].
	^ self baseFieldList , (self indexLabelsForSize: self objectSize)
]

{ #category : 'shout styling' }
Inspector >> hasBindingOf: aString [
	^ (self objectClass allInstVarNames includes: aString) or: [ 	bindings includesKey: aString ]
]

{ #category : 'shout styling' }
Inspector >> hasBindingThatBeginsWith: aString  [

	| beginsWithBlock |
	
	beginsWithBlock := [ :varName | varName beginsWith: aString ].
	
	^(self objectClass allInstVarNames anySatisfy: beginsWithBlock) or: [ bindings keys anySatisfy: beginsWithBlock ]
]

{ #category : 'constants' }
Inspector >> indexHead [
	"This is the max index shown before skipping to the 
	last i2 elements of very long arrays"
	^ 500
]

{ #category : 'constants' }
Inspector >> indexLabelsForSize: aSize [

	^aSize <= self indexSize
		ifTrue: [(1 to: aSize) collect: [:i | i printString]]
		ifFalse: [(1 to: self indexHead) , (aSize - self indexTail + 1 to: aSize) collect: [:i | i printString]]
]

{ #category : 'constants' }
Inspector >> indexSize [
	^self indexHead + self indexTail
]

{ #category : 'constants' }
Inspector >> indexTail [
	"This is the number of elements to show at the end
	of very long arrays"
	^ 30
]

{ #category : 'initialization' }
Inspector >> initialize [

	super initialize.
	acceptedContentsCache := ''.
	selectionIndex := 0.
	self initializeEvents.
	self initializeBindings 
]

{ #category : 'shout styling' }
Inspector >> initializeBindings [
	
	bindings := Dictionary new.
	self changed: #actualContents.

]

{ #category : 'initialization' }
Inspector >> initializeEvents [

	SystemChangeNotifier uniqueInstance
		when: #classDefinitionChanged send: #classDefinitionChangedFrom:to:oldClassDefinition: to: self
]

{ #category : 'initialization' }
Inspector >> inspect: anObject  [
	"Initialize the receiver so that it is inspecting anObject. There is no current selection."
	
	object := anObject. 
	self initialize
]

{ #category : 'user commands' }
Inspector >> inspectSelection [
	self selection inspect
]

{ #category : 'testing' }
Inspector >> is: aSymbol [
	^ aSymbol == #providesBindings or: [ super is: aSymbol ]
]

{ #category : 'constants' }
Inspector >> lastMetaField [
	"This value must be synchronized with the base field list, the
	string selection indices, and the senders of this message"

	^2
]

{ #category : 'contents' }
Inspector >> methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock [

	^[ self selectedClassOrMetaClass methodNodeFor: aSourceCode noPattern: true ] on: Error, UndeclaredVariableReference do:  aParsingErrorBlock 
]

{ #category : 'accessing' }
Inspector >> object: anObject  [
	"Set anObject to be the object being inspected by the receiver."

	| oldIndex |
	anObject == object ifTrue: [^self update].
	oldIndex := self selectionIsUnmodifiable ifTrue: [selectionIndex] ifFalse: [0].
	self inspect: anObject.
	oldIndex := oldIndex min: self fieldList size.
	self changed: #inspectObject.
	oldIndex > 0 ifTrue: [self toggleIndex: oldIndex].
	self changed: #fieldList.
	self acceptedContentsChanged
]

{ #category : 'private - mirror interface' }
Inspector >> object: anObject basicAt: index  [
	"Answer the value of an indexable element in the argument anObject without sending
	 it a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable. This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'private - mirror interface' }
Inspector >> object: anObject instVarAt: anIndex [
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable. Essential for the
	 debugger. See  Object documentation whatIsAPrimitive."

	<primitive: 73>
	"Access beyond fixed variables."
	^self object: anObject basicAt: anIndex - (self objectClass: anObject) instSize
]

{ #category : 'private' }
Inspector >> objectClass [

	^self objectClass: object
]

{ #category : 'private - mirror interface' }
Inspector >> objectClass: anObject [

	<primitive: 111>
	self primitiveFailed
]

{ #category : 'private' }
Inspector >> objectClassInstSize [

	^self objectClass instSize
]

{ #category : 'private' }
Inspector >> objectSize [

	^self objectSize: object
]

{ #category : 'private - mirror interface' }
Inspector >> objectSize: anObject [
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0
]

{ #category : 'user interface support' }
Inspector >> objectSpecificMenuSpecFrom: aWindow [

	^nil
]

{ #category : 'private' }
Inspector >> printStringErrorFieldName [

	^self selectionIsUnmodifiable
		ifTrue: ['self']
		ifFalse: [self selectedSlotName ifNil: ['??']]
]

{ #category : 'private' }
Inspector >> printStringErrorText [

	^('<error in printString: evaluate "',
		self printStringErrorFieldName,
		' printString" to debug>') asText
]

{ #category : 'constants' }
Inspector >> printStringLimit [

	^4000
]

{ #category : 'accessing' }
Inspector >> printedSelectionIndices [

	^#(0 2)
]

{ #category : 'selecting' }
Inspector >> replaceSelectionValue: anObject  [
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, 
	anObject."
	| si instVarIndex |
	self selectionIsUnmodifiable ifTrue: [
		self toggleIndex: (si := selectionIndex).  
		self toggleIndex: si.
		^ object].
	instVarIndex := self selectionIndexNonMeta.
	instVarIndex > self objectClassInstSize
		ifFalse: [^ object instVarAt: instVarIndex put: anObject].
	self objectClass isVariable or: [self error: 'Cannot replace selection'].
	^object basicAt: self selectedObjectIndex put: anObject
]

{ #category : 'selecting' }
Inspector >> safelyPrintWith: aBlock [

	^aBlock
		on: UnhandledError
		do:
			[:ex |
				ex return:
					(self printStringErrorText
						addAttribute: TextColor red;
						yourself)
			]
]

{ #category : 'accessing' }
Inspector >> selectedClass [
	"Answer the class of the receiver's current selection"

	self selectionIsUnmodifiable ifTrue: [^ self objectClass].
	^ self selection class
]

{ #category : 'accessing' }
Inspector >> selectedClassOrMetaClass [

	^ self selectedClass	"I don't know any better"
]

{ #category : 'selecting' }
Inspector >> selectedObjectIndex [
	"Answer the index of the inspectee's collection that the current selection refers to."

	| basicIndex |
	basicIndex := self selectionIndexNonMeta - self objectClassInstSize.
	^(self objectSize <= self indexSize or: [basicIndex <= self indexHead])
		ifTrue: [basicIndex]
		ifFalse: [self objectSize - self indexSize + basicIndex]
]

{ #category : 'selecting' }
Inspector >> selectedSlotName [

	^ self fieldList at: self selectionIndex ifAbsent: nil
]

{ #category : 'selecting' }
Inspector >> selection [
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| index |
	self selectionIsMeta ifTrue: [^self selectionMeta].
	self selectionIndexNonMeta <= self objectClassInstSize
		ifTrue: [^ self object: object instVarAt: self selectionIndexNonMeta].
	index := self selectedObjectIndex.
	^(object isObject and: [object isString])
		ifTrue: [ object at: index ]
		ifFalse: [ self object: object basicAt: index ]
]

{ #category : 'selecting' }
Inspector >> selectionIndex [
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. Answer the index into the list of the selected variable."

	^selectionIndex
]

{ #category : 'selecting' }
Inspector >> selectionIndexNonMeta [

	^self selectionIndex - self lastMetaField
]

{ #category : 'selecting' }
Inspector >> selectionIsMeta [

	^self selectionIndex <= self lastMetaField
]

{ #category : 'selecting' }
Inspector >> selectionIsUnmodifiable [
	"Answer if the current selected variable is modifiable via acceptance in the code pane.  For most inspectors, no selection and a selection of self (selectionIndex = 1) are unmodifiable"

	^self selectionIsMeta
]

{ #category : 'selecting' }
Inspector >> selectionMeta [
	"Answer the value of the selected meta field"

	self selectionIndex = 0 ifTrue: [^ ''].
	self selectionIndex = 1 ifTrue: [^ object].
	self selectionIndex = 2 ifTrue: [^ self safelyPrintWith: [object longPrintTextLimitedTo: self printStringLimit]].
	^self selectionMetaUnknown
]

{ #category : 'selecting' }
Inspector >> selectionMetaUnknown [
	
	^'<Unknown meta field index ', self selectionIndex printString, '>'
]

{ #category : 'selecting' }
Inspector >> selectionPrintString [
	"Returns the current selection as a string"
	
	^self shouldAssumeSelectionIsPrinted 
		ifTrue: [ self selection ]
		ifFalse: [ self safelyPrintWith: [ self selection printTextLimitedTo: self printStringLimit ] ].
]

{ #category : 'testing' }
Inspector >> shouldAssumeSelectionIsPrinted [

	^self printedSelectionIndices includes: self selectionIndex
]

{ #category : 'stepping' }
Inspector >> stepAt: millisecondSinceLast [
	| newText |
	newText := self selectionPrintString.
	newText = acceptedContentsCache ifFalse: [
		acceptedContentsCache := newText.
		self acceptedContentsChanged ]
]

{ #category : 'user interface support' }
Inspector >> textStylerClassFor: textGetter [

	(Preferences at: #syntaxHighlightingAsYouType)
		ifFalse: [ ^nil ].
	^SHTextStylerST80
]

{ #category : 'selecting' }
Inspector >> toggleIndex: anInteger [
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. If anInteger is the index of this variable, then deselect it. 
	Otherwise, make the variable whose index is anInteger be the selected 
	item."

	selectionIndex := selectionIndex = anInteger ifTrue: [0] ifFalse: [anInteger].
	self changedSelectionIndex
]

{ #category : 'accessing' }
Inspector >> update [
	"Reshow contents, assuming selected value may have changed."

	selectionIndex = 0 ifFalse: [self changedSelectionIndex]
]

{ #category : 'accessing' }
Inspector >> wantsSteps [
	^ true
]
