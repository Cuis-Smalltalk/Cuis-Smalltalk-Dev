"
I am an Abstract class.
Subclasses of me can create formatted, coloured, and styled copies of Text that is given to them.
They may perform their styling asynchronously, in a background process which I create and manage.
	

"
Class {
	#name : 'SHTextStyler',
	#superclass : 'Object',
	#instVars : [
		'backgroundProcess',
		'formattedText',
		'textModel',
		'mutex'
	],
	#category : 'Tools-Syntax Highlighting',
	#package : 'Tools',
	#tag : 'Syntax Highlighting'
}

{ #category : 'constants' }
SHTextStyler class >> maxSizeForStyling [
	"Rather arbitrary, but reasonable."
	^20000
]

{ #category : 'constants' }
SHTextStyler class >> minSizeForBackgroundStyling [
	"Rather arbitrary, but reasonable."
	^4096
]

{ #category : 'styling' }
SHTextStyler >> convertAndStyle: text allowBackgroundStyleProcess: aBoolean [
	"Do the styling on the model text.
	After finishing, tell model, by triggering #shoutStyled."
	self getReady.
	formattedText := text.
	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	(aBoolean and: [formattedText size >= self class minSizeForBackgroundStyling])
		ifTrue: [
			formattedText size <= self class maxSizeForStyling ifTrue: [
				self convertAndStyleInBackgroundProcess ]]
		ifFalse: [
			self parseText.
			self privateAutoHighlight.
			self privateConvertAssignmentsOffset: 0.
			self privateStyle.
			textModel changed: #shoutStyled ]
]

{ #category : 'styling' }
SHTextStyler >> convertAndStyleInBackgroundProcess [
	"Do the styling on a copy of the provided text (and in a separate process).
	After finishing, send it to the model, by triggering #shoutStyled
	The the model should grab the TextAttributes we added to the copy, as appropriate."

	self mutex critical: [
		"This part runs at low priority, and signals sem when finished"
		backgroundProcess :=  [
			self parseText.
			self privateAutoHighlight.
			self privateConvertAssignmentsOffset: 0.
			self privateStyle.
			UISupervisor whenUIinSafeState: [
				textModel changed: #shoutStyled ].
			] newProcess.
		backgroundProcess
			priority: Processor userBackgroundPriority;
			name: 'Shout format';
			resume
	]
]

{ #category : 'styling' }
SHTextStyler >> getReady [
	self subclassResponsibility 
]

{ #category : 'private' }
SHTextStyler >> mutex [
	mutex
		ifNil: [ mutex := Mutex new ].
	^mutex
]

{ #category : 'styling' }
SHTextStyler >> parseText [
	self subclassResponsibility 
]

{ #category : 'private' }
SHTextStyler >> privateAutoHighlight [
	self subclassResponsibility 
]

{ #category : 'private' }
SHTextStyler >> privateConvertAssignmentsOffset: delta [
	self subclassResponsibility 
]

{ #category : 'private' }
SHTextStyler >> privateStyle [
	self subclassResponsibility 
]

{ #category : 'private' }
SHTextStyler >> terminateBackgroundStylingProcess [
	self mutex critical: [
		backgroundProcess
			ifNotNil: [
				backgroundProcess isTerminated ifFalse: [
					backgroundProcess suspend.
					backgroundProcess terminate ].
				backgroundProcess := nil ].
	]
]

{ #category : 'accessing' }
SHTextStyler >> textModel: aTextModel [
	self terminateBackgroundStylingProcess.
	textModel := aTextModel
]
