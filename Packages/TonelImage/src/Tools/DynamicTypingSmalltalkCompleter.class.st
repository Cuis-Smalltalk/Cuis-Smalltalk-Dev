Class {
	#name : 'DynamicTypingSmalltalkCompleter',
	#superclass : 'SmalltalkCompleter',
	#category : 'Tools-Autocompletion',
	#package : 'Tools',
	#tag : 'Autocompletion'
}

{ #category : 'testing' }
DynamicTypingSmalltalkCompleter class >> isForCurrentTypeSystem [

	^ Smalltalk isLiveTypingInstalled not
]

{ #category : 'message entries - private' }
DynamicTypingSmalltalkCompleter >> computeEntriesOfUnaryMessageReturnNamed: aSelector     [
	
	self computeMessageEntriesForUnknowClass 
	
]

{ #category : 'message entries - private' }
DynamicTypingSmalltalkCompleter >> computeMessageEntriesOfCascadeReceiver    [
	
	self computeMessageEntriesForUnknowClass
	

]

{ #category : 'message entries - private' }
DynamicTypingSmalltalkCompleter >> computeMessageEntriesOfEnclosedExpressionReturn    [

	self computeMessageEntriesForUnknowClass 
	

]

{ #category : 'selector documentation' }
DynamicTypingSmalltalkCompleter >> documentationOf: aMethod [

	^aMethod dynamicTypingAutoCompleterDocumentation
]
