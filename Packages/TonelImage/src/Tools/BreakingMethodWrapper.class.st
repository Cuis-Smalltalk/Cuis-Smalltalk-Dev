Class {
	#name : 'BreakingMethodWrapper',
	#superclass : 'ProtoObject',
	#instVars : [
		'method'
	],
	#category : 'Tools-Debugger',
	#package : 'Tools',
	#tag : 'Debugger'
}

{ #category : 'instance creation' }
BreakingMethodWrapper class >> on: aCompiledMethod [
	"Create an instance of mine on aCompiledMethod."

	^ self basicNew initializeOn: aCompiledMethod
]

{ #category : 'class membership' }
BreakingMethodWrapper >> class [
	"Primitive. Answer the object which is the receiver's class. Essential. See 
	Object documentation whatIsAPrimitive."

	<primitive: 111>
	^ BreakingMethodWrapper
]

{ #category : 'system primitives' }
BreakingMethodWrapper >> doesNotUnderstand: aMessage [
	"Let my method perform aMessage and answer the result."

	^ method
		perform: aMessage selector
		withArguments: aMessage arguments
]

{ #category : 'error handling' }
BreakingMethodWrapper >> error: aString  [
	"Throw a generic Error exception."

	^Error new signal: aString
]

{ #category : 'breakpoints' }
BreakingMethodWrapper >> hasBreakpoint [
	"Answer true, because I am a breakpoint."

	^ true
]

{ #category : 'initialization' }
BreakingMethodWrapper >> initializeOn: aCompiledMethod [
	"Initialize me with aCompiledMethod."

	method := aCompiledMethod
]

{ #category : 'install/uninstall' }
BreakingMethodWrapper >> install [
	"Install me as a replacement for my method."

	method methodClass methodDictionary
		at: method selector
		put: self
]

{ #category : 'printing' }
BreakingMethodWrapper >> printAs: streamType limitedTo: limit [
	"Answer an instance of streamType whose characters describe the receiver.

	If you want to print without a character limit, use fullPrintString."

	| limitedString |
	limitedString := streamType streamContents: [ :stream |
		self printOn: stream ] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString ].
	^ limitedString , '[..]'
]

{ #category : 'printing' }
BreakingMethodWrapper >> printOn: aStream [
	"Print a nice representation of me on aStream."

	aStream 
		nextPutAll: 'Break when entering ';
		print: method
]

{ #category : 'printing' }
BreakingMethodWrapper >> printStringLimitedTo: limit [
	"Answer a String whose characters are a description of the receiver.

	If you want to print without a character limit, use fullPrintString."

	^self printAs: String limitedTo: limit
]

{ #category : 'printing' }
BreakingMethodWrapper >> printTextLimitedTo: limit [
	"Answer a Text whose characters are a description of the receiver."

	^self printAs: Text limitedTo: limit
]

{ #category : 'system primitives' }
BreakingMethodWrapper >> run: aSelector with: arguments in: aReceiver [
	"I am sent by the virtual machine, if an instance of a class other
	than CompiledMethod is bound in a method dictionary.

	See SystemDictionary>>#recreateSpecialObjectsArray at index 50."


	"Get rid of that break point, we don't want debuggers popping up while debugging."
	self uninstall.

	"Halt so users gets their debugger."
	Smalltalk isDevelopmentEnvironmentPresent ifTrue: [
		Halt signal ].

	"Evaluate our method. Maybe in the debugger or as usual if user proceeded.
	Answer result."
	^aReceiver withArgs: arguments executeMethod: method
]

{ #category : 'install/uninstall' }
BreakingMethodWrapper >> uninstall [
	"Uninstall me."

	method methodClass methodDictionary
		at: method selector
		put: method
]
