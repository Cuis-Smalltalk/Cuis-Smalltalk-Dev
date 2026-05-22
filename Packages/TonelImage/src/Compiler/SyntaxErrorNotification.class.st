"
A SyntaxErrorNotification is an Exception occuring when compiling a Smalltalk source code with incorrect syntax.
Note that in interactive mode, this exception is not raised because the Compiler will interact directly with source code editor.
The defaultAction is to raise a SyntaxError pop up window so as to enable interactive handling even in non interactive mode.

Instance Variables
	category:		<String | nil>
	code:		<String | Text | Stream>
	doitFlag:		<Boolean>
	errorMessage:		<String>
	inClass:		<Behavior>
	location:		<Integer>
	newSource:		<String | Text | Stream | nil>

category
	- the category in which the method will be classified

code
	- the source code to be compiled or evaluated

doitFlag
	- true if this is a doIt (code to evaluate), false if this is a method (code of a method to be compiled)

errorMessage
	- contains information about the syntax error

inClass
	- target class in which to compile the method

location
	- position in the source code where the syntax error occured

newSource
	- eventually hold a source code replacement typically passed by the SyntaxError window

"
Class {
	#name : 'SyntaxErrorNotification',
	#superclass : 'Error',
	#instVars : [
		'inClass',
		'code',
		'category',
		'doitFlag',
		'errorMessage',
		'location'
	],
	#category : 'Compiler-Exceptions',
	#package : 'Compiler',
	#tag : 'Exceptions'
}

{ #category : 'exceptionInstantiator' }
SyntaxErrorNotification class >> inClass: aClass category: aCategory withCode: codeString doitFlag: doitFlag  [
	^ (self new
		setClass: aClass
		category: aCategory 
		code: codeString
		doitFlag: doitFlag) signal
]

{ #category : 'exceptionInstantiator' }
SyntaxErrorNotification class >> inClass: aClass category: aCategory withCode: codeString doitFlag: doitFlag errorMessage: errorString location: location [
	^ (self new
		setClass: aClass
		category: aCategory 
		code: codeString
		doitFlag: doitFlag
		errorMessage: errorString
		location: location) signal
]

{ #category : 'accessing' }
SyntaxErrorNotification >> category [
	^category
]

{ #category : 'accessing' }
SyntaxErrorNotification >> doitFlag [
	^doitFlag
]

{ #category : 'accessing' }
SyntaxErrorNotification >> errorClass [
	^inClass
]

{ #category : 'accessing' }
SyntaxErrorNotification >> errorCode [
	^code
]

{ #category : 'accessing' }
SyntaxErrorNotification >> errorMessage [
	^errorMessage
]

{ #category : 'accessing' }
SyntaxErrorNotification >> location [
	^location
]

{ #category : 'accessing' }
SyntaxErrorNotification >> messageText [
	^ super messageText
		ifNil: [messageText := code]
]

{ #category : 'accessing' }
SyntaxErrorNotification >> setClass: aClass category: aCategory code: codeString doitFlag: aBoolean [
	inClass := aClass.
	category := aCategory.
	code := codeString.
	doitFlag := aBoolean 
]

{ #category : 'accessing' }
SyntaxErrorNotification >> setClass: aClass category: aCategory code: codeString doitFlag: aBoolean errorMessage: errorString location: anInteger [
	inClass := aClass.
	category := aCategory.
	code := codeString.
	doitFlag := aBoolean.
	errorMessage := errorString.
	location := anInteger
]
