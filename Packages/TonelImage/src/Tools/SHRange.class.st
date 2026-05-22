"
I associate a type with a range of characters in a String
I have these instance variables...
	start - the one based index of the first character of the range within the String.
	end - the one based index of the last character  of the range within the String.
	type - a Symbol describing the type of the range
	
A sequence of instances of me are created by an instance of SHParserST80 which can then used by an instance of  SHTextStyler to style Text. 
"
Class {
	#name : 'SHRange',
	#superclass : 'Object',
	#instVars : [
		'start',
		'end',
		'type'
	],
	#category : 'Tools-Syntax Highlighting',
	#package : 'Tools',
	#tag : 'Syntax Highlighting'
}

{ #category : 'instance creation' }
SHRange class >> nilObject [

	"I can not reference self inside backtick - Hernan"
	^`SHRange start: 0 end: 0 type: nil`
]

{ #category : 'instance creation' }
SHRange class >> start: s end: e type: aSymbol [
	
	^self new
		start: s end: e type: aSymbol;
		yourself
]

{ #category : 'testing' }
SHRange >> correspondsToIdentifier [

	^#(#incompleteIdentifier
		#blockTempVar #blockArg #tempVar #methodArg
		#instVar #classVar 
		#workspaceVar #poolConstant #globalVar ) statePointsTo: type
]

{ #category : 'testing' }
SHRange >> correspondsToMessage [

	^#(binary incompleteBinary keyword incompleteKeyword unary incompleteUnary) statePointsTo: type 
]

{ #category : 'testing' }
SHRange >> correspondsToMethodDefinition [
	^#(patternUnary patternBinary patternKeyword) statePointsTo: self rangeType
]

{ #category : 'testing' }
SHRange >> correspondsToPartialOrFullIdentifier: aParser [

	^self correspondsToIdentifier or: [  self correspondsToReservedNameIn: aParser ] 
]

{ #category : 'testing' }
SHRange >> correspondsToReservedNameIn: aParser [

	^aParser reservedNames statePointsTo: type
]

{ #category : 'testing' }
SHRange >> correspondsToSymbol [

	^self rangeType = #symbol

	
]

{ #category : 'accessing' }
SHRange >> end [
	^end
]

{ #category : 'accessing' }
SHRange >> end: anInteger [
	end := anInteger
]

{ #category : 'accessing' }
SHRange >> length [
	^end - start + 1
]

{ #category : 'printing' }
SHRange >> printOn: aStream [

	aStream 
		nextPutAll: type;
		space;
		nextPut: $(;
		print: start;
		nextPutAll: ' to: ';
		print: end;
		nextPut: $)
]

{ #category : 'accessing' }
SHRange >> rangeType [
	^type
]

{ #category : 'accessing' }
SHRange >> start [
	^start
]

{ #category : 'accessing' }
SHRange >> start: anInteger [
	start := anInteger
]

{ #category : 'accessing' }
SHRange >> start: startInteger end: endInteger type: typeSymbol [
	start := startInteger.
	end := endInteger.
	type := typeSymbol
]

{ #category : 'accessing' }
SHRange >> type: aSymbol [
	type := aSymbol
]
