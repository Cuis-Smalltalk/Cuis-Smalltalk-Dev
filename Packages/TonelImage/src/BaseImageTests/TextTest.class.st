Class {
	#name : 'TextTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Text',
	#package : 'BaseImageTests',
	#tag : 'System-Text'
}

{ #category : 'testing' }
TextTest >> testConcatenation [
	| t1 t2 |

	t1 := 'sample' bold.
	t2 := 'text' italic.
	self assert: (t1, t2) string = 'sampletext'.
	self assert: ((1 to: 6) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis bold}]).
	self assert: ((7 to: 10) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis italic}]).

	t1 := 'sample' bold optimizedForMutationSpeed.
	t2 := 'text' italic.
	self assert: (t1, t2) string = 'sampletext'.
	self assert: ((1 to: 6) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis bold}]).
	self assert: ((7 to: 10) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis italic}]).

	t1 := 'sample' bold.
	t2 := 'text' italic optimizedForMutationSpeed.
	self assert: (t1, t2) string = 'sampletext'.
	self assert: ((1 to: 6) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis bold}]).
	self assert: ((7 to: 10) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis italic}]).

	t1 := 'sample' bold optimizedForMutationSpeed.
	t2 := 'text' italic optimizedForMutationSpeed.
	self assert: (t1, t2) string = 'sampletext'.
	self assert: ((1 to: 6) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis bold}]).
	self assert: ((7 to: 10) allSatisfy: [ :i | ((t1, t2) runs at: i ) = {TextEmphasis italic}]).
]

{ #category : 'testing' }
TextTest >> testIsText [
	self assert: (Text new is: #Text).
]

{ #category : 'testing' }
TextTest >> testRunsBehavior [
	"Weird but true since Smalltalk-80"
	| c |
	self assert: ('stuff' bold runs at: 1000) = {TextEmphasis bold}.
	self assert: ('stuff' bold optimizedForMutationSpeed runs at: 1000) = {TextEmphasis bold}.
	
	self assert: ('stuff' bold runs copyFrom: 34 to: 34) first = {TextEmphasis bold}.
	self assert: ('stuff' bold optimizedForMutationSpeed runs copyFrom: 34 to: 34) first = {TextEmphasis bold}.
	
	self assert: ('stuff' bold runs rangeOf: TextEmphasis bold startingAt: 3) = (1 to: 5).
	self assert: ('stuff' bold optimizedForMutationSpeed runs rangeOf: TextEmphasis bold startingAt: 3) = (1 to: 5).
	
	self assert: ('stuff' bold runs runLengthAt: 3) = 3.
	self assert: ('stuff' bold optimizedForMutationSpeed runs runLengthAt: 3) = 3.
	
	c := OrderedCollection new.
	'stuff' bold runs runsFrom: 1 to: 5 do: [ :attr | c add: attr  ].
	self assert: c asSet asArray = {{TextEmphasis bold}}.
	c := OrderedCollection new.
	'stuff' bold optimizedForMutationSpeed runs runsFrom: 1 to: 5 do: [ :attr | c add: attr ].
	self assert: c asSet asArray = {{TextEmphasis bold}}.
	
	self assert: ('stuff' bold runs copyReplaceFrom: 3 to: 20 with: 'extralongstuff' italic runs) size = 16.
	self assert: ('stuff' bold optimizedForMutationSpeed runs copyReplaceFrom: 3 to: 20 with: 'extralongstuff' italic optimizedForMutationSpeed runs) size = 16.
]
