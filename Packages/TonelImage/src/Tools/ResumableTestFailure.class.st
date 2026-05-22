"
A ResumableTestFailure triggers a TestFailure, but lets execution of the TestCase continue. this is useful when iterating through collections, and #assert: ing on each element. in combination with methods like testcase>>#assert:description:, this lets you run through a whole collection and note which tests pass.

here''s an example:

	

	(1 to: 30) do: [ :each |
		self assert: each odd description: each printString, ' is even' resumable: true]

for each element where #odd returns <false>, the element will be printed to the Transcript. 
"
Class {
	#name : 'ResumableTestFailure',
	#superclass : 'TestFailure',
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'Camp Smalltalk' }
ResumableTestFailure >> isResumable [
	"Of course a ResumableTestFailure is resumable ;-)"

	^true
]

{ #category : 'Camp Smalltalk' }
ResumableTestFailure >> sunitExitWith: aValue [
	self resume: aValue
]
