'From Cuis7.9 [latest update: #8296] on 28 October 2026 at 11:43:28 am'!

!Text methodsFor: 'comparing' stamp: 'jmv 28/Oct/2026, 2:26:58 pm (UTC)'!
= other
	"Am I equal to the other Text or String?
	If one of the instances is String, UnicodeString or some kind of Symbol, compare just characters.
	If both are Text, also compare emphases."

	self == other ifTrue: [ ^ true ].
	(other is: #Text) ifTrue: [ "This is designed to run fast even for megabytes"
		string == other string ifTrue: [ ^ true ].
		string = other string ifFalse: [ ^false ].
		^self runs = other runs ].
	other isString ifTrue: [
		^ string == other or: [string = other]].
	^ false! !

