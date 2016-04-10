'From Cuis 4.2 of 25 July 2013 [latest update: #2733] on 10 April 2016 at 4:30:06.304051 pm'!

!Clipboard methodsFor: 'accessing' stamp: 'KenD 4/8/2016 13:58'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |
	
	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a TextAnchor incuding it..."
	(answer is: #Morph) ifTrue: [
		^ (Text string: '*' attribute: (TextAnchor new anchoredFormOrMorph: answer)) ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ '' ] ifNotNil: [ answer printString ]! !

