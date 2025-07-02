'From Cuis7.5 [latest update: #7279] on 25 June 2025 at 10:07:32 am'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 6/25/2025 10:02:13'!
undeclaredReferences
	"
	Smalltalk undeclaredReferences
	"
	| undeclaredReferences |
	
	undeclaredReferences := Dictionary new.
	self cleanOutUndeclared.
	Undeclared associationsDo: [ :undeclaredAssoc |
		undeclaredReferences
			at: undeclaredAssoc key
			put: (self allCallsOn: undeclaredAssoc) ].

	^undeclaredReferences! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'HAW 6/25/2025 10:00:33'!
browseUndeclaredReferences
	"
	Smalltalk browseUndeclaredReferences
	"
	| undeclaredReferences |
	
	undeclaredReferences := self undeclaredReferences.
	undeclaredReferences ifEmpty: [
		 ^(PopUpMenu labels: ' OK ') startUpWithCaption: 'There are no Undeclared at all' ].
	
	undeclaredReferences keysAndValuesDo: [ :undeclared :references |
		self
			browseMessageList: references
			name: 'References to Undeclared: ', undeclared printString ].! !

