'From Cuis7.3 [latest update: #7053] on 27 February 2025 at 9:57:24 am'!

!SystemDictionary methodsFor: 'browsing' stamp: 'eem 2/27/2025 09:56:09'!
browseAllCallsOn: aLiteral
	"Create and schedule a message browser on each method that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (self allCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key
				autoHighlight: aLiteral key
				allOccurrences: true ]
		ifFalse:
			[self
				browseMessageList: (self allCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral asString
				autoHighlight: aLiteral asString
				allOccurrences: true ].! !

!SystemDictionary methodsFor: 'browsing' stamp: 'eem 2/27/2025 09:56:23'!
browseAllCallsOn: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral. For example, Smalltalk browseAllCallsOn: #open:label:."
	aClass ifNil: [ ^ self inform: 'no selected class' ].
	(aLiteral isKindOf: LookupKey)
		ifTrue: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Users of ' , aLiteral key , ' in the full hierarchy of ' , aClass name
				autoHighlight: aLiteral key
				allOccurrences: true ]
		ifFalse: [
			self
				browseMessageList: (aClass allLocalCallsOn: aLiteral)
				name: 'Senders of ' , aLiteral asString , ' in the full hierarchy of ' , aClass name
				autoHighlight: aLiteral asString
				allOccurrences: true ].! !

