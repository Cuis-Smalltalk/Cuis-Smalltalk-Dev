'From Cuis 6.0 [latest update: #5746] on 3 April 2023 at 11:10:00 am'!

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 4/3/2023 11:05:16'!
browseAllReferencesToLiteral: aLiteral
	"Create and schedule a message browser on each method that references aLiteral. For example, 
	Smalltalk browseAllReferencesToLiteral: 47.
	Smalltalk browseAllReferencesToLiteral: 0@0.
	"
	^ self
		browseMessageList: (self allReferencesToLiteral: aLiteral)
		name: 'References to literal ' , aLiteral asString
		autoHighlight: aLiteral asString
		allOccurrences: true.! !

