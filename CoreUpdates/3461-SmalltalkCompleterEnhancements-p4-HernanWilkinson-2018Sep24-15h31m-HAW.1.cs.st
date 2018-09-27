'From Cuis 5.0 of 7 November 2016 [latest update: #3451] on 24 September 2018 at 3:44:30 pm'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'HAW 9/24/2018 15:44:19'!
computeIdentifierEntries
	"Use an aux Set to avoid duplicates, but keep the order given."
	| entriesSet |
	
	entriesSet _ Set new.
	entries _ Array streamContents: [ :strm | 
		parser namesBeginningWith: prefix do: [ :identifier |
			(entriesSet includes: identifier) ifFalse: [
				entriesSet  add: identifier.
				strm nextPut: identifier ]]]! !

