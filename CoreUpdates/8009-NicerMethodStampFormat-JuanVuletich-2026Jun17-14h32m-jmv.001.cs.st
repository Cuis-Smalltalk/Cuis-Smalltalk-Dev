'From Cuis7.9 [latest update: #8004] on 17 June 2026 at 2:33:25 pm'!

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 17/Jun/2026, 12:16:57 am (UTC)'!
authorAndStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	Format the date in the timestamp with the preferred style.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	| asSaved |
	asSaved := self authorAndStampAsSaved.
	asSaved isEmpty ifTrue: [ ^asSaved ].
	^String streamContents: [ :strm | 
		self class authorAndStamp: asSaved partsDo: [ :authorInitials :dateAndTime |
			strm nextPutAll: authorInitials.
			strm space.
			dateAndTime asUTC printNiceOn: strm ]]! !

