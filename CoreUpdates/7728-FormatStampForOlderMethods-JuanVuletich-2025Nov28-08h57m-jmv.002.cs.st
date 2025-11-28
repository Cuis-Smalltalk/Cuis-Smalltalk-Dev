'From Cuis7.5 [latest update: #7727] on 28 November 2025 at 9:08:58 am'!

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 09:07:00'!
authorAndStampAsSaved
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file.
	Answer the empty string if no time stamp is available.
	(Object compiledMethodAt: #printString) authorAndStampAsSaved
	(Object compiledMethodAt: #printString) authorAndStamp
	"
	| preamble tokens |
	preamble := self getPreamble.
	(preamble includesSubString: 'methodsFor:') ifFalse: [ ^''].
	tokens := Scanner new scanTokens: preamble.
	^(CompiledMethod stampFrom: tokens)! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 08:58:00'!
author
	"
	(CompiledMethod compiledMethodAt: #author) author
	(BitBlt>>#colorMap) author
	"
	
	^self class authorAndStamp: self authorAndStampAsSaved partsDo: [ :authorInitials :dateAndTime | authorInitials ]! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 09:07:00'!
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
	self class authorAndStamp: asSaved partsDo: [ :authorInitials :dateAndTime |
		^authorInitials, ' ' , (dateAndTime date printFormat: #(1 2 3 $/ 2 1)), ' ',
			(String streamContents: [:s | dateAndTime time print24: true showSeconds: true on: s]) ].
		! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 08:58:00'!
dateAndTime
	"Answer a DateAndTime object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateAndTime) dateAndTime
	"

	^self class authorAndStamp: self authorAndStampAsSaved partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 08:58:00'!
dateMethodLastSaved
	"Answer a Date object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateMethodLastSaved) dateMethodLastSaved
	"

	^self class authorAndStamp: self authorAndStampAsSaved partsDo: [ :authorInitials :dateAndTime | dateAndTime ifNotNil: [ dateAndTime date ]]! !

