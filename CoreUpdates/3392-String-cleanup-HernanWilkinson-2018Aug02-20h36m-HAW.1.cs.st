'From Cuis 5.0 of 7 November 2016 [latest update: #3389] on 3 August 2018 at 11:18:15 am'!

!String methodsFor: 'converting' stamp: 'HAW 8/3/2018 11:12:15'!
firstNoBlankIndex 

	^self findFirst: [:aChar | aChar isSeparator not ]! !

!String methodsFor: 'converting' stamp: 'HAW 8/3/2018 11:12:25'!
lastNoBlankIndex

	^ self findLast: [:aChar | aChar isSeparator not].
	! !


!String methodsFor: 'converting' stamp: 'HAW 8/3/2018 10:57:30'!
afterBlanksEndsWith: aTail

	^(self endsWith: aTail) and: [ self firstNoBlankIndex = (self size - aTail size + 1) ]
! !

!String methodsFor: 'converting' stamp: 'HAW 8/3/2018 11:13:24'!
withBlanksTrimmed
	"Return a copy of the receiver from which leading and trailing blanks have been trimmed."

	| first |
	
	first _ self firstNoBlankIndex.
	first = 0 ifTrue: [^ ''].  "no non-separator character"
	
	^ self copyFrom: first to: self lastNoBlankIndex 

	" ' abc  d   ' withBlanksTrimmed"
! !

!String methodsFor: 'converting' stamp: 'HAW 8/3/2018 11:02:05'!
withoutLeadingBlanks
	
	"Return a copy of the receiver from which leading blanks have been trimmed."
	
	| first |
	
	first := self firstNoBlankIndex.
	first = 0 ifTrue: [^ ''].  
	first = 1 ifTrue: [^ self ].
	
	"no non-separator character"	
	^ self copyFrom: first to: self size
		
	" '    abc  d  ' withoutLeadingBlanks"
! !

!String methodsFor: 'converting' stamp: 'HAW 8/3/2018 11:08:59'!
withoutTrailingBlanks
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	
	last _ self lastNoBlankIndex.
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	last = self size ifTrue: [ ^self ].
	
	^ self copyFrom: 1 to: last

	" ' abc  d   ' withoutTrailingBlanks"
! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 8/3/2018 11:05:15'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	self assureStartupStampLogged.
	aString _ aStringOrText asString.
	aString firstNoBlankIndex = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			"If want style changes in DoIt, use nextChunkPutWithStyle:, and allow Texts to get here"
			self forceChangesToDisk ]].
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString ]]! !

