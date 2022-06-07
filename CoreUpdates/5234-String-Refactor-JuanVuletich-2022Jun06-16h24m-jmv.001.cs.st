'From Cuis 6.0 [latest update: #5237] on 6 June 2022 at 4:28:22 pm'!

!String methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:06'!
firstNonSeparator

	^self findFirst: [:aChar | aChar isSeparator not ]! !

!String methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:54'!
lastNonSeparator

	^ self findLast: [:aChar | aChar isSeparator not].
	! !


!String methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:10'!
afterBlanksEndsWith: aTail

	^(self endsWith: aTail) and: [ self firstNonSeparator = (self size - aTail size + 1) ]
! !

!String methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:59'!
withBlanksTrimmed
	"Return a copy of the receiver from which leading and trailing blanks have been trimmed."

	| first |
	
	first _ self firstNonSeparator.
	first = 0 ifTrue: [^ ''].  "no non-separator character"
	
	^ self copyFrom: first to: self lastNonSeparator 

	" ' abc  d   ' withBlanksTrimmed"
! !

!String methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:36:16'!
withoutLeadingBlanks
	
	"Return a copy of the receiver from which leading blanks have been trimmed."
	
	| first |
	
	first := self firstNonSeparator.
	first = 0 ifTrue: [^ ''].  
	first = 1 ifTrue: [^ self ].
	
	"no non-separator character"	
	^ self copyFrom: first to: self size
		
	" '    abc  d  ' withoutLeadingBlanks"
! !

!String methodsFor: 'converting' stamp: 'jmv 6/6/2022 15:37:02'!
withoutTrailingBlanks
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	
	last _ self lastNonSeparator.
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	last = self size ifTrue: [ ^self ].
	
	^ self copyFrom: 1 to: last

	" ' abc  d   ' withoutTrailingBlanks"
! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 6/6/2022 15:36:19'!
logChange: aStringOrText preamble: preambleOrNil
	"Write the argument, aString, onto the changes file."
	| aString changesFile |
	self assureStartupStampLogged.
	aString _ aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "null doits confuse replay"
	
	(SourceFiles notNil and: [(changesFile _ SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].
	Utilities logsUserChanges ifTrue: [
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; newLine.
			preambleOrNil ifNotNil: [
				stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			stream nextChunkPut: aString ]]! !

!methodRemoval: String #firstNoBlankIndex stamp: 'jmv 6/6/2022 16:28:07'!
String removeSelector: #firstNoBlankIndex!
!methodRemoval: String #lastNoBlankIndex stamp: 'jmv 6/6/2022 16:27:45'!
String removeSelector: #lastNoBlankIndex!
