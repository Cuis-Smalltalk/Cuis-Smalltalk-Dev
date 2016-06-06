'From Cuis 4.2 of 25 July 2013 [latest update: #2792] on 6 June 2016 at 9:44:33.144724 am'!
!classDefinition: #RunNotArray category: #'Collections-Arrayed'!
ArrayedCollection variableSubclass: #RunNotArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!RunNotArray commentStamp: '<historical>' prior: 0!
A replacement for RunArray that does not optimize space. Essentially just an array, with a few idiosyncratic methods for compatibility with RunArray.

Rationale: When styling Smalltalk text, runs are very short. Space saving is not significant. Maybe 90% if the time is spent building and coaslescing RunArrays. So, don't save space. Save time!!!


!RunArray methodsFor: 'private' stamp: 'jmv 6/5/2016 15:19'!
basicReplaceAttributesFrom: start to: stop with: replacement
	"Private. Does not enforce invariants.
	replacement size = (stop-start-1) "
	
	| answer |
	answer _ self
		copyReplaceFrom: start
		to: stop
		with: replacement.
	answer coalesce.
	^ answer! !

!RunArray methodsFor: 'private' stamp: 'jmv 6/5/2016 13:35'!
find: attribute
	"Return the first interval over which this attribute applies"
	| begin end |
	begin _ 0.
	self withStartStopAndValueDo: [ :start :stop :attributes |
		(attributes includes: attribute)
			ifTrue: [
				begin = 0 ifTrue: [begin _ start].
				end _ stop]
			ifFalse: [
				begin > 0 ifTrue: [^ begin to: end]]].
	begin > 0 ifTrue: [^ begin to: end].
	^ nil! !


!RunNotArray methodsFor: 'accessing' stamp: 'jmv 6/5/2016 13:49'!
at: index
	"Tolerates index=0 and index=size+1 for copyReplace: "
	index = 0 ifTrue: [ ^ self at: 1 ].
	index = (self size+1) ifTrue: [ ^ self at: index-1 ].
	^ super at: index! !

!RunNotArray methodsFor: 'accessing' stamp: 'jmv 6/5/2016 13:02'!
canJoinMessage: aMessageSend
	"Nop. We never try to join."! !

!RunNotArray methodsFor: 'accessing' stamp: 'jmv 6/5/2016 13:37'!
rangeOf: attribute startingAt: startPos
	"Answer an interval that gives the range of attr at index position  startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos.  self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self."
	
	| firstRelevantPosition lastRelevantPosition value |
	value _ self at: startPos.
	(value includes: attribute) ifFalse: [
		^ startPos to: startPos - 1].

	lastRelevantPosition := startPos +1.
	[lastRelevantPosition <= self size 
		and: [(self at: lastRelevantPosition) includes: attribute]]
	whileTrue: [
		lastRelevantPosition := lastRelevantPosition + 1 ].

	firstRelevantPosition := startPos -1.
	[firstRelevantPosition >= 1 
		and: [(self at: firstRelevantPosition) includes: attribute]]
	whileTrue: [
		firstRelevantPosition := firstRelevantPosition - 1]. 
 
	^firstRelevantPosition+1 to: lastRelevantPosition-1! !

!RunNotArray methodsFor: 'accessing' stamp: 'jmv 6/5/2016 13:09'!
runLengthAt: i0
	"
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 1.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 2.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 3.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 4.
	({1. 2. 3. 3. 4} as: RunNotArray) runLengthAt: 5.
	"

	| value |
	value _ self at: i0.
	i0+1 to: self size do: [ :i |
		(self at: i) = value ifFalse: [ ^ i-i0 ]].
	^ self size-i0+1
		! !

!RunNotArray methodsFor: 'adding' stamp: 'jmv 6/5/2016 13:03'!
coalesce
	"Nop. We never combine adjacent runs"! !

!RunNotArray methodsFor: 'enumerating' stamp: 'jmv 6/6/2016 09:40'!
runsAndValuesDo: aBlock
	"Evaluate aBlock with run lengths and values from the receiver"
	self do: [ :attributes |
		aBlock value: 1 value: attributes ]! !

!RunNotArray methodsFor: 'enumerating' stamp: 'jmv 6/5/2016 13:10'!
runsFrom: start to: stop do: aBlock
	start to: stop do: [ :i | aBlock value: (self at: i )]! !

!RunNotArray methodsFor: 'Text support' stamp: 'jmv 6/5/2016 15:28'!
basicReplaceAttributesFrom: start to: stop with: replacement
	start to: stop do: [ :i |
		self at: i put: (replacement at: i-start) ]! !

!RunNotArray methodsFor: 'Text support' stamp: 'jmv 6/5/2016 13:39'!
find: attribute
	"Return the first interval over which this attribute applies"

	1 to: self size do: [ :i |
		((self at: i) includes: attribute) ifTrue: [
			^ self rangeOf: attribute startingAt: i ]].
	
	^ nil! !

!RunNotArray methodsFor: 'Text support' stamp: 'jmv 6/5/2016 13:04'!
mapValues: mapBlock
	self withIndexDo: [ :each :i |
		self at: i put: (mapBlock value: each) ]! !

!RunNotArray methodsFor: 'Text support' stamp: 'jmv 6/5/2016 13:11'!
values
	"yep. self"! !


!Text methodsFor: 'converting' stamp: 'jmv 6/5/2016 13:54'!
replaceFrom: start to: stop withString: replacementString attributes: attributesArray startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. 
	Do it to both the string and the runs.
	The size does not change"

	| newRepRuns |
	string replaceFrom: start to: stop with: replacementString startingAt: repStart.
	newRepRuns _ RunNotArray new: stop-start+1 withAll: attributesArray.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: newRepRuns).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacementString size - 1! !

!Text methodsFor: 'emphasis' stamp: 'jmv 6/5/2016 13:35'!
find: attribute
	"Return the first interval over which this attribute applies"
	^ runs find: attribute! !

!Text methodsFor: 'private' stamp: 'jmv 6/5/2016 15:20'!
basicReplaceAttributesFrom: start to: stop with: replacement
	"Private. Does not enforce invariants.
	replacement size = (stop-start-1) "
	
	self privateSetRuns: (runs basicReplaceAttributesFrom: start to: stop with: replacement)! !


!Text class methodsFor: 'instance creation' stamp: 'jmv 6/5/2016 13:54'!
string: aString attributes: atts
	"Answer an instance of me whose characters are those of aString.
	atts is an array of TextAttributes."

	^self string: aString runs: (RunNotArray new: aString size withAll: atts)! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 6/5/2016 15:36'!
setAttributesFromRanges: ranges

	formattedText removeAttributesThat: [ :attribute | attribute isForShout ].
	ranges do: [ :range |

		"Smalltalk text styling"
		(self attributesFor: range rangeType) ifNotNil: [ :attributes |
			attributes do: [ :each |
				formattedText addAttribute: each from: range start to: range end ]].

		"Show as subscripts if appropriate."
		(classOrMetaClass isNil or: [
			classOrMetaClass theNonMetaClass lastUnderscoreMeansSubscript]) ifTrue: [
				(#( instVar classVar globalVar workspaceVar poolConstant
					patternArg methodArg patternTempVar tempVar		
					blockPatternArg blockArg blockPatternTempVar blockTempVar 
					incompleteIdentifier undefinedIdentifier) pointsTo: range rangeType )
						ifTrue: [
							formattedText lastIndexOf: $_ startingAt: range end endingAt: range start do: [ :i |
								formattedText addAttribute: ShoutTextEmphasis subscript from: i to: range end ] ]]]! !

!methodRemoval: RunArray #addLast:times:!
RunArray removeSelector: #addLast:times:!
!methodRemoval: RunArray #repeatLast:ifEmpty:!
RunArray removeSelector: #repeatLast:ifEmpty:!
!methodRemoval: RunArray #repeatLastIfEmpty:!
RunArray removeSelector: #repeatLastIfEmpty:!
