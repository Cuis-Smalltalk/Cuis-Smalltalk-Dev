'From Cuis 6.0 [latest update: #5693] on 15 March 2023 at 3:48:54 pm'!

!MessageSet methodsFor: 'shout styling' stamp: 'jmv 3/15/2023 15:00:34'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"
	
	self isModeStyleable ifFalse: [ ^false ].
	anSHTextStyler classOrMetaClass: self selectedClassOrMetaClass.
	autoSelectString ifNotNil: [
		"To make highlight of autoSelectString correct."
		anSHTextStyler disableFormatAndConvert ].
	^true! !


!Debugger methodsFor: 'shout styling' stamp: 'jmv 3/15/2023 14:58:47'!
shouldStyle: text with: anSHTextStyler
	"This is a notification that anSHTextStyler is about to re-style its text.
	Set the classOrMetaClass in anSHTextStyler, so that identifiers
	will be resolved correctly.
	Answer true to allow styling to proceed, or false to veto the styling"

	self isModeStyleable ifFalse: [^false].
	anSHTextStyler 
		classOrMetaClass: self selectedClassOrMetaClass;
		disableFormatAndConvert;						"To make higlights of currently executing code correct."
		workspace: self.
	^true! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 3/15/2023 14:58:10'!
privateFormatAndConvert

	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow
			If disableFormatAnConvert, don't: changes in source code ranges are not allowed."
			disableFormatAndConvert ifFalse: [
				self replaceStringForRangesWithType: #ansiAssignment with: Character smalltalkLeftArrow asString ].
			"Turn $_ into a left arrow"
			self replaceStringForRangesWithType: #assignment with: Character smalltalkLeftArrow asString.
			"Turn $^into up arrow"
			self replaceStringForRangesWithType: #return with: Character smalltalkUpArrow asString ]
		ifFalse: [
			"Turn $_ into :=
			If disableFormatAndConvert, no changes in source code ranges are allowed, use single character left arrow."
			disableFormatAndConvert
				ifFalse: [
					self replaceStringForRangesWithType: #assignment with: ':='  ]
				ifTrue: [
					self replaceStringForRangesWithType: #assignment with: Character smalltalkLeftArrow asString  ]].! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 3/15/2023 14:49:59'!
replaceStringForRangesWithType: aSymbol with: aString
	"Answer aText if no replacements, or a copy of aText with 
	each range with a type of aSymbol replaced by aString"

	| toReplace increaseInLength |

	self parseSetWorkspace: false.
	toReplace _ parser ranges select: [ :each |
		each rangeType = aSymbol ].
	toReplace isEmpty ifTrue: [ ^self ].
	increaseInLength := 0.

	(toReplace asArray sort: [ :a :b | a start <= b start ]) 
		do: [ :each | | end start thisIncrease | 
			start := each start + increaseInLength.
			end := each end + increaseInLength.
			formattedText replaceFrom: start to: end with: aString.
			thisIncrease := aString size - each length.
			increaseInLength := increaseInLength + thisIncrease ]! !

!SHTextStylerST80 methodsFor: 'accessing' stamp: 'jmv 3/15/2023 14:57:04'!
disableFormatAndConvert
	"CodeProviders that require source code ranges computed on the actual source code
	to be also valid in Shout'ed code should set this flag.
	If this flag is set, conversions between ':=' and single character assigment ($_ and leftArrow)
	are disabled.
	See senders."

	disableFormatAndConvert := true! !

