'From Cuis7.5 [latest update: #7725] on 28 November 2025 at 8:28:29 am'!

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 07:54:48'!
authorAndStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available.
	(CompiledMethod compiledMethodAt: #timeStamp) timeStamp
	"
	| preamble tokens |
	preamble := self getPreamble.
	(preamble includesSubString: 'methodsFor:') ifFalse: [ ^''].
	tokens := Scanner new scanTokens: preamble.
	^CompiledMethod stampFrom: tokens! !


!CompiledMethod class methodsFor: 'services' stamp: 'jmv 28/Nov/2025 08:15:10'!
authorAndStamp: aStamp partsDo: aBlock
	| stamp dateIndex aux dateAndTimePart |
	stamp := aStamp.
	"Account for some unfortunately hacked stamps such as this: 'cwp 10/17/2007 23:29/eem 9/5/2009 11:10 - => :=' "
	aux := stamp lastIndexOf: $/.
	aux := stamp indexOf: $: startingAt: (aux max:1) ifAbsent: [ 0 ].
	aux > 0 ifTrue: [
		(aux > 0 and: [ aux + 2 ~= stamp size]) ifTrue: [
			stamp := stamp copyFrom: 1 to: aux + 2 ]].

	"Find start of date, if not possible, consider the whole stamp the author, and no date"
	aux := stamp
		lastIndexOf: $/
		startingAt: stamp size
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	aux := stamp
		lastIndexOf: $/
		startingAt: aux
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	dateIndex := stamp
		lastIndexOf: $ 
		startingAt: aux
		ifAbsent: [ ^ aBlock value: stamp value: nil ].
	"If only date, no time, add midnight time"
	dateAndTimePart := stamp copyFrom: dateIndex + 1 to: stamp size.
	(dateAndTimePart indexOf: $:) = 0 ifTrue:[
		dateAndTimePart := dateAndTimePart, ' 00:00' ].
	"Done"
	^aBlock
		value: (stamp copyFrom: 1 to: dateIndex-1)
		value: (DateAndTime fromString: dateAndTimePart)! !


!Behavior methodsFor: 'methods' stamp: 'jmv 28/Nov/2025 07:55:02'!
stampAt: selector
	^(self compiledMethodAt: selector ifAbsent: [ ^'' ]) authorAndStamp! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 28/Nov/2025 07:55:19'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.
	If moveSource true, then also set the source code pointer of the method."

	| compiledMethod oldPos newPos sourceFile endPos methodCode methodCodeText codeStyler |
	compiledMethod := self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	doPreamble ifTrue: [
		self
			printCategoryChunk: (self organization categoryOfElement: selector)
			on: outStream
			withStamp: compiledMethod authorAndStamp
			libraryName: (compiledMethod propertyValueAt: #libraryName)
			priorMethod: nil.
		outStream newLine ].

	((compiledMethod fileIndex = 0
		or: [(SourceFiles at: compiledMethod fileIndex) == nil])
		or: [(oldPos := compiledMethod filePosition) = 0])
			ifTrue: [
				"The source code is not accessible.  We must decompile..."
				outStream nextChunkPut: compiledMethod decompileString]
			ifFalse: [
				sourceFile := SourceFiles at: compiledMethod fileIndex.
				sourceFile position: oldPos.
				"Copy the method chunk"
				fileIndex = 0 ifFalse: [
					outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
				newPos := outStream position.
				"Copy method chunk"
				methodCode := sourceFile nextChunk.
				(methodCode isEmpty or: [ methodCode isSeparators ]) ifTrue: [
					"This is very unlikely, but possible source file is corrupted."
					methodCode := compiledMethod decompileString ].
				"Convert method code to use ANSI assignments"
				(Preferences at: #fileOutANSIassignment) ifTrue: [
					codeStyler := SHTextStylerST80 new.
					codeStyler classOrMetaClass: compiledMethod methodClass.
					methodCodeText := methodCode asText.
					codeStyler instVarNamed: 'formattedText' put: methodCodeText.
					codeStyler getReady.
					codeStyler parseText.
					codeStyler replaceStringForRangesWithType: #assignment with: ':=' offset: 0.
					methodCode := methodCodeText string ].
				outStream nextChunkPut: methodCode.
				moveSource ifTrue: [    "Set the new method source pointer"
					endPos := outStream position.
					compiledMethod checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex.
					compiledMethod setSourcePosition: newPos inFile: fileIndex]].
	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].
	outStream newLine.! !


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 08:15:45'!
author
	"
	(CompiledMethod compiledMethodAt: #timeStamp) author
	(BitBlt>>#destRect) author
	"
	
	^self class authorAndStamp: self authorAndStamp partsDo: [ :authorInitials :dateAndTime | authorInitials ]! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 08:15:56'!
dateAndTime
	"Answer a DateAndTime object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateAndTime) dateAndTime
	"

	^self class authorAndStamp: self authorAndStamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 28/Nov/2025 08:16:05'!
dateMethodLastSaved
	"Answer a Date object indicating when a method was last saved.  If there is no date stamp, return nil"
	"
	(CompiledMethod compiledMethodAt: #dateMethodLastSaved) dateMethodLastSaved
	"

	^self class authorAndStamp: self authorAndStamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ifNotNil: [ dateAndTime date ]]! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 28/Nov/2025 08:00:15'!
browseMyChanges
	"Browse only the changes (in the changes file) by the current author.
	Smalltalk browseMyChanges
	"
	self browseAllSelect: [ :method |
       method fileIndex > 1 "only look at changes file"
               and: [ method authorAndStamp beginsWith: Utilities authorInitials, ' ' ]]! !


!MethodSet methodsFor: 'filtering' stamp: 'jmv 28/Nov/2025 08:01:28'!
filterToCurrentAuthor
	"Filter down only to messages with my initials as most recent author"

	| myInitials aMethod aTimeStamp |
	(myInitials := Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod := aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp := aMethod authorAndStamp) notNil and:
						[aTimeStamp beginsWith: myInitials]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 28/Nov/2025 08:01:31'!
filterToNotCurrentAuthor
	"Filter down only to messages not stamped with my initials"

	| myInitials aMethod aTimeStamp |
	(myInitials := Utilities authorInitialsPerSe) ifNil: [^ self inform: 'No author initials set in this image'].
	self filterFrom:
		[:aClass :aSelector |
			(aClass notNil and: [aSelector notNil]) and:			
				[aMethod := aClass compiledMethodAt: aSelector ifAbsent: nil.
				aMethod notNil and:
					[(aTimeStamp := aMethod authorAndStamp) isNil or:
						[(aTimeStamp beginsWith: myInitials) not]]]]! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 28/Nov/2025 08:02:16'!
methodsWithInitials: targetInitials
	"Based on a do-it contributed to the Squeak mailing list by Göran Hultgen:
- Browse methods whose initials (in the time-stamp, as logged to disk) match the given initials.
- Print out the complete time-stamp table to the Transcript.
- Answer a list of (initials -> count) associations.

CAUTION: It may take several minutes for this to complete."

	"Time millisecondsToRun: [Utilities methodsWithInitials: 'bf'] 2031"

	| initials authorAndStamp  allSubmitters |
	initials := ''.
	authorAndStamp := ''.
	allSubmitters := Bag new.
	Smalltalk
		browseAllSelect:
			[:cm |
				authorAndStamp := cm authorAndStamp.
				initials := authorAndStamp isEmpty
					ifTrue:
						['']
					ifFalse:
						[authorAndStamp substrings first].
				initials := initials isEmpty
					ifTrue:
						['<no time stamp>']
					ifFalse:
						[initials first isDigit
							ifTrue:
								['<date>']
							ifFalse:
								[initials]].
				allSubmitters add: initials.
				(initials = targetInitials)]
		name: ('Methods with initials ', targetInitials)
		autoHighlight: nil.

	allSubmitters sortedCounts do: [:elem | Transcript newLine; show: elem asString].
	^ allSubmitters
! !


!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 28/Nov/2025 08:16:11'!
addSelector: thisSelector method: aCompiledMethod methodTimeStamp: aTimeStampString updateSortingOfSentMessages: aBoolean

	| thisMethodSortingValue sentMessages existingSortingValue |
	thisMethodSortingValue := CompiledMethod authorAndStamp: aTimeStampString partsDo: [ :authorInitials :dateAndTime |
		dateAndTime
			ifNil: [ 0 ]
			ifNotNil: [ (dateAndTime - (DateAndTime fromString: '01/01/1996 00:00')) totalMinutes max: 0 ]].
	sentMessages := aCompiledMethod messages.
	self protected: [
		thisMethodSortingValue > (Selectors at: thisSelector ifAbsent: [-1]) ifTrue:
			[
			Selectors at: thisSelector put: thisMethodSortingValue.
			aBoolean ifTrue:
				[
				sentMessages do:
					[ :sentMsg |
					existingSortingValue := Selectors at: sentMsg ifAbsent: [-1].
					"If found at Selectors, no need to do the more expensive search in all classes"
					(existingSortingValue > -1 or: [ Smalltalk isThereAnImplementorOf: sentMsg ]) ifTrue: 
						"This means add it with value 0 if it is not already there"
						[ thisMethodSortingValue > existingSortingValue ifTrue:
							[ Selectors at: sentMsg put: thisMethodSortingValue ]]]]]].! !

!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 28/Nov/2025 07:59:02'!
initialize
	"
	It takes about 6 seconds, mostly because of the time to fetch method stamps from source files...
	[ SmalltalkCompleter initialize ]timeToRun
	Selectors inspect
	"
	| maxSortValue |
	
	EntriesLimit := 100.
	
	SystemChangeNotifier uniqueInstance
		removeActionsWithReceiver: self.	 "avoid double registration"

	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: self;
		when: #methodAddedInProtocolTimeStamp send: #methodAdded:selector:inProtocol:class:methodTimeStamp:requestor: to: self;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: self.
	
	self protected: [
		Selectors := Trie new.
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				Selectors at: sel ifAbsentPut: [ 0 ].
				method messages do: [ :sentMsg |
					Selectors at: sentMsg put: (Selectors at: sentMsg ifAbsent: [ 0 ]) + 1 ]]].
		"
		Smalltalk allBehaviorsDo: [:class |
			class selectorsAndMethodsDo: [ :sel :method |
				self addSelector: sel method: method
					methodTimeStamp: method authorAndStamp updateSortingOfSentMessages: false ]].
		""
	
		"The following might not be found in #messages. Give them maximum priority."
		maxSortValue := SmallInteger maxVal.
		"From MessageNode>>#initialize"
		#(	ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:
			and: or:
			whileFalse: whileTrue: whileFalse whileTrue
			to:do: to:by:do:
			caseOf: caseOf:otherwise:
			ifNil: ifNotNil:  ifNil:ifNotNil: ifNotNil:ifNil:) do: [ :sel |
				Selectors at: sel put: maxSortValue ].
	
		maxSortValue := SmallInteger maxVal-1.
		"From SystemDictionary >>#recreateSpecialObjectsArray"
		(1 to: Smalltalk specialSelectorSize) do:  [ :i | | sym |
			sym := Smalltalk specialSelectorAt: i.
			(Selectors includesKey: sym)
				ifTrue: [ Selectors at: sym put: maxSortValue ]]]! !


!ChangeRecord methodsFor: 'accessing' stamp: 'jmv 28/Nov/2025 08:16:18'!
dateAndTime

	^CompiledMethod authorAndStamp: self stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]! !


!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 28/Nov/2025 08:02:34'!
methodsWithInitialsOtherThan: myInits
	"Return a collection of method refs whose author appears to be different from the given one"
	| slips |
	slips := OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
				(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
					method := aClass compiledMethodAt: mAssoc key ifAbsent: nil.
					method ifNotNil: [ | authorAndStamp |
						((authorAndStamp := method authorAndStamp) notNil and: [
							(authorAndStamp beginsWith: myInits) not])
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithInitialsOtherThan: 'sw') name: 'authoring problems'
	"! !

!methodRemoval: CompiledMethod class #timeStamp:partsDo: stamp: 'jmv 28/Nov/2025 08:26:52'!
CompiledMethod class removeSelector: #timeStamp:partsDo:!
!methodRemoval: CompiledMethod #timeStamp stamp: 'jmv 28/Nov/2025 08:28:22'!
CompiledMethod removeSelector: #timeStamp!
