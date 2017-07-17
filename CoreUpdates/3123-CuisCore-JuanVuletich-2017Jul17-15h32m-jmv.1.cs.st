'From Cuis 5.0 of 7 November 2016 [latest update: #3122] on 17 July 2017 at 3:52:45 pm'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/17/2017 15:44:04'!
scanFile: aFile from: startPosition to: stopPosition

	file _ aFile.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	file position: startPosition.
	'Scanning ', aFile localName, '...'
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [ :barBlock |
			[file position < stopPosition] whileTrue: [ | prevChar |
				barBlock value: file position.
				[file atEnd not and: [file peek isSeparator]]
					whileTrue: [prevChar _ file next].
				(file peekFor: $!!)
					ifTrue: [
						"A line starting with $!! means a specific ChangeRecord type"
						(prevChar notNil and: [ prevChar isLineSeparator ])
							ifTrue: [self scanSpecificChangeRecordType]]
					ifFalse: [
						"Otherwise, interpret it with #doIt:"
						| itemPosition item |
						itemPosition _ file position.
						item _ file nextChunk.
						item size > 0 ifTrue: [
							self
								addItem: (ChangeRecord new file: file position: itemPosition type: #doIt)
								text: 'do it: ' , (item contractTo: 160)]]]].
	self clearSelections! !


!ClassBuilder methodsFor: 'private' stamp: 'jmv 7/17/2017 15:48:14'!
informUserDuring: aBlock
	self class isSilent ifTrue:[^aBlock value].
	Utilities informUserDuring:[:barBlock|
		progress _ barBlock.
		aBlock value].
	progress _ nil.! !


!Integer class methodsFor: 'prime numbers' stamp: 'jmv 7/17/2017 15:44:55'!
verbosePrimesUpTo: max do: aBlock
	"Integer verbosePrimesUpTo: SmallInteger maxVal" "<- heh, heh"
	"Compute primes up to max, but be verbose about it"
	| lastTime |
	lastTime := Time localMillisecondClock.
	Utilities informUserDuring: [ :barBlock|
		barBlock value:'Computing primes...'.
		self primesUpTo: max do: [ :prime| | nowTime |
			aBlock value: prime.
			nowTime := Time localMillisecondClock.
			(nowTime - lastTime > 1000) ifTrue:[
				lastTime := nowTime.
				barBlock value: 'Last prime found: ', prime printString]]].! !


!LookupKey methodsFor: 'bindings' stamp: 'jmv 7/17/2017 15:45:04'!
recompileBindingsAnnouncing: aBool
	"Make the receiver (a global read-write binding) be a read-only binding"
	aBool ifTrue:[
		Utilities informUserDuring: [ :barBlock |
			(Smalltalk allCallsOn: self) do: [ :mref | 
				barBlock value: 'Recompiling ', mref stringVersion.
				mref actualClass recompile: mref methodSymbol ].
		].
	] ifFalse:[
		(Smalltalk allCallsOn: self) do: [ :mref |
			mref actualClass recompile: mref methodSymbol ]
	]! !


!SequenceableCollection methodsFor: 'gui' stamp: 'jmv 7/17/2017 15:45:17'!
do: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: self size
		during: [ :barBlock |
			self withIndexDo: [ :each :i |
				barBlock value: i.
				aBlock value: each]]! !


!String methodsFor: 'displaying' stamp: 'jmv 7/17/2017 15:41:46'!
displayProgressAt: aPoint from: minVal to: maxVal during: workBlock
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
		1 to: 100 do: [:x | 
			(Delay forMilliseconds: 10) wait.
			barBlock value: x.]].

['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
	1 to: 100 do: [:x | barBlock value: x.
			(Delay forMilliseconds: 100) wait]].] fork
	
['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: x \\ 11.
			(Delay forMilliseconds: 100) wait]]] fork

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: nil.
			(Delay forMilliseconds: 200) wait]].

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.

The use of ProgressInitiationException allows for avoiding actual
progress display, by catching the exception.
"
	^ProgressInitiationException 
		display: self
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock! !


!Set class methodsFor: 'initialization' stamp: 'jmv 7/17/2017 15:40:16'!
quickRehashAllSets  "Set rehashAllSets"
	| insts |
	self withAllSubclassesDo:
		[:c |
			insts _ c allInstances.
			(insts isEmpty or: [c = MethodDictionary]) ifFalse:
			['Rehashing instances of ' , c name
				displayProgressAt: Sensor mousePoint
				from: 1 to: insts size
				during: [ :barBlock | 1 to: insts size do: [:x | barBlock value: x. (insts at: x) rehash]]
			]
		]! !

!Set class methodsFor: 'initialization' stamp: 'jmv 7/17/2017 15:40:27'!
rehashAllSets  "Set rehashAllSets"
	| insts |
	self withAllSubclassesDo:
		[:c | insts _ c allInstances.
		insts isEmpty ifFalse:
		['Rehashing instances of ' , c name
			displayProgressAt: Sensor mousePoint
			from: 1 to: insts size
			during: [ :barBlock |
			1 to: insts size do:
				[ :x | barBlock value: x.
				(insts at: x) rehash]]]]! !


!Dictionary methodsFor: 'removing' stamp: 'jmv 7/17/2017 15:44:29'!
unreferencedKeys
	"| uk | (Time millisecondsToRun: [uk := TextConstants unreferencedKeys]) -> uk"

	^'Scanning for references . . .' 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size * 2
		during: 
			[:barBlock | | currentClass n associations referencedAssociations |
			currentClass := nil.
			n := 0.
			associations := self associations asIdentitySet.
			referencedAssociations := IdentitySet new: associations size.
			Smalltalk allSelect:
				[:m|
				m methodClass ~~ currentClass ifTrue:
					[currentClass := m methodClass.
					 barBlock value: (n := n + 1)].
				m literalsDo:
					[:l|
					(l isVariableBinding and: [associations includes: l]) ifTrue:
						[referencedAssociations add: l]].
				false].
			((associations reject: [:assoc | referencedAssociations includes: assoc]) collect: [:assoc| assoc key]) asSet]! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/17/2017 15:46:10'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStreamDo: [ :f |
		f timeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :barBlock |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class | 
					barBlock value: (classCount _ classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition _ f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old'.
	DirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFileEntry appendStream.

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/17/2017 15:46:14'!
condenseSources	
	"Move all the changes onto a compacted sources file."
	"Smalltalk condenseSources"

	| classCount newVersionString oldChanges oldChangesLocalName oldChangesPathName newChangesPathName newSourcesName |
	newVersionString _ FillInTheBlankMorph request: 'Please name the new sources file' initialAnswer: SourceFileVersionString.
	newVersionString ifNil: [^ self].
	newVersionString = SourceFileVersionString ifTrue: [
		^ self error: 'The new source file must not be the same as the old.'].
	SourceFileVersionString _ newVersionString.

	"Write all sources with fileIndex 1"
	newSourcesName _ self defaultSourcesName.
	newSourcesName asFileEntry writeStreamDo: [ :f |
		f timeStamp.
		'Condensing Sources File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :barBlock |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class |
					barBlock value: (classCount _ classCount + 1).
					class fileOutOn: f moveSource: true toFile: 1]]].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	"Make a new empty changes file"
	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	self closeSourceFiles.
	oldChangesPathName ifNotNil: [
		(oldChangesPathName, '.old') asFileEntry delete.
		oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old' ].
	newChangesPathName _ self defaultChangesName.
	newChangesPathName asFileEntry writeStreamDo: [ :stream |
		stream timeStamp ].
	LastQuitLogPosition _ 0.

	self openSourceFiles.
	self inform: 'Source files have been rewritten!!
 
Check that all is well, and then save/quit.
 
Otherwise, remove new sources/changes,
replace them with the former ones, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/17/2017 15:41:56'!
macroBenchmark1    "Smalltalk macroBenchmark1"
	"Decompiles and prettyPrints the source for every method in the system (or less depending on the *FILTER*, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same.  Because it never installs the new method, it should not cause any flusing of the method cache."
	 | methodNode oldMethod newMethod badOnes oldCodeString n classes |
	classes _ Smalltalk allClasses select: [:c | c name < 'B3'].
	badOnes _ OrderedCollection new.
'Decompiling and recompiling...'
displayProgressAt: Sensor mousePoint
from: 0 to: (classes detectSum: [:c | c selectors size])
during: [:barBlock | n _ 0.
	classes do:
		[:cls | 
		"Transcript cr; show: cls name."
		cls selectors do:
			[:selector | barBlock value: (n _ n+1).
			oldMethod _ cls compiledMethodAt: selector.
			oldCodeString _ (cls decompilerClass new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls notifying: nil ifFail: nil.
			newMethod _ methodNode generate: #(0 0 0 0).
			oldCodeString = (cls decompilerClass new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [badOnes add: cls name , ' ' , selector]]].
].
	^ badOnes size! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/17/2017 15:46:19'!
macroBenchmark3   "Smalltalk macroBenchmark3"
	| testBlock tallies prev receiver |
	"Runs the stepping simulator with the messageTally tree (like tallySends)."
	testBlock _
		['Running the context step simulator'
			displayProgressAt: Sensor mousePoint
			from: 0 to: 200
			during:
				[:barBlock |
				1 to: 200 do:
				[:x | barBlock value: x.
				Float pi printString.
				15 factorial printString]]].
	tallies _ MessageTally new class: testBlock receiver class
							method: testBlock method.
	receiver _ nil.
	prev _ testBlock.
	thisContext sender
		runSimulated: testBlock
		contextAtEachStep:
			[:current |
			current == prev ifFalse: [
				"call or return"
				prev sender ifNotNil: [
					"call only"
					(receiver == nil or: [current receiver == receiver])
						ifTrue: [tallies tally: current by: 1]].
				prev _ current]].
! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/17/2017 15:46:24'!
obsoleteMethodReferences
	"
	Smalltalk obsoleteMethodReferences
	Smalltalk browseObsoleteMethodReferences
	Open a browser on all referenced behaviors that are obsolete"
	| obsClasses obsRefs references |
	references _ WriteStream on: Array new.
	obsClasses _ self obsoleteBehaviors.
	'Scanning for methods referencing obsolete classes' displayProgressAt: Sensor mousePoint
		from: 1 to: obsClasses size during: [ :barBlock |
	obsClasses keysAndValuesDo: [ :index :each |
		barBlock value: index.
		obsRefs _ self pointersTo: each except: obsClasses.
		obsRefs do: [ :ref |
			"Figure out if it may be a global"
			(ref isVariableBinding and: [ ref key isString "or Symbol" ]) ifTrue: [
				(self pointersTo: ref) do: [ :meth |
					(meth is: #CompiledMethod) ifTrue: [
						meth methodReference ifNotNil: [ :mref |
							(mref isValid and: [ mref compiledMethod == meth]) ifTrue: [
								references nextPut: mref ]]]]]]].
	].
	^references contents! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/17/2017 15:46:30'!
testDecompiler
	"
	Smalltalk testDecompiler
	"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes _ OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldMethod _ cls compiledMethodAt: selector.
					oldCodeString _ (cls decompilerClass new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldCodeString =
						(cls decompilerClass new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: (MethodReference class: cls selector: selector) ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Decompiler Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/17/2017 15:46:37'!
testFormatter
	"Smalltalk testFormatter"
	"Reformats the source for every method in the system, and then compiles that source and verifies that it generates identical code.
	The formatting used will be classic monochrome."
	| newCodeString methodNode oldMethod newMethod badOnes n |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					newCodeString _ cls compilerClass new
						format: (cls sourceCodeAt: selector)
						in: cls
						notifying: nil.
					methodNode _ cls compilerClass new
						compile: newCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldMethod _ cls compiledMethodAt: selector.
					oldMethod = newMethod ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 7/17/2017 15:46:43'!
testFormatter2
	"Smalltalk testFormatter2"
	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged.
	The formatting used will be classic monochrome"
	| newCodeString badOnes n oldCodeString oldTokens newTokens |
	badOnes _ OrderedCollection new.
	'Formatting all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :barBlock |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ barBlock value: n ].
					oldCodeString _ (cls sourceCodeAt: selector) asString.
					newCodeString _ cls compilerClass new
						format: oldCodeString
						in: cls
						notifying: nil.
					oldTokens _ oldCodeString findTokens: Character separators.
					newTokens _ newCodeString findTokens: Character separators.
					oldTokens = newTokens ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: cls name , ' ' , selector ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/17/2017 15:45:58'!
allMethodsSourceStringMatching: aString
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring. 
	Search the class comments also.
	Argument might include $*, that matches any subsequence.
	For example, try:
		ensure:*[*close*]
	"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
	list add:
		(MethodReference new
			setStandardClass: mrClass
			methodSymbol: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :barBlock |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				barBlock value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						(aString match: (cl sourceCodeAt: sel)) ifTrue: [
							adder
								value: cl
								value: sel ]].
					
					(aString match: cl organization classComment asString) ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 7/17/2017 15:46:02'!
allMethodsWithSourceString: aString matchCase: caseSensitive
	"Answer a sorted Collection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"
	| list classCount adder |
	list _ Set new.
	adder _ [ :mrClass :mrSel |
	list add:
		(MethodReference new
			setStandardClass: mrClass
			methodSymbol: mrSel) ].
	'Searching all source code...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: Smalltalk classNames size
		during: [ :barBlock |
			classCount _ 0.
			Smalltalk allClassesDo: [ :class |
				barBlock value: (classCount _ classCount + 1).
				(Array
					with: class
					with: class class) do: [ :cl |
					cl selectorsDo: [ :sel |
						((cl sourceCodeAt: sel)
							findString: aString
							startingAt: 1
							caseSensitive: caseSensitive) > 0 ifTrue: [
							adder
								value: cl
								value: sel ]].
					(cl organization classComment asString
						findString: aString
						startingAt: 1
						caseSensitive: caseSensitive) > 0 ifTrue: [
						adder
							value: cl
							value: #Comment ]]]].
	^ list asArray sort.! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 7/17/2017 15:45:48'!
abandonSources
	"
	Smalltalk abandonSources
	"
	 | m bTotal bCount |
	(self confirm:
'This method will detach the image fom source code.
A fresh changes file will be created to record further changes.
-- CAUTION --
If you have backed up your system and
are prepared to face the consequences of
abandoning source code files, choose Yes.
If you have any doubts, you may choose No
to back out with no harm done.')
		==  true ifFalse: [^ self inform: 'Okay - no harm done'].
	bTotal _ 0.  bCount _ 0.
	Smalltalk allBehaviorsDo: [: b | bTotal _ bTotal + 1].
'Doing #destroySourcePointer ...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: bTotal
	during: [ :barBlock |
		Smalltalk allBehaviorsDo: [ :cl |
		"for testing"
		"{ EllipseMorph } do: [ :cl |"
			barBlock value: (bCount _ bCount + 1).
			cl selectors do: [:selector |
				m _ cl compiledMethodAt: selector.
				m destroySourcePointer ]]].
	Smalltalk allBehaviorsDo: [:b | b zapOrganization].
	Smalltalk closeSourceFiles.
	Preferences disable: #warnIfNoChangesFile.
	Preferences disable: #warnIfNoSourcesFile! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 7/17/2017 15:42:02'!
removeAllUnSentMessages
	"Smalltalk removeAllUnSentMessages"
	"[Smalltalk unusedClasses do: [:c | (Smalltalk at: c) removeFromSystem]. 
	Smalltalk removeAllUnSentMessages > 0] whileTrue."
	"Remove all implementations of unsent messages."
	| sels n |
	sels _ self allUnSentMessages.
	self presumedSentMessages
		do: [:sel | sels
				remove: sel
				ifAbsent: nil].
	sels size = 0
		ifTrue: [^ 0].
	n _ 0.
	Smalltalk
		allBehaviorsDo: [:x | n _ n + 1].
	'Removing ' , sels size printString , ' messages . . .'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: n
		during: [:barBlock | 
			n _ 0.
			self
				allBehaviorsDo: [:class | 
					barBlock value: (n _ n + 1).
					sels
						do: [:sel | class removeSelector: sel]]].
	^ sels size! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 7/17/2017 15:45:10'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :barBlock | 
			[ self atEnd ] whileFalse: [
					barBlock value: self position.
					self skipSeparators.
					
					[
						val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, 
									because next line is a doit that does the stuff
									(or because it is handled elsewhere)"
									(chunk beginsWith: 'description:  ')
									| (chunk beginsWith: 'provides: ')
									| (chunk beginsWith: 'requires: ')
									| (chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Compiler evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Compiler evaluate: chunk logged: true ]
										on: Error
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex pass ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true ] ].
			].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !

!PositionableStream methodsFor: 'gui' stamp: 'jmv 7/17/2017 15:38:57'!
untilEnd: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: self size
		during: [ :barBlock |
			[ self atEnd ] whileFalse: [
				barBlock value: self position.
				aBlock value ]]! !


!ReferenceStream methodsFor: 'statistics' stamp: 'jmv 7/17/2017 15:40:06'!
statisticsOfRefs
	"Analyze the information in references, the objects being written out"

	| parents n kids nm ownerBags tallies owners objParent normalReferences |
	normalReferences _ self references.	"Exclude unrealized weaks"
	parents _ IdentityDictionary new: normalReferences size * 2.
	n _ 0.
	'Finding Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:
		[ :parent | barBlock value: (n _ n+1).
		kids _ parent class isFixed
			ifTrue: [(1 to: parent class instSize) collect: [:i | parent instVarAt: i]]
			ifFalse: [parent class isBits ifTrue: [Array new]
					 ifFalse: [(1 to: parent basicSize) collect: [:i | parent basicAt: i]]].
		(kids select: [:x | normalReferences includesKey: x])
			do: [:child | parents at: child put: parent]]].
	ownerBags _ Dictionary new.
	tallies _ Bag new.
	n _ 0.
	'Tallying Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:  "For each class of obj, tally a bag of owner classes"
		[ :obj | barBlock value: (n _ n+1).
		nm _ obj class name.
		tallies add: nm.
		owners _ ownerBags at: nm ifAbsent: [ownerBags at: nm put: Bag new].
		(objParent _ parents at: obj ifAbsent: nil) ifNotNil: [
			owners add: objParent class name]]].
	^ String streamContents: [ :strm | 
		tallies sortedCounts do: [ :assn |
			n _ assn key.  nm _ assn value.
			owners _ ownerBags at: nm.
			strm newLine; nextPutAll: nm; space; print: n.
			owners size > 0 ifTrue: [
				strm newLine; tab; print: owners sortedCounts]]]! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 7/17/2017 15:40:37'!
nextPut: anObject
	"Really write three objects: (version, class structure, object).  But only when called from the outside.  If any instance-specific classes are present, prepend their source code.  byteStream will be in fileOut format.
	You can see an analysis of which objects are written out by doing: 
	(SmartRefStream statsOfSubObjects: anObject)
	(SmartRefStream tallyOfSubObjects: anObject)
	(SmartRefStream subObjects: anObject ofClass: aClass)"

| info |
topCall
	ifNil: [
		topCall _ anObject.
		'Please wait while objects are counted' 
			displayProgressAt: Sensor mousePoint
			from: 0 to: 10
			during: [ :barBlock | info _ self instVarInfo: anObject].
		byteStream binary.
		'Writing an object file' displayProgressAt: Sensor mousePoint
			from: 0 to: objCount*4	"estimate"
			during: [ :barBlock |
				objCount _ 0.
				progressBar _ barBlock.
				self setStream: byteStream reading: false.
					"set basePos, but keep any class renames"
				super nextPut: ReferenceStream versionCode.
				super nextPut: info.
				super nextPut: anObject.		"<- the real writing"
				].
					"Note: the terminator, $!!, is not doubled inside object data"
		"references is an IDict of every object that got written"
		byteStream ascii.
		byteStream nextPutAll: '!!'; newLine; newLine.
		byteStream padToEndWith: $ .	"really want to truncate file, but can't"
		topCall _ progressBar _ nil]	"reset it"
	ifNotNil: [
		super nextPut: anObject.
		progressBar ifNotNil: [progressBar value: (objCount _ objCount + 1)]].
! !

!SmartRefStream methodsFor: 'read write' stamp: 'jmv 7/17/2017 15:40:57'!
nextPutObjOnly: anObject
	"Really write three objects: (version, class structure, object).  But only when called from the outside.  Not in fileOut format.  No class definitions will be written for instance-specific classes.  Error if find one.  (Use nextPut: instead)"

	| info |
	topCall
		ifNil: [
			topCall _ anObject.
			super nextPut: ReferenceStream versionCode.
			'Please wait while objects are counted' displayProgressAt: Sensor mousePoint
				from: 0 to: 10
				during: [ :barBlock |
					info _ self instVarInfo: anObject].
			'Writing an object file' displayProgressAt: Sensor mousePoint
				from: 0 to: objCount*4	"estimate"
				during: [ :barBlock |
					objCount _ 0.
					progressBar _ barBlock.
					super nextPut: info.
					super nextPut: anObject.	"<- the real writing"
					"Class inst vars not written here!!"].
			"references is an IDict of every object that got written
			(in case you want to take statistics)"
			"Transcript cr; show: structures keys printString."		"debug"
			topCall _ progressBar _ nil]	"reset it"
		ifNotNil: [
			super nextPut: anObject.
			progressBar ifNotNil: [progressBar value: (objCount _ objCount + 1)]].! !


!ProgressInitiationException class methodsFor: 'examples and tests' stamp: 'jmv 7/17/2017 15:39:39'!
testInnermost

	"
	test the progress code WITHOUT special handling
	
	ProgressInitiationException  testInnermost
	"

	^'Now here''s some Real Progress'
		displayProgressAt: Sensor mousePoint
		from: 0 
		to: 10
		during: [ :barBlock |
			1 to: 10 do: [ :x | 
				barBlock value: x.
				(Delay forMilliseconds: 500) wait.
				x = 5 ifTrue: [1/0].	"just to make life interesting"
			].
			'done'
		].

! !


!Utilities class methodsFor: 'user interface' stamp: 'jmv 7/17/2017 15:48:35'!
informUserDuring: barBlock 
	"Display a message above (or below if insufficient room) the cursor during execution of the given block."

	"Utilities informUserDuring:[:barBlock|
		#(one two three) do:[:info|
			barBlock value: info.
			(Delay forSeconds: 1) wait]]"

	(MVCMenuMorph from: (SelectionMenu labels: '') title: '						') 
				informUserAt: Sensor mousePoint
				during: barBlock! !


!CodeFile methodsFor: 'reading' stamp: 'jmv 7/17/2017 15:44:20'!
buildFrom: aStream
	| chgRec changes |
	changes _ (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	('Processing ', self name) 
		displayProgressAt: Sensor mousePoint
		from: 1
		to: changes size
		during: [ :barBlock |
			1 to: changes size do:[:i|
				barBlock value: i.
				chgRec := changes at: i.
				chgRec class == MethodDeletionChangeRecord
					ifTrue: [ self removedMethod: chgRec command with: chgRec ]
					ifFalse: [ self perform: (chgRec changeType copyWith: $:) asSymbol with: chgRec ].
			].
		]! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 7/17/2017 15:45:36'!
printSpaceAnalysis: threshold on: aStream
	"
	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')
	"
	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |
	Smalltalk garbageCollect.
	totalCodeSpace _ totalInstCount _ totalInstSpace _ n _ 0.
	results _ OrderedCollection new: Smalltalk classNames size.
	'Taking statistics...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :barBlock |
			Smalltalk allClassesDo: [ :cl |
				codeSpace _ cl spaceUsed.
				barBlock value: (n _ n+1).
				Smalltalk garbageCollectMost.
				instCount _ cl instanceCount.
				instSpace _ (cl indexIfCompact > 0 ifTrue: [4] ifFalse: [8]) * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!!"
				cl isVariable
					ifTrue: [
						eltSize _ cl isBytes ifTrue: [1] ifFalse: [4].
						cl allInstancesDo: [ :x |
							instSpace _ instSpace + (x basicSize * eltSize)]]
					ifFalse: [instSpace _ instSpace + (cl instSize * instCount * 4)].
				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).
				totalCodeSpace _ totalCodeSpace + codeSpace.
				totalInstCount _ totalInstCount + instCount.
				totalInstSpace _ totalInstSpace + instSpace]].
	totalPercent _ 0.0.

	aStream timeStamp.
	aStream
		nextPutAll: ('Class' padded: #right to: 30 with: $ );
		nextPutAll: ('code space' padded: #left to: 12 with: $ );
		nextPutAll: ('# instances' padded: #left to: 12 with: $ );
		nextPutAll: ('inst space' padded: #left to: 12 with: $ );
		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.

	threshold > 0 ifTrue: [
		"If inst count threshold > 0, then sort by space"
		results _ (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])
			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].

	results do: [:s |
		aStream
			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );
			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );
			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );
			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).
		percent _ s spaceForInstances*100.0/totalInstSpace.
		totalPercent _ totalPercent + percent.
		percent >= 0.1 ifTrue: [
			percent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil ].
		aStream newLine].

	aStream
		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );
		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ ).
	totalPercent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil! !

