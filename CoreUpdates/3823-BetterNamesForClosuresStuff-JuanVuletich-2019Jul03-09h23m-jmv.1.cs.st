'From Cuis 5.0 [latest update: #3822] on 3 July 2019 at 9:40:21 am'!

!BlockClosure methodsFor: 'testing' stamp: 'jmv 7/3/2019 09:37:11'!
isCleanClosure
	"A clean closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do non local return (return from method to caller, i.e. ^ something)
	Therefore it doesn't close over a lexical scope, and in this sense they are trivial.
	They can also be called 'context free' or 'simple block'.
	"

	| recreated source |
	source _ self decompile decompileString.

	"This catches any acess to outer context!!"
	recreated _ [ Compiler evaluate: source ] on: UndeclaredVariableWarning do: [ :ex | ^ false ].

	"Fail if returns from outer context, or uses self"
	Smalltalk
		eliotsClosureMeasurementsOn: recreated outerContext method
		over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
			anyClosureDoesNonLocalReturn ifTrue: [ ^ false ].
			anyClosureUsesSelf ifTrue: [ ^ false ]].

	"Ok."
	^true! !


!BlockClosure methodsFor: 'objects from disk' stamp: 'jmv 7/3/2019 09:38:53'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a textual reference instead."
	self isCleanClosure ifFalse: [
		self error: 'Can only serialize clean (context free) closures.' ].
	^ DiskProxy
		global: #Compiler
		selector: #evaluate:
		args: (Array with: self decompile decompileString)! !


!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:24:10'!
browseMethodsWithClosuresThatAccessOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatAccessOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				anyClosureHasCopied ].
			])
		name: 'Closures that read or write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:24:14'!
browseMethodsWithClosuresThatOnlyReadOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatOnlyReadOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount
					:hasIndirectTemps :anyClosureHasCopiedValues :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				anyClosureHasCopiedValues & hasIndirectTemps not].
			])
		name: 'Closures that read but not write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:24:19'!
browseMethodsWithClosuresThatWriteOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatWriteOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				hasIndirectTemps ].
			])
		name: ' Closures that write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:24:25'!
browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise
	"
	Smalltalk browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				hasIndirectTemps and: [  anyClosureDoesNonLocalReturn not and: [ anyClosureUsesSelf not ] ] ].
			])
		name: ' Closures that write to outer temps, but clean otherwise'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:24:29'!
browseMethodsWithMoreThanOneClosure
	"
	Smalltalk browseMethodsWithMoreThanOneClosure
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				closuresCount > 1 ].
			])
		name: 'Methods with more than one Closure'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:24:34'!
browseMethodsWithOnlyCleanClosures
	"
	Smalltalk browseMethodsWithOnlyCleanClosures
	"
	self
		browseMessageList: (
			self allSelect: [ :m | 
				self eliotsClosureMeasurementsOn: m over: [ :closuresCount
						:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
					closuresCount > 0 and: [
						(anyClosureHasCopied or: [ anyClosureDoesNonLocalReturn or: [ anyClosureUsesSelf ]]) not ].
				]
			])
		name: 'Methods with only Clean Closures'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:24:43'!
closuresInfoStringForClass: aClass selector: aSelector
	"
	Smalltalk closuresInfoStringFor: PlayingWithClosures class >> #exp01Argument
	"
	| answer all someDo noneDoes method |
	method _ aClass compiledMethodAt: aSelector ifAbsent: [ ^'' ].
	self eliotsClosureMeasurementsOn: method over: [ 
				:closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
			
		closuresCount > 0
			ifFalse: [ answer _ 'No real (non-optimized) Closures' ]
			ifTrue: [
				closuresCount = 1
					ifTrue: [
						answer _ '1 Closure: '.
						all _ ''.
						someDo _ 'does'. 
						noneDoes _ 'does not' ]
					ifFalse: [
						answer _ closuresCount printString, ' Closures: '.
						all _ 'all '.
						someDo _ 'some do'. 
						noneDoes _ 'none does' ].
				(anyClosureHasCopied or: [ anyClosureDoesNonLocalReturn or: [ anyClosureUsesSelf ]])
					ifFalse: [ answer _ answer, all, 'clean' ]
					ifTrue: [
						answer _ answer, (anyClosureHasCopied
							ifTrue: [
								hasIndirectTemps
									ifTrue: [ someDo, ' write (and maybe ', someDo, ' read)' ]
									ifFalse: [ someDo, ' read (but ', noneDoes, ' write)' ] ]
							ifFalse: [ noneDoes, ' access' ]), ' outer temps; '.
						answer _ answer, (anyClosureDoesNonLocalReturn
							ifTrue: [ someDo ]
							ifFalse: [ noneDoes ]), ' ^return; '.
						answer _ answer, (anyClosureUsesSelf
							ifTrue: [ someDo ]
							ifFalse: [ noneDoes ]), ' use self'
					].
			]
		].
		^answer! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:34:34'!
eliotsClosureMeasurements
	"
	Smalltalk eliotsClosureMeasurements
	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	"
      | numMethods numMethodsWithClosure numMethodsWithIndirectTemps
         numClosures numClosuresWithCopiedValues numCopiedValuesForClosure
         numRemoteTemps numScopesWithRemoteTemps
         nonLocalReturnsInClosure closureUsesSelfs nonLocalReturnAndUsesSelfs numClean |

        numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
        numClosures := numClosuresWithCopiedValues := numCopiedValuesForClosure :=
        numRemoteTemps := numScopesWithRemoteTemps :=
        nonLocalReturnsInClosure := closureUsesSelfs := nonLocalReturnAndUsesSelfs := numClean := 0.
        self allSelect: [ :m |
                 | s hasClosure hasIndirectTemps blkPc blkSz doesNonLocalReturn usesSelf hasCopied sel |
                 sel _ false.
                 hasClosure := hasIndirectTemps := false.
                 s := InstructionStream on: m.
                 s scanFor:
                          [:b|
                          b = 143 "closure creation" ifTrue:
                                   [hasClosure := true.
                                   numClosures := numClosures + 1.
                                   s followingByte >= 16 ifTrue:
                                            [numClosuresWithCopiedValues := numClosuresWithCopiedValues + 1.
                                             numCopiedValuesForClosure := numCopiedValuesForClosure + (s followingByte >> 4)]].
                          (b = 138 "indirect temp vector creation"
                           and: [s followingByte <= 127]) ifTrue:
                                   [hasIndirectTemps := true.
                                    numScopesWithRemoteTemps := numScopesWithRemoteTemps + 1.
                                    numRemoteTemps := numRemoteTemps + s followingByte].
                          false].
                 numMethods := numMethods + 1.
                 hasClosure ifTrue:
                          [numMethodsWithClosure := numMethodsWithClosure + 1.
                           s pc: m initialPC; scanFor: [:b| b = 143].

"jmv-This looks like the correct place to do this"
                           hasCopied := s followingByte >= 16.

                           blkSz := s interpretNextInstructionFor: BlockStartLocator new.
                           blkPc := s pc.
                           doesNonLocalReturn := usesSelf := false.

"jmv-Doing this here looks like a bug. See the other comment"
                           hasCopied := s followingByte >= 16.

"jmv-Another bug. This only considers the first closure (and any nested closure in it), but not later ones"

                           s scanFor:
                                   [:b|
                                   s pc >= (blkPc + blkSz)
                                            ifTrue: [true]
                                            ifFalse:
                                                     [doesNonLocalReturn := doesNonLocalReturn or: [s willReturn and: [s willBlockReturn not]].
                                                      usesSelf := usesSelf or: [b = 112 "pushSelf"
                                                                                                  or: [b < 16 "pushInstVar"
                                                                                                  or: [(b = 128 and: [s followingByte <= 63]) "pushInstVar"
                                                                                                  or: [(b between: 96 and: 96 + 7) "storePopInstVar"
                                                                                                  or: [(b = 130 and: [s followingByte <= 63]) "storePopInstVar"
                                                                                                  or: [(b = 129 and: [s followingByte <= 63]) "storeInstVar"
                                                                                                  or: [b = 132 and: [s followingByte = 160]]]]]]]].
                                                     false]].
                           doesNonLocalReturn ifTrue:
                                   [nonLocalReturnsInClosure := nonLocalReturnsInClosure + 1].
                           usesSelf ifTrue:
                                   [closureUsesSelfs := closureUsesSelfs + 1].
                           (doesNonLocalReturn and: [usesSelf]) ifTrue:
                                   [nonLocalReturnAndUsesSelfs := nonLocalReturnAndUsesSelfs + 1].
                           (doesNonLocalReturn or: [usesSelf or: [hasCopied]]) ifFalse:
                                   [numClean := numClean + 1]].
                 hasIndirectTemps ifTrue: [numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
                 sel].
^        { {'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. {'MethodsWithIndirectTemps'. numMethodsWithIndirectTemps}.
         {'Closures'. numClosures}. {'CopiedValuesForClosures'. numCopiedValuesForClosure}. {'ClosuresWithCopiedValues'. numClosuresWithCopiedValues}.
         {'RemoteTemps'. numRemoteTemps}. {'ScopesWithRemoteTemps'. numScopesWithRemoteTemps}.
         {'MethodsWithNonLocalReturnsInClosures'. nonLocalReturnsInClosure}. {'MethodsWithReferencesToSelfInClosures'. closureUsesSelfs}. {'Both'. nonLocalReturnAndUsesSelfs}.
         {'MethodsWithOnlyCleanClosures'. numClean} }! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:30:32'!
eliotsClosureMeasurements2
	"
	Smalltalk eliotsClosureMeasurements2
	"
	| numMethods numMethodsWithClosure numMethodsWithIndirectTemps anyClosureDoesNonLocalReturnCount anyClosureUsesSelfCount bothCount onlyCleanBlocksCount anyClosureHasCopiedCount |

	numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
	anyClosureDoesNonLocalReturnCount := anyClosureUsesSelfCount := bothCount := onlyCleanBlocksCount := 0.
	anyClosureHasCopiedCount _ 0.
	self allSelect: [ :m | 
		self eliotsClosureMeasurementsOn: m over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
			numMethods := numMethods + 1.
			closuresCount > 0 ifTrue: [ numMethodsWithClosure := numMethodsWithClosure + 1 ].
			hasIndirectTemps ifTrue: [ numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
			anyClosureDoesNonLocalReturn ifTrue: [ anyClosureDoesNonLocalReturnCount := anyClosureDoesNonLocalReturnCount + 1].
			anyClosureUsesSelf ifTrue: [ anyClosureUsesSelfCount := anyClosureUsesSelfCount + 1].
			(anyClosureDoesNonLocalReturn and: [anyClosureUsesSelf]) ifTrue: [ bothCount := bothCount + 1].
			closuresCount > 0 ifTrue: [
				(anyClosureDoesNonLocalReturn or: [anyClosureUsesSelf or: [anyClosureHasCopied]]) ifFalse: [
					onlyCleanBlocksCount := onlyCleanBlocksCount + 1]].
			anyClosureHasCopied ifTrue: [ anyClosureHasCopiedCount _ anyClosureHasCopiedCount + 1 ].
			false.
		]
	].
	^{
		{'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. 
		{'WithClosuresAccessingOuterTemps'. anyClosureHasCopiedCount}.
		{'WithClosuresWritingOuterTemps'. numMethodsWithIndirectTemps}.
		{'WithNonLocalReturnsInClosures'. anyClosureDoesNonLocalReturnCount}. 
		{'WithReferencesToSelfInClosures'. anyClosureUsesSelfCount}. 
		{'BothAbove'. bothCount}.
		{'WithOnlyCleanClosures'. onlyCleanBlocksCount}.
	}! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:30:39'!
eliotsClosureMeasurements2On: aMethod
	"
	A Couple of Clean Closures
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01Argument
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01LocalTemp
	
	Closures reading and writing to outer temps
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTemp
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempWithAssignment

	Closure doing an method return
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01UpArrowReturn

	Closures sending messages to self & super
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01SelfSend
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01SuperSend

	A couple of non-closures, i.e. blocks that are optimized by the compiler and a closure is never created
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempOptimized
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempOptimizedWithAssignment
	
	A remote temp whose declaration can not be moved inside the block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempCantBeMovedInside
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempAssignedTwice
	A remote temp whose declaration can be moved inside the block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01RemoteTempCanBeMovedInside
	A not-so remote temp. The declaration was moved inside the block, making it a clean block
	Smalltalk eliotsClosureMeasurements2On: PlayingWithClosures class >> #exp01LocalTemp
	"
	| numMethods numMethodsWithClosure numMethodsWithIndirectTemps anyClosureDoesNonLocalReturnCount anyClosureUsesSelfCount bothCount onlyCleanBlocksCount anyClosureHasCopiedCount |

	numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
	anyClosureDoesNonLocalReturnCount := anyClosureUsesSelfCount := bothCount := onlyCleanBlocksCount := 0.
	anyClosureHasCopiedCount _ 0.
	self eliotsClosureMeasurementsOn: aMethod over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
		numMethods := numMethods + 1.
		closuresCount > 0 ifTrue: [ numMethodsWithClosure := numMethodsWithClosure + 1 ].
		hasIndirectTemps ifTrue: [ numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
		anyClosureDoesNonLocalReturn ifTrue: [ anyClosureDoesNonLocalReturnCount := anyClosureDoesNonLocalReturnCount + 1].
		anyClosureUsesSelf ifTrue: [ anyClosureUsesSelfCount := anyClosureUsesSelfCount + 1].
		(anyClosureDoesNonLocalReturn and: [anyClosureUsesSelf]) ifTrue: [ bothCount := bothCount + 1].
		closuresCount > 0 ifTrue: [
			(anyClosureDoesNonLocalReturn or: [anyClosureUsesSelf or: [anyClosureHasCopied]]) ifFalse: [
				onlyCleanBlocksCount := onlyCleanBlocksCount + 1]].
		anyClosureHasCopied ifTrue: [ anyClosureHasCopiedCount _ anyClosureHasCopiedCount + 1 ].
	].
	^{
		{'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. 
		{'WithClosuresAccessingOuterTemps'. anyClosureHasCopiedCount}.
		{'WithClosuresWritingOuterTemps'. numMethodsWithIndirectTemps}.
		{'WithNonLocalReturnsInClosures'. anyClosureDoesNonLocalReturnCount}. 
		{'WithReferencesToSelfInClosures'. anyClosureUsesSelfCount}. 
		{'BothAbove'. bothCount}.
		{'WithOnlyCleanClosures'. onlyCleanBlocksCount}.
	}! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 7/3/2019 09:28:27'!
eliotsClosureMeasurementsOn: m over: aFiveArgBlock
	"
	See senders.
	Or try something like:
		Smalltalk
			eliotsClosureMeasurementsOn: FileList >> #defaultContents
			over: [ :closuresCount :hasCopiedValuesForClosure :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesNonLocalReturn :anyClosureUsesSelf |
				(Array with: closuresCount with: hasCopiedValuesForClosure with: hasIndirectTemps with: anyClosureHasCopied with: anyClosureDoesNonLocalReturn with: anyClosureUsesSelf)]

	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	
	Note: This could perhaps be refactored to use the newer #embeddedBlockClosures and testing methods on the closures themselves.
	"
	| s nextScanStart thisClosureHasCopied closuresCount hasIndirectTemps blkPc blkSz anyClosureHasCopied anyClosureDoesNonLocalReturn anyClosureUsesSelf analyzedClosures |
	closuresCount := 0.
	hasIndirectTemps := false.
	anyClosureHasCopied :=  anyClosureDoesNonLocalReturn := anyClosureUsesSelf := false.
	s := InstructionStream on: m.
	s scanFor: [ :b |
		b = 16r8F "16r8F = 143 closure creation" ifTrue: [
			closuresCount := closuresCount + 1].
		(b = 16r8A "16r8A = 138indirect temp vector creation" and: [ s followingByte <= 127]) ifTrue: [
				hasIndirectTemps := true].
		false].
	nextScanStart := m initialPC.
	analyzedClosures := 0.
	[ analyzedClosures < closuresCount ] whileTrue: [
		s pc: nextScanStart; scanFor: [ :b | b = 16r8F ].	"16r8F = 143 Search for first closure"
		analyzedClosures := analyzedClosures + 1.
		thisClosureHasCopied := s followingByte >= 16r10.
		anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
		blkSz := s interpretNextInstructionFor: BlockStartLocator new.		"Findout size of first closure"
		blkPc := s pc.
		s scanFor: [ :b |
			s pc >= (blkPc + blkSz)
				ifTrue: [
					nextScanStart := s pc.
					true]
				ifFalse: [
					b = 16r8F ifTrue: [			
						thisClosureHasCopied := s followingByte >= 16r10.
						anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
						analyzedClosures := analyzedClosures + 1 ].
					anyClosureDoesNonLocalReturn := anyClosureDoesNonLocalReturn or: [s willReturn and: [s willBlockReturn not]].
					anyClosureUsesSelf := anyClosureUsesSelf or: [b = 16r70 "pushSelf"
										or: [b < 16r10 "pushInstVar"
										or: [(b = 16r80 and: [s followingByte <= 16r3F]) "pushInstVar"
										or: [(b between: 16r60 and: 16r60 + 7) "storePopInstVar"
										or: [(b = 16r82 and: [s followingByte <= 63]) "storePopInstVar"
										or: [(b = 16r81 and: [s followingByte <= 63]) "storeInstVar"
										or: [b = 16r84 and: [s followingByte = 160]]]]]]]].
					false]]].
	^aFiveArgBlock valueWithArguments: (Array
			with: closuresCount
			with: hasIndirectTemps
			with: anyClosureHasCopied
			with: anyClosureDoesNonLocalReturn
			with: anyClosureUsesSelf)! !

!methodRemoval: BlockClosure #isTrivialClosure!
BlockClosure removeSelector: #isTrivialClosure!
