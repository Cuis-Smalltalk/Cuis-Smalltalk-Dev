'From Cuis 4.2 of 25 July 2013 [latest update: #2326] on 16 May 2015 at 5:01:30.018884 pm'!

!BlockClosure methodsFor: 'testing' stamp: 'jmv 5/16/2015 16:52'!
isTrivialClosure
	"A trivial closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do ^ return (return from method to caller)
	Therefore it doesn't close over a lexical scope, and in this sense they are trivial.
	They can also be called 'context free', 'clean' or 'simple block'.
	"

	| recreated source |
	source _ self decompile decompileString.

	"This catches any acess to outer context!!"
	recreated _ [ Compiler evaluate: source ] on: UndeclaredVariableWarning do: [ :ex | ^ false ].

	"Fail if returns from outer context, or uses self"
	Smalltalk
		eliotsClosureMeasurementsOn: recreated outerContext method
		over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
			anyClosureDoesMethodReturn ifTrue: [ ^ false ].
			anyClosureUsesSelf ifTrue: [ ^ false ]].

	"Ok."
	^true! !


!BlockClosure methodsFor: 'objects from disk' stamp: 'jmv 5/16/2015 16:51'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a textual reference instead."
	self isTrivialClosure ifFalse: [
		self error: 'Can only serialize trivial (context free) closures.' ].
	^ DiskProxy
		global: #Compilers
		selector: #evaluate:
		args: (Array with: self decompile decompileString)! !


!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:43'!
browseMethodsWithClosuresThatAccessOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatAccessOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
				anyClosureHasCopied ].
			])
		name: 'Closures that read or write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:43'!
browseMethodsWithClosuresThatOnlyReadOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatOnlyReadOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount
					:hasIndirectTemps :anyClosureHasCopiedValues :anyClosureDoesMethodReturn :anyClosureUsesSelf |
				anyClosureHasCopiedValues & hasIndirectTemps not].
			])
		name: 'Closures that read but not write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:43'!
browseMethodsWithClosuresThatWriteOuterTemps
	"
	Smalltalk browseMethodsWithClosuresThatWriteOuterTemps
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
				hasIndirectTemps ].
			])
		name: ' Closures that write to outer temps'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 11:04'!
browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise
	"
	Smalltalk browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
				hasIndirectTemps and: [  anyClosureDoesMethodReturn not and: [ anyClosureUsesSelf not ] ] ].
			])
		name: ' Closures that write to outer temps, but clean otherwise'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:30'!
browseMethodsWithMoreThanOneClosure
	"
	Smalltalk browseMethodsWithMoreThanOneClosure
	"

	self
		browseMessageList: (self allSelect: [ :m | 
			self eliotsClosureMeasurementsOn: m over: [ :closuresCount 
					:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
				closuresCount > 1 ].
			])
		name: 'Methods with more than one Closure'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:31'!
browseMethodsWithOnlyCleanClosures
	"
	Smalltalk browseMethodsWithOnlyCleanClosures
	"
	self
		browseMessageList: (
			self allSelect: [ :m | 
				self eliotsClosureMeasurementsOn: m over: [ :closuresCount
						:hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
					closuresCount > 0 and: [
						(anyClosureHasCopied or: [ anyClosureDoesMethodReturn or: [ anyClosureUsesSelf ]]) not ].
				]
			])
		name: 'Methods with only Clean Closures'! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/7/2010 21:52'!
closuresInfoStringForClass: aClass selector: aSelector
	"
	Smalltalk closuresInfoStringFor: PlayingWithClosures class >> #exp01Argument
	"
	| answer all someDo noneDoes method |
	method _ aClass compiledMethodAt: aSelector ifAbsent: [ ^'' ].
	self eliotsClosureMeasurementsOn: method over: [ 
				:closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
			
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
				(anyClosureHasCopied or: [ anyClosureDoesMethodReturn or: [ anyClosureUsesSelf ]])
					ifFalse: [ answer _ answer, all, 'clean' ]
					ifTrue: [
						answer _ answer, (anyClosureHasCopied
							ifTrue: [
								hasIndirectTemps
									ifTrue: [ someDo, ' write (and maybe ', someDo, ' read)' ]
									ifFalse: [ someDo, ' read (but ', noneDoes, ' write)' ] ]
							ifFalse: [ noneDoes, ' access' ]), ' outer temps; '.
						answer _ answer, (anyClosureDoesMethodReturn
							ifTrue: [ someDo ]
							ifFalse: [ noneDoes ]), ' ^return; '.
						answer _ answer, (anyClosureUsesSelf
							ifTrue: [ someDo ]
							ifFalse: [ noneDoes ]), ' use self'
					].
			]
		].
		^answer! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 11:19'!
eliotsClosureMeasurements
	"
	Smalltalk eliotsClosureMeasurements
	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	"
      | numMethods numMethodsWithClosure numMethodsWithIndirectTemps
         numClosures numClosuresWithCopiedValues numCopiedValuesForClosure
         numRemoteTemps numScopesWithRemoteTemps
         methodReturnsInClosure closureUsesSelfs methodReturnAndUsesSelfs numClean |

        numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
        numClosures := numClosuresWithCopiedValues := numCopiedValuesForClosure :=
        numRemoteTemps := numScopesWithRemoteTemps :=
        methodReturnsInClosure := closureUsesSelfs := methodReturnAndUsesSelfs := numClean := 0.
        self allSelect:
                 [:m| | s hasClosure hasIndirectTemps blkPc blkSz doesMethodReturn usesSelf hasCopied sel|
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
                           doesMethodReturn := usesSelf := false.

"jmv-Doing this here looks like a bug. See the other comment"
                           hasCopied := s followingByte >= 16.

"jmv-Another bug. This only considers the first closure (and any nested closure in it), but not later ones"

                           s scanFor:
                                   [:b|
                                   s pc >= (blkPc + blkSz)
                                            ifTrue: [true]
                                            ifFalse:
                                                     [doesMethodReturn := doesMethodReturn or: [s willReturn and: [s willBlockReturn not]].
                                                      usesSelf := usesSelf or: [b = 112 "pushSelf"
                                                                                                  or: [b < 16 "pushInstVar"
                                                                                                  or: [(b = 128 and: [s followingByte <= 63]) "pushInstVar"
                                                                                                  or: [(b between: 96 and: 96 + 7) "storePopInstVar"
                                                                                                  or: [(b = 130 and: [s followingByte <= 63]) "storePopInstVar"
                                                                                                  or: [(b = 129 and: [s followingByte <= 63]) "storeInstVar"
                                                                                                  or: [b = 132 and: [s followingByte = 160]]]]]]]].
                                                     false]].
                           doesMethodReturn ifTrue:
                                   [methodReturnsInClosure := methodReturnsInClosure + 1].
                           usesSelf ifTrue:
                                   [closureUsesSelfs := closureUsesSelfs + 1].
                           (doesMethodReturn and: [usesSelf]) ifTrue:
                                   [methodReturnAndUsesSelfs := methodReturnAndUsesSelfs + 1].
                           (doesMethodReturn or: [usesSelf or: [hasCopied]]) ifFalse:
                                   [numClean := numClean + 1]].
                 hasIndirectTemps ifTrue: [numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
                 sel].
^        { {'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. {'MethodsWithIndirectTemps'. numMethodsWithIndirectTemps}.
         {'Closures'. numClosures}. {'CopiedValuesForClosures'. numCopiedValuesForClosure}. {'ClosuresWithCopiedValues'. numClosuresWithCopiedValues}.
         {'RemoteTemps'. numRemoteTemps}. {'ScopesWithRemoteTemps'. numScopesWithRemoteTemps}.
         {'MethodsWithMethodReturnsInClosures'. methodReturnsInClosure}. {'MethodsWithReferencesToSelfInClosures'. closureUsesSelfs}. {'Both'. methodReturnAndUsesSelfs}.
         {'MethodsWithOnlyCleanClosures'. numClean} }! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/5/2010 10:44'!
eliotsClosureMeasurements2
	"
	Smalltalk eliotsClosureMeasurements2
	"
	| numMethods numMethodsWithClosure numMethodsWithIndirectTemps anyClosureDoesMethodReturnCount anyClosureUsesSelfCount bothCount onlyCleanBlocksCount anyClosureHasCopiedCount |

	numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
	anyClosureDoesMethodReturnCount := anyClosureUsesSelfCount := bothCount := onlyCleanBlocksCount := 0.
	anyClosureHasCopiedCount _ 0.
	self allSelect: [ :m | 
		self eliotsClosureMeasurementsOn: m over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
			numMethods := numMethods + 1.
			closuresCount > 0 ifTrue: [ numMethodsWithClosure := numMethodsWithClosure + 1 ].
			hasIndirectTemps ifTrue: [ numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
			anyClosureDoesMethodReturn ifTrue: [ anyClosureDoesMethodReturnCount := anyClosureDoesMethodReturnCount + 1].
			anyClosureUsesSelf ifTrue: [ anyClosureUsesSelfCount := anyClosureUsesSelfCount + 1].
			(anyClosureDoesMethodReturn and: [anyClosureUsesSelf]) ifTrue: [ bothCount := bothCount + 1].
			closuresCount > 0 ifTrue: [
				(anyClosureDoesMethodReturn or: [anyClosureUsesSelf or: [anyClosureHasCopied]]) ifFalse: [
					onlyCleanBlocksCount := onlyCleanBlocksCount + 1]].
			anyClosureHasCopied ifTrue: [ anyClosureHasCopiedCount _ anyClosureHasCopiedCount + 1 ].
			false.
		]
	].
	^{
		{'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. 
		{'WithClosuresAccessingOuterTemps'. anyClosureHasCopiedCount}.
		{'WithClosuresWritingOuterTemps'. numMethodsWithIndirectTemps}.
		{'WithMethodReturnsInClosures'. anyClosureDoesMethodReturnCount}. 
		{'WithReferencesToSelfInClosures'. anyClosureUsesSelfCount}. 
		{'BothAbove'. bothCount}.
		{'WithOnlyCleanClosures'. onlyCleanBlocksCount}.
	}! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 2/7/2010 20:52'!
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
	| numMethods numMethodsWithClosure numMethodsWithIndirectTemps anyClosureDoesMethodReturnCount anyClosureUsesSelfCount bothCount onlyCleanBlocksCount anyClosureHasCopiedCount |

	numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
	anyClosureDoesMethodReturnCount := anyClosureUsesSelfCount := bothCount := onlyCleanBlocksCount := 0.
	anyClosureHasCopiedCount _ 0.
	self eliotsClosureMeasurementsOn: aMethod over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
		numMethods := numMethods + 1.
		closuresCount > 0 ifTrue: [ numMethodsWithClosure := numMethodsWithClosure + 1 ].
		hasIndirectTemps ifTrue: [ numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
		anyClosureDoesMethodReturn ifTrue: [ anyClosureDoesMethodReturnCount := anyClosureDoesMethodReturnCount + 1].
		anyClosureUsesSelf ifTrue: [ anyClosureUsesSelfCount := anyClosureUsesSelfCount + 1].
		(anyClosureDoesMethodReturn and: [anyClosureUsesSelf]) ifTrue: [ bothCount := bothCount + 1].
		closuresCount > 0 ifTrue: [
			(anyClosureDoesMethodReturn or: [anyClosureUsesSelf or: [anyClosureHasCopied]]) ifFalse: [
				onlyCleanBlocksCount := onlyCleanBlocksCount + 1]].
		anyClosureHasCopied ifTrue: [ anyClosureHasCopiedCount _ anyClosureHasCopiedCount + 1 ].
	].
	^{
		{'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. 
		{'WithClosuresAccessingOuterTemps'. anyClosureHasCopiedCount}.
		{'WithClosuresWritingOuterTemps'. numMethodsWithIndirectTemps}.
		{'WithMethodReturnsInClosures'. anyClosureDoesMethodReturnCount}. 
		{'WithReferencesToSelfInClosures'. anyClosureUsesSelfCount}. 
		{'BothAbove'. bothCount}.
		{'WithOnlyCleanClosures'. onlyCleanBlocksCount}.
	}! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 8/5/2011 18:52'!
eliotsClosureMeasurementsOn: m over: aFiveArgBlock
	"
	See senders.
	Or try something like:
		Smalltalk
			eliotsClosureMeasurementsOn: FileList >> #defaultContents
			over: [ :closuresCount :hasCopiedValuesForClosure :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
				(Array with: closuresCount with: hasCopiedValuesForClosure with: hasIndirectTemps with: anyClosureHasCopied with: anyClosureDoesMethodReturn with: anyClosureUsesSelf)]

	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	
	Note: This could perhaps be refactored to use the newer #embeddedBlockClosures and testing methods on the closures themselves.
	"
	| s nextScanStart thisClosureHasCopied closuresCount hasIndirectTemps blkPc blkSz anyClosureHasCopied anyClosureDoesMethodReturn anyClosureUsesSelf analyzedClosures |
	closuresCount := 0.
	hasIndirectTemps := false.
	anyClosureHasCopied :=  anyClosureDoesMethodReturn := anyClosureUsesSelf := false.
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
					anyClosureDoesMethodReturn := anyClosureDoesMethodReturn or: [s willReturn and: [s willBlockReturn not]].
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
			with: anyClosureDoesMethodReturn
			with: anyClosureUsesSelf)! !

!methodRemoval: BlockClosure #isClean!
BlockClosure removeSelector: #isClean!
!methodRemoval: BlockClosure #isTrivial!
BlockClosure removeSelector: #isTrivial!
