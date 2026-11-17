'From Cuis7.9 [latest update: #8376] on 17 November 2026 at 10:41:37 am'!

!CodeHistory methodsFor: 'services' stamp: 'jmv 17/Nov/2026, 1:40:40 pm (UTC)'!
methodSourceAndMetadataFor: historyPointer into: aBlock
	"Evaluate a block with information for method stored at historyPointer (that points to source code)
		- method source code
		- method stamp string as saved ('' if not present)
		- method properties (nil if not present)
		- previousHistoryPointer (0 if not present)
		- nextHistoryPointer (0 if not present)
	
	CodeHistory current methodSourceAndMetadataFor: (Object >> #=) historyPointer
		into: [ :methodSource :stamp :methodProperties :prev :next |
			'-----' print.
			methodSource print.
			stamp print.
			methodProperties print.
			prev print.
			next print ]
	"

	| fileIndex position preamble tokens methodSource methodPropertiesString authorAndStampAsSaved prevHistoryPointer nextHistoryPointer |

	methodSource := nil.
	methodPropertiesString := ''.
	authorAndStampAsSaved := ''.
	
	historyPointer = 0 ifTrue: [
		aBlock valueWithArguments: {methodSource. authorAndStampAsSaved. methodPropertiesString. 0. 0}.
		^ self ].

	historyPointer < 16rFC000000
		ifTrue: [
			fileIndex := self libraryIndexAt: historyPointer.
			fileIndex = 16rFFFFFFFF ifTrue: [ fileIndex := 1 ]. "Sources"
			"Maybe at some point no CompiledMethod will point to .changes. Only CodeHistory and .sources"
			fileIndex = 16rFFFFFFFE ifTrue: [ fileIndex := 2 ]. "Changes"
			position := self positionIndexAt: historyPointer.
			prevHistoryPointer := self prevItemIndexAt: historyPointer.
			nextHistoryPointer := self nextItemIndexAt: historyPointer ]
		ifFalse: [
			fileIndex := self class fileIndexFromHistoryPointer: historyPointer.
			position := self class filePositionFromHistoryPointer: historyPointer.
			prevHistoryPointer := 0.
			nextHistoryPointer := 0 ].
	
	"Not if historyPointer points to a method removal. In that case, methodSource will be nil."
	(fileIndex = 0 and: [position = 0]) ifFalse: [
		 (SourceFiles at: fileIndex) name asFullFileEntry readStreamDo: [ :sourceFile |
			"Read source"
			sourceFile position: position.
			methodSource := sourceFile nextChunk withCuisLineEndings.
			"Point to source. Find Preamble"
			sourceFile position: position.
			sourceFile backChunk. "Skip back blank space filler."
			"Get preamble (and Method Properties if present)"
			preamble := sourceFile backChunk.
			(preamble beginsWith: 'methodProperties:') ifTrue: [
				methodPropertiesString := (preamble copyFrom: 1 + 'methodProperties:' size to: preamble size).
				sourceFile backChunk. "Skip back blank space."
				preamble := sourceFile backChunk ]].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		(preamble includesSubString: 'methodsFor:')
			ifTrue: [
				tokens := Scanner new scanTokens: preamble.
				authorAndStampAsSaved := CompiledMethod stampFrom: tokens.
				"At some point, #prior: pointer in changes file will no longer be used."
				prevHistoryPointer = 0 ifTrue: [ 	"CodeHistory takes precedence"
					prevHistoryPointer := (CompiledMethod priorReferenceFrom: tokens) ifNil: [ 0 ].
					prevHistoryPointer = 0 ifFalse: [ prevHistoryPointer := prevHistoryPointer + 16rFC000000 - 16r1000000 ]]].
	].

	aBlock valueWithArguments: {methodSource. authorAndStampAsSaved. methodPropertiesString. prevHistoryPointer. nextHistoryPointer}.! !

