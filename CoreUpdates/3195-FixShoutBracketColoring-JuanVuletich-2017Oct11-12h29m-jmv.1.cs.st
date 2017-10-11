'From Cuis 5.0 of 7 November 2016 [latest update: #3194] on 11 October 2017 at 12:29:51 pm'!

!SHParserST80 methodsFor: 'parse support' stamp: 'tween 4/28/2004 09:48'!
enterBlock
	blockDepth := blockDepth + 1.
	bracketDepth := bracketDepth + 1! !

!SHParserST80 methodsFor: 'parse support' stamp: 'jmv 3/2/2010 10:06'!
leaveBlock
	arguments removeKey: blockDepth ifAbsent: nil.
	temporaries removeKey: blockDepth ifAbsent: nil.
	blockDepth := blockDepth - 1.
	bracketDepth := bracketDepth - 1! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 12/30/2016 10:51:03'!
parseBacktick
	self enterBlock.
	self scanPast: #backtick.
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $`.
	self scanPast: #backtick.
	self leaveBlock! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 10/11/2017 12:27:51'!
parseBlock

	"Just read $["
	blockDepths add: blockDepth+1.
	blockDepthsStartIndexes add: sourcePosition-1.

	self enterBlock.
	self scanPast: #blockStart level: bracketDepth.
	currentTokenFirst == $: ifTrue: [self parseBlockArguments].
	currentTokenFirst == $| ifTrue: [self parseBlockTemporaries].
	self parseStatementList.
	self failUnless: currentTokenFirst == $].

	"Just read $]"
	blockDepths add: blockDepth-1.
	blockDepthsStartIndexes add: sourcePosition.

	self scanPast: #blockEnd level: bracketDepth.
	self leaveBlock! !

