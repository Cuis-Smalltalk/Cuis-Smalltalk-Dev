'From Cuis 5.0 of 7 November 2016 [latest update: #3351] on 21 June 2018 at 2:06:28 pm'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 6/21/2018 13:41:37'!
parse: isAMethod 
	"Parse the receiver's text. If isAMethod is true
    then treat text as a method, if false as an
    expression with no message pattern"

	self initializeInstanceVariables.
	sourcePosition _ 1.
	arguments _ Dictionary new.
	temporaries _ Dictionary new.
	blockDepth _ bracketDepth := 0.
	blockDepths _ OrderedCollection with: blockDepth.
	blockDepthsStartIndexes _ OrderedCollection with: sourcePosition.
	ranges ifNil: [ ranges := OrderedCollection new: 100] ifNotNil: [ ranges reset].
	errorBlock _ [^false].
	[
		self scanNext.
		isAMethod 
			ifTrue: [
				self parseMessagePattern.
				self parsePragmaSequence].
		self parseMethodTemporaries.
		isAMethod ifTrue: [self parsePragmaSequence].
		"Iterate once for methods, but pontentially several times for workspaces
		(to recover after errors, for possible good next lines or chunks)"
		[ sourcePosition <= source size ] whileTrue: [
			self parseStatementList.
			currentToken ifNotNil: [
				"Only if we are parsing a method, consider everything after this point as error."
				isAMethod ifTrue: [self error]]
		].
	] ensure: [errorBlock _ nil].
	^true! !

