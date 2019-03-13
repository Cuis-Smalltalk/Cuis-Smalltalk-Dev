'From Cuis 5.0 of 7 November 2016 [latest update: #3660] on 13 March 2019 at 11:31:28 am'!

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 3/13/2019 11:30:42'!
parse: isAMethod 
	"Parse the receiver's text. If isAMethod is true
    then treat text as a method, if false as an
    expression with no message pattern"

	| continue prevSourcePosition |
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
		continue _ true.
		[ continue ] whileTrue: [
			prevSourcePosition _ sourcePosition.
			self parseStatementList.
			continue _ sourcePosition > prevSourcePosition.
			isAMethod
				ifTrue: [
					"Only if we are parsing a method, consider everything after this point as error."
					currentToken ifNotNil: [ self error ].
					continue _ false]
				ifFalse: [
					sourcePosition > source size ifTrue: [continue _ false]]].
	] ensure: [errorBlock _ nil].
	^true! !

