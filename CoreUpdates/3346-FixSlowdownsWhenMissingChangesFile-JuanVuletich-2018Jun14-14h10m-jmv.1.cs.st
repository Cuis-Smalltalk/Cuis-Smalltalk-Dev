'From Cuis 5.0 of 7 November 2016 [latest update: #3345] on 14 June 2018 at 2:11:19 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 6/14/2018 14:06:32'!
getPreambleFrom: aFileStream at: position
	|  writeStream c p |
	writeStream _ String new writeStream.
	p _ position.
	c _ nil.
	aFileStream position: p.
	aFileStream atEnd ifTrue: [ ^ nil ].
	[ p >= 0 and: [ c ~~ $!! ]] whileTrue: [
		aFileStream position: p.
		c _ aFileStream basicNext.
		p _ p - 1 ].
	[ p >= 0] whileTrue: [
		aFileStream position: p.
		c _ aFileStream basicNext.
		c == $!!
			ifTrue: [^ writeStream contents reverse ]
			ifFalse: [ writeStream nextPut: c ].
		p _ p - 1 ].
	^ nil! !


!RemoteString methodsFor: 'accessing' stamp: 'jmv 6/14/2018 14:11:09'!
string 
	"Answer the receiver's string if remote files are enabled."
	| theFile answer |
	(sourceFileNumber == nil or: [(SourceFiles at: sourceFileNumber) == nil]) ifTrue: [^nil].
	theFile _ SourceFiles at: sourceFileNumber.
	theFile position: filePositionHi.
	answer _ theFile nextChunk.
	^answer isEmpty ifTrue: [nil] ifFalse: [answer]! !

