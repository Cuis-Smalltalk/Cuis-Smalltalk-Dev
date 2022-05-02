'From Cuis 6.0 [latest update: #5117] on 2 May 2022 at 10:45:22 am'!

!CompiledMethod methodsFor: 'source code management' stamp: 'len 4/30/2022 18:38:39'!
getPreambleFrom: aFileStream at: position
	|  writeStream c p |
	writeStream _ String new writeStream.
	p _ position - 1.
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

