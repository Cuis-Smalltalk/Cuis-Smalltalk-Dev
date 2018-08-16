'From Cuis 5.0 of 7 November 2016 [latest update: #3418] on 15 August 2018 at 7:57:57 pm'!

!Compiler methodsFor: 'private' stamp: 'HAW 8/15/2018 19:56:18'!
translate: aStream noPattern: noPattern ifFail: failBlock
	^self parser
		sourceStreamGetter: sourceStreamGetter;			"Cuis specific. Do not remove!!"
		ignoreBacktick: false;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !

