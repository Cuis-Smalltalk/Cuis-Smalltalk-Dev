'From Cuis6.3 [latest update: #6218] on 7 February 2024 at 4:41:56 pm'!

!SequenceableCollection class methodsFor: 'stream creation' stamp: 'jmv 2/7/2024 16:28:16'!
streamContents: blockWithArg on: aCollection

	| stream originalContents |
	
	stream := aCollection writeStream.
	blockWithArg value: stream.
	originalContents := stream originalContents.
	
	^stream position = originalContents size
		ifTrue: [ originalContents asStreamResult ]
		ifFalse: [ stream contents ]! !


!OrderedCollection class methodsFor: 'stream creation' stamp: 'jmv 2/7/2024 16:29:14'!
streamContents: blockWithArg

	^self streamContents: blockWithArg on: self new! !

!OrderedCollection class methodsFor: 'stream creation' stamp: 'jmv 2/7/2024 16:39:22'!
streamContents: blockWithArg estimatedSize: estimatedSize
	"Compatibility. Simply ignore size hint."

	^self streamContents: blockWithArg! !


!SequenceableCollection class methodsFor: 'stream creation' stamp: 'jmv 2/7/2024 16:30:56'!
streamContents: blockWithArg estimatedSize: estimatedSize

	^self streamContents: blockWithArg on: (self new: estimatedSize)! !

