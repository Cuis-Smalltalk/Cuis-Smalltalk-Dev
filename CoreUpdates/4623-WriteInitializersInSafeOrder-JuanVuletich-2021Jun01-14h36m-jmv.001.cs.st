'From Cuis 5.0 [latest update: #4622] on 1 June 2021 at 2:37:09 pm'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 6/1/2021 14:36:41'!
write: classes initializersOn: aStream
	"Write the call to #initialize method of classes defined in us."

	classes do: [ :class |
		(class class includesSelector: #initialize) ifTrue: [
			aStream nextChunkPut: class name, ' initialize'; newLine ]]! !

