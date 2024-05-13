'From Cuis6.3 [latest update: #6360] on 12 May 2024 at 6:54:05 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 5/12/2024 18:53:31'!
primImageName: newName
	"Set the the full path name for the current image.  All further snapshots will use this."

	<primitive: 121>
	^ self primitiveFailed! !


!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 5/12/2024 18:53:53'!
changeImageNameTo: aString
	self primImageName: aString asByteString.
	LastImageName := self imageName! !

!methodRemoval: SystemDictionary #imageName: stamp: 'jmv 5/12/2024 18:53:55'!
SystemDictionary removeSelector: #imageName:!
