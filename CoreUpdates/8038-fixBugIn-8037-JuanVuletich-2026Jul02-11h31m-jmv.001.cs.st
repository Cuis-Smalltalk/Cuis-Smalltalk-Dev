'From Cuis7.9 [latest update: #8037] on 2 July 2026 at 11:33:05 am'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 2/Jul/2026, 3:31:13 pm (UTC)'!
userChangesFileName
	"
	Smalltalk userChangesFileName
	"
	^(FileIOAccessor default baseNameFor: self imageName pathAndLocalName second),
		(Utilities authorInitialsPerSe ifNotNil: [ :ai | '-', ai] ifNil: ['']),
		(Preferences at: #userChangesFileNameExtension)! !

