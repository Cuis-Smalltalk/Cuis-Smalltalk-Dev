'From Cuis7.9 [latest update: #8036] on 30 June 2026 at 2:46:29 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 30/Jun/2026, 6:45:28 pm (UTC)'!
userChangesFileName
	"
	Smalltalk userChangesFileName
	"
	^(FileIOAccessor default baseNameFor: self imageName pathAndLocalName second),
		(Utilities authorInitialsPerSe ifNotNil: [ :ai | '-', ai]),
		(Preferences at: #userChangesFileNameExtension)! !

