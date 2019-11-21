'From Cuis 5.0 [latest update: #3944] on 1 November 2019 at 4:37:44 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'HAW 11/1/2019 16:37:26'!
defaultUserChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultUserChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), 
		Preferences userChangesFileNameExtension ! !

