'From Cuis7.1 [latest update: #6536] on 26 July 2024 at 11:50:29 am'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'mir 7/23/2024 14:35:49'!
alternativeSourcesName
	"Answer the alternative full path to the sources file.
	If Sources not found at #defaultSourcesName (this image folder), 
	seek for them here (standard image folder)."
	"
	Smalltalk alternativeSourcesName
	"
	"Answer the default full path to the version-stable source code"
	^ self vmPath , 'CuisImage' , '/' , SourceFileVersionString , '.sources'! !

