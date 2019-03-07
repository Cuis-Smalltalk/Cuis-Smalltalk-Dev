'From Cuis 5.0 of 7 November 2016 [latest update: #3645] on 7 March 2019 at 4:30:23 pm'!

!Browser methodsFor: 'class functions' stamp: 'HAW 3/7/2019 16:29:51'!
classDefinitionText
	"return the text to display for the definition of the currently selected class"
	
	^self selectedClassOrMetaClass 
		ifNil: [''] 
 		ifNotNil: [ :theClass | theClass definition]! !

