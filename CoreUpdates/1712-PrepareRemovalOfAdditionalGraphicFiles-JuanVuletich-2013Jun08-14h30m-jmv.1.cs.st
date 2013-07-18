'From Cuis 4.1 of 12 December 2012 [latest update: #1711] on 8 June 2013 at 2:31:55.02709 pm'!

!ContentPack methodsFor: 'exporting' stamp: 'jmv 6/8/2013 14:07'!
exportInto: aDirectory

	aDirectory assureExistence.

	self associations do: [ :assoc |
		| klass thisDirectory fullPath | 
			klass _ assoc value class.
			thisDirectory _ aDirectory directoryNamed: assoc key.
			fullPath _  aDirectory fullName , 
						aDirectory pathNameDelimiter asString ,
						assoc key.

		self flag: #note. "Add support for new file export type mappings here. --cbr"
		klass = ContentPack
			ifTrue: [ assoc value exportInto: thisDirectory ].
		
		klass = ColorForm
			ifTrue: [ assoc value writeBMPfileNamed: fullPath ].
			
		klass = Form
			ifTrue: [ assoc value writeBMPfileNamed: fullPath ]
	]
! !


!Morph methodsFor: 'menus' stamp: 'jmv 6/8/2013 14:09'!
addExportMenuItems: aMenu hand: aHandMorph
	"Add export items to the menu"

	aMenu ifNotNil: [
		| aSubMenu |
		aSubMenu _ MenuMorph new defaultTarget: self.
		aSubMenu add: 'BMP file' action: #exportAsBMP.
		aSubMenu add: 'JPEG file' action: #exportAsJPEG.
		aMenu add: 'export...' subMenu: aSubMenu]
! !


!Utilities class methodsFor: 'common requests' stamp: 'jmv 6/8/2013 14:10'!
saveDisplay
	"Save Display to a file"

	"SampledSound playSoundNamed: 'camera'."
	Display writeBMPfileNamed: 'CuisDisplay.bmp'! !

!Utilities class methodsFor: 'common requests' stamp: 'jmv 6/8/2013 14:11'!
saveScreenshot
	"Make a screenshot of the world and save it to a file"

	"SampledSound playSoundNamed: 'camera'."
	(self runningWorld imageForm: 32)  writeBMPfileNamed: 'CuisWorld.bmp'! !

!methodRemoval: Morph #exportAsPNG!
Morph removeSelector: #exportAsPNG!
