'From Cuis 5.0 of 7 November 2016 [latest update: #3128] on 18 July 2017 at 10:23:11 am'!

!PluggableButtonMorph methodsFor: 'as yet unclassified' stamp: 'jmv 7/18/2017 10:22:53'!
morphExtent
	"Use extent if it has already been manually set, otherwise try to set it by computing from the label text and font, otherwise try using the icon extent, or finally fall back to the default value."
	^ extent ifNil: [
		extent := (self fontToUse notNil and: [ label notNil ])
			ifTrue: [ "Add a bit of padding"
				(self fontToUse widthOfString: label) + 10 @ (self fontToUse height + 10) ]
			ifFalse: [
				`20@15`
				"Usually button extent should not depend on icon extent. Icons are many times very big.
				For example, the icons in buttons in Taskbar are full size captures of the windows"
				"icon
					ifNil: [ `20@15` ]
					ifNotNil: [ icon extent ]"
				]].! !

