'From Cuis 5.0 [latest update: #4218] on 12 June 2020 at 11:30:20 am'!

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 6/11/2020 16:47:20'!
familyName: aString pointSize: aNumber
	"
	FontFamily familyName: FontFamily defaultFamilyName pointSize: 12
	FontFamily defaultFamilyPointSize: 12
	"
	| family |
	family _ AvailableFamilies at: aString ifAbsent: [^nil].
	^family atPointSize: aNumber! !

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 6/11/2020 16:52:09'!
familyNamed: aString
	"
	FontFamily familyNamed: FontFamily defaultFamilyName
	"
	^AvailableFamilies at: aString ifAbsent: [].! !

!FontFamily class methodsFor: 'accessing fonts' stamp: 'jmv 6/11/2020 16:39:34'!
pointSizesFor: aString
	"
	FontFamily pointSizesFor: FontFamily defaultFamilyName
	"
	| family |
	family _ AvailableFamilies at: aString ifAbsent: [^nil].
	^family pointSizes! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
dict _ (FontFamily bindingOf: 'AvailableFamilies') value.
#('DejaVu Sans Mono' 'DejaVu') do: [ :familyName |
	family _ FontFamily familyNamed: familyName.
	family class = StrikeFontFamily ifTrue: [
		newName _ familyName, ' Bitmap'.
		family instVarNamed: 'familyName' put: newName.
		dict at: newName put: family.
		dict removeKey: familyName.
		FontFamily defaultFamilyName = familyName ifTrue: [
			FontPicker setDefaultFont: newName.
			"FontFamily defaultFamilyName: newName defaultPointSize: nil" ].
		StrikeFont allInstancesDo: [ :sf |
			((sf name beginsWith: familyName) and: [ (sf name beginsWith: newName) not])
				ifTrue: [ sf name: (newName, (sf name copyFrom: familyName size+1 to: sf name size)) ]]
	].
].
StrikeFont allInstances collect: [ :sf | sf pointSize > 14 ifTrue: [ sf setGlyphsDepthAtMost: 4 ]].
UISupervisor whenUIinSafeState: [self runningWorld recreateDefaultDesktop]
!

