'From Cuis 5.0 [latest update: #4745] on 10 August 2021 at 10:36:39 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 8/10/2021 10:35:47'!
systemInformationString
	"Identify software version"
	^ SystemVersion current version, String newLineString,
		self lastUpdateString, String newLineString,
		'Running at :', self imageName.! !

