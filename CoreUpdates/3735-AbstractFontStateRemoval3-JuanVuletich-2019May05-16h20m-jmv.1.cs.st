'From Cuis 5.0 of 7 November 2016 [latest update: #3729] on 5 May 2019 at 4:21:34 pm'!
!classDefinition: #AbstractFont category: #'Graphics-Text'!
Object subclass: #AbstractFont
	instanceVariableNames: ''
	classVariableNames: 'AvailableFonts DefaultFont '
	poolDictionaries: ''
	category: 'Graphics-Text'!
!methodRemoval: FontFamily class #initialize!
FontFamily class removeSelector: #initialize!
!methodRemoval: FontFamily class #migrate!
FontFamily class removeSelector: #migrate!
!methodRemoval: AbstractFont class #availableFonts!
AbstractFont class removeSelector: #availableFonts!
!classDefinition: #AbstractFont category: #'Graphics-Text'!
Object subclass: #AbstractFont
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
FontFamily initialize!
