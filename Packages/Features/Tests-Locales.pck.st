'From Cuis7.7 [latest update: #7779] on 23 December 2025 at 4:40:34 pm'!
'Description '!
!provides: 'Tests-Locales' 1 2!
!requires: 'Locales' 1 12 nil!
!requires: 'Gettext' 1 30 nil!
SystemOrganization addCategory: #'Tests-Locales'!


!classDefinition: #RegisterableTranslatorMock category: #'Tests-Locales'!
Object subclass: #RegisterableTranslatorMock
	instanceVariableNames: 'translations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'RegisterableTranslatorMock class' category: #'Tests-Locales'!
RegisterableTranslatorMock class
	instanceVariableNames: ''!

!classDefinition: #ISOLanguageDefinitionTest category: #'Tests-Locales'!
TestCase subclass: #ISOLanguageDefinitionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'ISOLanguageDefinitionTest class' category: #'Tests-Locales'!
ISOLanguageDefinitionTest class
	instanceVariableNames: ''!

!classDefinition: #LocaleIDTest category: #'Tests-Locales'!
TestCase subclass: #LocaleIDTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'LocaleIDTest class' category: #'Tests-Locales'!
LocaleIDTest class
	instanceVariableNames: ''!

!classDefinition: #LocaleTest category: #'Tests-Locales'!
TestCase subclass: #LocaleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'LocaleTest class' category: #'Tests-Locales'!
LocaleTest class
	instanceVariableNames: ''!

!classDefinition: #NaturalLanguageTranslatorTest category: #'Tests-Locales'!
TestCase subclass: #NaturalLanguageTranslatorTest
	instanceVariableNames: 'previousRegistered'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'NaturalLanguageTranslatorTest class' category: #'Tests-Locales'!
NaturalLanguageTranslatorTest class
	instanceVariableNames: ''!


!RegisterableTranslatorMock commentStamp: '' prior: 0!
I'm a mock object to mock a translator!

!ISOLanguageDefinitionTest commentStamp: '' prior: 0!
Unit tests on ISO language definitions!

!LocaleIDTest commentStamp: '' prior: 0!
A unit test class for class LocaleID!

!LocaleTest commentStamp: '' prior: 0!
A unit test class for class Locale!

!NaturalLanguageTranslatorTest commentStamp: '' prior: 0!
Tests for NaturalLanguageTranslator!

!RegisterableTranslatorMock methodsFor: 'initialization' stamp: ''!
initialize
	"Initializes the receiver"
	
	super initialize.
	translations _ Dictionary new.
	translations at: #'TestCase' put: 'Testfall'! !

!RegisterableTranslatorMock methodsFor: 'translate' stamp: ''!
translate: aString
	"Dispatch to a registered translator to translate the given string or (if there is no translator) return the untranslated string."
	^translations at: aString asSymbol ifAbsent: [ aString ] 
! !

!RegisterableTranslatorMock methodsFor: 'translate' stamp: ''!
translate: aString toLocale: localeID
 
	^self translate: aString! !

!ISOLanguageDefinitionTest methodsFor: 'tests' stamp: ''!
testISO2LanguageDefinition

	self 
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'de') language equals: 'German';
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'fr') language equals: 'French'! !

!ISOLanguageDefinitionTest methodsFor: 'tests' stamp: ''!
testISO3LanguageDefinition

	self 
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'deu') language equals: 'German';
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'fra') language equals: 'French'! !

!LocaleIDTest methodsFor: 'tests' stamp: 'hlsf 4/Mar/2022 09:17:47'!
testComparision

	self deny: self germanLocaleID = self frenchLocaleID.
	self assert: self germanLocaleID equals: self germanLocaleID.	! !

!LocaleIDTest methodsFor: 'tests' stamp: ''!
testFromISOString

	| locale |
	locale _ LocaleID isoString: 'en-us'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry equals: 'us'! !

!LocaleIDTest methodsFor: 'tests' stamp: ''!
testFromSingleISOString

	| locale |
	locale _ LocaleID isoString: 'en'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry isNil! !

!LocaleIDTest methodsFor: 'tests' stamp: ''!
testHashing

	self assert: self germanLocaleID hash equals: self germanLocaleID hash.
	self assert: self frenchLocaleID hash equals: self frenchLocaleID hash.	
! !

!LocaleIDTest methodsFor: 'tests' stamp: ''!
testInstanceCreationWithISOLanguage

	|germanLocale|
	germanLocale _ LocaleID isoLanguage: 'de'.
	self assert: germanLocale isoLanguage equals: 'de'! !

!LocaleIDTest methodsFor: 'tests' stamp: ''!
testInstanceCreationWithISOLanguageAndCountry

	|locale|
	locale _ LocaleID isoLanguage: 'zh' isoCountry: 'CN'.
	self 
		assert: locale isoLanguage equals: 'zh';
		assert: locale isoCountry equals: 'CN'! !

!LocaleIDTest methodsFor: 'tests' stamp: ''!
testPosixNameConversion

	| locale |
	locale _ LocaleID posixName: 'en_us'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry equals: 'us'! !

!LocaleIDTest methodsFor: 'tests' stamp: ''!
testPrintString

	| locale |
	locale _ LocaleID isoString: 'en-us'.	
	self assert: locale printString equals: 'en-us'
! !

!LocaleIDTest methodsFor: 'tests - test data' stamp: ''!
frenchLocaleID

	^LocaleID isoLanguage: 'fr'! !

!LocaleIDTest methodsFor: 'tests - test data' stamp: ''!
germanLocaleID

	^LocaleID isoLanguage: 'de'! !

!LocaleTest methodsFor: 'tests' stamp: ''!
testCurrent

	self assert: Locale current notNil
	! !

!NaturalLanguageTranslatorTest methodsFor: 'running' stamp: ''!
setUp

	super setUp.
	previousRegistered _ NaturalLanguageTranslator current! !

!NaturalLanguageTranslatorTest methodsFor: 'running' stamp: ''!
tearDown

	NaturalLanguageTranslator current: previousRegistered.
	super tearDown! !

!NaturalLanguageTranslatorTest methodsFor: 'tests' stamp: ''!
testRegisterCustomTranslator

	NaturalLanguageTranslator current: RegisterableTranslatorMock new.
	
	"Our custom translator translates the string differently"
	self assert: 'TestCase' translated equals: 'Testfall'
	
	! !

!NaturalLanguageTranslatorTest methodsFor: 'tests' stamp: ''!
testTranslatePureString

	self assert: 'Pharo' translated equals: 'Pharo'! !
