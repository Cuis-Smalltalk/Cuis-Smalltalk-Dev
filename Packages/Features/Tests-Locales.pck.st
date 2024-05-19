'From Cuis6.3 [latest update: #6247] on 2 March 2024 at 9:00:22 pm'!
'Description '!
!provides: 'Tests-Locales' 1 2!
!requires: 'Locales' 1 12 nil!
!requires: 'Gettext' 1 30 nil!
SystemOrganization addCategory: 'Tests-Locales'!


!classDefinition: #ISOLanguageDefinitionTest category: 'Tests-Locales'!
TestCase subclass: #ISOLanguageDefinitionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'ISOLanguageDefinitionTest class' category: 'Tests-Locales'!
ISOLanguageDefinitionTest class
	instanceVariableNames: ''!

!classDefinition: #LocaleIDTest category: 'Tests-Locales'!
TestCase subclass: #LocaleIDTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'LocaleIDTest class' category: 'Tests-Locales'!
LocaleIDTest class
	instanceVariableNames: ''!

!classDefinition: #LocaleTest category: 'Tests-Locales'!
TestCase subclass: #LocaleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'LocaleTest class' category: 'Tests-Locales'!
LocaleTest class
	instanceVariableNames: ''!

!classDefinition: #NaturalLanguageTranslatorTest category: 'Tests-Locales'!
TestCase subclass: #NaturalLanguageTranslatorTest
	instanceVariableNames: 'previousRegistered'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'NaturalLanguageTranslatorTest class' category: 'Tests-Locales'!
NaturalLanguageTranslatorTest class
	instanceVariableNames: ''!

!classDefinition: #RegisterableTranslatorMock category: 'Tests-Locales'!
Object subclass: #RegisterableTranslatorMock
	instanceVariableNames: 'translations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Locales'!
!classDefinition: 'RegisterableTranslatorMock class' category: 'Tests-Locales'!
RegisterableTranslatorMock class
	instanceVariableNames: ''!


!ISOLanguageDefinitionTest commentStamp: '' prior: 0!
Unit tests on ISO language definitions!

!LocaleIDTest commentStamp: '' prior: 0!
A unit test class for class LocaleID!

!LocaleTest commentStamp: '' prior: 0!
A unit test class for class Locale!

!NaturalLanguageTranslatorTest commentStamp: '' prior: 0!
Tests for NaturalLanguageTranslator!

!RegisterableTranslatorMock commentStamp: '' prior: 0!
I'm a mock object to mock a translator!

!ISOLanguageDefinitionTest methodsFor: 'tests'!
testISO2LanguageDefinition

	self 
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'de') language equals: 'German';
		assert: (ISOLanguageDefinition iso2LanguageDefinition: 'fr') language equals: 'French'! !

!ISOLanguageDefinitionTest methodsFor: 'tests'!
testISO3LanguageDefinition

	self 
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'deu') language equals: 'German';
		assert: (ISOLanguageDefinition iso3LanguageDefinition: 'fra') language equals: 'French'! !

!LocaleIDTest methodsFor: 'tests' stamp: 'hlsf 3/4/2022 09:17:47'!
testComparision

	self deny: self germanLocaleID = self frenchLocaleID.
	self assert: self germanLocaleID equals: self germanLocaleID.	! !

!LocaleIDTest methodsFor: 'tests'!
testFromISOString

	| locale |
	locale _ LocaleID isoString: 'en-us'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry equals: 'us'! !

!LocaleIDTest methodsFor: 'tests'!
testFromSingleISOString

	| locale |
	locale _ LocaleID isoString: 'en'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry isNil! !

!LocaleIDTest methodsFor: 'tests'!
testHashing

	self assert: self germanLocaleID hash equals: self germanLocaleID hash.
	self assert: self frenchLocaleID hash equals: self frenchLocaleID hash.	
! !

!LocaleIDTest methodsFor: 'tests'!
testInstanceCreationWithISOLanguage

	|germanLocale|
	germanLocale _ LocaleID isoLanguage: 'de'.
	self assert: germanLocale isoLanguage equals: 'de'! !

!LocaleIDTest methodsFor: 'tests'!
testInstanceCreationWithISOLanguageAndCountry

	|locale|
	locale _ LocaleID isoLanguage: 'zh' isoCountry: 'CN'.
	self 
		assert: locale isoLanguage equals: 'zh';
		assert: locale isoCountry equals: 'CN'! !

!LocaleIDTest methodsFor: 'tests'!
testPosixNameConversion

	| locale |
	locale _ LocaleID posixName: 'en_us'.
	self 
		assert: locale isoLanguage equals: 'en';
		assert: locale isoCountry equals: 'us'! !

!LocaleIDTest methodsFor: 'tests'!
testPrintString

	| locale |
	locale _ LocaleID isoString: 'en-us'.	
	self assert: locale printString equals: 'en-us'
! !

!LocaleIDTest methodsFor: 'tests - test data'!
frenchLocaleID

	^LocaleID isoLanguage: 'fr'! !

!LocaleIDTest methodsFor: 'tests - test data'!
germanLocaleID

	^LocaleID isoLanguage: 'de'! !

!LocaleTest methodsFor: 'tests'!
testCurrent

	self assert: Locale current notNil
	! !

!NaturalLanguageTranslatorTest methodsFor: 'running'!
setUp

	super setUp.
	previousRegistered _ NaturalLanguageTranslator current! !

!NaturalLanguageTranslatorTest methodsFor: 'running'!
tearDown

	NaturalLanguageTranslator current: previousRegistered.
	super tearDown! !

!NaturalLanguageTranslatorTest methodsFor: 'tests'!
testRegisterCustomTranslator

	NaturalLanguageTranslator current: RegisterableTranslatorMock new.
	
	"Our custom translator translates the string differently"
	self assert: 'TestCase' translated equals: 'Testfall'
	
	! !

!NaturalLanguageTranslatorTest methodsFor: 'tests'!
testTranslatePureString

	self assert: 'Pharo' translated equals: 'Pharo'! !

!RegisterableTranslatorMock methodsFor: 'initialization'!
initialize
	"Initializes the receiver"
	
	super initialize.
	translations _ Dictionary new.
	translations at: #'TestCase' put: 'Testfall'! !

!RegisterableTranslatorMock methodsFor: 'translate'!
translate: aString
	"Dispatch to a registered translator to translate the given string or (if there is no translator) return the untranslated string."
	^translations at: aString asSymbol ifAbsent: [ aString ] 
! !

!RegisterableTranslatorMock methodsFor: 'translate'!
translate: aString toLocale: localeID
 
	^self translate: aString! !
