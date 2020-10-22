'From Cuis 5.0 [latest update: #4415] on 22 October 2020 at 9:22:39 pm'!
'Description '!
!provides: 'Tests-ExtendedClipboard' 1 0!
!requires: 'ExtendedClipboard' 1 12 nil!
SystemOrganization addCategory: 'Tests-ExtendedClipboard'!


!classDefinition: #ExtendedClipboardTest category: 'Tests-ExtendedClipboard'!
TestCase subclass: #ExtendedClipboardTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-ExtendedClipboard'!
!classDefinition: 'ExtendedClipboardTest class' category: 'Tests-ExtendedClipboard'!
ExtendedClipboardTest class
	instanceVariableNames: ''!


!ExtendedClipboardTest methodsFor: 'testing' stamp: 'jmv 7/14/2015 22:57'!
testStar16BitToAndFromImageClipboard
	"
	ExtendedClipboardTest new testStar16BitToAndFromImageClipboard
	"
	| form form1 form2 |
	form _ (Display copy: (10@10 extent: 60@40)) asFormOfDepth: 16.

	"This will use a refStreamed object"
	Clipboard default storeObject: form.
	form2 _ Clipboard default retrieveObject.
	form1 _ form copy.
	form1 addDeltasFrom: form2.
	self assert: form1 primCountBits = 0 description: 'Form copyPasted from Clipboard appears to be bad.'.

	"This will also test storing and retrieving PNG data from platform Clipboard, and PNG conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (PNGReadWriter bytesFor: form)  dataFormat: 'public.png'.
	form2 _ Clipboard default retrieveObject.
	form1 _ form copy.
	form1 addDeltasFrom: form2.
	self assert: form1 primCountBits = 0 description: 'Form copyPasted from Clipboard appears to be bad.'.! !

!ExtendedClipboardTest methodsFor: 'testing' stamp: 'jmv 4/2/2016 14:40'!
testStar8BitToAndFromImageClipboard
	"
	ExtendedClipboardTest new testStar8BitToAndFromImageClipboard
	"
	| form form1 form2 |
	self assert: Clipboard default extendedClipboardInterface canStore description: 'Extended Clipboard cant store'.

	form _ (Display copy: (10@10 extent: 60@40)) asFormOfDepth: 8.

	"This will use a refStreamed object"
	Clipboard default storeObject: form.
	form2 _ Clipboard default retrieveObject.
	self assert: (form2 is: #Form).
	form1 _ form copy.
	form1 addDeltasFrom: form2.
	self assert: form1 primCountBits = 0 description: 'Form copyPasted from Clipboard appears to be bad.'.

	"This will also test storing and retrieving PNG data from platform Clipboard, and PNG conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (PNGReadWriter bytesFor: form)  dataFormat: 'public.png'.
	form2 _ Clipboard default retrieveObject.
	form1 _ form copy.
	form1 addDeltasFrom: form2.
	self assert: form1 primCountBits = 0 description: 'Form copyPasted from Clipboard appears to be bad.'.! !

!ExtendedClipboardTest methodsFor: 'testing' stamp: 'jmv 7/14/2015 22:57'!
testStarToAndFromImageClipboard
	"
	ExtendedClipboardTest new testStarToAndFromImageClipboard
	"
	| form form1 form2 |
	form _ Display copy: (10@10 extent: 60@40).

	"This will use a refStreamed object"
	Clipboard default storeObject: form.
	form2 _ Clipboard default retrieveObject.
	form1 _ form copy.
	form1 addDeltasFrom: form2.
	self assert: form1 primCountBits = 0 description: 'Form copyPasted from Clipboard appears to be bad.'.

	"This will also test storing and retrieving PNG data from platform Clipboard, and PNG conversion"
	ExtendedClipboardInterface current
		clearClipboard;
		addClipboardData: (PNGReadWriter bytesFor: form)  dataFormat: 'public.png'.
	form2 _ Clipboard default retrieveObject.
	form1 _ form copy.
	form1 addDeltasFrom: form2.
	self assert: form1 primCountBits = 0 description: 'Form copyPasted from Clipboard appears to be bad.'.! !
