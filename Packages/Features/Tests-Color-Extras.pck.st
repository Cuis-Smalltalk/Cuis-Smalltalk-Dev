'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 5:15:32 pm'!
'Description '!
!provides: 'Tests-Color-Extras' 1 1!
!requires: 'Color-Extras' 1 7 nil!
SystemOrganization addCategory: #'Tests-Color-Extras'!


!classDefinition: #ColorTest category: #'Tests-Color-Extras'!
TestCase subclass: #ColorTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Color-Extras'!
!classDefinition: 'ColorTest class' category: #'Tests-Color-Extras'!
ColorTest class
	instanceVariableNames: ''!


!ColorTest methodsFor: 'tests' stamp: 'sqr 12/14/2018 20:31:38'!
testIsGrey

	self assert: Color black isGrey.
	self assert: Color grey isGrey.
	self assert: Color white isGrey.
	self deny: Color red isGrey.
	self deny: Color green isGrey.
	self deny: Color blue isGrey! !
